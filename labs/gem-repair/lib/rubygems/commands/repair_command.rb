require 'rubygems/command'
require 'rubygems/installer'


class Gem::Commands::RepairCommand < Gem::Command
  def initialize
    super 'repair', 'Repairs gems with missing extensions by reinstalling them'

    add_option('-j', '--jobs JOBS', Integer,
               'Number of parallel threads to use (default: 4)') do |value, options|
      options[:jobs] = value
    end
  end

  def arguments # :nodoc:
    ""
  end

  def description # :nodoc:
    <<-EOF
The repair command finds all installed gems that are missing their compiled
extensions and attempts to reinstall them. This can be useful after upgrading
Ruby or changing system libraries.

Use -j to specify the number of parallel threads (default: 4).
    EOF
  end

  def usage # :nodoc:
    "#{program_name} [options]"
  end

  def execute
    say "Searching for gems with missing extensions..."

    specs = Gem::Specification.select do |spec|
      spec.platform == RUBY_ENGINE && spec.respond_to?(:missing_extensions?) && spec.missing_extensions?
    end

    specs = specs.group_by { |s| [s.name, s.version] }.map do |_, gems|
      non_default_specs = gems.select { |s| !s.default_gem? }
      if non_default_specs.empty?
        gems
      else
        non_default_specs
      end
    end.flatten(1).shuffle

    if specs.empty?
      say "No gems found with missing extensions."
      return
    end

    say "Found #{specs.count} gem(s) to repair: #{specs.map(&:full_name).join(', ')}"

    queue = Queue.new
    specs.each { |spec| queue << spec }

    threads = []
    # Get number of threads from -j option, default to 4
    num_threads = options[:jobs] || 4

    say "Repairing gems using #{num_threads} parallel threads..."

    num_threads.times do
      threads << Thread.new do
        while !queue.empty? && (spec = queue.pop(true) rescue nil)
          begin
            say "Repairing #{spec.full_name}..."
            # Ensure spec.base_dir is correct and writable
            # The installer might need specific options, ensure they are correctly set up
            installer_options = {
              wrappers: true,
              force: true, # Reinstall even if it appears installed
              install_dir: spec.base_dir, # Install into the same location
              env_shebang: true,
              build_args: spec.build_args,
              # Add other options as needed, e.g., :user_install => false if installing to system gems
              # ignore_dependencies: true # Usually good for a restore/pristine operation
            }

            # Use spec.cache_file if available and valid, otherwise the installer might re-download
            # Forcing a specific installer might be needed if default behavior isn't right
            installer = Gem::Installer.at(spec.cache_file, installer_options)
            installer.install
            say "Successfully repaired #{spec.full_name} to #{spec.base_dir}"
          rescue Gem::Ext::BuildError, Gem::Package::FormatError, Gem::InstallError, Zlib::BufError, NameError => e
            alert_error "Failed to repair #{spec.full_name}: #{e.message}\n  Backtrace: #{e.backtrace.join("\n             ")}"
          rescue => e
            alert_error "An unexpected error occurred while repairing #{spec.full_name}: #{e.message}\n  Backtrace: #{e.backtrace.join("\n             ")}"
          end
        end
      end
    end

    threads.each(&:join)
    say "Gem repair process complete."
  end
end
