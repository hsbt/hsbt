#!/usr/bin/env ruby
# rb-branch-backport.rb - Script for creating backport branches and applying commits
# Usage: rb-branch-backport.rb <commit-hash> <Ruby-version>
# Example: rb-branch-backport.rb a1b2c3d 3.2
#          rb-branch-backport.rb a1b2c3d all  # Apply to all maintained branches (3.1, 3.2, 3.3)

require 'fileutils'

# Check arguments
commit_hash = ARGV[0]
ruby_version = ARGV[1]

if commit_hash.nil? || ruby_version.nil?
  puts "Usage: rb-branch-backport.rb <commit-hash> <Ruby-version>"
  puts "Example: rb-branch-backport.rb a1b2c3d 3.2"
  puts "         rb-branch-backport.rb a1b2c3d all  # Apply to all maintained branches"
  exit 1
end

# Define target versions when 'all' is specified
ruby_versions = if ruby_version.downcase == 'all'
                  ['3.1', '3.2', '3.3']  # Maintained Ruby versions
                else
                  [ruby_version]
                end

# Save current directory to restore later
original_dir = Dir.pwd

# Process each Ruby version
ruby_versions.each do |version|
  puts "\n=== Processing Ruby #{version} ==="
  
  # Convert Ruby version format (e.g., 3.2 → ruby_3_2)
  base_branch = "ruby_#{version.gsub('.', '_')}"
  # Create new branch name (e.g., gh-a1b2c3d-3-2)
  new_branch = "gh-#{commit_hash}-#{version.gsub('.', '-')}"
  
  puts "Base branch: #{base_branch}"
  puts "New branch: #{new_branch}"
  
  # Move to Ruby repository directory
  Dir.chdir("../ruby.github") do
    # Save current branch
    current_branch = `git branch --show-current`.strip
    
    puts "Updating base branch (#{base_branch}) to latest state..."
    system("git checkout #{base_branch}") || abort("Failed to switch to #{base_branch} branch")
    system("git pull --ff-only origin #{base_branch}") || abort("Failed to update #{base_branch} branch")
    
    puts "Creating new branch (#{new_branch})..."
    system("git checkout -b #{new_branch}") || abort("Failed to create new branch")
    
    puts "Cherry-picking commit #{commit_hash}..."
    cherry_pick_result = system("git cherry-pick #{commit_hash}")
    
    if cherry_pick_result
      puts "Successfully cherry-picked commit #{commit_hash}"
      puts "If needed, resolve conflicts and execute the following commands:"
      puts "  git cherry-pick --continue"
      puts "  git push fork #{new_branch}"
    else
      puts "Failed to cherry-pick commit #{commit_hash}"
      puts "If cherry-pick conflicts occurred, resolve the conflicts and then run:"
      puts "  git cherry-pick --continue"
      puts "  git push fork #{new_branch}"
      puts ""
      puts "To cancel cherry-pick:"
      puts "  git cherry-pick --abort"
      puts "  git checkout #{current_branch}"
      puts "  git branch -D #{new_branch}"
    end
    
    # Return to the original branch if not the last version
    if version != ruby_versions.last
      puts "Returning to original branch: #{current_branch}"
      system("git checkout #{current_branch}")
    end
  end
end

puts "All backport operations completed"