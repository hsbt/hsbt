#!/usr/bin/env ruby
# encoding: utf-8

require 'fileutils'
require 'pathname'
require 'tmpdir'
require 'zip'  # rubyzip gem is used with 'zip' require name
require 'nokogiri'
require 'securerandom'
require 'optparse'
require 'time'

# Command line argument settings
options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: #{File.basename(__FILE__)} [options]"
  opts.on("--repo-url=URL", "MatzEssays repository URL (default: https://github.com/asciidwango/MatzEssays)") do |url|
    options[:repo_url] = url
  end
end.parse!

options[:repo_url] ||= "https://github.com/asciidwango/MatzEssays"

# Set up working directory
BASE_DIR = Pathname.new(File.dirname(File.expand_path(__FILE__)))
OUTPUT_DIR = BASE_DIR.join("output")
FileUtils.mkdir_p(OUTPUT_DIR)

# Function to clone repository from GitHub
def clone_repository(repo_url)
  puts "Cloning repository: #{repo_url}"
  
  # Create temporary directory
  tmp_dir = Dir.mktmpdir("matz_essays_")
  
  # Clone repository using git command
  result = system("git", "clone", "--depth=1", repo_url, tmp_dir)
  
  unless result
    FileUtils.remove_entry_secure(tmp_dir) if File.directory?(tmp_dir)
    raise "Failed to clone repository: #{repo_url}"
  end
  
  return Pathname.new(tmp_dir)
end

# Function to create basic EPUB structure
def create_epub_structure(temp_dir, volume)
  # Create standard EPUB directory structure
  meta_inf_dir = temp_dir.join("META-INF")
  FileUtils.mkdir_p(meta_inf_dir)
  
  oebps_dir = temp_dir.join("OEBPS")
  FileUtils.mkdir_p(oebps_dir)
  
  # Create necessary subdirectories
  oebps_images_dir = oebps_dir.join("images")
  FileUtils.mkdir_p(oebps_images_dir)
  
  oebps_styles_dir = oebps_dir.join("styles")
  FileUtils.mkdir_p(oebps_styles_dir)
  
  oebps_text_dir = oebps_dir.join("text")
  FileUtils.mkdir_p(oebps_text_dir)
  
  # Create container.xml
  container_xml = meta_inf_dir.join("container.xml")
  File.open(container_xml, "w", encoding: "utf-8") do |f|
    f.write(<<~XML)
      <?xml version="1.0" encoding="UTF-8"?>
      <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
          <rootfiles>
              <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
          </rootfiles>
      </container>
    XML
  end
  
  # Create mimetype file
  File.open(temp_dir.join("mimetype"), "w", encoding: "utf-8") do |f|
    f.write("application/epub+zip")
  end
  
  return [oebps_dir, oebps_text_dir, oebps_images_dir, oebps_styles_dir]
end

# Function to copy content files
def copy_content_files(volume_path, oebps_dir, oebps_text_dir, oebps_images_dir, oebps_styles_dir)
  # Copy XHTML files
  xhtml_dir = volume_path.join("xhtml")
  Dir.glob(xhtml_dir.join("*.xhtml")).each do |xhtml_file|
    FileUtils.cp(xhtml_file, oebps_text_dir)
  end
  
  # Copy image files
  image_dir = volume_path.join("image")
  Dir.glob(image_dir.join("**/*")) do |img_file|
    if File.file?(img_file)
      # Create corresponding subdirectory
      relative_path = Pathname.new(img_file).relative_path_from(image_dir)
      target_dir = oebps_images_dir.join(relative_path.dirname)
      FileUtils.mkdir_p(target_dir)
      FileUtils.cp(img_file, target_dir.join(File.basename(img_file)))
    end
  end
  
  # Copy stylesheets
  style_dir = volume_path.parent.join("style")
  Dir.glob(style_dir.join("*.css")).each do |css_file|
    FileUtils.cp(css_file, oebps_styles_dir)
  end
  
  return collect_files_info(oebps_text_dir, oebps_images_dir, oebps_styles_dir, volume_path)
end

# Function to collect file information
def collect_files_info(text_dir, images_dir, styles_dir, volume_path)
  text_files = Dir.glob(text_dir.join("*.xhtml")).sort.map { |f| Pathname.new(f) }
  image_files = []
  Dir.glob(images_dir.join("**/*")).each do |file|
    if File.file?(file) && file =~ /\.(jpg|jpeg|png|gif)$/i
      image_files << Pathname.new(file)
    end
  end
  style_files = Dir.glob(styles_dir.join("*.css")).sort.map { |f| Pathname.new(f) }
  
  # Read table of contents information from index.xhtml
  toc_items = []
  spine_items = []
  begin
    doc = Nokogiri::HTML(File.read(volume_path.join("index.xhtml")))
    
    # Find table of contents entries
    doc.css("a").each do |link|
      href = link['href']
      if href && href.start_with?("xhtml/")
        # Modify relative path
        modified_href = "text/" + File.basename(href)
        title = link.text
        if title && !title.empty?
          toc_items << [title, modified_href]
          # Add to spine_items without duplication
          spine_item = File.basename(href)
          spine_items << spine_item unless spine_items.include?(spine_item)
        end
      end
    end
  rescue => e
    puts "Error occurred while getting table of contents information: #{e}"
    # If an error occurs, simply sort by file name
    spine_items = text_files.map(&:basename).map(&:to_s).sort
  end
  
  {
    "text_files" => text_files,
    "image_files" => image_files,
    "style_files" => style_files,
    "toc_items" => toc_items,
    "spine_items" => spine_items
  }
end

# Function to generate OPF file
def create_opf_file(oebps_dir, files_info, volume, title)
  opf_path = oebps_dir.join("content.opf")
  
  # Generate UUID
  book_id = "urn:uuid:#{SecureRandom.uuid}"
  
  File.open(opf_path, "w", encoding: "utf-8") do |f|
    f.puts <<~XML
      <?xml version="1.0" encoding="UTF-8"?>
      <package xmlns="http://www.idpf.org/2007/opf" version="3.0" unique-identifier="BookID">
          <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
              <dc:identifier id="BookID">#{book_id}</dc:identifier>
              <dc:title>#{title}</dc:title>
              <dc:creator>Yukihiro Matsumoto</dc:creator>
              <dc:language>ja</dc:language>
              <dc:publisher>ASCII DWANGO</dc:publisher>
              <meta property="dcterms:modified">#{Time.now.utc.strftime('%Y-%m-%dT%H:%M:%SZ')}</meta>
    XML
    
    # Set cover image (if exists)
    if files_info["cover_image"]
      f.puts "        <meta name=\"cover\" content=\"cover-image\"/>"
    end
    
    f.puts "      </metadata>"
    f.puts "      <manifest>"
    
    # Cover file (if exists)
    if files_info["cover_image"]
      f.puts "        <item id=\"cover\" href=\"cover.xhtml\" media-type=\"application/xhtml+xml\" properties=\"svg\"/>"
      f.puts "        <item id=\"cover-image\" href=\"#{files_info["cover_image"]}\" media-type=\"image/jpeg\"/>"
    end
    
    # Add all files to manifest
    
    # Text files
    files_info["text_files"].each_with_index do |file_path, i|
      file_name = file_path.basename.to_s
      media_type = "application/xhtml+xml"
      f.puts "        <item id=\"text_#{i}\" href=\"text/#{file_name}\" media-type=\"#{media_type}\"/>"
    end
    
    # Image files
    files_info["image_files"].each_with_index do |file_path, i|
      relative_path = file_path.relative_path_from(oebps_dir)
      file_ext = File.extname(file_path).downcase
      media_type = case file_ext
                   when '.jpg', '.jpeg'
                     "image/jpeg"
                   when '.png'
                     "image/png"
                   when '.gif'
                     "image/gif"
                   else
                     "image/jpeg"  # Default
                   end
      
      # Skip cover image ID as already defined
      next if files_info["cover_image"] && relative_path.to_s == files_info["cover_image"] 
      
      f.puts "        <item id=\"image_#{i}\" href=\"#{relative_path}\" media-type=\"#{media_type}\"/>"
    end
    
    # Style files
    files_info["style_files"].each_with_index do |file_path, i|
      file_name = file_path.basename.to_s
      f.puts "        <item id=\"style_#{i}\" href=\"styles/#{file_name}\" media-type=\"text/css\"/>"
    end
    
    # NCX file
    f.puts '        <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>'
    f.puts '        <item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav"/>'
    
    # spine and guide elements
    f.puts '    </manifest>'
    f.puts '    <spine toc="ncx">'
    
    # Add cover to beginning of spine (if exists)
    if files_info["cover_image"]
      f.puts '        <itemref idref="cover" linear="yes"/>'
    end
    
    # Spine items
    files_info["spine_items"].each_with_index do |file_name, i|
      text_id = -1
      files_info["text_files"].each_with_index do |text_file, j|
        if text_file.basename.to_s == file_name
          text_id = j
          break
        end
      end
      
      if text_id >= 0
        f.puts "        <itemref idref=\"text_#{text_id}\" linear=\"yes\"/>"
      end
    end
    
    f.puts '    </spine>'
    
    # Add guide section
    f.puts '    <guide>'
    if files_info["cover_image"]
      f.puts '        <reference type="cover" title="Cover" href="cover.xhtml"/>'
    end
    f.puts '    </guide>'
    
    f.puts '</package>'
  end
end

# Function to create NCX file (table of contents)
def create_ncx_file(oebps_dir, files_info, title)
  ncx_path = oebps_dir.join("toc.ncx")
  
  File.open(ncx_path, "w", encoding: "utf-8") do |f|
    f.puts <<~XML
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">
      <ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">
          <head>
              <meta name="dtb:uid" content="urn:uuid:12345"/>
              <meta name="dtb:depth" content="1"/>
              <meta name="dtb:totalPageCount" content="0"/>
              <meta name="dtb:maxPageNumber" content="0"/>
          </head>
          <docTitle>
              <text>#{title}</text>
          </docTitle>
          <navMap>
    XML
    
    # Table of contents items
    files_info["toc_items"].each_with_index do |(nav_title, nav_href), i|
      f.puts <<~XML
              <navPoint id="navPoint-#{i+1}" playOrder="#{i+1}">
                  <navLabel>
                      <text>#{nav_title}</text>
                  </navLabel>
                  <content src="#{nav_href}"/>
              </navPoint>
      XML
    end
    
    f.puts '      </navMap>'
    f.puts '</ncx>'
  end
end

# Function to create navigation document
def create_nav_document(oebps_dir, files_info, title)
  nav_path = oebps_dir.join("nav.xhtml")
  
  File.open(nav_path, "w", encoding: "utf-8") do |f|
    f.puts <<~HTML
      <?xml version="1.0" encoding="utf-8"?>
      <!DOCTYPE html>
      <html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops" lang="ja" xml:lang="ja">
      <head>
          <title>#{title} - Table of Contents</title>
          <meta charset="utf-8"/>
      </head>
      <body>
          <nav epub:type="toc" id="toc">
              <h1>Table of Contents</h1>
              <ol>
    HTML
    
    # Table of contents items
    files_info["toc_items"].each do |nav_title, nav_href|
      f.puts "                  <li><a href=\"#{nav_href}\">#{nav_title}</a></li>"
    end
    
    f.puts <<~HTML
              </ol>
          </nav>
      </body>
      </html>
    HTML
  end
end

# Function to fix XHTML links in files
def fix_file_links(oebps_text_dir)
  Dir.glob(oebps_text_dir.join("*.xhtml")) do |xhtml_file|
    content = File.read(xhtml_file, encoding: "utf-8")
    
    # Fix CSS link paths
    content = content.gsub('../style/', '../styles/')
    
    # Fix internal link paths
    content = content.gsub(/href="(\.\.\/)?xhtml\//, 'href="')
    
    # Fix image paths
    content = content.gsub(/src="(\.\.\/)?image\//, 'src="../images/')
    
    File.open(xhtml_file, "w", encoding: "utf-8") do |f|
      f.write(content)
    end
  end
end

# Function to create EPUB file
def create_epub(volume, source_dir, output_path)
  puts "Creating EPUB for #{volume}..."
  
  volume_path = source_dir.join(volume)
  title = "Matz Essays #{volume.gsub('vol', 'Volume ')}"
  
  # Path to cover image
  cover_image_path = volume_path.join("image", "cover.jpg")
  # Alternative path
  if !File.exist?(cover_image_path)
    cover_image_path = volume_path.parent.join('image', "#{volume}-cover.jpg")
  end
  
  # Delete output file if it already exists
  FileUtils.rm_f(output_path) if File.exist?(output_path)
  
  # Create temporary directory
  Dir.mktmpdir do |temp_dir_str|
    temp_dir = Pathname.new(temp_dir_str)
    
    # Create EPUB directory structure
    oebps_dir, oebps_text_dir, oebps_images_dir, oebps_styles_dir = create_epub_structure(temp_dir, volume)
    
    # Copy cover image
    has_cover = false
    if File.exist?(cover_image_path)
      cover_target_path = oebps_images_dir.join("cover.jpg")
      FileUtils.cp(cover_image_path, cover_target_path)
      has_cover = true
      puts "Set cover image: #{cover_image_path}"
    else
      puts "Warning: Cover image not found: #{cover_image_path}"
    end
    
    # Copy content files
    files_info = copy_content_files(volume_path, oebps_dir, oebps_text_dir, oebps_images_dir, oebps_styles_dir)
    
    # Add cover image information
    if has_cover
      files_info["cover_image"] = "images/cover.jpg"
    end
    
    # Fix links in files
    fix_file_links(oebps_text_dir)
    
    # Create necessary metadata files
    create_opf_file(oebps_dir, files_info, volume, title)
    create_ncx_file(oebps_dir, files_info, title)
    create_nav_document(oebps_dir, files_info, title)
    
    # Create cover XHTML (if cover image exists)
    if has_cover
      create_cover_html(oebps_dir)
    end
    
    # Create EPUB file
    Zip::File.open(output_path, Zip::File::CREATE) do |epub_zip|
      # mimetype must be added first and without compression
      epub_zip.add('mimetype', temp_dir.join('mimetype'))
      
      # Add all other files
      Dir.glob("#{temp_dir}/**/*").each do |file|
        next if File.directory?(file)
        next if File.basename(file) == 'mimetype' # Already added
        
        arc_name = Pathname.new(file).relative_path_from(temp_dir).to_s
        epub_zip.add(arc_name, file)
      end
    end
  end
  
  puts "Created #{output_path}."
end

# Function to create cover HTML file
def create_cover_html(oebps_dir)
  cover_path = oebps_dir.join("cover.xhtml")
  
  File.open(cover_path, "w", encoding: "utf-8") do |f|
    f.puts <<~HTML
      <?xml version="1.0" encoding="utf-8"?>
      <!DOCTYPE html>
      <html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops" lang="ja" xml:lang="ja">
      <head>
        <title>Cover</title>
        <meta charset="utf-8"/>
        <style type="text/css">
          html, body {
            margin: 0;
            padding: 0;
            width: 100%;
            height: 100%;
            overflow: hidden;
          }
          body {
            display: flex;
            justify-content: center;
            align-items: center;
          }
          img {
            max-width: 100%;
            max-height: 100%;
            object-fit: contain;
          }
        </style>
      </head>
      <body epub:type="cover">
        <img src="images/cover.jpg" alt="Cover Image" />
      </body>
      </html>
    HTML
  end
end

# Main process
begin
  puts "Cloning MatzEssays repository..."
  repo_dir = clone_repository(options[:repo_url])
  
  begin
    puts "Repository clone completed: #{repo_dir}"
    
    # Always process both vol1 and vol2
    volumes = ['vol1', 'vol2']
    
    volumes.each do |vol|
      output_path = OUTPUT_DIR.join("matz_essays_#{vol}.epub")
      create_epub(vol, repo_dir, output_path)
      puts "EPUB conversion completed for #{vol}: #{output_path}"
    end
  ensure
    # Delete cloned repository
    if repo_dir && File.directory?(repo_dir)
      puts "Removing temporary directory: #{repo_dir}"
      FileUtils.remove_entry_secure(repo_dir)
    end
  end
  
  puts "Processing completed."
rescue => e
  puts "An error occurred: #{e}"
  puts e.backtrace
  exit(1)
end