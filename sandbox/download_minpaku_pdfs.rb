require 'open-uri'
require 'fileutils'
require 'date'

BASE_URL = "https://www.r.minpaku.ac.jp/gekkan_minpaku/pdf/"
# Save PDFs in a subdirectory relative to the script's location
# スクリプトの場所を基準としたサブディレクトリにPDFを保存します
SCRIPT_DIR = File.dirname(__FILE__)
OUTPUT_DIR = File.join(SCRIPT_DIR, "minpaku_pdfs")

FileUtils.mkdir_p(OUTPUT_DIR) unless Dir.exist?(OUTPUT_DIR)

start_date = Date.new(2005, 4, 1)
end_date = Date.new(2024, 12, 1)

current_date = start_date
while current_date <= end_date
  year = current_date.year
  month = current_date.month

  # YY (year % 100) and MM (month) should be two digits with leading zeros if necessary
  # YY (年 % 100) と MM (月) は、必要に応じて先頭にゼロを付けて2桁にする必要があります
  yy = format('%02d', year % 100)
  mm = format('%02d', month)

  filename = "MP#{yy}#{mm}.pdf"
  url = "#{BASE_URL}#{filename}"
  output_path = File.join(OUTPUT_DIR, filename)

  if File.exist?(output_path)
    puts "File #{filename} already exists. Skipping."
    # ファイル #{filename} は既に存在します。スキップします。
    current_date = current_date.next_month
    next
  end

  puts "Downloading #{url} to #{output_path}..."
  # #{url} を #{output_path} にダウンロードしています...

  begin
    # In Ruby 3.0+, open-uri's open method was renamed to URI.open
    # Ruby 3.0以降、open-uriのopenメソッドはURI.openに名前が変更されました
    if URI.respond_to?(:open)
      URI.open(url) do |file|
        File.open(output_path, 'wb') do |output_file|
          output_file.write(file.read)
        end
      end
    else # older Rubies
      open(url) do |file| # rubocop:disable Security/Open
        File.open(output_path, 'wb') do |output_file|
          output_file.write(file.read)
        end
      end
    end
    puts "Successfully downloaded #{filename}."
    # #{filename} のダウンロードに成功しました。
  rescue OpenURI::HTTPError => e
    puts "Error downloading #{filename}: #{e.message}. Skipping."
    # #{filename} のダウンロード中にエラーが発生しました: #{e.message}。スキップします。
  rescue StandardError => e
    puts "An unexpected error occurred while downloading #{filename}: #{e.message}. Skipping."
    # #{filename} のダウンロード中に予期せぬエラーが発生しました: #{e.message}。スキップします。
  end

  current_date = current_date.next_month
end

puts "Download process finished. Files are saved in #{OUTPUT_DIR}."
# ダウンロード処理が完了しました。ファイルは #{OUTPUT_DIR} に保存されています。
