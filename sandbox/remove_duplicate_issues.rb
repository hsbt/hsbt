#!/usr/bin/env ruby

require 'pathname'
require 'set'

def process_file(file_path)
  lines = File.readlines(file_path)
  
  # ファイルを解析してセクションを特定
  sections = {}
  current_section = nil
  section_start_line = 0
  
  lines.each_with_index do |line, i|
    if line.strip.start_with?('##')
      if current_section
        sections[current_section] = [section_start_line, i]
      end
      current_section = line.strip
      section_start_line = i
    end
  end
  
  # 最後のセクション
  if current_section
    sections[current_section] = [section_start_line, lines.length]
  end
  
  # issueとpull requestセクションが存在するかチェック
  return [false, 0] unless sections.key?('## issue') && sections.key?('## pull request')
  
  # issueセクションの内容を取得
  issue_start, issue_end = sections['## issue']
  issue_lines = lines[(issue_start + 1)...issue_end]
  
  # pull requestセクションの内容を取得
  pr_start, pr_end = sections['## pull request']
  pr_lines = lines[(pr_start + 1)...pr_end]
  
  # PR項目からURLを抽出
  pr_urls = Set.new
  pr_lines.each do |line|
    if !line.strip.empty? && line.strip.start_with?('*')
      url_match = line.match(/https:\/\/github\.com\/[^)]+/)
      if url_match
        pr_urls.add(url_match[0])
      end
    end
  end
  
  # 削除対象のissue行を特定
  lines_to_remove = []
  duplicates_count = 0
  
  issue_lines.each do |line|
    if !line.strip.empty? && line.strip.start_with?('*') && !line.include?("No issues found")
      url_match = line.match(/https:\/\/github\.com\/[^)]+/)
      if url_match && pr_urls.include?(url_match[0])
        lines_to_remove << line
        duplicates_count += 1
      end
    end
  end
  
  return [false, 0] if duplicates_count == 0
  
  # 新しいissueセクションを作成
  new_issue_lines = issue_lines.reject { |line| lines_to_remove.include?(line) }
  
  # 残りのissue項目があるかチェック
  has_issues = new_issue_lines.any? do |line|
    !line.strip.empty? && line.strip.start_with?('*') && !line.include?("No issues found")
  end
  
  unless has_issues
    new_issue_lines = ["  * No issues found\n"]
  end
  
  # 新しいファイル内容を構築
  new_lines = []
  new_lines.concat(lines[0..issue_start])      # issue セクションのヘッダーまで
  new_lines.concat(new_issue_lines)            # 新しいissue内容
  new_lines.concat(lines[issue_end..-1])       # issue セクション以降
  
  # ファイルに書き込み
  File.write(file_path, new_lines.join)
  
  [true, duplicates_count]
end

def main
  github_dir = "/Users/hsbt/Documents/github.com/hsbt/vault/github"
  md_files = Dir.glob(File.join(github_dir, "*.md"))
  
  total_files_modified = 0
  total_duplicates_removed = 0
  
  md_files.each do |file_path|
    was_modified, duplicates_removed = process_file(file_path)
    if was_modified
      total_files_modified += 1
      total_duplicates_removed += duplicates_removed
      puts "Modified #{File.basename(file_path)}: removed #{duplicates_removed} duplicated issue(s)"
    end
  end
  
  puts "\nSummary: Modified #{total_files_modified} files, removed #{total_duplicates_removed} duplicated issues"
end

if __FILE__ == $0
  main
end
