#!/usr/bin/env ruby

require 'net/http'
require 'uri'
require 'json'
require 'date'
require 'optparse'

# PEPABOの記事一覧ページのベースURL
BASE_URL = 'https://tech.pepabo.com/authors/'

def fetch_page(url, redirect_limit = 5)
  raise "Too many redirects" if redirect_limit == 0
  
  uri = URI.parse(url)
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  request = Net::HTTP::Get.new(uri.request_uri)
  response = http.request(request)
  
  case response
  when Net::HTTPSuccess
    # エンコーディングを UTF-8 に強制
    body = response.body
    if body.encoding == Encoding::ASCII_8BIT || !body.valid_encoding?
      # Content-Type ヘッダーからエンコーディングを取得
      content_type = response['Content-Type']
      charset = content_type.match(/charset=([^;]*)/i)&.captures&.first || 'utf-8'
      
      begin
        # 指定されたエンコーディングで解釈し、UTF-8に変換
        body.force_encoding(charset)
        body = body.encode('UTF-8', invalid: :replace, undef: :replace)
      rescue => e
        # エンコーディングエラーが発生した場合は強制的にUTF-8として扱う
        body.force_encoding('UTF-8')
        body.encode!('UTF-8', invalid: :replace, undef: :replace)
      end
    end
    body
  when Net::HTTPRedirection
    location = response['location']
    puts "リダイレクト: #{url} -> #{location}"
    fetch_page(location, redirect_limit - 1)
  else
    raise "HTTP Error: #{response.code} #{response.message}"
  end
end

def extract_article_links(html)
  links = []
  
  # 記事のリンクを抽出（日付付きのリンクを探す）
  html.scan(/href=["']([^"']*\/\d{4}\/\d{2}\/\d{2}\/[^"']*)["']/i) do |match|
    url = match[0]
    # 相対URLの場合は絶対URLに変換
    full_url = url.start_with?('http') ? url : "https://tech.pepabo.com#{url}"
    links << full_url unless links.include?(full_url)
  end
  
  links.uniq
end

def extract_article_content(html, url)
  # タイトルを取得
  title = nil
  if html.match(/<h1[^>]*>(.*?)<\/h1>/mi)
    title = $1.gsub(/<[^>]*>/, '').strip
  elsif html.match(/<title[^>]*>(.*?)<\/title>/mi)
    title = $1.gsub(/<[^>]*>/, '').strip
  end
  
  # 日付をURLから抽出
  date_match = url.match(/\/(\d{4})\/(\d{2})\/(\d{2})\//)
  if date_match
    year, month, day = date_match[1], date_match[2], date_match[3]
    filename = "#{year}#{month}#{day}.md"
  else
    filename = "unknown_date.md"
  end
  
  # 記事本文を抽出 - より堅牢な方法に変更
  content = ""
  article_body_match = html.match(/<div[^>]*class=["']article__body["'][^>]*>(.*)<\/div>\s*<footer/mi)
  if article_body_match
    content = extract_text_from_html(article_body_match[1])
  elsif html.match(/<article[^>]*>(.*?)<\/article>/mi)
    content = extract_text_from_html($1)
  else
    content = "記事の本文を取得できませんでした。"
  end
  
  {
    title: title,
    content: content,
    filename: filename,
    url: url
  }
end



def extract_text_from_html(html)
  # エンコーディングが正しいことを確認
  if html.encoding != Encoding::UTF_8 || !html.valid_encoding?
    html = html.encode('UTF-8', invalid: :replace, undef: :replace)
  end
  
  # まず、コードブロックを安全に変換する
  code_blocks = []
  code_placeholder = "CODE_BLOCK_PLACEHOLDER_"
  
  # コードブロックを抽出して一時的なプレースホルダーに置き換え
  html = html.gsub(/<div class="highlight"><pre class="highlight .*?"><code>(.*?)<\/code><\/pre><\/div>/mi) do |match|
    code_content = $1.to_s
    # spanタグなどを除去
    code_content = code_content.gsub(/<span[^>]*>(.*?)<\/span>/mi, '\1')
    # HTMLエンティティをデコード
    code_content = code_content.gsub(/&lt;/, '<').gsub(/&gt;/, '>').gsub(/&amp;/, '&')
    placeholder = "#{code_placeholder}#{code_blocks.length}"
    code_blocks << "\n```\n#{code_content.strip}\n```\n\n"
    placeholder
  end
  
  # スクリプトとスタイルを除去
  text = html.gsub(/<script[^>]*>.*?<\/script>/mi, '')
            .gsub(/<style[^>]*>.*?<\/style>/mi, '')
  
  # ブロック要素の変換
  text = text.gsub(/<h1[^>]*>(.*?)<\/h1>/mi) { "\n# #{$1.gsub(/<[^>]*>/, '').strip}\n\n" }
            .gsub(/<h2[^>]*>(.*?)<\/h2>/mi) { "\n## #{$1.gsub(/<[^>]*>/, '').strip}\n\n" }
            .gsub(/<h3[^>]*>(.*?)<\/h3>/mi) { "\n### #{$1.gsub(/<[^>]*>/, '').strip}\n\n" }
            .gsub(/<h4[^>]*>(.*?)<\/h4>/mi) { "\n#### #{$1.gsub(/<[^>]*>/, '').strip}\n\n" }
            .gsub(/<h5[^>]*>(.*?)<\/h5>/mi) { "\n##### #{$1.gsub(/<[^>]*>/, '').strip}\n\n" }
            .gsub(/<h6[^>]*>(.*?)<\/h6>/mi) { "\n###### #{$1.gsub(/<[^>]*>/, '').strip}\n\n" }
            .gsub(/<blockquote[^>]*>(.*?)<\/blockquote>/mi) { "\n> #{$1.gsub(/<[^>]*>/, '').strip}\n\n" }
            .gsub(/<ul[^>]*>(.*?)<\/ul>/mi) { convert_list($1) }
            .gsub(/<ol[^>]*>(.*?)<\/ol>/mi) { convert_list($1, true) }
  
  # インライン要素の変換
  text = text.gsub(/<a[^>]*href=["']([^"']*)["'][^>]*>(.*?)<\/a>/mi) do
    link_url = $1.to_s
    link_text = $2.gsub(/<[^>]*>/, '').strip
    
    # 空のリンクや "#" だけのリンクの場合は適切なURLを設定
    if link_url.empty? || link_url == "#"
      # 既知のリンク対応表
      known_links = {
        'furik' => 'https://github.com/pepabo/furik',
        '@hsbt' => 'https://github.com/hsbt',
        'hsbt' => 'https://github.com/hsbt',
        'Pit' => 'https://github.com/cho45/pit',
        '大切にしてほしい3つのこと' => 'http://pepabo.com/recruit/important/',
        'GMOペパボ株式会社 採用サイト' => 'https://recruit.pepabo.com/'
      }
      
      if known_links[link_text]
        "[#{link_text}](#{known_links[link_text]})"
      else
        # 既知のリンクでない場合は、テキスト自体をリンクテキストとして保存
        "[#{link_text}](https://www.google.com/search?q=#{URI.encode_www_form_component(link_text)})"
      end
    else
      # 通常のリンク
      "[#{link_text}](#{link_url})"
    end
  end
            .gsub(/<strong[^>]*>(.*?)<\/strong>/mi) { "**#{$1.gsub(/<[^>]*>/, '').strip}**" }
            .gsub(/<b[^>]*>(.*?)<\/b>/mi) { "**#{$1.gsub(/<[^>]*>/, '').strip}**" }
            .gsub(/<em[^>]*>(.*?)<\/em>/mi) { "*#{$1.gsub(/<[^>]*>/, '').strip}*" }
            .gsub(/<i[^>]*>(.*?)<\/i>/mi) { "*#{$1.gsub(/<[^>]*>/, '').strip}*" }
            .gsub(/<code[^>]*>(.*?)<\/code>/mi) { "`#{$1.gsub(/<[^>]*>/, '').strip}`" }
            .gsub(/<img[^>]*alt=["']([^"']*)["'][^>]*src=["']([^"']*)["'][^>]*\/?>/mi) { "![#{$1}](#{$2})" }
            .gsub(/<img[^>]*src=["']([^"']*)["'][^>]*alt=["']([^"']*)["'][^>]*\/?>/mi) { "![#{$2}](#{$1})" }
            .gsub(/<img[^>]*src=["']([^"']*)["'][^>]*\/?>/mi) { "![](#{$1})" }
  
  # 改行タグの処理
  text = text.gsub(/<br\s*\/?>/i, "\n")
  
  # 段落タグの処理
  text = text.gsub(/<p[^>]*>(.*?)<\/p>/mi) { "#{$1.gsub(/<[^>]*>/, '').strip}\n\n" }
  
  # 残りのHTMLタグを除去
  text = text.gsub(/<[^>]*>/, '')
  
  # 文字エンティティのデコード
  text = text.gsub(/&amp;/, '&')
            .gsub(/&lt;/, '<')
            .gsub(/&gt;/, '>')
            .gsub(/&quot;/, '"')
            .gsub(/&#39;/, "'")
            .gsub(/&nbsp;/, ' ')
  
  # 余った空白や改行を整理
  text = text.gsub(/[ \t]+/, ' ')
            .gsub(/\n[ \t]+/, "\n")
            .gsub(/\n{3,}/, "\n\n")
            .strip
  
  # プレースホルダーをコードブロックに戻す
  code_blocks.each_with_index do |code, i|
    text = text.gsub("CODE_BLOCK_PLACEHOLDER_#{i}", code)
  end
  
  text
end

def convert_list(list_content, ordered = false)
  items = list_content.scan(/<li[^>]*>(.*?)<\/li>/mi)
  result = "\n"
  items.each_with_index do |item, index|
    content = item[0].gsub(/<[^>]*>/, '').strip
    if ordered
      result += "#{index + 1}. #{content}\n"
    else
      result += "- #{content}\n"
    end
  end
  result += "\n"
  result
end

def save_article(article_data, output_dir)
  filepath = File.join(output_dir, article_data[:filename])
  
  # ファイルが既に存在する場合は確認
  if File.exist?(filepath)
    puts "#{article_data[:filename]} は既に存在します。スキップします。"
    return
  end
  
  # "## GMOペパボでは、新しい仲間を募集しています" 以降の内容を削除する処理
  clean_content = article_data[:content].split(/^## GMOペパボでは、新しい仲間を募集しています/).first.to_s.strip

  content = "# #{article_data[:title]}\n\n"
  content += "**URL:** #{article_data[:url]}\n\n"
  content += "---\n\n"
  content += clean_content
  
  File.write(filepath, content)
  puts "保存しました: #{filepath}"
end

# メイン処理
def main
  options = {}

  # コマンドラインオプションの解析
  OptionParser.new do |opts|
    opts.banner = "使用法: ruby fetch_pepabo_articles.rb [オプション]"

    opts.on("-a", "--author AUTHOR", "著者名を指定（必須）") do |a|
      options[:author] = a
    end

    opts.on("-o", "--output-dir DIR", "出力先ディレクトリを指定（必須）") do |o|
      options[:output_dir] = o
    end

    opts.on("-h", "--help", "ヘルプを表示") do
      puts opts
      exit
    end
  end.parse!
  
  # 必須オプションの確認
  if options[:author].nil? || options[:output_dir].nil?
    puts "エラー: author と output-dir の両方を指定してください"
    exit 1
  end

  output_dir = options[:output_dir]
  author = options[:author]
  author_url = "#{BASE_URL}#{author}"
  
  # 出力ディレクトリが存在しない場合は作成
  Dir.mkdir(output_dir) unless Dir.exist?(output_dir)
  
  puts "#{author} の記事一覧を取得中..."
  author_page_html = fetch_page(author_url)
  article_links = extract_article_links(author_page_html)
  
  puts "#{article_links.length} 件の記事を発見しました。"
  
  article_links.each_with_index do |url, index|
    puts "\n進行状況: #{index + 1}/#{article_links.length}"
    puts "取得中: #{url}"
    
    begin
      html = fetch_page(url)
      article_data = extract_article_content(html, url)
      
      if article_data[:title]
        save_article(article_data, output_dir)
      else
        puts "  エラー: タイトルを取得できませんでした"
      end
      
      # サーバーに負荷をかけないよう少し待機
      sleep(1)
      
    rescue => e
      puts "  エラー: #{e.message}"
      next
    end
  end
  
  puts "\n完了しました！"
end

if __FILE__ == $0
  main
end
