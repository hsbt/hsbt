# slide.rb プラグイン
#
# www.hsbt.org/slide/ に置いた発表資料を、最初のページの画像とタイトルを並べたカードで表示する。
#   {{slide '20261121-SendaiRubyKaigi03'}}
# 引数にはスライドページの URL も渡せる。タイトルとイベント名は mina deploy が
# PDF の隣に置く slides.json から読む。編集画面には新しい資料を並べ、クリックで本文に追加できる。
require 'json'

def slide(name)
  name = name.to_s[%r{/slide/([^/]+)/?\z}, 1] || name.to_s
  url = "https://www.hsbt.org/slide/#{name}/"
  talk = slide_talks[name]
  return %Q|<a href="#{h url}">#{h url}</a>| unless talk

  %Q|<a class="slide-card" href="#{h url}">| +
    %Q|<img src="https://www.hsbt.org/slide/#{h name}.jpg" alt="">| +
    %Q|<span class="slide-card-title">#{h talk['title']}</span>| +
    %Q|<span class="slide-card-description">#{h talk['description']}</span></a>|
end

def slide_talks
  @slide_talks ||= begin
    JSON.parse(File.read(@conf['slide.json'] || '/home/ubuntu/www/hsbt.org/slide/slides.json'))
  rescue SystemCallError, JSON::ParserError
    {}
  end
end

add_header_proc do
  <<~HTML
    <style>
    a.slide-card { display: block; max-width: 480px; margin: 1em 0; border: 1px solid #ddd; border-radius: 6px; overflow: hidden; color: inherit; text-decoration: none; }
    a.slide-card img { display: block; width: 100%; height: auto; margin: 0; border: 0; }
    a.slide-card span { display: block; padding: 0 0.8em; }
    a.slide-card .slide-card-title { padding-top: 0.5em; font-weight: bold; }
    a.slide-card .slide-card-description { padding-bottom: 0.5em; font-size: 90%; color: #666; }
    </style>
  HTML
end

add_form_proc do |date|
  talks = slide_talks.sort.reverse.first(5)
  next '' if talks.empty?

  items = talks.map do |name, talk|
    %Q|<img class="slide-pick" src="https://www.hsbt.org/slide/#{h name}.jpg" data-name="#{h name}" title="#{h talk['title']}" alt="#{h name}" width="160">|
  end
  <<~HTML
    <div class="form">
    <div class="caption">発表資料 - 画像をクリックすると本文に追加できます</div>
    <div>#{items.join("\n")}</div>
    </div>
    <script>
    $(document).on('click', '.slide-pick', function(){
      $('#body').insertAtCaret($.makePluginTag('slide', [$(this).attr('data-name')]));
    });
    </script>
  HTML
end
