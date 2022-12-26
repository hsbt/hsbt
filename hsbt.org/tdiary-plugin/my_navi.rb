# my_navi.rb プラグイン
#
# 通常のnaviに、カレンダーとRSS feedのアイコンを追加。
# dropdown_calendar.rbを有効にしておくこと。
# @options['dropdown_calendar.label']は空('')にしておく。
def navi
  result = %(<div class="adminmenu">\n)
  result << calendar
  result << navi_user
  result << navi_admin
  result << %(<a href="index.rdf"><img style="border-width: 0px;" src="https://www.hsbt.org/diary/images/feed-icon-12x12.png" width="12" height="12" alt="RSS feed"></a>)
  result << %(</div>)
end
