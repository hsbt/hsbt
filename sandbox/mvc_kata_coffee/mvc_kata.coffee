window.MvcKata = {}

$ ->
  enemey = if ((Math.random() * 100) % 2) > 1
    new MvcKata.Slime()
  else
    new MvcKata.Dragon()

  player = new MvcKata.Player()
  
  new MvcKata.BattleView(model: player, enemey: enemey).render()

class MvcKata.Slime extends Backbone.Model
  initialize: ->
    @set
      name: 'SLIME'
      hp: 10
      attackPower: 4
      exp: 1

class MvcKata.Dragon extends Backbone.Model
  initialize: ->
    @set
      name: 'DRAGON'
      hp: 20
      attackPower: 8
      exp: 3000

class MvcKata.Player extends Backbone.Model
  initialize: ->
    @set
      name: 'PLAYER'
      hp: 10
      mp: 20
      attackPower: 3  

class MvcKata.BattleView extends Backbone.View
  el: '#container'

  events:
    'click .setJa': 'setJa'
    'click .setEn': 'setEn'

  template: _.template '''
    <h1>MVC Kata</h1>
    <div class='locale'>
      <a href="#" class="setJa">日本語</a> / <a href="#" class="setEn">English</a>
    </div>
    <div class="message"></div>
    <div class="action"></div>
  '''

  templateJa: _.template '''
    <h1>ほげほげ</h1>
  '''

  templateEn: _.template '''
    <h1>hogehoge</h1>
  '''

  render: =>
    $(@el).html @template()

    if window.locale == 'ja'
      $('.message').html @templateJa()
    else
      $('.message').html @templateEn()

  setJa: ->
    window.locale = 'ja'
    @render()

  setEn: ->
    window.locale = 'en'
    @render()

class MvcKata.EventHistoryView extends Backbone.View

class MvcKata.EventMenuView extends Backbone.View
