window.MvcKata = {}

$ ->
  enemey = if ((Math.random() * 100) % 2) > 1
    new MvcKata.Slime()
  else
    new MvcKata.Dragon()

  player = new MvcKata.Player()
  
  new MvcKata.BattleView(model: player, enemey: enemey).render()

class MvcKata.Slime extends Backbone.Model
  defaults:
    name: 'SLIME'
    hp: 10
    attackPower: 4
    exp: 1
    message: ''

  attack: (player) ->

class MvcKata.Dragon extends Backbone.Model
  defaults:
    name: 'DRAGON'
    hp: 20
    attackPower: 8
    exp: 3000
    message: ''

  attack: (player) ->

class MvcKata.Player extends Backbone.Model
  defaults:
    name: 'PLAYER'
    hp: 10
    mp: 20
    attackPower: 3
    message: ''

  attack: (enemey) ->

  hoimi: ->

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

  render: =>
    $(@el).html @template()
    @$('.message').html new MvcKata.EventHistoryView(model: @model, enemey: @options.enemey).render().el
    @$('.action').html new MvcKata.EventMenuView(model: @model, enemey: @options.enemery).render().el

  setJa: ->
    window.locale = 'ja'
    @render()

  setEn: ->
    window.locale = 'en'
    @render()

class MvcKata.EventHistoryView extends Backbone.View
  templateJa: _.template '''
    <h1>ほげほげ</h1>
  '''

  templateEn: _.template '''
    <h1>hogehoge</h1>
  '''

  render: =>
    if window.locale == 'ja'
      $(@el).html @templateJa()
    else
      $(@el).html @templateEn()
    this

class MvcKata.EventMenuView extends Backbone.View
  initialize: ->
    @model.bind 'change', @render

  events:
    'click .attack': 'attack'
    'click .hoimi': 'hoimi'

  templateJa: _.template '''
    <div><%= name %>のHP: <%= hp %></div>
    <ol>
      <li><a href="#" class="attack">アタック</a></li>
      <li><a href="#" class="hoimi">ホイミ</a></li>
    </ol>
  '''

  templateEn: _.template '''
     <div><%= name %> HP: <%= hp %></div>
    <ol>
      <li><a href="#" class="attack">Attack</a></li>
      <li><a href="#" class="hoimi">Hoimi</a></li>
    </ol>
  '''

  render: =>
    if window.locale == 'ja'
      $(@el).html @templateJa(@model.toJSON())
    else
      $(@el).html  @templateEn(@model.toJSON())
    this

  attack: ->
    @model.attack(@options.enemey)
    @enemey.attack(@model)

  hoimi: ->
    @model.hoimi
    @enemey.attack(@model)
