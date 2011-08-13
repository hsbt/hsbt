window.MvcKata = {}

$ ->
  enemey = if Math.floor((Math.random() * 100) % 2) == 1
    new MvcKata.Slime()
  else
    new MvcKata.Dragon()

  player = new MvcKata.Player()

  new MvcKata.BattleView(model: player, enemey: enemey).render()

class MvcKata.Living extends Backbone.Model
  defaults:
    message: ''

  attack: (target) ->
    damage_point = Math.floor((Math.random() * @get('attackPower')) + 1)

    if window.locale == 'ja'
      @set message: @get('name') + ' のこうげき'
    else
      @set message: @get('name') + ' attack.'

    target.set hp: target.get('hp') - damage_point

    if window.locale == 'ja'
      @set message: target.get('name') + "に" + damage_point + "のダメージ"
    else
      @set message: target.get('name') + " damaged " + damage_point + " point(s)"

class MvcKata.Slime extends MvcKata.Living
  defaults:
    name: 'SLIME'
    hp: 10
    attackPower: 4
    exp: 1

  attack: (player) ->
    MvcKata.Living.prototype.attack.call(this, player)

class MvcKata.Dragon extends MvcKata.Living
  defaults:
    name: 'DRAGON'
    hp: 20
    attackPower: 8
    exp: 3000

  attack: (player) ->
    MvcKata.Living.prototype.attack.call(this, player)

class MvcKata.Player extends MvcKata.Living
  defaults:
    name: 'PLAYER'
    maxHp: 10
    hp: 10
    mp: 20
    attackPower: 3
    turnCount: 0

  attack: (enemey) ->
    MvcKata.Living.prototype.attack.call(this, enemey)

  hoimi: ->
    cure_point = Math.floor((Math.random() * 8) + 1)

    if window.locale == 'ja'
      @set message: @get('name') + ' はホイミをとなえた'
    else
      @set message: @get('name') + ' call hoimi.'

    @set hp: (_.min([cure_point + @get('hp'), @get('maxHp')]))

    if window.locale == 'ja'
      @set message: "HPが" + cure_point + "回復した"
    else
      @set message: @get('name') + " cured " + cure_point + " point(s)"

  encount: (target) ->
    if window.locale == 'ja'
      @set message: target.get('name') + "があらわれた"
    else
      @set message: @get('name') + " encounted a " + target.get('name')

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
  '''

  render: =>
    $(@el).html @template()

    $(@el).append new MvcKata.EventMenuView(model: @model, enemey: @options.enemey).render().el
    $(@el).append new MvcKata.EventHistoryView(model: @model, enemey: @options.enemey).render().el

    @model.encount(@options.enemey)

  setJa: ->
    window.locale = 'ja'
    @render()

  setEn: ->
    window.locale = 'en'
    @render()

class MvcKata.EventHistoryView extends Backbone.View
  initialize: ->
    @model.bind 'change', @renderPlayerMessage
    @options.enemey.bind 'change', @renderEnemeyMessage

  template: _.template '''
    <div class="message"></div>
  '''

  messageTemplate: _.template '''
    <p><%= message %></p>
  '''

  render: =>
    $(@el).html @template()

    this

  renderPlayerMessage: =>
    return unless @model.get('message')
    @$('.message').append @messageTemplate(@model.toJSON())
    @model.set message: ''

  renderEnemeyMessage: =>
    return unless @options.enemey.get('message')
    @$('.message').append @messageTemplate(@options.enemey.toJSON())
    @options.enemey.set message: ''

class MvcKata.EventMenuView extends Backbone.View
  initialize: ->
    @model.bind 'change', @render

  events:
    'click .attack': 'attack'
    'click .hoimi': 'hoimi'

  template: _.template '''
    <div class="action"></div>
  '''

  templateJa: _.template '''
    <div><%= name %>のHP: <%= hp %>/<%= maxHp %> ターン数: <%= turnCount %></div>
    <ol>
      <li><a href="#" class="attack">アタック</a></li>
      <li><a href="#" class="hoimi">ホイミ</a></li>
    </ol>
  '''

  templateEn: _.template '''
     <div><%= name %> HP: <%= hp %>/<%= maxHp %> Turn: <%= turnCount %></div>
    <ol>
      <li><a href="#" class="attack">Attack</a></li>
      <li><a href="#" class="hoimi">Hoimi</a></li>
    </ol>
  '''

  render: =>
    $(@el).html @template()

    if window.locale == 'ja'
      @$('.action').html @templateJa(@model.toJSON())
    else
      @$('.action').html  @templateEn(@model.toJSON())

    this

  attack: ->
    @model.attack(@options.enemey)
    @options.enemey.attack(@model)

    @model.set turnCount: @model.get('turnCount') + 1

  hoimi: ->
    @model.hoimi()
    @options.enemey.attack(@model)

    @model.set turnCount: @model.get('turnCount') + 1
