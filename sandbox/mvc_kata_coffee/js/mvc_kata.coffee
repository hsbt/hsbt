window.MvcKata = {}

$ ->
  enemey = if Math.floor((Math.random() * 100) % 2) == 1
    new MvcKata.Slime()
  else
    new MvcKata.Dragon()

  player = new MvcKata.Player()
  state = new MvcKata.State()

  new MvcKata.BattleView(model: state, player: player, enemey: enemey).render()

class MvcKata.State extends Backbone.Model
  defaults:
    message: ''

class MvcKata.Living extends Backbone.Model
  attack: (target, state) ->
    damage_point = Math.floor((Math.random() * @get('attackPower')) + 1)
    target.set hp: target.get('hp') - damage_point

    if window.locale == 'ja'
      state.set message: @get('name') + ' のこうげき'
      state.set message: target.get('name') + "に" + damage_point + "のダメージ"
    else
      state.set message: @get('name') + ' attack.'
      state.set message: target.get('name') + " damaged " + damage_point + " point(s)"

    target.check(target)

class MvcKata.Enemey extends MvcKata.Living
  check: (player)->
    if @get('hp') <= 0
      if window.locale == 'ja'
        alert @get('name') + "をたおした\n経験値" + @get('exp') + 'かくとく'
      else
        alert player.get('name') + " win!\n" + player.get('name') + ' get ' + @get('exp') + ' EXP'

      location.reload()

class MvcKata.Slime extends MvcKata.Enemey
  defaults:
    name: 'SLIME'
    hp: 10
    attackPower: 4
    exp: 1

  attack: (player, state) ->
    MvcKata.Living.prototype.attack.call(this, player, state)

  check: (player) ->
    MvcKata.Enemey.prototype.check.call(this, player)

class MvcKata.Dragon extends MvcKata.Living
  defaults:
    name: 'DRAGON'
    hp: 20
    attackPower: 8
    exp: 3000

  attack: (player, state) ->
    MvcKata.Living.prototype.attack.call(this, player, state)

  check: (player) ->
    MvcKata.Enemey.prototype.check.call(this, player)

class MvcKata.Player extends MvcKata.Living
  defaults:
    name: 'PLAYER'
    maxHp: 10
    hp: 10
    mp: 20
    attackPower: 3
    turnCount: 1

  attack: (enemey, state) ->
    MvcKata.Living.prototype.attack.call(this, enemey, state)

  hoimi: (state)->
    cure_point = Math.floor((Math.random() * 8) + 1)
    @set hp: (_.min([cure_point + @get('hp'), @get('maxHp')]))

    if window.locale == 'ja'
      state.set message: @get('name') + ' はホイミをとなえた'
      state.set message: "HPが" + cure_point + "回復した"
    else
      state.set message: @get('name') + ' call hoimi.'
      state.set message: @get('name') + " cured " + cure_point + " point(s)"

  encounter: (target, state) ->
    if window.locale == 'ja'
      state.set message: target.get('name') + "があらわれた"
    else
      state.set message: @get('name') + " encounted a " + target.get('name')

  check: ->
    if @get('hp') <= 0
      if window.locale == 'ja'
        alert @get('name') + "はたおれました\nゲームオーバー"
      else
        alert @get('name') + " lose...\nGAME OVER"

      location.reload()

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

    $(@el).append new MvcKata.EventMenuView(model: @model, player: @options.player, enemey: @options.enemey).render().el
    $(@el).append new MvcKata.EventHistoryView(model: @model).render().el

    @options.player.encounter(@options.enemey, @model)

  setJa: ->
    window.locale = 'ja'
    @render()

  setEn: ->
    window.locale = 'en'
    @render()

class MvcKata.EventHistoryView extends Backbone.View
  initialize: ->
    @model.bind 'change', @renderMessage

  template: _.template '''
    <div class="message"></div>
  '''

  messageTemplate: _.template '''
    <p><%= message %></p>
  '''

  render: =>
    $(@el).html @template()

    this

  renderMessage: =>
    return unless @model.get('message')

    @$('.message').append @messageTemplate(@model.toJSON())

class MvcKata.EventMenuView extends Backbone.View
  events:
    'click .attack': 'attack'
    'click .hoimi': 'hoimi'

  template: _.template '''
    <div class="action"></div>
  '''

  templateJa: _.template '''
    <ol>
      <li><a href="#" class="attack">アタック</a></li>
      <li><a href="#" class="hoimi">ホイミ</a></li>
    </ol>
  '''

  templateEn: _.template '''
    <ol>
      <li><a href="#" class="attack">Attack</a></li>
      <li><a href="#" class="hoimi">Hoimi</a></li>
    </ol>
  '''

  render: =>
    $(@el).html @template()

    @$('.action').append new MvcKata.PlayerStatusView(model: @options.player).render().el

    if window.locale == 'ja'
      @$('.action').append @templateJa(@options.player.toJSON())
    else
      @$('.action').append @templateEn(@options.player.toJSON())

    this

  attack: ->
    @options.player.attack(@options.enemey, @model)

    @options.enemey.attack(@options.player, @model)

    @options.player.set turnCount: @options.player.get('turnCount') + 1

  hoimi: ->
    @options.player.hoimi(@model)

    @options.enemey.attack(@options.player, @model)

    @options.player.set turnCount: @options.player.get('turnCount') + 1

class MvcKata.PlayerStatusView extends Backbone.View
  initialize: ->
    @model.bind 'change', @render

  render: =>
    if window.locale == 'ja'
      $(@el).html @templateJa(@model.toJSON())
    else
      $(@el).html @templateEn(@model.toJSON())

    this

  templateJa: _.template '''
    <div><%= name %>のHP: <%= hp %>/<%= maxHp %> ターン数: <%= turnCount %></div>
  '''

  templateEn: _.template '''
     <div><%= name %> HP: <%= hp %>/<%= maxHp %> Turn: <%= turnCount %></div>
  '''
