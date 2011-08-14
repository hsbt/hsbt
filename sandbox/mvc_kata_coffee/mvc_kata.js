(function() {
  var __hasProp = Object.prototype.hasOwnProperty, __extends = function(child, parent) {
    for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; }
    function ctor() { this.constructor = child; }
    ctor.prototype = parent.prototype;
    child.prototype = new ctor;
    child.__super__ = parent.prototype;
    return child;
  }, __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };
  window.MvcKata = {};
  $(function() {
    var enemey, player, state;
    enemey = Math.floor((Math.random() * 100) % 2) === 1 ? new MvcKata.Slime() : new MvcKata.Dragon();
    player = new MvcKata.Player();
    state = new MvcKata.State();
    return new MvcKata.BattleView({
      model: state,
      player: player,
      enemey: enemey
    }).render();
  });
  MvcKata.State = (function() {
    __extends(State, Backbone.Model);
    function State() {
      State.__super__.constructor.apply(this, arguments);
    }
    State.prototype.defaults = {
      message: ''
    };
    return State;
  })();
  MvcKata.Living = (function() {
    __extends(Living, Backbone.Model);
    function Living() {
      Living.__super__.constructor.apply(this, arguments);
    }
    Living.prototype.attack = function(target, state) {
      var damage_point;
      damage_point = Math.floor((Math.random() * this.get('attackPower')) + 1);
      target.set({
        hp: target.get('hp') - damage_point
      });
      if (window.locale === 'ja') {
        state.set({
          message: this.get('name') + ' のこうげき'
        });
        state.set({
          message: target.get('name') + "に" + damage_point + "のダメージ"
        });
      } else {
        state.set({
          message: this.get('name') + ' attack.'
        });
        state.set({
          message: target.get('name') + " damaged " + damage_point + " point(s)"
        });
      }
      return target.check(target);
    };
    return Living;
  })();
  MvcKata.Enemey = (function() {
    __extends(Enemey, MvcKata.Living);
    function Enemey() {
      Enemey.__super__.constructor.apply(this, arguments);
    }
    Enemey.prototype.check = function(player) {
      if (this.get('hp') <= 0) {
        if (window.locale === 'ja') {
          alert(this.get('name') + "をたおした\n経験値" + this.get('exp') + 'かくとく');
        } else {
          alert(player.get('name') + " win!\n" + player.get('name') + ' get ' + this.get('exp') + ' EXP');
        }
        return location.reload();
      }
    };
    return Enemey;
  })();
  MvcKata.Slime = (function() {
    __extends(Slime, MvcKata.Enemey);
    function Slime() {
      Slime.__super__.constructor.apply(this, arguments);
    }
    Slime.prototype.defaults = {
      name: 'SLIME',
      hp: 10,
      attackPower: 4,
      exp: 1
    };
    Slime.prototype.attack = function(player, state) {
      return MvcKata.Living.prototype.attack.call(this, player, state);
    };
    Slime.prototype.check = function(player) {
      return MvcKata.Enemey.prototype.check.call(this, player);
    };
    return Slime;
  })();
  MvcKata.Dragon = (function() {
    __extends(Dragon, MvcKata.Living);
    function Dragon() {
      Dragon.__super__.constructor.apply(this, arguments);
    }
    Dragon.prototype.defaults = {
      name: 'DRAGON',
      hp: 20,
      attackPower: 8,
      exp: 3000
    };
    Dragon.prototype.attack = function(player, state) {
      return MvcKata.Living.prototype.attack.call(this, player, state);
    };
    Dragon.prototype.check = function(player) {
      return MvcKata.Enemey.prototype.check.call(this, player);
    };
    return Dragon;
  })();
  MvcKata.Player = (function() {
    __extends(Player, MvcKata.Living);
    function Player() {
      Player.__super__.constructor.apply(this, arguments);
    }
    Player.prototype.defaults = {
      name: 'PLAYER',
      maxHp: 10,
      hp: 10,
      mp: 20,
      attackPower: 3,
      turnCount: 1
    };
    Player.prototype.attack = function(enemey, state) {
      return MvcKata.Living.prototype.attack.call(this, enemey, state);
    };
    Player.prototype.hoimi = function(state) {
      var cure_point;
      cure_point = Math.floor((Math.random() * 8) + 1);
      if (window.locale === 'ja') {
        state.set({
          message: this.get('name') + ' はホイミをとなえた'
        });
      } else {
        state.set({
          message: this.get('name') + ' call hoimi.'
        });
      }
      this.set({
        hp: _.min([cure_point + this.get('hp'), this.get('maxHp')])
      });
      if (window.locale === 'ja') {
        return state.set({
          message: "HPが" + cure_point + "回復した"
        });
      } else {
        return state.set({
          message: this.get('name') + " cured " + cure_point + " point(s)"
        });
      }
    };
    Player.prototype.encounter = function(target, state) {
      if (window.locale === 'ja') {
        return state.set({
          message: target.get('name') + "があらわれた"
        });
      } else {
        return state.set({
          message: this.get('name') + " encounted a " + target.get('name')
        });
      }
    };
    Player.prototype.check = function() {
      if (this.get('hp') <= 0) {
        if (window.locale === 'ja') {
          alert(this.get('name') + "はたおれました\nゲームオーバー");
        } else {
          alert(this.get('name') + " lose...\nGAME OVER");
        }
        return location.reload();
      }
    };
    return Player;
  })();
  MvcKata.BattleView = (function() {
    __extends(BattleView, Backbone.View);
    function BattleView() {
      this.render = __bind(this.render, this);
      BattleView.__super__.constructor.apply(this, arguments);
    }
    BattleView.prototype.el = '#container';
    BattleView.prototype.events = {
      'click .setJa': 'setJa',
      'click .setEn': 'setEn'
    };
    BattleView.prototype.template = _.template('<h1>MVC Kata</h1>\n<div class=\'locale\'>\n  <a href="#" class="setJa">日本語</a> / <a href="#" class="setEn">English</a>\n</div>');
    BattleView.prototype.render = function() {
      $(this.el).html(this.template());
      $(this.el).append(new MvcKata.EventMenuView({
        model: this.model,
        player: this.options.player,
        enemey: this.options.enemey
      }).render().el);
      $(this.el).append(new MvcKata.EventHistoryView({
        model: this.model
      }).render().el);
      return this.options.player.encounter(this.options.enemey, this.model);
    };
    BattleView.prototype.setJa = function() {
      window.locale = 'ja';
      return this.render();
    };
    BattleView.prototype.setEn = function() {
      window.locale = 'en';
      return this.render();
    };
    return BattleView;
  })();
  MvcKata.EventHistoryView = (function() {
    __extends(EventHistoryView, Backbone.View);
    function EventHistoryView() {
      this.renderMessage = __bind(this.renderMessage, this);
      this.render = __bind(this.render, this);
      EventHistoryView.__super__.constructor.apply(this, arguments);
    }
    EventHistoryView.prototype.initialize = function() {
      return this.model.bind('change', this.renderMessage);
    };
    EventHistoryView.prototype.template = _.template('<div class="message"></div>');
    EventHistoryView.prototype.messageTemplate = _.template('<p><%= message %></p>');
    EventHistoryView.prototype.render = function() {
      $(this.el).html(this.template());
      return this;
    };
    EventHistoryView.prototype.renderMessage = function() {
      if (!this.model.get('message')) {
        return;
      }
      return this.$('.message').append(this.messageTemplate(this.model.toJSON()));
    };
    return EventHistoryView;
  })();
  MvcKata.EventMenuView = (function() {
    __extends(EventMenuView, Backbone.View);
    function EventMenuView() {
      this.render = __bind(this.render, this);
      EventMenuView.__super__.constructor.apply(this, arguments);
    }
    EventMenuView.prototype.initialize = function() {
      return this.options.player.bind('change', this.render);
    };
    EventMenuView.prototype.events = {
      'click .attack': 'attack',
      'click .hoimi': 'hoimi'
    };
    EventMenuView.prototype.template = _.template('<div class="action"></div>');
    EventMenuView.prototype.templateJa = _.template('<div><%= name %>のHP: <%= hp %>/<%= maxHp %> ターン数: <%= turnCount %></div>\n<ol>\n  <li><a href="#" class="attack">アタック</a></li>\n  <li><a href="#" class="hoimi">ホイミ</a></li>\n</ol>');
    EventMenuView.prototype.templateEn = _.template(' <div><%= name %> HP: <%= hp %>/<%= maxHp %> Turn: <%= turnCount %></div>\n<ol>\n  <li><a href="#" class="attack">Attack</a></li>\n  <li><a href="#" class="hoimi">Hoimi</a></li>\n</ol>');
    EventMenuView.prototype.render = function() {
      $(this.el).html(this.template());
      if (window.locale === 'ja') {
        this.$('.action').html(this.templateJa(this.options.player.toJSON()));
      } else {
        this.$('.action').html(this.templateEn(this.options.player.toJSON()));
      }
      return this;
    };
    EventMenuView.prototype.attack = function() {
      this.options.player.attack(this.options.enemey, this.model);
      this.options.enemey.attack(this.options.player, this.model);
      return this.options.player.set({
        turnCount: this.options.player.get('turnCount') + 1
      });
    };
    EventMenuView.prototype.hoimi = function() {
      this.options.player.hoimi(this.model);
      this.options.enemey.attack(this.options.player, this.model);
      return this.options.player.set({
        turnCount: this.options.player.get('turnCount') + 1
      });
    };
    return EventMenuView;
  })();
}).call(this);
