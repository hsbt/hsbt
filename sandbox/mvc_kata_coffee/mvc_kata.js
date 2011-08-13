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
    var enemey, player;
    enemey = Math.floor((Math.random() * 100) % 2) === 1 ? new MvcKata.Slime() : new MvcKata.Dragon();
    player = new MvcKata.Player();
    return new MvcKata.BattleView({
      model: player,
      enemey: enemey
    }).render();
  });
  MvcKata.Living = (function() {
    __extends(Living, Backbone.Model);
    function Living() {
      Living.__super__.constructor.apply(this, arguments);
    }
    Living.prototype.defaults = {
      message: ''
    };
    Living.prototype.attack = function(target) {
      var damage_point;
      damage_point = Math.floor((Math.random() * this.get('attackPower')) + 1);
      if (window.locale === 'ja') {
        this.set({
          message: this.get('name') + ' のこうげき'
        });
      } else {
        this.set({
          message: this.get('name') + ' attack.'
        });
      }
      target.set({
        hp: target.get('hp') - damage_point
      });
      if (window.locale === 'ja') {
        return this.set({
          message: target.get('name') + "に" + damage_point + "のダメージ"
        });
      } else {
        return this.set({
          message: target.get('name') + " damaged " + damage_point + " point(s)"
        });
      }
    };
    return Living;
  })();
  MvcKata.Slime = (function() {
    __extends(Slime, MvcKata.Living);
    function Slime() {
      Slime.__super__.constructor.apply(this, arguments);
    }
    Slime.prototype.defaults = {
      name: 'SLIME',
      hp: 10,
      attackPower: 4,
      exp: 1
    };
    Slime.prototype.attack = function(player) {
      return MvcKata.Living.prototype.attack.call(this, player);
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
    Dragon.prototype.attack = function(player) {
      return MvcKata.Living.prototype.attack.call(this, player);
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
      turnCount: 0
    };
    Player.prototype.attack = function(enemey) {
      return MvcKata.Living.prototype.attack.call(this, enemey);
    };
    Player.prototype.hoimi = function() {
      var cure_point;
      cure_point = Math.floor((Math.random() * 8) + 1);
      if (window.locale === 'ja') {
        this.set({
          message: this.get('name') + ' はホイミをとなえた'
        });
      } else {
        this.set({
          message: this.get('name') + ' call hoimi.'
        });
      }
      this.set({
        hp: _.min([cure_point + this.get('hp'), this.get('maxHp')])
      });
      if (window.locale === 'ja') {
        return this.set({
          message: "HPが" + cure_point + "回復した"
        });
      } else {
        return this.set({
          message: this.get('name') + " cured " + cure_point + " point(s)"
        });
      }
    };
    Player.prototype.encount = function(target) {
      alert('hoge');
      if (window.locale === 'ja') {
        return this.set({
          message: target.get('name') + "があらわれた"
        });
      } else {
        return this.set({
          message: this.get('name') + " encounted a " + target.get('name')
        });
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
        enemey: this.options.enemey
      }).render().el);
      $(this.el).append(new MvcKata.EventHistoryView({
        model: this.model,
        enemey: this.options.enemey
      }).render().el);
      return this.model.encount(this.options.enemey);
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
      this.renderEnemeyMessage = __bind(this.renderEnemeyMessage, this);
      this.renderPlayerMessage = __bind(this.renderPlayerMessage, this);
      this.render = __bind(this.render, this);
      EventHistoryView.__super__.constructor.apply(this, arguments);
    }
    EventHistoryView.prototype.initialize = function() {
      this.model.bind('change', this.renderPlayerMessage);
      return this.options.enemey.bind('change', this.renderEnemeyMessage);
    };
    EventHistoryView.prototype.template = _.template('<div class="message"></div>');
    EventHistoryView.prototype.messageTemplate = _.template('<p><%= message %></p>');
    EventHistoryView.prototype.render = function() {
      $(this.el).html(this.template());
      return this;
    };
    EventHistoryView.prototype.renderPlayerMessage = function() {
      if (!this.model.get('message')) {
        return;
      }
      this.$('.message').append(this.messageTemplate(this.model.toJSON()));
      return this.model.set({
        message: ''
      });
    };
    EventHistoryView.prototype.renderEnemeyMessage = function() {
      if (!this.options.enemey.get('message')) {
        return;
      }
      this.$('.message').append(this.messageTemplate(this.options.enemey.toJSON()));
      return this.options.enemey.set({
        message: ''
      });
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
      return this.model.bind('change', this.render);
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
        this.$('.action').html(this.templateJa(this.model.toJSON()));
      } else {
        this.$('.action').html(this.templateEn(this.model.toJSON()));
      }
      return this;
    };
    EventMenuView.prototype.attack = function() {
      this.model.attack(this.options.enemey);
      this.options.enemey.attack(this.model);
      return this.model.set({
        turnCount: this.model.get('turnCount') + 1
      });
    };
    EventMenuView.prototype.hoimi = function() {
      this.model.hoimi();
      this.options.enemey.attack(this.model);
      return this.model.set({
        turnCount: this.model.get('turnCount') + 1
      });
    };
    return EventMenuView;
  })();
}).call(this);
