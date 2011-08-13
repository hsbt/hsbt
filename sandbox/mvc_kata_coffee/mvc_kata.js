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
    enemey = ((Math.random() * 100) % 2) > 1 ? new MvcKata.Slime() : new MvcKata.Dragon();
    player = new MvcKata.Player();
    return new MvcKata.BattleView({
      model: player,
      enemey: enemey
    }).render();
  });
  MvcKata.Slime = (function() {
    __extends(Slime, Backbone.Model);
    function Slime() {
      Slime.__super__.constructor.apply(this, arguments);
    }
    Slime.prototype.defaults = {
      name: 'SLIME',
      hp: 10,
      attackPower: 4,
      exp: 1,
      message: ''
    };
    Slime.prototype.attack = function(player) {};
    return Slime;
  })();
  MvcKata.Dragon = (function() {
    __extends(Dragon, Backbone.Model);
    function Dragon() {
      Dragon.__super__.constructor.apply(this, arguments);
    }
    Dragon.prototype.defaults = {
      name: 'DRAGON',
      hp: 20,
      attackPower: 8,
      exp: 3000,
      message: ''
    };
    Dragon.prototype.attack = function(player) {};
    return Dragon;
  })();
  MvcKata.Player = (function() {
    __extends(Player, Backbone.Model);
    function Player() {
      Player.__super__.constructor.apply(this, arguments);
    }
    Player.prototype.defaults = {
      name: 'PLAYER',
      hp: 10,
      mp: 20,
      attackPower: 3,
      message: ''
    };
    Player.prototype.attack = function(enemey) {};
    Player.prototype.hoimi = function() {};
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
    BattleView.prototype.template = _.template('<h1>MVC Kata</h1>\n<div class=\'locale\'>\n  <a href="#" class="setJa">日本語</a> / <a href="#" class="setEn">English</a>\n</div>\n<div class="message"></div>\n<div class="action"></div>');
    BattleView.prototype.render = function() {
      $(this.el).html(this.template());
      this.$('.message').html(new MvcKata.EventHistoryView({
        model: this.model,
        enemey: this.options.enemey
      }).render().el);
      return this.$('.action').html(new MvcKata.EventMenuView({
        model: this.model,
        enemey: this.options.enemery
      }).render().el);
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
      this.render = __bind(this.render, this);
      EventHistoryView.__super__.constructor.apply(this, arguments);
    }
    EventHistoryView.prototype.templateJa = _.template('<h1>ほげほげ</h1>');
    EventHistoryView.prototype.templateEn = _.template('<h1>hogehoge</h1>');
    EventHistoryView.prototype.render = function() {
      if (window.locale === 'ja') {
        $(this.el).html(this.templateJa());
      } else {
        $(this.el).html(this.templateEn());
      }
      return this;
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
    EventMenuView.prototype.templateJa = _.template('<div><%= name %>のHP: <%= hp %></div>\n<ol>\n  <li><a href="#" class="attack">アタック</a></li>\n  <li><a href="#" class="hoimi">ホイミ</a></li>\n</ol>');
    EventMenuView.prototype.templateEn = _.template(' <div><%= name %> HP: <%= hp %></div>\n<ol>\n  <li><a href="#" class="attack">Attack</a></li>\n  <li><a href="#" class="hoimi">Hoimi</a></li>\n</ol>');
    EventMenuView.prototype.render = function() {
      if (window.locale === 'ja') {
        $(this.el).html(this.templateJa(this.model.toJSON()));
      } else {
        $(this.el).html(this.templateEn(this.model.toJSON()));
      }
      return this;
    };
    EventMenuView.prototype.attack = function() {
      this.model.attack(this.options.enemey);
      return this.enemey.attack(this.model);
    };
    EventMenuView.prototype.hoimi = function() {
      this.model.hoimi;
      return this.enemey.attack(this.model);
    };
    return EventMenuView;
  })();
}).call(this);
