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
    Slime.prototype.initialize = function() {
      return this.set({
        name: 'SLIME',
        hp: 10,
        attackPower: 4,
        exp: 1
      });
    };
    return Slime;
  })();
  MvcKata.Dragon = (function() {
    __extends(Dragon, Backbone.Model);
    function Dragon() {
      Dragon.__super__.constructor.apply(this, arguments);
    }
    Dragon.prototype.initialize = function() {
      return this.set({
        name: 'DRAGON',
        hp: 20,
        attackPower: 8,
        exp: 3000
      });
    };
    return Dragon;
  })();
  MvcKata.Player = (function() {
    __extends(Player, Backbone.Model);
    function Player() {
      Player.__super__.constructor.apply(this, arguments);
    }
    Player.prototype.initialize = function() {
      return this.set({
        name: 'PLAYER',
        hp: 10,
        mp: 20,
        attackPower: 3
      });
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
    BattleView.prototype.template = _.template('<h1>MVC Kata</h1>\n<div class=\'locale\'>\n  <a href="#" class="setJa">日本語</a> / <a href="#" class="setEn">English</a>\n</div>\n<div class="message"></div>\n<div class="action"></div>');
    BattleView.prototype.templateJa = _.template('<h1>ほげほげ</h1>');
    BattleView.prototype.templateEn = _.template('<h1>hogehoge</h1>');
    BattleView.prototype.render = function() {
      $(this.el).html(this.template());
      if (window.locale === 'ja') {
        return $('.message').html(this.templateJa());
      } else {
        return $('.message').html(this.templateEn());
      }
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
      EventHistoryView.__super__.constructor.apply(this, arguments);
    }
    return EventHistoryView;
  })();
  MvcKata.EventMenuView = (function() {
    __extends(EventMenuView, Backbone.View);
    function EventMenuView() {
      EventMenuView.__super__.constructor.apply(this, arguments);
    }
    return EventMenuView;
  })();
}).call(this);
