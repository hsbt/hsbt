process.env.NODE_ENV = 'test';

var app = require('../app'),
    assert = require('assert');

module.exports = {
  'POST /documents.json': function() {
    assert.response(app, {
      url: '/documents.json',
      method: 'POST',
      data: JSON.stringify({document: {title: 'Test'}}),
      headers: { 'Content-Type': 'applicaiton/json' }
    }, {
      status: 200,
      headers: { 'Content-Type': 'application/json' }
    }, function(res) {
      var document = JSON.parse(res.body);
      assert.equal('Test', document.title);
    });
  },

  'GET /': function(){
    assert.response(app, {
      url: '/'
    }, {
      status: 200,
      headers: { 'Content-Type': 'text/html; charset=utf-8' }
    }, function (res) {
      assert.includes(res.body, '<h1>Express</h1>');
      process.exit();
    });
  }
};