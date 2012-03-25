
/**
 * Module dependencies.
 */

var express = require('express'),
    routes = require('./routes'),
    mongoose = require('mongoose'),
    db,
    Document;

var app = module.exports = express.createServer();

// Configuration

app.configure(function(){
  app.set('views', __dirname + '/views');
  app.set('view engine', 'jade');
  app.use(express.bodyParser());
  app.use(express.methodOverride());
  app.use(express.logger());
  app.use(express.compiler({src: __dirname + '/public', enable: ['less']}));
  app.use(app.router);
  app.use(express.static(__dirname + '/public'));
});

app.configure('development', function(){
  app.use(express.logger({ format: ':method :uri' }));
  app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
  db = mongoose.connect('mongodb://localhost/nodepad_development');
});

app.configure('test', function() {
  app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
  db = mongoose.connect('mongodb://localhost/nodepad_test');
});

app.configure('production', function(){
  app.use(express.logger());
  app.use(express.errorHandler());
  db = mongoose.connect('mongodb://localhost/nodepad_production');
});

app.Document = Document = require('./models.js').Document(db);

// Routes

app.get('/', routes.index);

app.get('/documents.:format', function(req, res) {
  Document.find().all(function(documents) {
    switch(req.params.format) {
    case 'json':
      res.send(documents.map(function(d) {
	return d.__doc;
      }));
      break;
    default:
      res.render('documents/index.jade');
    }
  });
});

app.post('/documents.:format?', function(req, res) {
  var document = new Document(req.body['document']);
  document.save(function() {
    switch(req.params.format) {
    case 'json':
      res.send(document.__doc);
      break;
    default:
      res.redirect('/documents');
    }
  });
});

app.get('/documents/:id.:format?', function(req, res) {
});

app.put('/documents/:id.:format?', function(req, res) {
});

app.del('/documents/:id.:format?', function(req, res) {
});

app.listen(3000);
console.log("Express server listening on port %d in %s mode", app.address().port, app.settings.env);
