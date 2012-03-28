var util = require('util'),
    http = require('http'),
    url = require('url'),
    path = require('path'),
    fs = require('fs'),
    events = require('events');

function load_static_file(uri, res) {
  var filename = path.join(process.cwd(), uri);

  path.exists(filename, function(exists) {
    if(!exists) {
      res.writeHead(404, {"Content-Type": "text/plain"});
      res.write("404 Not Found\n");
      res.end();
      return;
    }

    fs.readFile(filename, "binary", function(err, file) {
      if(err) {
	res.writeHead(500, {"Content-Type": "text/plain"});
	res.write(err + "\n");
	res.end();
	return;
      }

      res.writeHead(200);
      res.write(file, "binary");
      res.end();
    });
  });
}

var twitter_client = http.createClient(80, "api.twitter.com"),
    tweet_emitter = new events.EventEmitter();

function get_tweets() {
  var req = twitter_client.request('GET', '/1/statuses/public_timeline.json', {'host': 'api.twitter.com'});

  req.addListener('response', function(res) {
    var body = "";
    res.addListener('data', function(data) {
      body += data;
    });

    res.addListener('end', function() {
      var tweets = JSON.parse(body);
      if(tweets.length > 0) {
	tweet_emitter.emit('tweets', tweets);
      }
    });
  });

  req.end();
}

setInterval(get_tweets, 5000);

http.createServer(function(req, res) {
  var uri = url.parse(req.url).pathname;
  if(uri === "/stream") {
    var listener = tweet_emitter.addListener("tweets", function(tweets) {
      res.writeHead(200, {"Content-Type": "text/plain" });
      res.write(JSON.stringify(tweets));
      res.end();

      clearTimeout(timeout);
    });

    var timeout = setTimeout(function() {
      res.writeHead(200, {"Content-Type": "text/plain" });
      res.write(JSON.stringify([]));
      res.end();
      tweet_emitter.removeListener("tweets", listener);
    }, 10000);
  }
  else {
    load_static_file(uri, res);
  }
}).listen(8080);

util.puts("Server running at http://localhost:8080");
