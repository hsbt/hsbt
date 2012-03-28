var util = require('util'),
    http = require('http');

http.createServer(function(req, res) {
  res.writeHead(200, {"Content-Type": "text/html"});
  res.write("Heelo World");
  res.end();
}).listen(8080);

util.puts("Server running at http://localhost:8080/");
