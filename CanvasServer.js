/* 
 * Kristoffer Just Andersen, Aarhus University
 * kja@cs.au.dk
 * October, 2013
 * Backing server for the 'Canvas' family of Elm applications.
 *
 * See the accompanying report, 'Collaborative User-Interfaces with
 * Functional Reactive Programming'
 * 
 * With many appreciative thanks to Matin Sikora @
 * http://martinsikora.com/nodejs-and-websocket-simple-chat-tutorial
 */
var WebSocketServer = require('websocket').server;
var http = require('http');
var fs = require('fs');
var path = require('path');

var typeDict = {
    '.js' : 'text/javascript',
    '.css' : 'text/css'
}

var server = http.createServer(function(request, response) {
    console.log('request starting...');
     
    var filePath = '.' + request.url;
    if (filePath == './')
        filePath = './index.htm';
    
    path.exists(filePath, function(exists) {
        if (exists) {
            fs.readFile(filePath, function(error, content) {
                if (error) {
                    response.writeHead(500);
                    response.end();
                }
                else {
		    var extension = path.extname(filePath);
                    response.writeHead(200, { 'Content-Type': typeDict[extension] || 'text/html'});
                    response.end(content, 'utf-8');
                }
            });
        } else {
            response.writeHead(404);
            response.end();
        }
    });
     
}).listen(8125);

// create the server
wsServer = new WebSocketServer({
    httpServer: server
});

var canvas = {};
var clients = [];

// WebSocket server
wsServer.on('request', function(request) {
    var connection = request.accept(null, request.origin);
    var index = clients.push(connection) - 1;

    console.log("newcomer, sending" + JSON.stringify(canvas));
    connection.sendUTF(JSON.stringify(canvas));

    // This is the most important callback for us, we'll handle
    // all messages from users here.
    connection.on('message', function(message) {
	console.log("received " + message);
	if(message.type == 'utf8') {
	    value = JSON.parse(message.utf8Data);
	    console.log("containing " + JSON.stringify(value));
	    if (value.tag == "line") {
		canvas = { "tag"  : "lines",
                           "from" : value.from,
                           "to"   : value.to,
                           "lines": canvas };
		console.log("broadcasting " + JSON.stringify(value));
		for (var i = 0; i < clients.length; i++) {
		    clients[i].sendUTF(JSON.stringify(value));
		}

	    } else if (value.tag == "lines") {
		while (value.line) {
		    canvas = { "tag"  : "lines",
                               "from" : value.line.from,
                               "to"   : value.line.to,
                               "lines": canvas };
		    console.log("broadcasting " + JSON.stringify(value.line))
		    for (var i = 0; i < clients.length; i++) {
			clients[i].sendUTF(JSON.stringify(value.line));
		    }
		    value = value.lines;
		}

	    } else {
		console.log("Received garbage!");
	    }
	}
    });

    connection.on('close', function(connection) {
	clients.splice(index,1);
    });
});
