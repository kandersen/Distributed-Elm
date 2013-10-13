/*
 * Kristoffer Just Andersen, Aarhus University
 * October, 2013
 * kja@cs.au.dk
 * Backing server for the 'Simple' family of Elm applications.
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

var pos = {"tag":"pair", "x":0, "y":0};
var clients = [ ];

// WebSocket server
wsServer.on('request', function(request) {
    var connection = request.accept(null, request.origin);
    var index = clients.push(connection) - 1;
    
    connection.sendUTF(JSON.stringify(pos));

    // This is the most important callback for us, we'll handle
    // all messages from users here.
    connection.on('message', function(message) {
	//console.log("RECEIVED:" + message);
	if(message.type == 'utf8') {
	    pair = JSON.parse(message.utf8Data);
	    if (pair.x == 0 && pair.y == 0) {
		return;
	    } else {
		pos.x = pair.x;
		pos.y = pair.y;
		for (var i = 0; i < clients.length; i++) {
		    console.log(JSON.stringify(pos));
		    clients[i].sendUTF(JSON.stringify(pos));
		}
	    }
	}
    });

    connection.on('close', function(connection) {
	clients.splice(index,1);
    });
});
