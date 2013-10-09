Native.MySockets = function(elm) {
    'use strict';
    if (elm.Native.MySockets) return elm.Native.MySockets;

    var Signal = Elm.Signal(elm);
    var Native = Elm.JavaScript(elm);
    var List = Elm.Native.List(elm);

    function open(url, outgoing) {
	var incoming = Signal.constant(List.Nil);
	var ws = new WebSocket(Native.fromString(url));

	var pending = [];
	var ready = false;
	
	ws.onopen = function(e) {
	    var len = pending.length;
	    for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
	    ready = true;
	};
	ws.onmessage = function(event) {
	    elm.notify(incoming.id, Native.toString(event.data));
	};
	
	function send(msg) {
	    var s = Native.fromString(msg);
	    ready ? ws.send(s) : pending.push(s);
	}
	
	function take1(x,y) { return x }
	return A3(Signal.lift2, F2(take1), incoming, A2(Signal.lift, send, outgoing));
    }

    return elm.Native.MySockets = { open: F2(open) };
};