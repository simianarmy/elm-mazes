var Elm = Elm || { Native: {} };
Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a / b) | 0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error('Cannot perform mod 0. Division by zero error.');
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a, b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a, b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n, lo) < 0 ? lo : Utils.cmp(n, hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity;
	}

	function truncate(n)
	{
		return n | 0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees: degrees,
		turns: turns,
		fromPolar: fromPolar,
		toPolar: toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Port = {};

Elm.Native.Port.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;

	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				'Port Error:\n' +
				'No argument was given for the port named \'' + name + '\' with type:\n\n' +
				'    ' + type.split('\n').join('\n        ') + '\n\n' +
				'You need to provide an initial value!\n\n' +
				'Find out more about ports here <http://elm-lang.org/learn/Ports.elm>'
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				'Port Error:\n' +
				'Regarding the port named \'' + name + '\' with type:\n\n' +
				'    ' + type.split('\n').join('\n        ') + '\n\n' +
				'You just sent the value:\n\n' +
				'    ' + JSON.stringify(value) + '\n\n' +
				'but it cannot be converted to the necessary type.\n' +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};

if (!Elm.fullscreen) {
	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];

				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild === "function")
				{
					container.appendChild(errorNode(error.message));
				}
				else
				{
					console.error(error.message);
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		}

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode('Open the developer console for more details.'));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.ctor === 'Element_elm_builtin')
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000 / 60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							'Unexpected draw callback.\n' +
							'Please report this to <https://github.com/elm-lang/core/issues>.'
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld, nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld, nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b); }; }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a, b, c); }; };
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a, b, c, d); }; }; };
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a, b, c, d, e, f); }; }; }; }; };
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun, a, b)
	{
		return fun.arity === 2
			? fun.func(a, b)
			: fun(a)(b);
	}
	function A3(fun, a, b, c)
	{
		return fun.arity === 3
			? fun.func(a, b, c)
			: fun(a)(b)(c);
	}
	function A4(fun, a, b, c, d)
	{
		return fun.arity === 4
			? fun.func(a, b, c, d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun, a, b, c, d, e)
	{
		return fun.arity === 5
			? fun.func(a, b, c, d, e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun, a, b, c, d, e, f)
	{
		return fun.arity === 6
			? fun.func(a, b, c, d, e, f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun, a, b, c, d, e, f, g)
	{
		return fun.arity === 7
			? fun.func(a, b, c, d, e, f, g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun, a, b, c, d, e, f, g, h)
	{
		return fun.arity === 8
			? fun.func(a, b, c, d, e, f, g, h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun, a, b, c, d, e, f, g, h, i)
	{
		return fun.arity === 9
			? fun.func(a, b, c, d, e, f, g, h, i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}


	// COMPARISONS

	function eq(l, r)
	{
		var stack = [{'x': l, 'y': r}];
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === 'object')
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT', 'EQ', 'GT'];

	function compare(x, y)
	{
		return {
			ctor: ord[cmp(x, y) + 1]
		};
	}

	function cmp(x, y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === '::' || x.ctor === '[]')
		{
			while (true)
			{
				if (x.ctor === '[]' && y.ctor === '[]')
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0, 6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	// TUPLES

	var Tuple0 = {
		ctor: '_Tuple0'
	};

	function Tuple2(x, y)
	{
		return {
			ctor: '_Tuple2',
			_0: x,
			_1: y
		};
	}


	// LITERALS

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}


	// GUID

	var count = 0;
	function guid(_)
	{
		return count++;
	}


	// RECORDS

	function update(oldRecord, updatedFields)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = (key in updatedFields) ? updatedFields[key] : oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}


	// MOUSE COORDINATES

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor: '[]' };

	function Cons(hd, tl)
	{
		return {
			ctor: '::',
			_0: hd,
			_1: tl
		};
	}

	function list(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo, hi)
	{
		var list = Nil;
		if (lo <= hi)
		{
			do
			{
				list = Cons(hi, list);
			}
			while (hi-- > lo);
		}
		return list;
	}

	function append(xs, ys)
	{
		// append Strings
		if (typeof xs === 'string')
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0, 5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}


		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}


	// CRASHES

	function crash(moduleName, region)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}

	function crashCase(moduleName, region, value)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
				+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
				+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}

	function regionToString(region)
	{
		if (region.start.line == region.end.line)
		{
			return 'on line ' + region.start.line;
		}
		return 'between lines ' + region.start.line + ' and ' + region.end.line;
	}


	// BAD PORTS

	function badPort(expected, received)
	{
		throw new Error(
			'Runtime error when sending values through a port.\n\n'
			+ 'Expecting ' + expected + ' but was given ' + formatValue(received)
		);
	}

	function formatValue(value)
	{
		// Explicity format undefined values as "undefined"
		// because JSON.stringify(undefined) unhelpfully returns ""
		return (value === undefined) ? "undefined" : JSON.stringify(value);
	}


	// TO STRING

	var _Array;
	var Dict;
	var List;

	var toString = function(v)
	{
		var type = typeof v;
		if (type === 'function')
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === 'boolean')
		{
			return v ? 'True' : 'False';
		}
		else if (type === 'number')
		{
			return v + '';
		}
		else if ((v instanceof String) && v.isChar)
		{
			return '\'' + addSlashes(v, true) + '\'';
		}
		else if (type === 'string')
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === 'object' && 'ctor' in v)
		{
			if (v.ctor.substring(0, 6) === '_Tuple')
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return '(' + output.join(',') + ')';
			}
			else if (v.ctor === '_Array')
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return 'Array.fromList ' + toString(list);
			}
			else if (v.ctor === '::')
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === '::')
				{
					output += ',' + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === '[]')
			{
				return '[]';
			}
			else if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin' || v.ctor === 'Set_elm_builtin')
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				var list;
				var name;
				if (v.ctor === 'Set_elm_builtin')
				{
					if (!List)
					{
						List = Elm.List.make(localRuntime);
					}
					name = 'Set';
					list = A2(List.map, function(x) {return x._0; }, Dict.toList(v._0));
				}
				else
				{
					name = 'Dict';
					list = Dict.toList(v);
				}
				return name + '.fromList ' + toString(list);
			}
			else if (v.ctor.slice(0, 5) === 'Text:')
			{
				return '<text>';
			}
			else if (v.ctor === 'Element_elm_builtin')
			{
				return '<element>'
			}
			else if (v.ctor === 'Form_elm_builtin')
			{
				return '<form>'
			}
			else
			{
				var output = '';
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		else if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return '<signal>';
		}
		else if (type === 'object')
		{
			var output = [];
			for (var k in v)
			{
				output.push(k + ' = ' + toString(v[k]));
			}
			if (output.length === 0)
			{
				return '{}';
			}
			return '{ ' + output.join(', ') + ' }';
		}
		return '<internal structure>';
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, '\\\'');
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		update: update,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		list: list,
		range: range,
		append: F2(append),

		crash: crash,
		crashCase: crashCase,
		badPort: badPort,

		toString: toString
	};
};

Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values) return _elm.Basics.values;
   var _U = Elm.Native.Utils.make(_elm),$Native$Basics = Elm.Native.Basics.make(_elm),$Native$Utils = Elm.Native.Utils.make(_elm);
   var _op = {};
   var uncurry = F2(function (f,_p0) {    var _p1 = _p0;return A2(f,_p1._0,_p1._1);});
   var curry = F3(function (f,a,b) {    return f({ctor: "_Tuple2",_0: a,_1: b});});
   var flip = F3(function (f,b,a) {    return A2(f,a,b);});
   var snd = function (_p2) {    var _p3 = _p2;return _p3._1;};
   var fst = function (_p4) {    var _p5 = _p4;return _p5._0;};
   var always = F2(function (a,_p6) {    return a;});
   var identity = function (x) {    return x;};
   _op["<|"] = F2(function (f,x) {    return f(x);});
   _op["|>"] = F2(function (x,f) {    return f(x);});
   _op[">>"] = F3(function (f,g,x) {    return g(f(x));});
   _op["<<"] = F3(function (g,f,x) {    return g(f(x));});
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Utils.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {    return t;};
   return _elm.Basics.values = {_op: _op
                               ,max: max
                               ,min: min
                               ,compare: compare
                               ,not: not
                               ,xor: xor
                               ,rem: rem
                               ,negate: negate
                               ,abs: abs
                               ,sqrt: sqrt
                               ,clamp: clamp
                               ,logBase: logBase
                               ,e: e
                               ,pi: pi
                               ,cos: cos
                               ,sin: sin
                               ,tan: tan
                               ,acos: acos
                               ,asin: asin
                               ,atan: atan
                               ,atan2: atan2
                               ,round: round
                               ,floor: floor
                               ,ceiling: ceiling
                               ,truncate: truncate
                               ,toFloat: toFloat
                               ,degrees: degrees
                               ,radians: radians
                               ,turns: turns
                               ,toPolar: toPolar
                               ,fromPolar: fromPolar
                               ,isNaN: isNaN
                               ,isInfinite: isInfinite
                               ,toString: toString
                               ,fst: fst
                               ,snd: snd
                               ,identity: identity
                               ,always: always
                               ,flip: flip
                               ,curry: curry
                               ,uncurry: uncurry
                               ,LT: LT
                               ,EQ: EQ
                               ,GT: GT};
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values) return _elm.Maybe.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var withDefault = F2(function ($default,maybe) {    var _p0 = maybe;if (_p0.ctor === "Just") {    return _p0._0;} else {    return $default;}});
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      oneOf: while (true) {
         var _p1 = maybes;
         if (_p1.ctor === "[]") {
               return Nothing;
            } else {
               var _p3 = _p1._0;
               var _p2 = _p3;
               if (_p2.ctor === "Nothing") {
                     var _v3 = _p1._1;
                     maybes = _v3;
                     continue oneOf;
                  } else {
                     return _p3;
                  }
            }
      }
   };
   var andThen = F2(function (maybeValue,callback) {
      var _p4 = maybeValue;
      if (_p4.ctor === "Just") {
            return callback(_p4._0);
         } else {
            return Nothing;
         }
   });
   var Just = function (a) {    return {ctor: "Just",_0: a};};
   var map = F2(function (f,maybe) {    var _p5 = maybe;if (_p5.ctor === "Just") {    return Just(f(_p5._0));} else {    return Nothing;}});
   var map2 = F3(function (func,ma,mb) {
      var _p6 = {ctor: "_Tuple2",_0: ma,_1: mb};
      if (_p6.ctor === "_Tuple2" && _p6._0.ctor === "Just" && _p6._1.ctor === "Just") {
            return Just(A2(func,_p6._0._0,_p6._1._0));
         } else {
            return Nothing;
         }
   });
   var map3 = F4(function (func,ma,mb,mc) {
      var _p7 = {ctor: "_Tuple3",_0: ma,_1: mb,_2: mc};
      if (_p7.ctor === "_Tuple3" && _p7._0.ctor === "Just" && _p7._1.ctor === "Just" && _p7._2.ctor === "Just") {
            return Just(A3(func,_p7._0._0,_p7._1._0,_p7._2._0));
         } else {
            return Nothing;
         }
   });
   var map4 = F5(function (func,ma,mb,mc,md) {
      var _p8 = {ctor: "_Tuple4",_0: ma,_1: mb,_2: mc,_3: md};
      if (_p8.ctor === "_Tuple4" && _p8._0.ctor === "Just" && _p8._1.ctor === "Just" && _p8._2.ctor === "Just" && _p8._3.ctor === "Just") {
            return Just(A4(func,_p8._0._0,_p8._1._0,_p8._2._0,_p8._3._0));
         } else {
            return Nothing;
         }
   });
   var map5 = F6(function (func,ma,mb,mc,md,me) {
      var _p9 = {ctor: "_Tuple5",_0: ma,_1: mb,_2: mc,_3: md,_4: me};
      if (_p9.ctor === "_Tuple5" && _p9._0.ctor === "Just" && _p9._1.ctor === "Just" && _p9._2.ctor === "Just" && _p9._3.ctor === "Just" && _p9._4.ctor === "Just")
      {
            return Just(A5(func,_p9._0._0,_p9._1._0,_p9._2._0,_p9._3._0,_p9._4._0));
         } else {
            return Nothing;
         }
   });
   return _elm.Maybe.values = {_op: _op
                              ,andThen: andThen
                              ,map: map
                              ,map2: map2
                              ,map3: map3
                              ,map4: map4
                              ,map5: map5
                              ,withDefault: withDefault
                              ,oneOf: oneOf
                              ,Just: Just
                              ,Nothing: Nothing};
};
Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	var fromArray = Utils.list;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil: Nil,
		Cons: Cons,
		cons: F2(Cons),
		toArray: toArray,
		fromArray: fromArray,

		foldl: F3(foldl),
		foldr: F3(foldr),

		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		sortBy: F2(sortBy),
		sortWith: F2(sortWith),
		take: F2(take)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;
};

Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values) return _elm.List.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm),$Maybe = Elm.Maybe.make(_elm),$Native$List = Elm.Native.List.make(_elm);
   var _op = {};
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {    return A2(sortBy,$Basics.identity,xs);};
   var drop = F2(function (n,list) {
      drop: while (true) if (_U.cmp(n,0) < 1) return list; else {
            var _p0 = list;
            if (_p0.ctor === "[]") {
                  return list;
               } else {
                  var _v1 = n - 1,_v2 = _p0._1;
                  n = _v1;
                  list = _v2;
                  continue drop;
               }
         }
   });
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = F2(function (isOkay,list) {
      any: while (true) {
         var _p1 = list;
         if (_p1.ctor === "[]") {
               return false;
            } else {
               if (isOkay(_p1._0)) return true; else {
                     var _v4 = isOkay,_v5 = _p1._1;
                     isOkay = _v4;
                     list = _v5;
                     continue any;
                  }
            }
      }
   });
   var all = F2(function (isOkay,list) {    return $Basics.not(A2(any,function (_p2) {    return $Basics.not(isOkay(_p2));},list));});
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {    return A3(foldl,F2(function (_p3,i) {    return i + 1;}),0,xs);};
   var sum = function (numbers) {    return A3(foldl,F2(function (x,y) {    return x + y;}),0,numbers);};
   var product = function (numbers) {    return A3(foldl,F2(function (x,y) {    return x * y;}),1,numbers);};
   var maximum = function (list) {
      var _p4 = list;
      if (_p4.ctor === "::") {
            return $Maybe.Just(A3(foldl,$Basics.max,_p4._0,_p4._1));
         } else {
            return $Maybe.Nothing;
         }
   };
   var minimum = function (list) {
      var _p5 = list;
      if (_p5.ctor === "::") {
            return $Maybe.Just(A3(foldl,$Basics.min,_p5._0,_p5._1));
         } else {
            return $Maybe.Nothing;
         }
   };
   var indexedMap = F2(function (f,xs) {    return A3(map2,f,_U.range(0,length(xs) - 1),xs);});
   var member = F2(function (x,xs) {    return A2(any,function (a) {    return _U.eq(a,x);},xs);});
   var isEmpty = function (xs) {    var _p6 = xs;if (_p6.ctor === "[]") {    return true;} else {    return false;}};
   var tail = function (list) {    var _p7 = list;if (_p7.ctor === "::") {    return $Maybe.Just(_p7._1);} else {    return $Maybe.Nothing;}};
   var head = function (list) {    var _p8 = list;if (_p8.ctor === "::") {    return $Maybe.Just(_p8._0);} else {    return $Maybe.Nothing;}};
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {    return A3(foldr,F2(function (x,acc) {    return A2(_op["::"],f(x),acc);}),_U.list([]),xs);});
   var filter = F2(function (pred,xs) {
      var conditionalCons = F2(function (x,xs$) {    return pred(x) ? A2(_op["::"],x,xs$) : xs$;});
      return A3(foldr,conditionalCons,_U.list([]),xs);
   });
   var maybeCons = F3(function (f,mx,xs) {    var _p9 = f(mx);if (_p9.ctor === "Just") {    return A2(_op["::"],_p9._0,xs);} else {    return xs;}});
   var filterMap = F2(function (f,xs) {    return A3(foldr,maybeCons(f),_U.list([]),xs);});
   var reverse = function (list) {    return A3(foldl,F2(function (x,y) {    return A2(_op["::"],x,y);}),_U.list([]),list);};
   var scanl = F3(function (f,b,xs) {
      var scan1 = F2(function (x,accAcc) {
         var _p10 = accAcc;
         if (_p10.ctor === "::") {
               return A2(_op["::"],A2(f,x,_p10._0),accAcc);
            } else {
               return _U.list([]);
            }
      });
      return reverse(A3(foldl,scan1,_U.list([b]),xs));
   });
   var append = F2(function (xs,ys) {
      var _p11 = ys;
      if (_p11.ctor === "[]") {
            return xs;
         } else {
            return A3(foldr,F2(function (x,y) {    return A2(_op["::"],x,y);}),ys,xs);
         }
   });
   var concat = function (lists) {    return A3(foldr,append,_U.list([]),lists);};
   var concatMap = F2(function (f,list) {    return concat(A2(map,f,list));});
   var partition = F2(function (pred,list) {
      var step = F2(function (x,_p12) {
         var _p13 = _p12;
         var _p15 = _p13._0;
         var _p14 = _p13._1;
         return pred(x) ? {ctor: "_Tuple2",_0: A2(_op["::"],x,_p15),_1: _p14} : {ctor: "_Tuple2",_0: _p15,_1: A2(_op["::"],x,_p14)};
      });
      return A3(foldr,step,{ctor: "_Tuple2",_0: _U.list([]),_1: _U.list([])},list);
   });
   var unzip = function (pairs) {
      var step = F2(function (_p17,_p16) {
         var _p18 = _p17;
         var _p19 = _p16;
         return {ctor: "_Tuple2",_0: A2(_op["::"],_p18._0,_p19._0),_1: A2(_op["::"],_p18._1,_p19._1)};
      });
      return A3(foldr,step,{ctor: "_Tuple2",_0: _U.list([]),_1: _U.list([])},pairs);
   };
   var intersperse = F2(function (sep,xs) {
      var _p20 = xs;
      if (_p20.ctor === "[]") {
            return _U.list([]);
         } else {
            var step = F2(function (x,rest) {    return A2(_op["::"],sep,A2(_op["::"],x,rest));});
            var spersed = A3(foldr,step,_U.list([]),_p20._1);
            return A2(_op["::"],_p20._0,spersed);
         }
   });
   var repeatHelp = F3(function (result,n,value) {
      repeatHelp: while (true) if (_U.cmp(n,0) < 1) return result; else {
            var _v18 = A2(_op["::"],value,result),_v19 = n - 1,_v20 = value;
            result = _v18;
            n = _v19;
            value = _v20;
            continue repeatHelp;
         }
   });
   var repeat = F2(function (n,value) {    return A3(repeatHelp,_U.list([]),n,value);});
   return _elm.List.values = {_op: _op
                             ,isEmpty: isEmpty
                             ,length: length
                             ,reverse: reverse
                             ,member: member
                             ,head: head
                             ,tail: tail
                             ,filter: filter
                             ,take: take
                             ,drop: drop
                             ,repeat: repeat
                             ,append: append
                             ,concat: concat
                             ,intersperse: intersperse
                             ,partition: partition
                             ,unzip: unzip
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,filterMap: filterMap
                             ,concatMap: concatMap
                             ,indexedMap: indexedMap
                             ,foldr: foldr
                             ,foldl: foldl
                             ,sum: sum
                             ,product: product
                             ,maximum: maximum
                             ,minimum: minimum
                             ,all: all
                             ,any: any
                             ,scanl: scanl
                             ,sort: sort
                             ,sortBy: sortBy
                             ,sortWith: sortWith};
};
Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1, 0, 0, 0, 1, 0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t, m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11 * c + m12 * s, -m11 * s + m12 * c, m[2],
					  m21 * c + m22 * s, -m21 * s + m22 * c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11 * n11 + m12 * n21,
					  m11 * n12 + m12 * n22,
					  m11 * ndx + m12 * ndy + mdx,
					  m21 * n11 + m22 * n21,
					  m21 * n12 + m22 * n22,
					  m21 * ndx + m22 * ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity: identity,
		matrix: F6(matrix),
		rotation: rotation,
		multiply: F2(multiply)
		/*
		transform: F7(transform),
		rotate: F2(rotate),
		move: F2(move),
		scale: F2(scale),
		scaleX: F2(scaleX),
		scaleY: F2(scaleY),
		reflectX: reflectX,
		reflectY: reflectY
		*/
	};
};

Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values) return _elm.Transform2D.values;
   var _U = Elm.Native.Utils.make(_elm),$Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var _op = {};
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,y) {    return A6(matrix,1,0,0,1,x,y);});
   var scale = function (s) {    return A6(matrix,s,0,0,s,0,0);};
   var scaleX = function (x) {    return A6(matrix,x,0,0,1,0,0);};
   var scaleY = function (y) {    return A6(matrix,1,0,0,y,0,0);};
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   return _elm.Transform2D.values = {_op: _op
                                    ,identity: identity
                                    ,matrix: matrix
                                    ,multiply: multiply
                                    ,rotation: rotation
                                    ,translation: translation
                                    ,scale: scale
                                    ,scaleX: scaleX
                                    ,scaleY: scaleY};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(redo, ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx, style, path)
	{
		if (style.dashing.ctor === '[]')
		{
			trace(ctx, path);
		}
		else
		{
			customLineHelp(ctx, style, path);
		}
		ctx.scale(1, -1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1 = 0, y1 = 0, dx = 0, dy = 0, remaining = 0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0, y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(redo, ctx, style);
		ctx.scale(1, -1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0, -2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w / 2,
			destY = -h / 2,
			destW = w,
			destH = h;

		ctx.scale(1, -1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta % (Math.PI * 2));
		}
		if (scale !== 1)
		{
			ctx.scale(scale, scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta;
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i]._0.form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i]._0;
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w, h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = 'block';
		canvas.style.position = 'absolute';
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w, h, div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w, h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem._0.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w, h, div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w, h, forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w: w, h: h, forms: forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};
};

Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values) return _elm.Color.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm);
   var _op = {};
   var Radial = F5(function (a,b,c,d,e) {    return {ctor: "Radial",_0: a,_1: b,_2: c,_3: d,_4: e};});
   var radial = Radial;
   var Linear = F3(function (a,b,c) {    return {ctor: "Linear",_0: a,_1: b,_2: c};});
   var linear = Linear;
   var fmod = F2(function (f,n) {    var integer = $Basics.floor(f);return $Basics.toFloat(A2($Basics._op["%"],integer,n)) + f - $Basics.toFloat(integer);});
   var rgbToHsl = F3(function (red,green,blue) {
      var b = $Basics.toFloat(blue) / 255;
      var g = $Basics.toFloat(green) / 255;
      var r = $Basics.toFloat(red) / 255;
      var cMax = A2($Basics.max,A2($Basics.max,r,g),b);
      var cMin = A2($Basics.min,A2($Basics.min,r,g),b);
      var c = cMax - cMin;
      var lightness = (cMax + cMin) / 2;
      var saturation = _U.eq(lightness,0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
      var hue = $Basics.degrees(60) * (_U.eq(cMax,r) ? A2(fmod,(g - b) / c,6) : _U.eq(cMax,g) ? (b - r) / c + 2 : (r - g) / c + 4);
      return {ctor: "_Tuple3",_0: hue,_1: saturation,_2: lightness};
   });
   var hslToRgb = F3(function (hue,saturation,lightness) {
      var hue$ = hue / $Basics.degrees(60);
      var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
      var x = chroma * (1 - $Basics.abs(A2(fmod,hue$,2) - 1));
      var _p0 = _U.cmp(hue$,0) < 0 ? {ctor: "_Tuple3",_0: 0,_1: 0,_2: 0} : _U.cmp(hue$,1) < 0 ? {ctor: "_Tuple3",_0: chroma,_1: x,_2: 0} : _U.cmp(hue$,
      2) < 0 ? {ctor: "_Tuple3",_0: x,_1: chroma,_2: 0} : _U.cmp(hue$,3) < 0 ? {ctor: "_Tuple3",_0: 0,_1: chroma,_2: x} : _U.cmp(hue$,4) < 0 ? {ctor: "_Tuple3"
                                                                                                                                               ,_0: 0
                                                                                                                                               ,_1: x
                                                                                                                                               ,_2: chroma} : _U.cmp(hue$,
      5) < 0 ? {ctor: "_Tuple3",_0: x,_1: 0,_2: chroma} : _U.cmp(hue$,6) < 0 ? {ctor: "_Tuple3",_0: chroma,_1: 0,_2: x} : {ctor: "_Tuple3",_0: 0,_1: 0,_2: 0};
      var r = _p0._0;
      var g = _p0._1;
      var b = _p0._2;
      var m = lightness - chroma / 2;
      return {ctor: "_Tuple3",_0: r + m,_1: g + m,_2: b + m};
   });
   var toRgb = function (color) {
      var _p1 = color;
      if (_p1.ctor === "RGBA") {
            return {red: _p1._0,green: _p1._1,blue: _p1._2,alpha: _p1._3};
         } else {
            var _p2 = A3(hslToRgb,_p1._0,_p1._1,_p1._2);
            var r = _p2._0;
            var g = _p2._1;
            var b = _p2._2;
            return {red: $Basics.round(255 * r),green: $Basics.round(255 * g),blue: $Basics.round(255 * b),alpha: _p1._3};
         }
   };
   var toHsl = function (color) {
      var _p3 = color;
      if (_p3.ctor === "HSLA") {
            return {hue: _p3._0,saturation: _p3._1,lightness: _p3._2,alpha: _p3._3};
         } else {
            var _p4 = A3(rgbToHsl,_p3._0,_p3._1,_p3._2);
            var h = _p4._0;
            var s = _p4._1;
            var l = _p4._2;
            return {hue: h,saturation: s,lightness: l,alpha: _p3._3};
         }
   };
   var HSLA = F4(function (a,b,c,d) {    return {ctor: "HSLA",_0: a,_1: b,_2: c,_3: d};});
   var hsla = F4(function (hue,saturation,lightness,alpha) {
      return A4(HSLA,hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),saturation,lightness,alpha);
   });
   var hsl = F3(function (hue,saturation,lightness) {    return A4(hsla,hue,saturation,lightness,1);});
   var complement = function (color) {
      var _p5 = color;
      if (_p5.ctor === "HSLA") {
            return A4(hsla,_p5._0 + $Basics.degrees(180),_p5._1,_p5._2,_p5._3);
         } else {
            var _p6 = A3(rgbToHsl,_p5._0,_p5._1,_p5._2);
            var h = _p6._0;
            var s = _p6._1;
            var l = _p6._2;
            return A4(hsla,h + $Basics.degrees(180),s,l,_p5._3);
         }
   };
   var grayscale = function (p) {    return A4(HSLA,0,0,1 - p,1);};
   var greyscale = function (p) {    return A4(HSLA,0,0,1 - p,1);};
   var RGBA = F4(function (a,b,c,d) {    return {ctor: "RGBA",_0: a,_1: b,_2: c,_3: d};});
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {    return A4(RGBA,r,g,b,1);});
   var lightRed = A4(RGBA,239,41,41,1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,164,0,0,1);
   var lightOrange = A4(RGBA,252,175,62,1);
   var orange = A4(RGBA,245,121,0,1);
   var darkOrange = A4(RGBA,206,92,0,1);
   var lightYellow = A4(RGBA,255,233,79,1);
   var yellow = A4(RGBA,237,212,0,1);
   var darkYellow = A4(RGBA,196,160,0,1);
   var lightGreen = A4(RGBA,138,226,52,1);
   var green = A4(RGBA,115,210,22,1);
   var darkGreen = A4(RGBA,78,154,6,1);
   var lightBlue = A4(RGBA,114,159,207,1);
   var blue = A4(RGBA,52,101,164,1);
   var darkBlue = A4(RGBA,32,74,135,1);
   var lightPurple = A4(RGBA,173,127,168,1);
   var purple = A4(RGBA,117,80,123,1);
   var darkPurple = A4(RGBA,92,53,102,1);
   var lightBrown = A4(RGBA,233,185,110,1);
   var brown = A4(RGBA,193,125,17,1);
   var darkBrown = A4(RGBA,143,89,2,1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,255,255,255,1);
   var lightGrey = A4(RGBA,238,238,236,1);
   var grey = A4(RGBA,211,215,207,1);
   var darkGrey = A4(RGBA,186,189,182,1);
   var lightGray = A4(RGBA,238,238,236,1);
   var gray = A4(RGBA,211,215,207,1);
   var darkGray = A4(RGBA,186,189,182,1);
   var lightCharcoal = A4(RGBA,136,138,133,1);
   var charcoal = A4(RGBA,85,87,83,1);
   var darkCharcoal = A4(RGBA,46,52,54,1);
   return _elm.Color.values = {_op: _op
                              ,rgb: rgb
                              ,rgba: rgba
                              ,hsl: hsl
                              ,hsla: hsla
                              ,greyscale: greyscale
                              ,grayscale: grayscale
                              ,complement: complement
                              ,linear: linear
                              ,radial: radial
                              ,toRgb: toRgb
                              ,toHsl: toHsl
                              ,red: red
                              ,orange: orange
                              ,yellow: yellow
                              ,green: green
                              ,blue: blue
                              ,purple: purple
                              ,brown: brown
                              ,lightRed: lightRed
                              ,lightOrange: lightOrange
                              ,lightYellow: lightYellow
                              ,lightGreen: lightGreen
                              ,lightBlue: lightBlue
                              ,lightPurple: lightPurple
                              ,lightBrown: lightBrown
                              ,darkRed: darkRed
                              ,darkOrange: darkOrange
                              ,darkYellow: darkYellow
                              ,darkGreen: darkGreen
                              ,darkBlue: darkBlue
                              ,darkPurple: darkPurple
                              ,darkBrown: darkBrown
                              ,white: white
                              ,lightGrey: lightGrey
                              ,grey: grey
                              ,darkGrey: darkGrey
                              ,lightCharcoal: lightCharcoal
                              ,charcoal: charcoal
                              ,darkCharcoal: darkCharcoal
                              ,black: black
                              ,lightGray: lightGray
                              ,gray: gray
                              ,darkGray: darkGray};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	var createNode =
		typeof document === 'undefined'
			?
				function(_)
				{
					return {
						style: {},
						appendChild: function() {}
					};
				}
			:
				function(elementType)
				{
					var node = document.createElement(elementType);
					node.style.padding = '0';
					node.style.margin = '0';
					return node;
				}
			;


	function newElement(width, height, elementPrim)
	{
		return {
			ctor: 'Element_elm_builtin',
			_0: {
				element: elementPrim,
				props: {
					id: Utils.guid(),
					width: width,
					height: height,
					opacity: 1,
					color: Maybe.Nothing,
					href: '',
					tag: '',
					hover: Utils.Tuple0,
					click: Utils.Tuple0
				}
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width | 0) + 'px';
		node.style.height = (height | 0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img, props.width, props.height, img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = 'block';
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = 'hidden';

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw) | 0) + 'px';
			img.style.height = ((this.height * sh) | 0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw) | 0) + 'px';
			img.style.marginTop = ((- pos._1 * sh) | 0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp: goDown,
		DDown: goDown,
		DLeft: goRight,
		DRight: goRight,
		DIn: goOut,
		DOut: goOut
	};
	function needsReversal(dir)
	{
		return dir === 'DUp' || dir === 'DLeft' || dir === 'DIn';
	}

	function flow(dir, elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir === goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === 'Absolute'
			? pos._0 + 'px'
			: (pos._0 * 100) + '%';
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos, wrappedElement, e)
	{
		var elem = wrappedElement._0;
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w / 2) | 0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h / 2) | 0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos, elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = 'hidden';
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(wrappedElement)
	{
		var elem = wrappedElement._0;
		return setProps(elem, makeElement(elem));
	}

	function makeElement(e)
	{
		var elem = e.element;
		switch (elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, wrappedCurrent, wrappedNext)
	{
		var curr = wrappedCurrent._0;
		var next = wrappedNext._0;
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(wrappedNext);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch (nextE.ctor)
		{
			case 'Spacer':
				updateProps(node, curr, next);
				return rootNode;

			case 'RawHtml':
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Image':
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE, currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(wrappedNext);
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Flow':
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i]._0.element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(wrappedNext);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(wrappedNext);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i--; )
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Container':
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case 'Custom':
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(wrappedNext);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width | 0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height | 0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor: 'RawHtml',
				html: Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		};
	}

	function markdown(text)
	{
		var raw = {
			ctor: 'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	var htmlHeight =
		typeof document !== 'undefined'
			? realHtmlHeight
			: function(a, b) { return Utils.Tuple2(0, 0); };

	function realHtmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + 'px';
		}
		temp.style.visibility = 'hidden';
		temp.style.styleFloat = 'left';
		temp.style.cssFloat = 'left';

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue('width').slice(0, -2) - 0);
		var h = Math.ceil(style.getPropertyValue('height').slice(0, -2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w, h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== 'Nothing')
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas.href;
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length === 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  '&#38;')
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  '&#39;')
			.replace(/</g,  '&#60;')
			.replace(/>/g,  '&#62;');
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length === 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] === ' ')
		{
			arr[0] = '&nbsp;';
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] === ' ' && arr[i - 1] === ' ')
			{
				arr[i - 1] = arr[i - 1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] === ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length - 1] === ' ')
		{
			return arr.slice(0, -1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values) return _elm.Text.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var _op = {};
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {    return A3($List.foldr,append,empty,texts);};
   var join = F2(function (seperator,texts) {    return concat(A2($List.intersperse,seperator,texts));});
   var defaultStyle = {typeface: _U.list([]),height: $Maybe.Nothing,color: $Color.black,bold: false,italic: false,line: $Maybe.Nothing};
   var Style = F6(function (a,b,c,d,e,f) {    return {typeface: a,height: b,color: c,bold: d,italic: e,line: f};});
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   return _elm.Text.values = {_op: _op
                             ,fromString: fromString
                             ,empty: empty
                             ,append: append
                             ,concat: concat
                             ,join: join
                             ,link: link
                             ,style: style
                             ,defaultStyle: defaultStyle
                             ,typeface: typeface
                             ,monospace: monospace
                             ,height: height
                             ,color: color
                             ,bold: bold
                             ,italic: italic
                             ,line: line
                             ,Style: Style
                             ,Under: Under
                             ,Over: Over
                             ,Through: Through};
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values) return _elm.Graphics.Element.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var _op = {};
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var RawPosition = F4(function (a,b,c,d) {    return {horizontal: a,vertical: b,x: c,y: d};});
   var Position = function (a) {    return {ctor: "Position",_0: a};};
   var Relative = function (a) {    return {ctor: "Relative",_0: a};};
   var relative = Relative;
   var Absolute = function (a) {    return {ctor: "Absolute",_0: a};};
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeft = Position({horizontal: N,vertical: N,x: Absolute(0),y: Absolute(0)});
   var bottomLeftAt = F2(function (x,y) {    return Position({horizontal: N,vertical: N,x: x,y: y});});
   var Z = {ctor: "Z"};
   var middle = Position({horizontal: Z,vertical: Z,x: Relative(0.5),y: Relative(0.5)});
   var midLeft = Position({horizontal: N,vertical: Z,x: Absolute(0),y: Relative(0.5)});
   var midBottom = Position({horizontal: Z,vertical: N,x: Relative(0.5),y: Absolute(0)});
   var middleAt = F2(function (x,y) {    return Position({horizontal: Z,vertical: Z,x: x,y: y});});
   var midLeftAt = F2(function (x,y) {    return Position({horizontal: N,vertical: Z,x: x,y: y});});
   var midBottomAt = F2(function (x,y) {    return Position({horizontal: Z,vertical: N,x: x,y: y});});
   var P = {ctor: "P"};
   var topLeft = Position({horizontal: N,vertical: P,x: Absolute(0),y: Absolute(0)});
   var topRight = Position({horizontal: P,vertical: P,x: Absolute(0),y: Absolute(0)});
   var bottomRight = Position({horizontal: P,vertical: N,x: Absolute(0),y: Absolute(0)});
   var midRight = Position({horizontal: P,vertical: Z,x: Absolute(0),y: Relative(0.5)});
   var midTop = Position({horizontal: Z,vertical: P,x: Relative(0.5),y: Absolute(0)});
   var topLeftAt = F2(function (x,y) {    return Position({horizontal: N,vertical: P,x: x,y: y});});
   var topRightAt = F2(function (x,y) {    return Position({horizontal: P,vertical: P,x: x,y: y});});
   var bottomRightAt = F2(function (x,y) {    return Position({horizontal: P,vertical: N,x: x,y: y});});
   var midRightAt = F2(function (x,y) {    return Position({horizontal: P,vertical: Z,x: x,y: y});});
   var midTopAt = F2(function (x,y) {    return Position({horizontal: Z,vertical: P,x: x,y: y});});
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {    return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));};
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {    return {ctor: "Cropped",_0: a};};
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {    return {ctor: "Flow",_0: a,_1: b};});
   var Container = F2(function (a,b) {    return {ctor: "Container",_0: a,_1: b};});
   var Image = F4(function (a,b,c,d) {    return {ctor: "Image",_0: a,_1: b,_2: c,_3: d};});
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,h,src) {    return A3(newElement,w,h,A4(Image,Plain,w,h,src));});
   var fittedImage = F3(function (w,h,src) {    return A3(newElement,w,h,A4(Image,Fitted,w,h,src));});
   var croppedImage = F4(function (pos,w,h,src) {    return A3(newElement,w,h,A4(Image,Cropped(pos),w,h,src));});
   var tiledImage = F3(function (w,h,src) {    return A3(newElement,w,h,A4(Image,Tiled,w,h,src));});
   var container = F4(function (w,h,_p0,e) {    var _p1 = _p0;return A3(newElement,w,h,A2(Container,_p1._0,e));});
   var spacer = F2(function (w,h) {    return A3(newElement,w,h,Spacer);});
   var sizeOf = function (_p2) {    var _p3 = _p2;var _p4 = _p3._0;return {ctor: "_Tuple2",_0: _p4.props.width,_1: _p4.props.height};};
   var heightOf = function (_p5) {    var _p6 = _p5;return _p6._0.props.height;};
   var widthOf = function (_p7) {    var _p8 = _p7;return _p8._0.props.width;};
   var above = F2(function (hi,lo) {
      return A3(newElement,A2($Basics.max,widthOf(hi),widthOf(lo)),heightOf(hi) + heightOf(lo),A2(Flow,DDown,_U.list([hi,lo])));
   });
   var below = F2(function (lo,hi) {
      return A3(newElement,A2($Basics.max,widthOf(hi),widthOf(lo)),heightOf(hi) + heightOf(lo),A2(Flow,DDown,_U.list([hi,lo])));
   });
   var beside = F2(function (lft,rht) {
      return A3(newElement,widthOf(lft) + widthOf(rht),A2($Basics.max,heightOf(lft),heightOf(rht)),A2(Flow,right,_U.list([lft,rht])));
   });
   var layers = function (es) {
      var hs = A2($List.map,heightOf,es);
      var ws = A2($List.map,widthOf,es);
      return A3(newElement,A2($Maybe.withDefault,0,$List.maximum(ws)),A2($Maybe.withDefault,0,$List.maximum(hs)),A2(Flow,DOut,es));
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,es) {
      var newFlow = F2(function (w,h) {    return A3(newElement,w,h,A2(Flow,dir,es));});
      var maxOrZero = function (list) {    return A2($Maybe.withDefault,0,$List.maximum(list));};
      var hs = A2($List.map,heightOf,es);
      var ws = A2($List.map,widthOf,es);
      if (_U.eq(es,_U.list([]))) return empty; else {
            var _p9 = dir;
            switch (_p9.ctor)
            {case "DUp": return A2(newFlow,maxOrZero(ws),$List.sum(hs));
               case "DDown": return A2(newFlow,maxOrZero(ws),$List.sum(hs));
               case "DLeft": return A2(newFlow,$List.sum(ws),maxOrZero(hs));
               case "DRight": return A2(newFlow,$List.sum(ws),maxOrZero(hs));
               case "DIn": return A2(newFlow,maxOrZero(ws),maxOrZero(hs));
               default: return A2(newFlow,maxOrZero(ws),maxOrZero(hs));}
         }
   });
   var Properties = F9(function (a,b,c,d,e,f,g,h,i) {    return {id: a,width: b,height: c,opacity: d,color: e,href: f,tag: g,hover: h,click: i};});
   var Element_elm_builtin = function (a) {    return {ctor: "Element_elm_builtin",_0: a};};
   var width = F2(function (newWidth,_p10) {
      var _p11 = _p10;
      var _p14 = _p11._0.props;
      var _p13 = _p11._0.element;
      var newHeight = function () {
         var _p12 = _p13;
         switch (_p12.ctor)
         {case "Image": return $Basics.round($Basics.toFloat(_p12._2) / $Basics.toFloat(_p12._1) * $Basics.toFloat(newWidth));
            case "RawHtml": return $Basics.snd(A2($Native$Graphics$Element.htmlHeight,newWidth,_p13));
            default: return _p14.height;}
      }();
      return Element_elm_builtin({element: _p13,props: _U.update(_p14,{width: newWidth,height: newHeight})});
   });
   var height = F2(function (newHeight,_p15) {
      var _p16 = _p15;
      return Element_elm_builtin({element: _p16._0.element,props: _U.update(_p16._0.props,{height: newHeight})});
   });
   var size = F3(function (w,h,e) {    return A2(height,h,A2(width,w,e));});
   var opacity = F2(function (givenOpacity,_p17) {
      var _p18 = _p17;
      return Element_elm_builtin({element: _p18._0.element,props: _U.update(_p18._0.props,{opacity: givenOpacity})});
   });
   var color = F2(function (clr,_p19) {
      var _p20 = _p19;
      return Element_elm_builtin({element: _p20._0.element,props: _U.update(_p20._0.props,{color: $Maybe.Just(clr)})});
   });
   var tag = F2(function (name,_p21) {    var _p22 = _p21;return Element_elm_builtin({element: _p22._0.element,props: _U.update(_p22._0.props,{tag: name})});});
   var link = F2(function (href,_p23) {
      var _p24 = _p23;
      return Element_elm_builtin({element: _p24._0.element,props: _U.update(_p24._0.props,{href: href})});
   });
   return _elm.Graphics.Element.values = {_op: _op
                                         ,image: image
                                         ,fittedImage: fittedImage
                                         ,croppedImage: croppedImage
                                         ,tiledImage: tiledImage
                                         ,leftAligned: leftAligned
                                         ,rightAligned: rightAligned
                                         ,centered: centered
                                         ,justified: justified
                                         ,show: show
                                         ,width: width
                                         ,height: height
                                         ,size: size
                                         ,color: color
                                         ,opacity: opacity
                                         ,link: link
                                         ,tag: tag
                                         ,widthOf: widthOf
                                         ,heightOf: heightOf
                                         ,sizeOf: sizeOf
                                         ,flow: flow
                                         ,up: up
                                         ,down: down
                                         ,left: left
                                         ,right: right
                                         ,inward: inward
                                         ,outward: outward
                                         ,layers: layers
                                         ,above: above
                                         ,below: below
                                         ,beside: beside
                                         ,empty: empty
                                         ,spacer: spacer
                                         ,container: container
                                         ,middle: middle
                                         ,midTop: midTop
                                         ,midBottom: midBottom
                                         ,midLeft: midLeft
                                         ,midRight: midRight
                                         ,topLeft: topLeft
                                         ,topRight: topRight
                                         ,bottomLeft: bottomLeft
                                         ,bottomRight: bottomRight
                                         ,absolute: absolute
                                         ,relative: relative
                                         ,middleAt: middleAt
                                         ,midTopAt: midTopAt
                                         ,midBottomAt: midBottomAt
                                         ,midLeftAt: midLeftAt
                                         ,midRightAt: midRightAt
                                         ,topLeftAt: topLeftAt
                                         ,topRightAt: topRightAt
                                         ,bottomLeftAt: bottomLeftAt
                                         ,bottomRightAt: bottomRightAt};
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values) return _elm.Graphics.Collage.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var _op = {};
   var Shape = function (a) {    return {ctor: "Shape",_0: a};};
   var polygon = function (points) {    return Shape(points);};
   var rect = F2(function (w,h) {
      var hh = h / 2;
      var hw = w / 2;
      return Shape(_U.list([{ctor: "_Tuple2",_0: 0 - hw,_1: 0 - hh}
                           ,{ctor: "_Tuple2",_0: 0 - hw,_1: hh}
                           ,{ctor: "_Tuple2",_0: hw,_1: hh}
                           ,{ctor: "_Tuple2",_0: hw,_1: 0 - hh}]));
   });
   var square = function (n) {    return A2(rect,n,n);};
   var oval = F2(function (w,h) {
      var hh = h / 2;
      var hw = w / 2;
      var n = 50;
      var t = 2 * $Basics.pi / n;
      var f = function (i) {    return {ctor: "_Tuple2",_0: hw * $Basics.cos(t * i),_1: hh * $Basics.sin(t * i)};};
      return Shape(A2($List.map,f,_U.range(0,n - 1)));
   });
   var circle = function (r) {    return A2(oval,2 * r,2 * r);};
   var ngon = F2(function (n,r) {
      var m = $Basics.toFloat(n);
      var t = 2 * $Basics.pi / m;
      var f = function (i) {    return {ctor: "_Tuple2",_0: r * $Basics.cos(t * i),_1: r * $Basics.sin(t * i)};};
      return Shape(A2($List.map,f,_U.range(0,m - 1)));
   });
   var Path = function (a) {    return {ctor: "Path",_0: a};};
   var path = function (ps) {    return Path(ps);};
   var segment = F2(function (p1,p2) {    return Path(_U.list([p1,p2]));});
   var collage = $Native$Graphics$Collage.collage;
   var Fill = function (a) {    return {ctor: "Fill",_0: a};};
   var Line = function (a) {    return {ctor: "Line",_0: a};};
   var FGroup = F2(function (a,b) {    return {ctor: "FGroup",_0: a,_1: b};});
   var FElement = function (a) {    return {ctor: "FElement",_0: a};};
   var FImage = F4(function (a,b,c,d) {    return {ctor: "FImage",_0: a,_1: b,_2: c,_3: d};});
   var FText = function (a) {    return {ctor: "FText",_0: a};};
   var FOutlinedText = F2(function (a,b) {    return {ctor: "FOutlinedText",_0: a,_1: b};});
   var FShape = F2(function (a,b) {    return {ctor: "FShape",_0: a,_1: b};});
   var FPath = F2(function (a,b) {    return {ctor: "FPath",_0: a,_1: b};});
   var LineStyle = F6(function (a,b,c,d,e,f) {    return {color: a,width: b,cap: c,join: d,dashing: e,dashOffset: f};});
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {    return {ctor: "Sharp",_0: a};};
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {color: $Color.black,width: 1,cap: Flat,join: Sharp(10),dashing: _U.list([]),dashOffset: 0};
   var solid = function (clr) {    return _U.update(defaultLine,{color: clr});};
   var dashed = function (clr) {    return _U.update(defaultLine,{color: clr,dashing: _U.list([8,4])});};
   var dotted = function (clr) {    return _U.update(defaultLine,{color: clr,dashing: _U.list([3,3])});};
   var Grad = function (a) {    return {ctor: "Grad",_0: a};};
   var Texture = function (a) {    return {ctor: "Texture",_0: a};};
   var Solid = function (a) {    return {ctor: "Solid",_0: a};};
   var Form_elm_builtin = function (a) {    return {ctor: "Form_elm_builtin",_0: a};};
   var form = function (f) {    return Form_elm_builtin({theta: 0,scale: 1,x: 0,y: 0,alpha: 1,form: f});};
   var fill = F2(function (style,_p0) {    var _p1 = _p0;return form(A2(FShape,Fill(style),_p1._0));});
   var filled = F2(function (color,shape) {    return A2(fill,Solid(color),shape);});
   var textured = F2(function (src,shape) {    return A2(fill,Texture(src),shape);});
   var gradient = F2(function (grad,shape) {    return A2(fill,Grad(grad),shape);});
   var outlined = F2(function (style,_p2) {    var _p3 = _p2;return form(A2(FShape,Line(style),_p3._0));});
   var traced = F2(function (style,_p4) {    var _p5 = _p4;return form(A2(FPath,style,_p5._0));});
   var sprite = F4(function (w,h,pos,src) {    return form(A4(FImage,w,h,pos,src));});
   var toForm = function (e) {    return form(FElement(e));};
   var group = function (fs) {    return form(A2(FGroup,$Transform2D.identity,fs));};
   var groupTransform = F2(function (matrix,fs) {    return form(A2(FGroup,matrix,fs));});
   var text = function (t) {    return form(FText(t));};
   var outlinedText = F2(function (ls,t) {    return form(A2(FOutlinedText,ls,t));});
   var move = F2(function (_p7,_p6) {
      var _p8 = _p7;
      var _p9 = _p6;
      var _p10 = _p9._0;
      return Form_elm_builtin(_U.update(_p10,{x: _p10.x + _p8._0,y: _p10.y + _p8._1}));
   });
   var moveX = F2(function (x,_p11) {    var _p12 = _p11;var _p13 = _p12._0;return Form_elm_builtin(_U.update(_p13,{x: _p13.x + x}));});
   var moveY = F2(function (y,_p14) {    var _p15 = _p14;var _p16 = _p15._0;return Form_elm_builtin(_U.update(_p16,{y: _p16.y + y}));});
   var scale = F2(function (s,_p17) {    var _p18 = _p17;var _p19 = _p18._0;return Form_elm_builtin(_U.update(_p19,{scale: _p19.scale * s}));});
   var rotate = F2(function (t,_p20) {    var _p21 = _p20;var _p22 = _p21._0;return Form_elm_builtin(_U.update(_p22,{theta: _p22.theta + t}));});
   var alpha = F2(function (a,_p23) {    var _p24 = _p23;return Form_elm_builtin(_U.update(_p24._0,{alpha: a}));});
   return _elm.Graphics.Collage.values = {_op: _op
                                         ,collage: collage
                                         ,toForm: toForm
                                         ,filled: filled
                                         ,textured: textured
                                         ,gradient: gradient
                                         ,outlined: outlined
                                         ,traced: traced
                                         ,text: text
                                         ,outlinedText: outlinedText
                                         ,move: move
                                         ,moveX: moveX
                                         ,moveY: moveY
                                         ,scale: scale
                                         ,rotate: rotate
                                         ,alpha: alpha
                                         ,group: group
                                         ,groupTransform: groupTransform
                                         ,rect: rect
                                         ,oval: oval
                                         ,square: square
                                         ,circle: circle
                                         ,ngon: ngon
                                         ,polygon: polygon
                                         ,segment: segment
                                         ,path: path
                                         ,solid: solid
                                         ,dashed: dashed
                                         ,dotted: dotted
                                         ,defaultLine: defaultLine
                                         ,LineStyle: LineStyle
                                         ,Flat: Flat
                                         ,Round: Round
                                         ,Padded: Padded
                                         ,Smooth: Smooth
                                         ,Sharp: Sharp
                                         ,Clipped: Clipped};
};
Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Utils.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary: F3(watchSummary)
	};
};

Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values) return _elm.Debug.values;
   var _U = Elm.Native.Utils.make(_elm),$Graphics$Collage = Elm.Graphics.Collage.make(_elm),$Native$Debug = Elm.Native.Debug.make(_elm);
   var _op = {};
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   return _elm.Debug.values = {_op: _op,log: log,crash: crash,watch: watch,watchSummary: watchSummary,trace: trace};
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values) return _elm.Result.values;
   var _U = Elm.Native.Utils.make(_elm),$Maybe = Elm.Maybe.make(_elm);
   var _op = {};
   var toMaybe = function (result) {    var _p0 = result;if (_p0.ctor === "Ok") {    return $Maybe.Just(_p0._0);} else {    return $Maybe.Nothing;}};
   var withDefault = F2(function (def,result) {    var _p1 = result;if (_p1.ctor === "Ok") {    return _p1._0;} else {    return def;}});
   var Err = function (a) {    return {ctor: "Err",_0: a};};
   var andThen = F2(function (result,callback) {    var _p2 = result;if (_p2.ctor === "Ok") {    return callback(_p2._0);} else {    return Err(_p2._0);}});
   var Ok = function (a) {    return {ctor: "Ok",_0: a};};
   var map = F2(function (func,ra) {    var _p3 = ra;if (_p3.ctor === "Ok") {    return Ok(func(_p3._0));} else {    return Err(_p3._0);}});
   var map2 = F3(function (func,ra,rb) {
      var _p4 = {ctor: "_Tuple2",_0: ra,_1: rb};
      if (_p4._0.ctor === "Ok") {
            if (_p4._1.ctor === "Ok") {
                  return Ok(A2(func,_p4._0._0,_p4._1._0));
               } else {
                  return Err(_p4._1._0);
               }
         } else {
            return Err(_p4._0._0);
         }
   });
   var map3 = F4(function (func,ra,rb,rc) {
      var _p5 = {ctor: "_Tuple3",_0: ra,_1: rb,_2: rc};
      if (_p5._0.ctor === "Ok") {
            if (_p5._1.ctor === "Ok") {
                  if (_p5._2.ctor === "Ok") {
                        return Ok(A3(func,_p5._0._0,_p5._1._0,_p5._2._0));
                     } else {
                        return Err(_p5._2._0);
                     }
               } else {
                  return Err(_p5._1._0);
               }
         } else {
            return Err(_p5._0._0);
         }
   });
   var map4 = F5(function (func,ra,rb,rc,rd) {
      var _p6 = {ctor: "_Tuple4",_0: ra,_1: rb,_2: rc,_3: rd};
      if (_p6._0.ctor === "Ok") {
            if (_p6._1.ctor === "Ok") {
                  if (_p6._2.ctor === "Ok") {
                        if (_p6._3.ctor === "Ok") {
                              return Ok(A4(func,_p6._0._0,_p6._1._0,_p6._2._0,_p6._3._0));
                           } else {
                              return Err(_p6._3._0);
                           }
                     } else {
                        return Err(_p6._2._0);
                     }
               } else {
                  return Err(_p6._1._0);
               }
         } else {
            return Err(_p6._0._0);
         }
   });
   var map5 = F6(function (func,ra,rb,rc,rd,re) {
      var _p7 = {ctor: "_Tuple5",_0: ra,_1: rb,_2: rc,_3: rd,_4: re};
      if (_p7._0.ctor === "Ok") {
            if (_p7._1.ctor === "Ok") {
                  if (_p7._2.ctor === "Ok") {
                        if (_p7._3.ctor === "Ok") {
                              if (_p7._4.ctor === "Ok") {
                                    return Ok(A5(func,_p7._0._0,_p7._1._0,_p7._2._0,_p7._3._0,_p7._4._0));
                                 } else {
                                    return Err(_p7._4._0);
                                 }
                           } else {
                              return Err(_p7._3._0);
                           }
                     } else {
                        return Err(_p7._2._0);
                     }
               } else {
                  return Err(_p7._1._0);
               }
         } else {
            return Err(_p7._0._0);
         }
   });
   var formatError = F2(function (f,result) {    var _p8 = result;if (_p8.ctor === "Ok") {    return Ok(_p8._0);} else {    return Err(f(_p8._0));}});
   var fromMaybe = F2(function (err,maybe) {    var _p9 = maybe;if (_p9.ctor === "Just") {    return Ok(_p9._0);} else {    return Err(err);}});
   return _elm.Result.values = {_op: _op
                               ,withDefault: withDefault
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,andThen: andThen
                               ,toMaybe: toMaybe
                               ,fromMaybe: fromMaybe
                               ,formatError: formatError
                               ,Ok: Ok
                               ,Err: Err};
};
Elm.Native.Signal = {};

Elm.Native.Signal.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a, b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a, b, c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a, b, c, d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a, b, c, d, e]);
	}


	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			value: A2(tieBreaker, leftStream.value, rightStream.value),
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.Task = {};

Elm.Native.Task.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			if (workQueue.length > 0)
			{
				var task = workQueue[0];

				setTimeout(function() {
					runTask(task, onComplete);
				}, 0);
			}
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values) return _elm.Task.values;
   var _U = Elm.Native.Utils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {    return {ctor: "ThreadID",_0: a};};
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,task) {    return A2(onError,task,function (err) {    return fail(f(err));});});
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,taskA) {    return A2(andThen,taskA,function (a) {    return succeed(func(a));});});
   var map2 = F3(function (func,taskA,taskB) {
      return A2(andThen,taskA,function (a) {    return A2(andThen,taskB,function (b) {    return succeed(A2(func,a,b));});});
   });
   var map3 = F4(function (func,taskA,taskB,taskC) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,taskB,function (b) {    return A2(andThen,taskC,function (c) {    return succeed(A3(func,a,b,c));});});
      });
   });
   var map4 = F5(function (func,taskA,taskB,taskC,taskD) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,taskC,function (c) {    return A2(andThen,taskD,function (d) {    return succeed(A4(func,a,b,c,d));});});
         });
      });
   });
   var map5 = F6(function (func,taskA,taskB,taskC,taskD,taskE) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,
            taskC,
            function (c) {
               return A2(andThen,taskD,function (d) {    return A2(andThen,taskE,function (e) {    return succeed(A5(func,a,b,c,d,e));});});
            });
         });
      });
   });
   var andMap = F2(function (taskFunc,taskValue) {
      return A2(andThen,taskFunc,function (func) {    return A2(andThen,taskValue,function (value) {    return succeed(func(value));});});
   });
   var sequence = function (tasks) {
      var _p0 = tasks;
      if (_p0.ctor === "[]") {
            return succeed(_U.list([]));
         } else {
            return A3(map2,F2(function (x,y) {    return A2($List._op["::"],x,y);}),_p0._0,sequence(_p0._1));
         }
   };
   var toMaybe = function (task) {    return A2(onError,A2(map,$Maybe.Just,task),function (_p1) {    return succeed($Maybe.Nothing);});};
   var fromMaybe = F2(function ($default,maybe) {    var _p2 = maybe;if (_p2.ctor === "Just") {    return succeed(_p2._0);} else {    return fail($default);}});
   var toResult = function (task) {    return A2(onError,A2(map,$Result.Ok,task),function (msg) {    return succeed($Result.Err(msg));});};
   var fromResult = function (result) {    var _p3 = result;if (_p3.ctor === "Ok") {    return succeed(_p3._0);} else {    return fail(_p3._0);}};
   var Task = {ctor: "Task"};
   return _elm.Task.values = {_op: _op
                             ,succeed: succeed
                             ,fail: fail
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,andMap: andMap
                             ,sequence: sequence
                             ,andThen: andThen
                             ,onError: onError
                             ,mapError: mapError
                             ,toMaybe: toMaybe
                             ,fromMaybe: fromMaybe
                             ,toResult: toResult
                             ,fromResult: fromResult
                             ,spawn: spawn
                             ,sleep: sleep};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values) return _elm.Signal.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var send = F2(function (_p0,value) {
      var _p1 = _p0;
      return A2($Task.onError,_p1._0(value),function (_p2) {    return $Task.succeed({ctor: "_Tuple0"});});
   });
   var Message = function (a) {    return {ctor: "Message",_0: a};};
   var message = F2(function (_p3,value) {    var _p4 = _p3;return Message(_p4._0(value));});
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {    return {ctor: "Address",_0: a};};
   var forwardTo = F2(function (_p5,f) {    var _p6 = _p5;return Address(function (x) {    return _p6._0(f(x));});});
   var Mailbox = F2(function (a,b) {    return {address: a,signal: b};});
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,base,signal) {
      return A3(filterMap,function (value) {    return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;},base,signal);
   });
   var merge = F2(function (left,right) {    return A3($Native$Signal.genericMerge,$Basics.always,left,right);});
   var mergeMany = function (signalList) {
      var _p7 = $List.reverse(signalList);
      if (_p7.ctor === "[]") {
            return _U.crashCase("Signal",{start: {line: 184,column: 3},end: {line: 189,column: 40}},_p7)("mergeMany was given an empty list!");
         } else {
            return A3($List.foldl,merge,_p7._0,_p7._1);
         }
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   var map = $Native$Signal.map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   return _elm.Signal.values = {_op: _op
                               ,merge: merge
                               ,mergeMany: mergeMany
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,constant: constant
                               ,dropRepeats: dropRepeats
                               ,filter: filter
                               ,filterMap: filterMap
                               ,sampleOn: sampleOn
                               ,foldp: foldp
                               ,mailbox: mailbox
                               ,send: send
                               ,message: message
                               ,forwardTo: forwardTo
                               ,Mailbox: Mailbox};
};
Elm.Native.String = {};

Elm.Native.String.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr, str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd = str[0];
		if (hd)
		{
			return Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)));
		}
		return Maybe.Nothing;
	}
	function append(a, b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f, str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred, str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f, b, str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f, b, str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}
	function slice(start, end, str)
	{
		return str.slice(start, end);
	}
	function left(n, str)
	{
		return n < 1 ? '' : str.slice(0, n);
	}
	function right(n, str)
	{
		return n < 1 ? '' : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0, -n);
	}
	function pad(n, chr, str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
	}
	function padRight(n, chr, str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n, chr, str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.replace(/^\s+/, '');
	}
	function trimRight(str)
	{
		return str.replace(/\s+$/, '');
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode: function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode: function(c) { return c.charCodeAt(0); },
		toUpper: function(c) { return Utils.chr(c.toUpperCase()); },
		toLower: function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper: function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower: function(c) { return Utils.chr(c.toLocaleLowerCase()); }
	};
};

Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values) return _elm.Char.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm),$Native$Char = Elm.Native.Char.make(_elm);
   var _op = {};
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,high,$char) {    var code = toCode($char);return _U.cmp(code,toCode(low)) > -1 && _U.cmp(code,toCode(high)) < 1;});
   var isUpper = A2(isBetween,_U.chr("A"),_U.chr("Z"));
   var isLower = A2(isBetween,_U.chr("a"),_U.chr("z"));
   var isDigit = A2(isBetween,_U.chr("0"),_U.chr("9"));
   var isOctDigit = A2(isBetween,_U.chr("0"),_U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,_U.chr("a"),_U.chr("f"),$char) || A3(isBetween,_U.chr("A"),_U.chr("F"),$char));
   };
   return _elm.Char.values = {_op: _op
                             ,isUpper: isUpper
                             ,isLower: isLower
                             ,isDigit: isDigit
                             ,isOctDigit: isOctDigit
                             ,isHexDigit: isHexDigit
                             ,toUpper: toUpper
                             ,toLower: toLower
                             ,toLocaleUpper: toLocaleUpper
                             ,toLocaleLower: toLocaleLower
                             ,toCode: toCode
                             ,fromCode: fromCode};
};
Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values) return _elm.String.values;
   var _U = Elm.Native.Utils.make(_elm),$Maybe = Elm.Maybe.make(_elm),$Native$String = Elm.Native.String.make(_elm),$Result = Elm.Result.make(_elm);
   var _op = {};
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {    return A2(cons,$char,"");};
   var isEmpty = $Native$String.isEmpty;
   return _elm.String.values = {_op: _op
                               ,isEmpty: isEmpty
                               ,length: length
                               ,reverse: reverse
                               ,repeat: repeat
                               ,cons: cons
                               ,uncons: uncons
                               ,fromChar: fromChar
                               ,append: append
                               ,concat: concat
                               ,split: split
                               ,join: join
                               ,words: words
                               ,lines: lines
                               ,slice: slice
                               ,left: left
                               ,right: right
                               ,dropLeft: dropLeft
                               ,dropRight: dropRight
                               ,contains: contains
                               ,startsWith: startsWith
                               ,endsWith: endsWith
                               ,indexes: indexes
                               ,indices: indices
                               ,toInt: toInt
                               ,toFloat: toFloat
                               ,toList: toList
                               ,fromList: fromList
                               ,toUpper: toUpper
                               ,toLower: toLower
                               ,pad: pad
                               ,padLeft: padLeft
                               ,padRight: padRight
                               ,trim: trim
                               ,trimLeft: trimLeft
                               ,trimRight: trimRight
                               ,map: map
                               ,filter: filter
                               ,foldl: foldl
                               ,foldr: foldr
                               ,any: any
                               ,all: all};
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values) return _elm.Dict.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var foldr = F3(function (f,acc,t) {
      foldr: while (true) {
         var _p0 = t;
         if (_p0.ctor === "RBEmpty_elm_builtin") {
               return acc;
            } else {
               var _v1 = f,_v2 = A3(f,_p0._1,_p0._2,A3(foldr,f,acc,_p0._4)),_v3 = _p0._3;
               f = _v1;
               acc = _v2;
               t = _v3;
               continue foldr;
            }
      }
   });
   var keys = function (dict) {    return A3(foldr,F3(function (key,value,keyList) {    return A2($List._op["::"],key,keyList);}),_U.list([]),dict);};
   var values = function (dict) {    return A3(foldr,F3(function (key,value,valueList) {    return A2($List._op["::"],value,valueList);}),_U.list([]),dict);};
   var toList = function (dict) {
      return A3(foldr,F3(function (key,value,list) {    return A2($List._op["::"],{ctor: "_Tuple2",_0: key,_1: value},list);}),_U.list([]),dict);
   };
   var foldl = F3(function (f,acc,dict) {
      foldl: while (true) {
         var _p1 = dict;
         if (_p1.ctor === "RBEmpty_elm_builtin") {
               return acc;
            } else {
               var _v5 = f,_v6 = A3(f,_p1._1,_p1._2,A3(foldl,f,acc,_p1._3)),_v7 = _p1._4;
               f = _v5;
               acc = _v6;
               dict = _v7;
               continue foldl;
            }
      }
   });
   var reportRemBug = F4(function (msg,c,lgot,rgot) {
      return $Native$Debug.crash($String.concat(_U.list(["Internal red-black tree invariant violated, expected "
                                                        ,msg
                                                        ," and got "
                                                        ,$Basics.toString(c)
                                                        ,"/"
                                                        ,lgot
                                                        ,"/"
                                                        ,rgot
                                                        ,"\nPlease report this bug to <https://github.com/elm-lang/core/issues>"])));
   });
   var isBBlack = function (dict) {
      var _p2 = dict;
      _v8_2: do {
         if (_p2.ctor === "RBNode_elm_builtin") {
               if (_p2._0.ctor === "BBlack") {
                     return true;
                  } else {
                     break _v8_2;
                  }
            } else {
               if (_p2._0.ctor === "LBBlack") {
                     return true;
                  } else {
                     break _v8_2;
                  }
            }
      } while (false);
      return false;
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var sizeHelp = F2(function (n,dict) {
      sizeHelp: while (true) {
         var _p3 = dict;
         if (_p3.ctor === "RBEmpty_elm_builtin") {
               return n;
            } else {
               var _v10 = A2(sizeHelp,n + 1,_p3._4),_v11 = _p3._3;
               n = _v10;
               dict = _v11;
               continue sizeHelp;
            }
      }
   });
   var size = function (dict) {    return A2(sizeHelp,0,dict);};
   var get = F2(function (targetKey,dict) {
      get: while (true) {
         var _p4 = dict;
         if (_p4.ctor === "RBEmpty_elm_builtin") {
               return $Maybe.Nothing;
            } else {
               var _p5 = A2($Basics.compare,targetKey,_p4._1);
               switch (_p5.ctor)
               {case "LT": var _v14 = targetKey,_v15 = _p4._3;
                    targetKey = _v14;
                    dict = _v15;
                    continue get;
                  case "EQ": return $Maybe.Just(_p4._2);
                  default: var _v16 = targetKey,_v17 = _p4._4;
                    targetKey = _v16;
                    dict = _v17;
                    continue get;}
            }
      }
   });
   var member = F2(function (key,dict) {    var _p6 = A2(get,key,dict);if (_p6.ctor === "Just") {    return true;} else {    return false;}});
   var maxWithDefault = F3(function (k,v,r) {
      maxWithDefault: while (true) {
         var _p7 = r;
         if (_p7.ctor === "RBEmpty_elm_builtin") {
               return {ctor: "_Tuple2",_0: k,_1: v};
            } else {
               var _v20 = _p7._1,_v21 = _p7._2,_v22 = _p7._4;
               k = _v20;
               v = _v21;
               r = _v22;
               continue maxWithDefault;
            }
      }
   });
   var RBEmpty_elm_builtin = function (a) {    return {ctor: "RBEmpty_elm_builtin",_0: a};};
   var RBNode_elm_builtin = F5(function (a,b,c,d,e) {    return {ctor: "RBNode_elm_builtin",_0: a,_1: b,_2: c,_3: d,_4: e};});
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty_elm_builtin(LBlack);
   var isEmpty = function (dict) {    return _U.eq(dict,empty);};
   var map = F2(function (f,dict) {
      var _p8 = dict;
      if (_p8.ctor === "RBEmpty_elm_builtin") {
            return RBEmpty_elm_builtin(LBlack);
         } else {
            var _p9 = _p8._1;
            return A5(RBNode_elm_builtin,_p8._0,_p9,A2(f,_p9,_p8._2),A2(map,f,_p8._3),A2(map,f,_p8._4));
         }
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      var _p10 = dict;
      if (_p10.ctor === "RBNode_elm_builtin" && _p10._0.ctor === "Red") {
            return A5(RBNode_elm_builtin,Black,_p10._1,_p10._2,_p10._3,_p10._4);
         } else {
            return dict;
         }
   };
   var blackish = function (t) {
      var _p11 = t;
      if (_p11.ctor === "RBNode_elm_builtin") {
            var _p12 = _p11._0;
            return _U.eq(_p12,Black) || _U.eq(_p12,BBlack);
         } else {
            return true;
         }
   };
   var blacken = function (t) {
      var _p13 = t;
      if (_p13.ctor === "RBEmpty_elm_builtin") {
            return RBEmpty_elm_builtin(LBlack);
         } else {
            return A5(RBNode_elm_builtin,Black,_p13._1,_p13._2,_p13._3,_p13._4);
         }
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      var _p14 = color;
      switch (_p14.ctor)
      {case "Black": return BBlack;
         case "Red": return Black;
         case "NBlack": return Red;
         default: return $Native$Debug.crash("Can\'t make a double black node more black!");}
   };
   var lessBlack = function (color) {
      var _p15 = color;
      switch (_p15.ctor)
      {case "BBlack": return Black;
         case "Black": return Red;
         case "Red": return NBlack;
         default: return $Native$Debug.crash("Can\'t make a negative black node less black!");}
   };
   var lessBlackTree = function (dict) {
      var _p16 = dict;
      if (_p16.ctor === "RBNode_elm_builtin") {
            return A5(RBNode_elm_builtin,lessBlack(_p16._0),_p16._1,_p16._2,_p16._3,_p16._4);
         } else {
            return RBEmpty_elm_builtin(LBlack);
         }
   };
   var balancedTree = function (col) {
      return function (xk) {
         return function (xv) {
            return function (yk) {
               return function (yv) {
                  return function (zk) {
                     return function (zv) {
                        return function (a) {
                           return function (b) {
                              return function (c) {
                                 return function (d) {
                                    return A5(RBNode_elm_builtin,
                                    lessBlack(col),
                                    yk,
                                    yv,
                                    A5(RBNode_elm_builtin,Black,xk,xv,a,b),
                                    A5(RBNode_elm_builtin,Black,zk,zv,c,d));
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var redden = function (t) {
      var _p17 = t;
      if (_p17.ctor === "RBEmpty_elm_builtin") {
            return $Native$Debug.crash("can\'t make a Leaf red");
         } else {
            return A5(RBNode_elm_builtin,Red,_p17._1,_p17._2,_p17._3,_p17._4);
         }
   };
   var balanceHelp = function (tree) {
      var _p18 = tree;
      _v31_6: do {
         _v31_5: do {
            _v31_4: do {
               _v31_3: do {
                  _v31_2: do {
                     _v31_1: do {
                        _v31_0: do {
                           if (_p18.ctor === "RBNode_elm_builtin") {
                                 if (_p18._3.ctor === "RBNode_elm_builtin") {
                                       if (_p18._4.ctor === "RBNode_elm_builtin") {
                                             switch (_p18._3._0.ctor)
                                             {case "Red": switch (_p18._4._0.ctor)
                                                  {case "Red": if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red") {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red") {
                                                                   break _v31_1;
                                                                } else {
                                                                   if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red") {
                                                                         break _v31_2;
                                                                      } else {
                                                                         if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red") {
                                                                               break _v31_3;
                                                                            } else {
                                                                               break _v31_6;
                                                                            }
                                                                      }
                                                                }
                                                          }
                                                     case "NBlack": if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red") {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red") {
                                                                   break _v31_1;
                                                                } else {
                                                                   if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_4;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          }
                                                     default: if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red") {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red") {
                                                                   break _v31_1;
                                                                } else {
                                                                   break _v31_6;
                                                                }
                                                          }}
                                                case "NBlack": switch (_p18._4._0.ctor)
                                                  {case "Red": if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red") {
                                                             break _v31_2;
                                                          } else {
                                                             if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red") {
                                                                   break _v31_3;
                                                                } else {
                                                                   if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_5;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          }
                                                     case "NBlack": if (_p18._0.ctor === "BBlack") {
                                                             if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                             {
                                                                   break _v31_4;
                                                                } else {
                                                                   if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_5;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          } else {
                                                             break _v31_6;
                                                          }
                                                     default:
                                                     if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                       {
                                                             break _v31_5;
                                                          } else {
                                                             break _v31_6;
                                                          }}
                                                default: switch (_p18._4._0.ctor)
                                                  {case "Red": if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red") {
                                                             break _v31_2;
                                                          } else {
                                                             if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red") {
                                                                   break _v31_3;
                                                                } else {
                                                                   break _v31_6;
                                                                }
                                                          }
                                                     case "NBlack":
                                                     if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                       {
                                                             break _v31_4;
                                                          } else {
                                                             break _v31_6;
                                                          }
                                                     default: break _v31_6;}}
                                          } else {
                                             switch (_p18._3._0.ctor)
                                             {case "Red": if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red") {
                                                        break _v31_0;
                                                     } else {
                                                        if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red") {
                                                              break _v31_1;
                                                           } else {
                                                              break _v31_6;
                                                           }
                                                     }
                                                case "NBlack":
                                                if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                  {
                                                        break _v31_5;
                                                     } else {
                                                        break _v31_6;
                                                     }
                                                default: break _v31_6;}
                                          }
                                    } else {
                                       if (_p18._4.ctor === "RBNode_elm_builtin") {
                                             switch (_p18._4._0.ctor)
                                             {case "Red": if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red") {
                                                        break _v31_2;
                                                     } else {
                                                        if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red") {
                                                              break _v31_3;
                                                           } else {
                                                              break _v31_6;
                                                           }
                                                     }
                                                case "NBlack":
                                                if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                  {
                                                        break _v31_4;
                                                     } else {
                                                        break _v31_6;
                                                     }
                                                default: break _v31_6;}
                                          } else {
                                             break _v31_6;
                                          }
                                    }
                              } else {
                                 break _v31_6;
                              }
                        } while (false);
                        return balancedTree(_p18._0)(_p18._3._3._1)(_p18._3._3._2)(_p18._3._1)(_p18._3._2)(_p18._1)(_p18._2)(_p18._3._3._3)(_p18._3._3._4)(_p18._3._4)(_p18._4);
                     } while (false);
                     return balancedTree(_p18._0)(_p18._3._1)(_p18._3._2)(_p18._3._4._1)(_p18._3._4._2)(_p18._1)(_p18._2)(_p18._3._3)(_p18._3._4._3)(_p18._3._4._4)(_p18._4);
                  } while (false);
                  return balancedTree(_p18._0)(_p18._1)(_p18._2)(_p18._4._3._1)(_p18._4._3._2)(_p18._4._1)(_p18._4._2)(_p18._3)(_p18._4._3._3)(_p18._4._3._4)(_p18._4._4);
               } while (false);
               return balancedTree(_p18._0)(_p18._1)(_p18._2)(_p18._4._1)(_p18._4._2)(_p18._4._4._1)(_p18._4._4._2)(_p18._3)(_p18._4._3)(_p18._4._4._3)(_p18._4._4._4);
            } while (false);
            return A5(RBNode_elm_builtin,
            Black,
            _p18._4._3._1,
            _p18._4._3._2,
            A5(RBNode_elm_builtin,Black,_p18._1,_p18._2,_p18._3,_p18._4._3._3),
            A5(balance,Black,_p18._4._1,_p18._4._2,_p18._4._3._4,redden(_p18._4._4)));
         } while (false);
         return A5(RBNode_elm_builtin,
         Black,
         _p18._3._4._1,
         _p18._3._4._2,
         A5(balance,Black,_p18._3._1,_p18._3._2,redden(_p18._3._3),_p18._3._4._3),
         A5(RBNode_elm_builtin,Black,_p18._1,_p18._2,_p18._3._4._4,_p18._4));
      } while (false);
      return tree;
   };
   var balance = F5(function (c,k,v,l,r) {    var tree = A5(RBNode_elm_builtin,c,k,v,l,r);return blackish(tree) ? balanceHelp(tree) : tree;});
   var bubble = F5(function (c,k,v,l,r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,moreBlack(c),k,v,lessBlackTree(l),lessBlackTree(r)) : A5(RBNode_elm_builtin,c,k,v,l,r);
   });
   var removeMax = F5(function (c,k,v,l,r) {
      var _p19 = r;
      if (_p19.ctor === "RBEmpty_elm_builtin") {
            return A3(rem,c,l,r);
         } else {
            return A5(bubble,c,k,v,l,A5(removeMax,_p19._0,_p19._1,_p19._2,_p19._3,_p19._4));
         }
   });
   var rem = F3(function (c,l,r) {
      var _p20 = {ctor: "_Tuple2",_0: l,_1: r};
      if (_p20._0.ctor === "RBEmpty_elm_builtin") {
            if (_p20._1.ctor === "RBEmpty_elm_builtin") {
                  var _p21 = c;
                  switch (_p21.ctor)
                  {case "Red": return RBEmpty_elm_builtin(LBlack);
                     case "Black": return RBEmpty_elm_builtin(LBBlack);
                     default: return $Native$Debug.crash("cannot have bblack or nblack nodes at this point");}
               } else {
                  var _p24 = _p20._1._0;
                  var _p23 = _p20._0._0;
                  var _p22 = {ctor: "_Tuple3",_0: c,_1: _p23,_2: _p24};
                  if (_p22.ctor === "_Tuple3" && _p22._0.ctor === "Black" && _p22._1.ctor === "LBlack" && _p22._2.ctor === "Red") {
                        return A5(RBNode_elm_builtin,Black,_p20._1._1,_p20._1._2,_p20._1._3,_p20._1._4);
                     } else {
                        return A4(reportRemBug,"Black/LBlack/Red",c,$Basics.toString(_p23),$Basics.toString(_p24));
                     }
               }
         } else {
            if (_p20._1.ctor === "RBEmpty_elm_builtin") {
                  var _p27 = _p20._1._0;
                  var _p26 = _p20._0._0;
                  var _p25 = {ctor: "_Tuple3",_0: c,_1: _p26,_2: _p27};
                  if (_p25.ctor === "_Tuple3" && _p25._0.ctor === "Black" && _p25._1.ctor === "Red" && _p25._2.ctor === "LBlack") {
                        return A5(RBNode_elm_builtin,Black,_p20._0._1,_p20._0._2,_p20._0._3,_p20._0._4);
                     } else {
                        return A4(reportRemBug,"Black/Red/LBlack",c,$Basics.toString(_p26),$Basics.toString(_p27));
                     }
               } else {
                  var _p31 = _p20._0._2;
                  var _p30 = _p20._0._4;
                  var _p29 = _p20._0._1;
                  var l$ = A5(removeMax,_p20._0._0,_p29,_p31,_p20._0._3,_p30);
                  var _p28 = A3(maxWithDefault,_p29,_p31,_p30);
                  var k = _p28._0;
                  var v = _p28._1;
                  return A5(bubble,c,k,v,l$,r);
               }
         }
   });
   var update = F3(function (k,alter,dict) {
      var up = function (dict) {
         var _p32 = dict;
         if (_p32.ctor === "RBEmpty_elm_builtin") {
               var _p33 = alter($Maybe.Nothing);
               if (_p33.ctor === "Nothing") {
                     return {ctor: "_Tuple2",_0: Same,_1: empty};
                  } else {
                     return {ctor: "_Tuple2",_0: Insert,_1: A5(RBNode_elm_builtin,Red,k,_p33._0,empty,empty)};
                  }
            } else {
               var _p44 = _p32._2;
               var _p43 = _p32._4;
               var _p42 = _p32._3;
               var _p41 = _p32._1;
               var _p40 = _p32._0;
               var _p34 = A2($Basics.compare,k,_p41);
               switch (_p34.ctor)
               {case "EQ": var _p35 = alter($Maybe.Just(_p44));
                    if (_p35.ctor === "Nothing") {
                          return {ctor: "_Tuple2",_0: Remove,_1: A3(rem,_p40,_p42,_p43)};
                       } else {
                          return {ctor: "_Tuple2",_0: Same,_1: A5(RBNode_elm_builtin,_p40,_p41,_p35._0,_p42,_p43)};
                       }
                  case "LT": var _p36 = up(_p42);
                    var flag = _p36._0;
                    var newLeft = _p36._1;
                    var _p37 = flag;
                    switch (_p37.ctor)
                    {case "Same": return {ctor: "_Tuple2",_0: Same,_1: A5(RBNode_elm_builtin,_p40,_p41,_p44,newLeft,_p43)};
                       case "Insert": return {ctor: "_Tuple2",_0: Insert,_1: A5(balance,_p40,_p41,_p44,newLeft,_p43)};
                       default: return {ctor: "_Tuple2",_0: Remove,_1: A5(bubble,_p40,_p41,_p44,newLeft,_p43)};}
                  default: var _p38 = up(_p43);
                    var flag = _p38._0;
                    var newRight = _p38._1;
                    var _p39 = flag;
                    switch (_p39.ctor)
                    {case "Same": return {ctor: "_Tuple2",_0: Same,_1: A5(RBNode_elm_builtin,_p40,_p41,_p44,_p42,newRight)};
                       case "Insert": return {ctor: "_Tuple2",_0: Insert,_1: A5(balance,_p40,_p41,_p44,_p42,newRight)};
                       default: return {ctor: "_Tuple2",_0: Remove,_1: A5(bubble,_p40,_p41,_p44,_p42,newRight)};}}
            }
      };
      var _p45 = up(dict);
      var flag = _p45._0;
      var updatedDict = _p45._1;
      var _p46 = flag;
      switch (_p46.ctor)
      {case "Same": return updatedDict;
         case "Insert": return ensureBlackRoot(updatedDict);
         default: return blacken(updatedDict);}
   });
   var insert = F3(function (key,value,dict) {    return A3(update,key,$Basics.always($Maybe.Just(value)),dict);});
   var singleton = F2(function (key,value) {    return A3(insert,key,value,empty);});
   var union = F2(function (t1,t2) {    return A3(foldl,insert,t2,t1);});
   var fromList = function (assocs) {
      return A3($List.foldl,F2(function (_p47,dict) {    var _p48 = _p47;return A3(insert,_p48._0,_p48._1,dict);}),empty,assocs);
   };
   var filter = F2(function (predicate,dictionary) {
      var add = F3(function (key,value,dict) {    return A2(predicate,key,value) ? A3(insert,key,value,dict) : dict;});
      return A3(foldl,add,empty,dictionary);
   });
   var intersect = F2(function (t1,t2) {    return A2(filter,F2(function (k,_p49) {    return A2(member,k,t2);}),t1);});
   var partition = F2(function (predicate,dict) {
      var add = F3(function (key,value,_p50) {
         var _p51 = _p50;
         var _p53 = _p51._1;
         var _p52 = _p51._0;
         return A2(predicate,key,value) ? {ctor: "_Tuple2",_0: A3(insert,key,value,_p52),_1: _p53} : {ctor: "_Tuple2",_0: _p52,_1: A3(insert,key,value,_p53)};
      });
      return A3(foldl,add,{ctor: "_Tuple2",_0: empty,_1: empty},dict);
   });
   var remove = F2(function (key,dict) {    return A3(update,key,$Basics.always($Maybe.Nothing),dict);});
   var diff = F2(function (t1,t2) {    return A3(foldl,F3(function (k,v,t) {    return A2(remove,k,t);}),t1,t2);});
   return _elm.Dict.values = {_op: _op
                             ,empty: empty
                             ,singleton: singleton
                             ,insert: insert
                             ,update: update
                             ,isEmpty: isEmpty
                             ,get: get
                             ,remove: remove
                             ,member: member
                             ,size: size
                             ,filter: filter
                             ,partition: partition
                             ,foldl: foldl
                             ,foldr: foldr
                             ,map: map
                             ,union: union
                             ,intersect: intersect
                             ,diff: diff
                             ,keys: keys
                             ,values: values
                             ,toList: toList
                             ,fromList: fromList};
};
Elm.Set = Elm.Set || {};
Elm.Set.make = function (_elm) {
   "use strict";
   _elm.Set = _elm.Set || {};
   if (_elm.Set.values) return _elm.Set.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm),$Dict = Elm.Dict.make(_elm),$List = Elm.List.make(_elm);
   var _op = {};
   var foldr = F3(function (f,b,_p0) {    var _p1 = _p0;return A3($Dict.foldr,F3(function (k,_p2,b) {    return A2(f,k,b);}),b,_p1._0);});
   var foldl = F3(function (f,b,_p3) {    var _p4 = _p3;return A3($Dict.foldl,F3(function (k,_p5,b) {    return A2(f,k,b);}),b,_p4._0);});
   var toList = function (_p6) {    var _p7 = _p6;return $Dict.keys(_p7._0);};
   var size = function (_p8) {    var _p9 = _p8;return $Dict.size(_p9._0);};
   var member = F2(function (k,_p10) {    var _p11 = _p10;return A2($Dict.member,k,_p11._0);});
   var isEmpty = function (_p12) {    var _p13 = _p12;return $Dict.isEmpty(_p13._0);};
   var Set_elm_builtin = function (a) {    return {ctor: "Set_elm_builtin",_0: a};};
   var empty = Set_elm_builtin($Dict.empty);
   var singleton = function (k) {    return Set_elm_builtin(A2($Dict.singleton,k,{ctor: "_Tuple0"}));};
   var insert = F2(function (k,_p14) {    var _p15 = _p14;return Set_elm_builtin(A3($Dict.insert,k,{ctor: "_Tuple0"},_p15._0));});
   var fromList = function (xs) {    return A3($List.foldl,insert,empty,xs);};
   var map = F2(function (f,s) {    return fromList(A2($List.map,f,toList(s)));});
   var remove = F2(function (k,_p16) {    var _p17 = _p16;return Set_elm_builtin(A2($Dict.remove,k,_p17._0));});
   var union = F2(function (_p19,_p18) {    var _p20 = _p19;var _p21 = _p18;return Set_elm_builtin(A2($Dict.union,_p20._0,_p21._0));});
   var intersect = F2(function (_p23,_p22) {    var _p24 = _p23;var _p25 = _p22;return Set_elm_builtin(A2($Dict.intersect,_p24._0,_p25._0));});
   var diff = F2(function (_p27,_p26) {    var _p28 = _p27;var _p29 = _p26;return Set_elm_builtin(A2($Dict.diff,_p28._0,_p29._0));});
   var filter = F2(function (p,_p30) {    var _p31 = _p30;return Set_elm_builtin(A2($Dict.filter,F2(function (k,_p32) {    return p(k);}),_p31._0));});
   var partition = F2(function (p,_p33) {
      var _p34 = _p33;
      var _p35 = A2($Dict.partition,F2(function (k,_p36) {    return p(k);}),_p34._0);
      var p1 = _p35._0;
      var p2 = _p35._1;
      return {ctor: "_Tuple2",_0: Set_elm_builtin(p1),_1: Set_elm_builtin(p2)};
   });
   return _elm.Set.values = {_op: _op
                            ,empty: empty
                            ,singleton: singleton
                            ,insert: insert
                            ,remove: remove
                            ,isEmpty: isEmpty
                            ,member: member
                            ,size: size
                            ,foldl: foldl
                            ,foldr: foldr
                            ,map: map
                            ,filter: filter
                            ,partition: partition
                            ,union: union
                            ,intersect: intersect
                            ,diff: diff
                            ,toList: toList
                            ,fromList: fromList};
};
Elm.List = Elm.List || {};
Elm.List.Extra = Elm.List.Extra || {};
Elm.List.Extra.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   _elm.List.Extra = _elm.List.Extra || {};
   if (_elm.List.Extra.values) return _elm.List.Extra.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var zip5 = $List.map5(F5(function (v0,v1,v2,v3,v4) {    return {ctor: "_Tuple5",_0: v0,_1: v1,_2: v2,_3: v3,_4: v4};}));
   var zip4 = $List.map4(F4(function (v0,v1,v2,v3) {    return {ctor: "_Tuple4",_0: v0,_1: v1,_2: v2,_3: v3};}));
   var zip3 = $List.map3(F3(function (v0,v1,v2) {    return {ctor: "_Tuple3",_0: v0,_1: v1,_2: v2};}));
   var zip = $List.map2(F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}));
   var isPrefixOf = function (prefix) {
      return function (_p0) {
         return A2($List.all,$Basics.identity,A3($List.map2,F2(function (x,y) {    return _U.eq(x,y);}),prefix,_p0));
      };
   };
   var isSuffixOf = F2(function (suffix,xs) {    return A2(isPrefixOf,$List.reverse(suffix),$List.reverse(xs));});
   var selectSplit = function (xs) {
      var _p1 = xs;
      if (_p1.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p5 = _p1._1;
            var _p4 = _p1._0;
            return A2($List._op["::"],
            {ctor: "_Tuple3",_0: _U.list([]),_1: _p4,_2: _p5},
            A2($List.map,
            function (_p2) {
               var _p3 = _p2;
               return {ctor: "_Tuple3",_0: A2($List._op["::"],_p4,_p3._0),_1: _p3._1,_2: _p3._2};
            },
            selectSplit(_p5)));
         }
   };
   var select = function (xs) {
      var _p6 = xs;
      if (_p6.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p10 = _p6._1;
            var _p9 = _p6._0;
            return A2($List._op["::"],
            {ctor: "_Tuple2",_0: _p9,_1: _p10},
            A2($List.map,function (_p7) {    var _p8 = _p7;return {ctor: "_Tuple2",_0: _p8._0,_1: A2($List._op["::"],_p9,_p8._1)};},select(_p10)));
         }
   };
   var tailsHelp = F2(function (e,list) {
      var _p11 = list;
      if (_p11.ctor === "::") {
            var _p12 = _p11._0;
            return A2($List._op["::"],A2($List._op["::"],e,_p12),A2($List._op["::"],_p12,_p11._1));
         } else {
            return _U.list([]);
         }
   });
   var tails = A2($List.foldr,tailsHelp,_U.list([_U.list([])]));
   var isInfixOf = F2(function (infix,xs) {    return A2($List.any,isPrefixOf(infix),tails(xs));});
   var inits = A2($List.foldr,
   F2(function (e,acc) {    return A2($List._op["::"],_U.list([]),A2($List.map,F2(function (x,y) {    return A2($List._op["::"],x,y);})(e),acc));}),
   _U.list([_U.list([])]));
   var groupByTransitive = F2(function (cmp,xs$) {
      var _p13 = xs$;
      if (_p13.ctor === "[]") {
            return _U.list([]);
         } else {
            if (_p13._1.ctor === "[]") {
                  return _U.list([_U.list([_p13._0])]);
               } else {
                  var _p15 = _p13._0;
                  var _p14 = A2(groupByTransitive,cmp,_p13._1);
                  if (_p14.ctor === "::") {
                        return A2(cmp,_p15,_p13._1._0) ? A2($List._op["::"],A2($List._op["::"],_p15,_p14._0),_p14._1) : A2($List._op["::"],
                        _U.list([_p15]),
                        _p14);
                     } else {
                        return _U.list([]);
                     }
               }
         }
   });
   var stripPrefix = F2(function (prefix,xs) {
      var step = F2(function (e,m) {
         var _p16 = m;
         if (_p16.ctor === "Nothing") {
               return $Maybe.Nothing;
            } else {
               if (_p16._0.ctor === "[]") {
                     return $Maybe.Nothing;
                  } else {
                     return _U.eq(e,_p16._0._0) ? $Maybe.Just(_p16._0._1) : $Maybe.Nothing;
                  }
            }
      });
      return A3($List.foldl,step,$Maybe.Just(xs),prefix);
   });
   var dropWhileEnd = function (p) {
      return A2($List.foldr,F2(function (x,xs) {    return p(x) && $List.isEmpty(xs) ? _U.list([]) : A2($List._op["::"],x,xs);}),_U.list([]));
   };
   var takeWhileEnd = function (p) {
      var step = F2(function (x,_p17) {
         var _p18 = _p17;
         var _p19 = _p18._0;
         return p(x) && _p18._1 ? {ctor: "_Tuple2",_0: A2($List._op["::"],x,_p19),_1: true} : {ctor: "_Tuple2",_0: _p19,_1: false};
      });
      return function (_p20) {
         return $Basics.fst(A3($List.foldr,step,{ctor: "_Tuple2",_0: _U.list([]),_1: true},_p20));
      };
   };
   var splitAt = F2(function (n,xs) {    return {ctor: "_Tuple2",_0: A2($List.take,n,xs),_1: A2($List.drop,n,xs)};});
   var unfoldr = F2(function (f,seed) {
      var _p21 = f(seed);
      if (_p21.ctor === "Nothing") {
            return _U.list([]);
         } else {
            return A2($List._op["::"],_p21._0._0,A2(unfoldr,f,_p21._0._1));
         }
   });
   var scanr1 = F2(function (f,xs$) {
      var _p22 = xs$;
      if (_p22.ctor === "[]") {
            return _U.list([]);
         } else {
            if (_p22._1.ctor === "[]") {
                  return _U.list([_p22._0]);
               } else {
                  var _p23 = A2(scanr1,f,_p22._1);
                  if (_p23.ctor === "::") {
                        return A2($List._op["::"],A2(f,_p22._0,_p23._0),_p23);
                     } else {
                        return _U.list([]);
                     }
               }
         }
   });
   var scanr = F3(function (f,acc,xs$) {
      var _p24 = xs$;
      if (_p24.ctor === "[]") {
            return _U.list([acc]);
         } else {
            var _p25 = A3(scanr,f,acc,_p24._1);
            if (_p25.ctor === "::") {
                  return A2($List._op["::"],A2(f,_p24._0,_p25._0),_p25);
               } else {
                  return _U.list([]);
               }
         }
   });
   var scanl1 = F2(function (f,xs$) {
      var _p26 = xs$;
      if (_p26.ctor === "[]") {
            return _U.list([]);
         } else {
            return A3($List.scanl,f,_p26._0,_p26._1);
         }
   });
   var foldr1 = F2(function (f,xs) {
      var mf = F2(function (x,m) {
         return $Maybe.Just(function () {    var _p27 = m;if (_p27.ctor === "Nothing") {    return x;} else {    return A2(f,x,_p27._0);}}());
      });
      return A3($List.foldr,mf,$Maybe.Nothing,xs);
   });
   var foldl1 = F2(function (f,xs) {
      var mf = F2(function (x,m) {
         return $Maybe.Just(function () {    var _p28 = m;if (_p28.ctor === "Nothing") {    return x;} else {    return A2(f,_p28._0,x);}}());
      });
      return A3($List.foldl,mf,$Maybe.Nothing,xs);
   });
   var uniqueHelp = F2(function (existing,remaining) {
      uniqueHelp: while (true) {
         var _p29 = remaining;
         if (_p29.ctor === "[]") {
               return _U.list([]);
            } else {
               var _p31 = _p29._1;
               var _p30 = _p29._0;
               if (A2($Set.member,_p30,existing)) {
                     var _v18 = existing,_v19 = _p31;
                     existing = _v18;
                     remaining = _v19;
                     continue uniqueHelp;
                  } else return A2($List._op["::"],_p30,A2(uniqueHelp,A2($Set.insert,_p30,existing),_p31));
            }
      }
   });
   var unique = function (list) {    return A2(uniqueHelp,$Set.empty,list);};
   var interweaveHelp = F3(function (l1,l2,acc) {
      interweaveHelp: while (true) {
         var _p32 = {ctor: "_Tuple2",_0: l1,_1: l2};
         _v20_1: do {
            if (_p32._0.ctor === "::") {
                  if (_p32._1.ctor === "::") {
                        var _v21 = _p32._0._1,_v22 = _p32._1._1,_v23 = A2($Basics._op["++"],acc,_U.list([_p32._0._0,_p32._1._0]));
                        l1 = _v21;
                        l2 = _v22;
                        acc = _v23;
                        continue interweaveHelp;
                     } else {
                        break _v20_1;
                     }
               } else {
                  if (_p32._1.ctor === "[]") {
                        break _v20_1;
                     } else {
                        return A2($Basics._op["++"],acc,_p32._1);
                     }
               }
         } while (false);
         return A2($Basics._op["++"],acc,_p32._0);
      }
   });
   var interweave = F2(function (l1,l2) {    return A3(interweaveHelp,l1,l2,_U.list([]));});
   var permutations = function (xs$) {
      var _p33 = xs$;
      if (_p33.ctor === "[]") {
            return _U.list([_U.list([])]);
         } else {
            var f = function (_p34) {
               var _p35 = _p34;
               return A2($List.map,F2(function (x,y) {    return A2($List._op["::"],x,y);})(_p35._0),permutations(_p35._1));
            };
            return A2($List.concatMap,f,select(_p33));
         }
   };
   var isPermutationOf = F2(function (permut,xs) {    return A2($List.member,permut,permutations(xs));});
   var subsequencesNonEmpty = function (xs) {
      var _p36 = xs;
      if (_p36.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p37 = _p36._0;
            var f = F2(function (ys,r) {    return A2($List._op["::"],ys,A2($List._op["::"],A2($List._op["::"],_p37,ys),r));});
            return A2($List._op["::"],_U.list([_p37]),A3($List.foldr,f,_U.list([]),subsequencesNonEmpty(_p36._1)));
         }
   };
   var subsequences = function (xs) {    return A2($List._op["::"],_U.list([]),subsequencesNonEmpty(xs));};
   var isSubsequenceOf = F2(function (subseq,xs) {    return A2($List.member,subseq,subsequences(xs));});
   var transpose = function (ll) {
      transpose: while (true) {
         var _p38 = ll;
         if (_p38.ctor === "[]") {
               return _U.list([]);
            } else {
               if (_p38._0.ctor === "[]") {
                     var _v28 = _p38._1;
                     ll = _v28;
                     continue transpose;
                  } else {
                     var _p39 = _p38._1;
                     var tails = A2($List.filterMap,$List.tail,_p39);
                     var heads = A2($List.filterMap,$List.head,_p39);
                     return A2($List._op["::"],A2($List._op["::"],_p38._0._0,heads),transpose(A2($List._op["::"],_p38._0._1,tails)));
                  }
            }
      }
   };
   var intercalate = function (xs) {    return function (_p40) {    return $List.concat(A2($List.intersperse,xs,_p40));};};
   var removeWhen = F2(function (pred,list) {    return A2($List.filter,function (_p41) {    return $Basics.not(pred(_p41));},list);});
   var singleton = function (x) {    return _U.list([x]);};
   var replaceIf = F3(function (predicate,replacement,list) {
      return A2($List.map,function (item) {    return predicate(item) ? replacement : item;},list);
   });
   var findIndices = function (p) {
      return function (_p42) {
         return A2($List.map,
         $Basics.fst,
         A2($List.filter,
         function (_p43) {
            var _p44 = _p43;
            return p(_p44._1);
         },
         A2($List.indexedMap,F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),_p42)));
      };
   };
   var findIndex = function (p) {    return function (_p45) {    return $List.head(A2(findIndices,p,_p45));};};
   var elemIndices = function (x) {    return findIndices(F2(function (x,y) {    return _U.eq(x,y);})(x));};
   var elemIndex = function (x) {    return findIndex(F2(function (x,y) {    return _U.eq(x,y);})(x));};
   var find = F2(function (predicate,list) {
      find: while (true) {
         var _p46 = list;
         if (_p46.ctor === "[]") {
               return $Maybe.Nothing;
            } else {
               var _p47 = _p46._0;
               if (predicate(_p47)) return $Maybe.Just(_p47); else {
                     var _v31 = predicate,_v32 = _p46._1;
                     predicate = _v31;
                     list = _v32;
                     continue find;
                  }
            }
      }
   });
   var notMember = function (x) {    return function (_p48) {    return $Basics.not(A2($List.member,x,_p48));};};
   var andThen = $Basics.flip($List.concatMap);
   var lift2 = F3(function (f,la,lb) {    return A2(andThen,la,function (a) {    return A2(andThen,lb,function (b) {    return _U.list([A2(f,a,b)]);});});});
   var lift3 = F4(function (f,la,lb,lc) {
      return A2(andThen,
      la,
      function (a) {
         return A2(andThen,lb,function (b) {    return A2(andThen,lc,function (c) {    return _U.list([A3(f,a,b,c)]);});});
      });
   });
   var lift4 = F5(function (f,la,lb,lc,ld) {
      return A2(andThen,
      la,
      function (a) {
         return A2(andThen,
         lb,
         function (b) {
            return A2(andThen,lc,function (c) {    return A2(andThen,ld,function (d) {    return _U.list([A4(f,a,b,c,d)]);});});
         });
      });
   });
   var andMap = F2(function (fl,l) {    return A3($List.map2,F2(function (x,y) {    return x(y);}),fl,l);});
   var dropDuplicates = function (list) {
      var step = F2(function (next,_p49) {
         var _p50 = _p49;
         var _p52 = _p50._0;
         var _p51 = _p50._1;
         return A2($Set.member,next,_p52) ? {ctor: "_Tuple2",_0: _p52,_1: _p51} : {ctor: "_Tuple2"
                                                                                  ,_0: A2($Set.insert,next,_p52)
                                                                                  ,_1: A2($List._op["::"],next,_p51)};
      });
      return $List.reverse($Basics.snd(A3($List.foldl,step,{ctor: "_Tuple2",_0: $Set.empty,_1: _U.list([])},list)));
   };
   var dropWhile = F2(function (predicate,list) {
      dropWhile: while (true) {
         var _p53 = list;
         if (_p53.ctor === "[]") {
               return _U.list([]);
            } else {
               if (predicate(_p53._0)) {
                     var _v35 = predicate,_v36 = _p53._1;
                     predicate = _v35;
                     list = _v36;
                     continue dropWhile;
                  } else return list;
            }
      }
   });
   var takeWhile = F2(function (predicate,list) {
      var _p54 = list;
      if (_p54.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p55 = _p54._0;
            return predicate(_p55) ? A2($List._op["::"],_p55,A2(takeWhile,predicate,_p54._1)) : _U.list([]);
         }
   });
   var span = F2(function (p,xs) {    return {ctor: "_Tuple2",_0: A2(takeWhile,p,xs),_1: A2(dropWhile,p,xs)};});
   var $break = function (p) {    return span(function (_p56) {    return $Basics.not(p(_p56));});};
   var groupBy = F2(function (eq,xs$) {
      var _p57 = xs$;
      if (_p57.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p59 = _p57._0;
            var _p58 = A2(span,eq(_p59),_p57._1);
            var ys = _p58._0;
            var zs = _p58._1;
            return A2($List._op["::"],A2($List._op["::"],_p59,ys),A2(groupBy,eq,zs));
         }
   });
   var group = groupBy(F2(function (x,y) {    return _U.eq(x,y);}));
   var minimumBy = F2(function (f,ls) {
      var minBy = F2(function (x,_p60) {
         var _p61 = _p60;
         var _p62 = _p61._1;
         var fx = f(x);
         return _U.cmp(fx,_p62) < 0 ? {ctor: "_Tuple2",_0: x,_1: fx} : {ctor: "_Tuple2",_0: _p61._0,_1: _p62};
      });
      var _p63 = ls;
      if (_p63.ctor === "::") {
            if (_p63._1.ctor === "[]") {
                  return $Maybe.Just(_p63._0);
               } else {
                  var _p64 = _p63._0;
                  return $Maybe.Just($Basics.fst(A3($List.foldl,minBy,{ctor: "_Tuple2",_0: _p64,_1: f(_p64)},_p63._1)));
               }
         } else {
            return $Maybe.Nothing;
         }
   });
   var maximumBy = F2(function (f,ls) {
      var maxBy = F2(function (x,_p65) {
         var _p66 = _p65;
         var _p67 = _p66._1;
         var fx = f(x);
         return _U.cmp(fx,_p67) > 0 ? {ctor: "_Tuple2",_0: x,_1: fx} : {ctor: "_Tuple2",_0: _p66._0,_1: _p67};
      });
      var _p68 = ls;
      if (_p68.ctor === "::") {
            if (_p68._1.ctor === "[]") {
                  return $Maybe.Just(_p68._0);
               } else {
                  var _p69 = _p68._0;
                  return $Maybe.Just($Basics.fst(A3($List.foldl,maxBy,{ctor: "_Tuple2",_0: _p69,_1: f(_p69)},_p68._1)));
               }
         } else {
            return $Maybe.Nothing;
         }
   });
   var uncons = function (xs) {
      var _p70 = xs;
      if (_p70.ctor === "[]") {
            return $Maybe.Nothing;
         } else {
            return $Maybe.Just({ctor: "_Tuple2",_0: _p70._0,_1: _p70._1});
         }
   };
   var iterate = F2(function (f,x) {
      var _p71 = f(x);
      if (_p71.ctor === "Just") {
            return A2($List._op["::"],x,A2(iterate,f,_p71._0));
         } else {
            return _U.list([x]);
         }
   });
   var getAt = F2(function (xs,idx) {    return $List.head(A2($List.drop,idx,xs));});
   _op["!!"] = getAt;
   var init = function () {
      var maybe = F2(function (d,f) {    return function (_p72) {    return A2($Maybe.withDefault,d,A2($Maybe.map,f,_p72));};});
      return A2($List.foldr,
      function (_p73) {
         return A2(F2(function (x,y) {    return function (_p74) {    return x(y(_p74));};}),
         $Maybe.Just,
         A2(maybe,_U.list([]),F2(function (x,y) {    return A2($List._op["::"],x,y);})(_p73)));
      },
      $Maybe.Nothing);
   }();
   var last = foldl1($Basics.flip($Basics.always));
   return _elm.List.Extra.values = {_op: _op
                                   ,last: last
                                   ,init: init
                                   ,getAt: getAt
                                   ,uncons: uncons
                                   ,minimumBy: minimumBy
                                   ,maximumBy: maximumBy
                                   ,andMap: andMap
                                   ,andThen: andThen
                                   ,takeWhile: takeWhile
                                   ,dropWhile: dropWhile
                                   ,dropDuplicates: dropDuplicates
                                   ,replaceIf: replaceIf
                                   ,singleton: singleton
                                   ,removeWhen: removeWhen
                                   ,iterate: iterate
                                   ,intercalate: intercalate
                                   ,transpose: transpose
                                   ,subsequences: subsequences
                                   ,permutations: permutations
                                   ,interweave: interweave
                                   ,unique: unique
                                   ,foldl1: foldl1
                                   ,foldr1: foldr1
                                   ,scanl1: scanl1
                                   ,scanr: scanr
                                   ,scanr1: scanr1
                                   ,unfoldr: unfoldr
                                   ,splitAt: splitAt
                                   ,takeWhileEnd: takeWhileEnd
                                   ,dropWhileEnd: dropWhileEnd
                                   ,span: span
                                   ,$break: $break
                                   ,stripPrefix: stripPrefix
                                   ,group: group
                                   ,groupBy: groupBy
                                   ,groupByTransitive: groupByTransitive
                                   ,inits: inits
                                   ,tails: tails
                                   ,select: select
                                   ,selectSplit: selectSplit
                                   ,isPrefixOf: isPrefixOf
                                   ,isSuffixOf: isSuffixOf
                                   ,isInfixOf: isInfixOf
                                   ,isSubsequenceOf: isSubsequenceOf
                                   ,isPermutationOf: isPermutationOf
                                   ,notMember: notMember
                                   ,find: find
                                   ,elemIndex: elemIndex
                                   ,elemIndices: elemIndices
                                   ,findIndex: findIndex
                                   ,findIndices: findIndices
                                   ,zip: zip
                                   ,zip3: zip3
                                   ,zip4: zip4
                                   ,zip5: zip5
                                   ,lift2: lift2
                                   ,lift3: lift3
                                   ,lift4: lift4};
};
Elm.Native.Json = {};

Elm.Native.Json.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Json = localRuntime.Native.Json || {};
	if (localRuntime.Native.Json.values) {
		return localRuntime.Native.Json.values;
	}

	var ElmArray = Elm.Native.Array.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function crash(expected, actual) {
		throw new Error(
			'expecting ' + expected + ' but got ' + JSON.stringify(actual)
		);
	}


	// PRIMITIVE VALUES

	function decodeNull(successValue) {
		return function(value) {
			if (value === null) {
				return successValue;
			}
			crash('null', value);
		};
	}


	function decodeString(value) {
		if (typeof value === 'string' || value instanceof String) {
			return value;
		}
		crash('a String', value);
	}


	function decodeFloat(value) {
		if (typeof value === 'number') {
			return value;
		}
		crash('a Float', value);
	}


	function decodeInt(value) {
		if (typeof value !== 'number') {
			crash('an Int', value);
		}

		if (value < 2147483647 && value > -2147483647 && (value | 0) === value) {
			return value;
		}

		if (isFinite(value) && !(value % 1)) {
			return value;
		}

		crash('an Int', value);
	}


	function decodeBool(value) {
		if (typeof value === 'boolean') {
			return value;
		}
		crash('a Bool', value);
	}


	// ARRAY

	function decodeArray(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i--; ) {
					array[i] = decoder(value[i]);
				}
				return ElmArray.fromJSArray(array);
			}
			crash('an Array', value);
		};
	}


	// LIST

	function decodeList(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var list = List.Nil;
				for (var i = len; i--; ) {
					list = List.Cons( decoder(value[i]), list );
				}
				return list;
			}
			crash('a List', value);
		};
	}


	// MAYBE

	function decodeMaybe(decoder) {
		return function(value) {
			try {
				return Maybe.Just(decoder(value));
			} catch(e) {
				return Maybe.Nothing;
			}
		};
	}


	// FIELDS

	function decodeField(field, decoder) {
		return function(value) {
			var subValue = value[field];
			if (subValue !== undefined) {
				return decoder(subValue);
			}
			crash("an object with field '" + field + "'", value);
		};
	}


	// OBJECTS

	function decodeKeyValuePairs(decoder) {
		return function(value) {
			var isObject =
				typeof value === 'object'
					&& value !== null
					&& !(value instanceof Array);

			if (isObject) {
				var keyValuePairs = List.Nil;
				for (var key in value)
				{
					var elmValue = decoder(value[key]);
					var pair = Utils.Tuple2(key, elmValue);
					keyValuePairs = List.Cons(pair, keyValuePairs);
				}
				return keyValuePairs;
			}

			crash('an object', value);
		};
	}

	function decodeObject1(f, d1) {
		return function(value) {
			return f(d1(value));
		};
	}

	function decodeObject2(f, d1, d2) {
		return function(value) {
			return A2( f, d1(value), d2(value) );
		};
	}

	function decodeObject3(f, d1, d2, d3) {
		return function(value) {
			return A3( f, d1(value), d2(value), d3(value) );
		};
	}

	function decodeObject4(f, d1, d2, d3, d4) {
		return function(value) {
			return A4( f, d1(value), d2(value), d3(value), d4(value) );
		};
	}

	function decodeObject5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
		};
	}

	function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			return A6( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value)
			);
		};
	}

	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			return A7( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value)
			);
		};
	}

	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			return A8( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value),
				d8(value)
			);
		};
	}


	// TUPLES

	function decodeTuple1(f, d1) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 1 ) {
				crash('a Tuple of length 1', value);
			}
			return f( d1(value[0]) );
		};
	}

	function decodeTuple2(f, d1, d2) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 2 ) {
				crash('a Tuple of length 2', value);
			}
			return A2( f, d1(value[0]), d2(value[1]) );
		};
	}

	function decodeTuple3(f, d1, d2, d3) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 3 ) {
				crash('a Tuple of length 3', value);
			}
			return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
		};
	}


	function decodeTuple4(f, d1, d2, d3, d4) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 4 ) {
				crash('a Tuple of length 4', value);
			}
			return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
		};
	}


	function decodeTuple5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 5 ) {
				crash('a Tuple of length 5', value);
			}
			return A5( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4])
			);
		};
	}


	function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 6 ) {
				crash('a Tuple of length 6', value);
			}
			return A6( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5])
			);
		};
	}

	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 7 ) {
				crash('a Tuple of length 7', value);
			}
			return A7( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6])
			);
		};
	}


	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 8 ) {
				crash('a Tuple of length 8', value);
			}
			return A8( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6]),
				d8(value[7])
			);
		};
	}


	// CUSTOM DECODERS

	function decodeValue(value) {
		return value;
	}

	function runDecoderValue(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function customDecoder(decoder, callback) {
		return function(value) {
			var result = callback(decoder(value));
			if (result.ctor === 'Err') {
				throw new Error('custom decoder failed: ' + result._0);
			}
			return result._0;
		};
	}

	function andThen(decode, callback) {
		return function(value) {
			var result = decode(value);
			return callback(result)(value);
		};
	}

	function fail(msg) {
		return function(value) {
			throw new Error(msg);
		};
	}

	function succeed(successValue) {
		return function(value) {
			return successValue;
		};
	}


	// ONE OF MANY

	function oneOf(decoders) {
		return function(value) {
			var errors = [];
			var temp = decoders;
			while (temp.ctor !== '[]') {
				try {
					return temp._0(value);
				} catch(e) {
					errors.push(e.message);
				}
				temp = temp._1;
			}
			throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
		};
	}

	function get(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}


	// ENCODE / DECODE

	function runDecoderString(decoder, string) {
		try {
			return Result.Ok(decoder(JSON.parse(string)));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function encode(indentLevel, value) {
		return JSON.stringify(value, null, indentLevel);
	}

	function identity(value) {
		return value;
	}

	function encodeObject(keyValuePairs) {
		var obj = {};
		while (keyValuePairs.ctor !== '[]') {
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}

	return localRuntime.Native.Json.values = {
		encode: F2(encode),
		runDecoderString: F2(runDecoderString),
		runDecoderValue: F2(runDecoderValue),

		get: F2(get),
		oneOf: oneOf,

		decodeNull: decodeNull,
		decodeInt: decodeInt,
		decodeFloat: decodeFloat,
		decodeString: decodeString,
		decodeBool: decodeBool,

		decodeMaybe: decodeMaybe,

		decodeList: decodeList,
		decodeArray: decodeArray,

		decodeField: F2(decodeField),

		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,

		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),

		andThen: F2(andThen),
		decodeValue: decodeValue,
		customDecoder: F2(customDecoder),
		fail: fail,
		succeed: succeed,

		identity: identity,
		encodeNull: null,
		encodeArray: ElmArray.toJSArray,
		encodeList: List.toArray,
		encodeObject: encodeObject

	};
};

Elm.Native.Array = {};
Elm.Native.Array.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Array = localRuntime.Native.Array || {};
	if (localRuntime.Native.Array.values)
	{
		return localRuntime.Native.Array.values;
	}
	if ('values' in Elm.Native.Array)
	{
		return localRuntime.Native.Array.values = Elm.Native.Array.values;
	}

	var List = Elm.Native.List.make(localRuntime);

	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes

	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;

	// An empty array.
	var empty = {
		ctor: '_Array',
		height: 0,
		table: []
	};


	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				'Index ' + i + ' is out of range. Check the length of ' +
				'your array first or use getMaybe or getWithDefault.');
		}
		return unsafeGet(i, array);
	}


	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}


	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}


	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);

		if (array.height === 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}


	function initialize(len, f)
	{
		if (len <= 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}

	function initialize_(f, h, from, to)
	{
		if (h === 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: '_Array',
				height: 0,
				table: table
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}

	function fromList(list)
	{
		if (list === List.Nil)
		{
			return empty;
		}

		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = [];
		var i = 0;

		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;

			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i === M)
			{
				var leaf = {
					ctor: '_Array',
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}

		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table.splice(0, i)
			};
			fromListPush(leaf, nodes);
		}

		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}

		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length === 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}

	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;

		// Maybe the node on this height does not exist.
		if (nodes.length === h)
		{
			var node = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
			nodes.push(node);
		}

		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);

		if (nodes[h].table.length === M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
		}
	}

	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}

		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}

	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height === 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: '_Array',
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}

		// Recursively push
		var pushed = push_(item, botRight(a));

		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed !== null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}

		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}

	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(List.Nil, a);
	}

	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height === 0
					? List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}

	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}

	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}

	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
		}
		return newA;
	}

	function foldl(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}

	function foldr(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}

	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}

	function sliceRight(to, a)
	{
		if (to === length(a))
		{
			return a;
		}

		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}

		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right === 0)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}

	function sliceLeft(from, a)
	{
		if (from === 0)
		{
			return a;
		}

		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}

		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left === a.table.length - 1)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}

		return newA;
	}

	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}

		var c = append_(a, b);

		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}

			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}

			return c[0];
		}

		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}

		return siblise(c[0], c[1]);
	}

	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}

		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));

				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);

				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));

				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}

		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a, b];
		}

		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a, b];
		}
		return shuffle(a, b, toRemove);
	}

	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node);
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}

	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);

			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}

	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}

		var toRemove = a.table.length + b.table.length;
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}

	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}

	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}

	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);

		var l = (index === 0 || index === a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);

		set2(a.lengths, b.lengths, index, l + length(slot));
	}

	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: '_Array',
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}

	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M === 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}

		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;

		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length);

			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}

			from += to;

			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}

			// Only create a new slot if the current one is filled up.
			if (slot.table.length === M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1, 0);
				write++;
			}
		}

		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}

		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}

		return [newA, newB];
	}

	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}

	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}

	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height === 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}

	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}

	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: '_Array',
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}

	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h === tree.height)
		{
			return tree;
		}

		return {
			ctor: '_Array',
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}

	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: '_Array',
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}

	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}

	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height === 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t === 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}

	function fromJSArray(jsArray)
	{
		if (jsArray.length === 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}

	function fromJSArray_(jsArray, h, from, to)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: jsArray.slice(from, to)
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}

	Elm.Native.Array.values = {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,

		toJSArray: toJSArray,
		fromJSArray: fromJSArray
	};

	return localRuntime.Native.Array.values = Elm.Native.Array.values;
};

Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values) return _elm.Array.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var _op = {};
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var isEmpty = function (array) {    return _U.eq(length(array),0);};
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,array) {
      return _U.cmp(0,i) < 1 && _U.cmp(i,$Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,i,array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,arr) {
      var update = F2(function (x,xs) {    return isOkay(x) ? A2($Native$Array.push,x,xs) : xs;});
      return A3($Native$Array.foldl,update,$Native$Array.empty,arr);
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),
      _U.range(0,$Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {    return A2(initialize,n,$Basics.always(e));});
   var Array = {ctor: "Array"};
   return _elm.Array.values = {_op: _op
                              ,empty: empty
                              ,repeat: repeat
                              ,initialize: initialize
                              ,fromList: fromList
                              ,isEmpty: isEmpty
                              ,length: length
                              ,push: push
                              ,append: append
                              ,get: get
                              ,set: set
                              ,slice: slice
                              ,toList: toList
                              ,toIndexedList: toIndexedList
                              ,map: map
                              ,indexedMap: indexedMap
                              ,filter: filter
                              ,foldl: foldl
                              ,foldr: foldr};
};
Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values) return _elm.Json.Encode.values;
   var _U = Elm.Native.Utils.make(_elm),$Array = Elm.Array.make(_elm),$Native$Json = Elm.Native.Json.make(_elm);
   var _op = {};
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   return _elm.Json.Encode.values = {_op: _op
                                    ,encode: encode
                                    ,string: string
                                    ,$int: $int
                                    ,$float: $float
                                    ,bool: bool
                                    ,$null: $null
                                    ,list: list
                                    ,array: array
                                    ,object: object};
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values) return _elm.Json.Decode.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,decoder) {    return A3($List.foldr,F2(function (x,y) {    return A2(_op[":="],x,y);}),decoder,fields);});
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {    return A2(map,$Dict.fromList,keyValuePairs(decoder));};
   var Decoder = {ctor: "Decoder"};
   return _elm.Json.Decode.values = {_op: _op
                                    ,decodeString: decodeString
                                    ,decodeValue: decodeValue
                                    ,string: string
                                    ,$int: $int
                                    ,$float: $float
                                    ,bool: bool
                                    ,$null: $null
                                    ,list: list
                                    ,array: array
                                    ,tuple1: tuple1
                                    ,tuple2: tuple2
                                    ,tuple3: tuple3
                                    ,tuple4: tuple4
                                    ,tuple5: tuple5
                                    ,tuple6: tuple6
                                    ,tuple7: tuple7
                                    ,tuple8: tuple8
                                    ,at: at
                                    ,object1: object1
                                    ,object2: object2
                                    ,object3: object3
                                    ,object4: object4
                                    ,object5: object5
                                    ,object6: object6
                                    ,object7: object7
                                    ,object8: object8
                                    ,keyValuePairs: keyValuePairs
                                    ,dict: dict
                                    ,maybe: maybe
                                    ,oneOf: oneOf
                                    ,map: map
                                    ,fail: fail
                                    ,succeed: succeed
                                    ,andThen: andThen
                                    ,value: value
                                    ,customDecoder: customDecoder};
};
Elm.Trampoline = Elm.Trampoline || {};
Elm.Trampoline.make = function (_elm) {
   "use strict";
   _elm.Trampoline = _elm.Trampoline || {};
   if (_elm.Trampoline.values) return _elm.Trampoline.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var trampoline = function (tramp) {
      trampoline: while (true) {
         var _p0 = tramp;
         if (_p0.ctor === "Done") {
               return _p0._0;
            } else {
               var _v1 = _p0._0({ctor: "_Tuple0"});
               tramp = _v1;
               continue trampoline;
            }
      }
   };
   var Continue = function (a) {    return {ctor: "Continue",_0: a};};
   var Done = function (a) {    return {ctor: "Done",_0: a};};
   return _elm.Trampoline.values = {_op: _op,trampoline: trampoline,Done: Done,Continue: Continue};
};
Elm.Native.Bitwise = {};
Elm.Native.Bitwise.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Bitwise = localRuntime.Native.Bitwise || {};
	if (localRuntime.Native.Bitwise.values)
	{
		return localRuntime.Native.Bitwise.values;
	}

	function and(a, b) { return a & b; }
	function or(a, b) { return a | b; }
	function xor(a, b) { return a ^ b; }
	function not(a) { return ~a; }
	function sll(a, offset) { return a << offset; }
	function sra(a, offset) { return a >> offset; }
	function srl(a, offset) { return a >>> offset; }

	return localRuntime.Native.Bitwise.values = {
		and: F2(and),
		or: F2(or),
		xor: F2(xor),
		complement: not,
		shiftLeft: F2(sll),
		shiftRightArithmatic: F2(sra),
		shiftRightLogical: F2(srl)
	};
};

Elm.Bitwise = Elm.Bitwise || {};
Elm.Bitwise.make = function (_elm) {
   "use strict";
   _elm.Bitwise = _elm.Bitwise || {};
   if (_elm.Bitwise.values) return _elm.Bitwise.values;
   var _U = Elm.Native.Utils.make(_elm),$Native$Bitwise = Elm.Native.Bitwise.make(_elm);
   var _op = {};
   var shiftRightLogical = $Native$Bitwise.shiftRightLogical;
   var shiftRight = $Native$Bitwise.shiftRightArithmatic;
   var shiftLeft = $Native$Bitwise.shiftLeft;
   var complement = $Native$Bitwise.complement;
   var xor = $Native$Bitwise.xor;
   var or = $Native$Bitwise.or;
   var and = $Native$Bitwise.and;
   return _elm.Bitwise.values = {_op: _op
                                ,and: and
                                ,or: or
                                ,xor: xor
                                ,complement: complement
                                ,shiftLeft: shiftLeft
                                ,shiftRight: shiftRight
                                ,shiftRightLogical: shiftRightLogical};
};
Elm.Native.Time = {};

Elm.Native.Time.make = function(localRuntime)
{
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input, state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value, initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2(NS.map, fst, NS.timestamp(ticker));
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new Date(t); },
		read: read
	};
};

Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values) return _elm.Time.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var delay = $Native$Signal.delay;
   var since = F2(function (time,signal) {
      var stop = A2($Signal.map,$Basics.always(-1),A2(delay,time,signal));
      var start = A2($Signal.map,$Basics.always(1),signal);
      var delaydiff = A3($Signal.foldp,F2(function (x,y) {    return x + y;}),0,A2($Signal.merge,start,stop));
      return A2($Signal.map,F2(function (x,y) {    return !_U.eq(x,y);})(0),delaydiff);
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {    return A2(fpsWhen,targetFrames,$Signal.constant(true));};
   var inMilliseconds = function (t) {    return t;};
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {    return t / hour;};
   var inMinutes = function (t) {    return t / minute;};
   var inSeconds = function (t) {    return t / second;};
   return _elm.Time.values = {_op: _op
                             ,millisecond: millisecond
                             ,second: second
                             ,minute: minute
                             ,hour: hour
                             ,inMilliseconds: inMilliseconds
                             ,inSeconds: inSeconds
                             ,inMinutes: inMinutes
                             ,inHours: inHours
                             ,fps: fps
                             ,fpsWhen: fpsWhen
                             ,every: every
                             ,timestamp: timestamp
                             ,delay: delay
                             ,since: since};
};
Elm.Random = Elm.Random || {};
Elm.Random.make = function (_elm) {
   "use strict";
   _elm.Random = _elm.Random || {};
   if (_elm.Random.values) return _elm.Random.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm),$List = Elm.List.make(_elm);
   var _op = {};
   var magicNum8 = 2147483562;
   var range = function (_p0) {    return {ctor: "_Tuple2",_0: 0,_1: magicNum8};};
   var magicNum7 = 2137383399;
   var magicNum6 = 2147483563;
   var magicNum5 = 3791;
   var magicNum4 = 40692;
   var magicNum3 = 52774;
   var magicNum2 = 12211;
   var magicNum1 = 53668;
   var magicNum0 = 40014;
   var generate = F2(function (_p1,seed) {    var _p2 = _p1;return _p2._0(seed);});
   var Seed = function (a) {    return {ctor: "Seed",_0: a};};
   var State = F2(function (a,b) {    return {ctor: "State",_0: a,_1: b};});
   var initState = function (s$) {
      var s = A2($Basics.max,s$,0 - s$);
      var q = s / (magicNum6 - 1) | 0;
      var s2 = A2($Basics._op["%"],q,magicNum7 - 1);
      var s1 = A2($Basics._op["%"],s,magicNum6 - 1);
      return A2(State,s1 + 1,s2 + 1);
   };
   var next = function (_p3) {
      var _p4 = _p3;
      var _p6 = _p4._1;
      var _p5 = _p4._0;
      var k$ = _p6 / magicNum3 | 0;
      var s2$ = magicNum4 * (_p6 - k$ * magicNum3) - k$ * magicNum5;
      var s2$$ = _U.cmp(s2$,0) < 0 ? s2$ + magicNum7 : s2$;
      var k = _p5 / magicNum1 | 0;
      var s1$ = magicNum0 * (_p5 - k * magicNum1) - k * magicNum2;
      var s1$$ = _U.cmp(s1$,0) < 0 ? s1$ + magicNum6 : s1$;
      var z = s1$$ - s2$$;
      var z$ = _U.cmp(z,1) < 0 ? z + magicNum8 : z;
      return {ctor: "_Tuple2",_0: z$,_1: A2(State,s1$$,s2$$)};
   };
   var split = function (_p7) {
      var _p8 = _p7;
      var _p11 = _p8._1;
      var _p10 = _p8._0;
      var _p9 = $Basics.snd(next(_p8));
      var t1 = _p9._0;
      var t2 = _p9._1;
      var new_s2 = _U.eq(_p11,1) ? magicNum7 - 1 : _p11 - 1;
      var new_s1 = _U.eq(_p10,magicNum6 - 1) ? 1 : _p10 + 1;
      return {ctor: "_Tuple2",_0: A2(State,new_s1,t2),_1: A2(State,t1,new_s2)};
   };
   var initialSeed = function (n) {    return Seed({state: initState(n),next: next,split: split,range: range});};
   var Generator = function (a) {    return {ctor: "Generator",_0: a};};
   var andThen = F2(function (_p12,callback) {
      var _p13 = _p12;
      return Generator(function (seed) {
         var _p14 = _p13._0(seed);
         var result = _p14._0;
         var newSeed = _p14._1;
         var _p15 = callback(result);
         var genB = _p15._0;
         return genB(newSeed);
      });
   });
   var map5 = F6(function (func,_p20,_p19,_p18,_p17,_p16) {
      var _p21 = _p20;
      var _p22 = _p19;
      var _p23 = _p18;
      var _p24 = _p17;
      var _p25 = _p16;
      return Generator(function (seed0) {
         var _p26 = _p21._0(seed0);
         var a = _p26._0;
         var seed1 = _p26._1;
         var _p27 = _p22._0(seed1);
         var b = _p27._0;
         var seed2 = _p27._1;
         var _p28 = _p23._0(seed2);
         var c = _p28._0;
         var seed3 = _p28._1;
         var _p29 = _p24._0(seed3);
         var d = _p29._0;
         var seed4 = _p29._1;
         var _p30 = _p25._0(seed4);
         var e = _p30._0;
         var seed5 = _p30._1;
         return {ctor: "_Tuple2",_0: A5(func,a,b,c,d,e),_1: seed5};
      });
   });
   var map4 = F5(function (func,_p34,_p33,_p32,_p31) {
      var _p35 = _p34;
      var _p36 = _p33;
      var _p37 = _p32;
      var _p38 = _p31;
      return Generator(function (seed0) {
         var _p39 = _p35._0(seed0);
         var a = _p39._0;
         var seed1 = _p39._1;
         var _p40 = _p36._0(seed1);
         var b = _p40._0;
         var seed2 = _p40._1;
         var _p41 = _p37._0(seed2);
         var c = _p41._0;
         var seed3 = _p41._1;
         var _p42 = _p38._0(seed3);
         var d = _p42._0;
         var seed4 = _p42._1;
         return {ctor: "_Tuple2",_0: A4(func,a,b,c,d),_1: seed4};
      });
   });
   var map3 = F4(function (func,_p45,_p44,_p43) {
      var _p46 = _p45;
      var _p47 = _p44;
      var _p48 = _p43;
      return Generator(function (seed0) {
         var _p49 = _p46._0(seed0);
         var a = _p49._0;
         var seed1 = _p49._1;
         var _p50 = _p47._0(seed1);
         var b = _p50._0;
         var seed2 = _p50._1;
         var _p51 = _p48._0(seed2);
         var c = _p51._0;
         var seed3 = _p51._1;
         return {ctor: "_Tuple2",_0: A3(func,a,b,c),_1: seed3};
      });
   });
   var map2 = F3(function (func,_p53,_p52) {
      var _p54 = _p53;
      var _p55 = _p52;
      return Generator(function (seed0) {
         var _p56 = _p54._0(seed0);
         var a = _p56._0;
         var seed1 = _p56._1;
         var _p57 = _p55._0(seed1);
         var b = _p57._0;
         var seed2 = _p57._1;
         return {ctor: "_Tuple2",_0: A2(func,a,b),_1: seed2};
      });
   });
   var map = F2(function (func,_p58) {
      var _p59 = _p58;
      return Generator(function (seed0) {    var _p60 = _p59._0(seed0);var a = _p60._0;var seed1 = _p60._1;return {ctor: "_Tuple2",_0: func(a),_1: seed1};});
   });
   var listHelp = F4(function (list,n,generate,seed) {
      listHelp: while (true) if (_U.cmp(n,1) < 0) return {ctor: "_Tuple2",_0: $List.reverse(list),_1: seed}; else {
            var _p61 = generate(seed);
            var value = _p61._0;
            var newSeed = _p61._1;
            var _v19 = A2($List._op["::"],value,list),_v20 = n - 1,_v21 = generate,_v22 = newSeed;
            list = _v19;
            n = _v20;
            generate = _v21;
            seed = _v22;
            continue listHelp;
         }
   });
   var list = F2(function (n,_p62) {    var _p63 = _p62;return Generator(function (seed) {    return A4(listHelp,_U.list([]),n,_p63._0,seed);});});
   var pair = F2(function (genA,genB) {    return A3(map2,F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),genA,genB);});
   var minInt = -2147483648;
   var maxInt = 2147483647;
   var iLogBase = F2(function (b,i) {    return _U.cmp(i,b) < 0 ? 1 : 1 + A2(iLogBase,b,i / b | 0);});
   var $int = F2(function (a,b) {
      return Generator(function (_p64) {
         var _p65 = _p64;
         var _p70 = _p65._0;
         var base = 2147483561;
         var f = F3(function (n,acc,state) {
            f: while (true) {
               var _p66 = n;
               if (_p66 === 0) {
                     return {ctor: "_Tuple2",_0: acc,_1: state};
                  } else {
                     var _p67 = _p70.next(state);
                     var x = _p67._0;
                     var state$ = _p67._1;
                     var _v26 = n - 1,_v27 = x + acc * base,_v28 = state$;
                     n = _v26;
                     acc = _v27;
                     state = _v28;
                     continue f;
                  }
            }
         });
         var _p68 = _U.cmp(a,b) < 0 ? {ctor: "_Tuple2",_0: a,_1: b} : {ctor: "_Tuple2",_0: b,_1: a};
         var lo = _p68._0;
         var hi = _p68._1;
         var k = hi - lo + 1;
         var n = A2(iLogBase,base,k);
         var _p69 = A3(f,n,1,_p70.state);
         var v = _p69._0;
         var state$ = _p69._1;
         return {ctor: "_Tuple2",_0: lo + A2($Basics._op["%"],v,k),_1: Seed(_U.update(_p70,{state: state$}))};
      });
   });
   var $float = F2(function (a,b) {
      return Generator(function (seed) {
         var _p71 = A2(generate,A2($int,minInt,maxInt),seed);
         var number = _p71._0;
         var newSeed = _p71._1;
         var negativeOneToOne = $Basics.toFloat(number) / $Basics.toFloat(maxInt - minInt);
         var _p72 = _U.cmp(a,b) < 0 ? {ctor: "_Tuple2",_0: a,_1: b} : {ctor: "_Tuple2",_0: b,_1: a};
         var lo = _p72._0;
         var hi = _p72._1;
         var scaled = (lo + hi) / 2 + (hi - lo) * negativeOneToOne;
         return {ctor: "_Tuple2",_0: scaled,_1: newSeed};
      });
   });
   var bool = A2(map,F2(function (x,y) {    return _U.eq(x,y);})(1),A2($int,0,1));
   return _elm.Random.values = {_op: _op
                               ,bool: bool
                               ,$int: $int
                               ,$float: $float
                               ,list: list
                               ,pair: pair
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,andThen: andThen
                               ,minInt: minInt
                               ,maxInt: maxInt
                               ,generate: generate
                               ,initialSeed: initialSeed};
};
(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){

},{}],2:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":1}],3:[function(require,module,exports){
"use strict";

module.exports = function isObject(x) {
	return typeof x === "object" && x !== null;
};

},{}],4:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],5:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook.js")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, propName, propValue, previous);
        } else if (isHook(propValue)) {
            removeProperty(node, propName, propValue, previous)
            if (propValue.hook) {
                propValue.hook(node,
                    propName,
                    previous ? previous[propName] : undefined)
            }
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, propName, propValue, previous) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        } else if (previousValue.unhook) {
            previousValue.unhook(node, propName, propValue)
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"../vnode/is-vhook.js":13,"is-object":3}],6:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("../vnode/is-vnode.js")
var isVText = require("../vnode/is-vtext.js")
var isWidget = require("../vnode/is-widget.js")
var handleThunk = require("../vnode/handle-thunk.js")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"../vnode/handle-thunk.js":11,"../vnode/is-vnode.js":14,"../vnode/is-vtext.js":15,"../vnode/is-widget.js":16,"./apply-properties":5,"global/document":2}],7:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],8:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("../vnode/is-widget.js")
var VPatch = require("../vnode/vpatch.js")

var render = require("./create-element")
var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = render(vText, renderOptions)

        if (parentNode && newNode !== domNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    var updating = updateWidget(leftVNode, widget)
    var newNode

    if (updating) {
        newNode = widget.update(leftVNode, domNode) || domNode
    } else {
        newNode = render(widget, renderOptions)
    }

    var parentNode = domNode.parentNode

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    if (!updating) {
        destroyWidget(domNode, leftVNode)
    }

    return newNode
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = render(vNode, renderOptions)

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, moves) {
    var childNodes = domNode.childNodes
    var keyMap = {}
    var node
    var remove
    var insert

    for (var i = 0; i < moves.removes.length; i++) {
        remove = moves.removes[i]
        node = childNodes[remove.from]
        if (remove.key) {
            keyMap[remove.key] = node
        }
        domNode.removeChild(node)
    }

    var length = childNodes.length
    for (var j = 0; j < moves.inserts.length; j++) {
        insert = moves.inserts[j]
        node = keyMap[insert.key]
        // this is the weirdest bug i've ever seen in webkit
        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to])
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"../vnode/is-widget.js":16,"../vnode/vpatch.js":19,"./apply-properties":5,"./create-element":6,"./update-widget":10}],9:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches) {
    return patchRecursive(rootNode, patches)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions) {
        renderOptions = { patch: patchRecursive }
        if (ownerDocument !== document) {
            renderOptions.document = ownerDocument
        }
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./dom-index":7,"./patch-op":8,"global/document":2,"x-is-array":4}],10:[function(require,module,exports){
var isWidget = require("../vnode/is-widget.js")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"../vnode/is-widget.js":16}],11:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":12,"./is-vnode":14,"./is-vtext":15,"./is-widget":16}],12:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],13:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook &&
      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
}

},{}],14:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":17}],15:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":17}],16:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],17:[function(require,module,exports){
module.exports = "2"

},{}],18:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var hasThunks = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property) && property.unhook) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!hasThunks && child.hasThunks) {
                hasThunks = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        } else if (!hasThunks && isThunk(child)) {
            hasThunks = true;
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hasThunks = hasThunks
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-thunk":12,"./is-vhook":13,"./is-vnode":14,"./is-widget":16,"./version":17}],19:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":17}],20:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":17}],21:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook")

module.exports = diffProps

function diffProps(a, b) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (aValue === bValue) {
            continue
        } else if (isObject(aValue) && isObject(bValue)) {
            if (getPrototype(bValue) !== getPrototype(aValue)) {
                diff = diff || {}
                diff[aKey] = bValue
            } else if (isHook(bValue)) {
                 diff = diff || {}
                 diff[aKey] = bValue
            } else {
                var objectDiff = diffProps(aValue, bValue)
                if (objectDiff) {
                    diff = diff || {}
                    diff[aKey] = objectDiff
                }
            }
        } else {
            diff = diff || {}
            diff[aKey] = bValue
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
  if (Object.getPrototypeOf) {
    return Object.getPrototypeOf(value)
  } else if (value.__proto__) {
    return value.__proto__
  } else if (value.constructor) {
    return value.constructor.prototype
  }
}

},{"../vnode/is-vhook":13,"is-object":3}],22:[function(require,module,exports){
var isArray = require("x-is-array")

var VPatch = require("../vnode/vpatch")
var isVNode = require("../vnode/is-vnode")
var isVText = require("../vnode/is-vtext")
var isWidget = require("../vnode/is-widget")
var isThunk = require("../vnode/is-thunk")
var handleThunk = require("../vnode/handle-thunk")

var diffProps = require("./diff-props")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        return
    }

    var apply = patch[index]
    var applyClear = false

    if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (b == null) {

        // If a is a widget we will add a remove patch for it
        // Otherwise any child widgets/hooks must be destroyed.
        // This prevents adding two remove patches for a widget.
        if (!isWidget(a)) {
            clearState(a, patch, index)
            apply = patch[index]
        }

        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                applyClear = true
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            applyClear = true
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            applyClear = true
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        if (!isWidget(a)) {
            applyClear = true
        }

        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))
    }

    if (apply) {
        patch[index] = apply
    }

    if (applyClear) {
        clearState(a, patch, index)
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var orderedSet = reorder(aChildren, b.children)
    var bChildren = orderedSet.children

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (orderedSet.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(
            VPatch.ORDER,
            a,
            orderedSet.moves
        ))
    }

    return apply
}

function clearState(vNode, patch, index) {
    // TODO: Make this a single walk, not two
    unhook(vNode, patch, index)
    destroyWidgets(vNode, patch, index)
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(VPatch.REMOVE, vNode, null)
            )
        }
    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b)
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true
        }
    }

    return false
}

// Execute hooks when two nodes are identical
function unhook(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(
                    VPatch.PROPS,
                    vNode,
                    undefinedKeys(vNode.hooks)
                )
            )
        }

        if (vNode.descendantHooks || vNode.hasThunks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                unhook(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

function undefinedKeys(obj) {
    var result = {}

    for (var key in obj) {
        result[key] = undefined
    }

    return result
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {
    // O(M) time, O(M) memory
    var bChildIndex = keyIndex(bChildren)
    var bKeys = bChildIndex.keys
    var bFree = bChildIndex.free

    if (bFree.length === bChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(N) time, O(N) memory
    var aChildIndex = keyIndex(aChildren)
    var aKeys = aChildIndex.keys
    var aFree = aChildIndex.free

    if (aFree.length === aChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(MAX(N, M)) memory
    var newChildren = []

    var freeIndex = 0
    var freeCount = bFree.length
    var deletedItems = 0

    // Iterate through a and match a node in b
    // O(N) time,
    for (var i = 0 ; i < aChildren.length; i++) {
        var aItem = aChildren[i]
        var itemIndex

        if (aItem.key) {
            if (bKeys.hasOwnProperty(aItem.key)) {
                // Match up the old keys
                itemIndex = bKeys[aItem.key]
                newChildren.push(bChildren[itemIndex])

            } else {
                // Remove old keyed items
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        } else {
            // Match the item in a with the next free item in b
            if (freeIndex < freeCount) {
                itemIndex = bFree[freeIndex++]
                newChildren.push(bChildren[itemIndex])
            } else {
                // There are no free items in b to match with
                // the free items in a, so the extra free nodes
                // are deleted.
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        }
    }

    var lastFreeIndex = freeIndex >= bFree.length ?
        bChildren.length :
        bFree[freeIndex]

    // Iterate through b and append any new keys
    // O(M) time
    for (var j = 0; j < bChildren.length; j++) {
        var newItem = bChildren[j]

        if (newItem.key) {
            if (!aKeys.hasOwnProperty(newItem.key)) {
                // Add any new keyed items
                // We are adding new items to the end and then sorting them
                // in place. In future we should insert new items in place.
                newChildren.push(newItem)
            }
        } else if (j >= lastFreeIndex) {
            // Add any leftover non-keyed items
            newChildren.push(newItem)
        }
    }

    var simulate = newChildren.slice()
    var simulateIndex = 0
    var removes = []
    var inserts = []
    var simulateItem

    for (var k = 0; k < bChildren.length;) {
        var wantedItem = bChildren[k]
        simulateItem = simulate[simulateIndex]

        // remove items
        while (simulateItem === null && simulate.length) {
            removes.push(remove(simulate, simulateIndex, null))
            simulateItem = simulate[simulateIndex]
        }

        if (!simulateItem || simulateItem.key !== wantedItem.key) {
            // if we need a key in this position...
            if (wantedItem.key) {
                if (simulateItem && simulateItem.key) {
                    // if an insert doesn't put this key in place, it needs to move
                    if (bKeys[simulateItem.key] !== k + 1) {
                        removes.push(remove(simulate, simulateIndex, simulateItem.key))
                        simulateItem = simulate[simulateIndex]
                        // if the remove didn't put the wanted item in place, we need to insert it
                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
                            inserts.push({key: wantedItem.key, to: k})
                        }
                        // items are matching, so skip ahead
                        else {
                            simulateIndex++
                        }
                    }
                    else {
                        inserts.push({key: wantedItem.key, to: k})
                    }
                }
                else {
                    inserts.push({key: wantedItem.key, to: k})
                }
                k++
            }
            // a key in simulate has no matching wanted key, remove it
            else if (simulateItem && simulateItem.key) {
                removes.push(remove(simulate, simulateIndex, simulateItem.key))
            }
        }
        else {
            simulateIndex++
            k++
        }
    }

    // remove all the remaining nodes from simulate
    while(simulateIndex < simulate.length) {
        simulateItem = simulate[simulateIndex]
        removes.push(remove(simulate, simulateIndex, simulateItem && simulateItem.key))
    }

    // If the only moves we have are deletes then we can just
    // let the delete patch remove these items.
    if (removes.length === deletedItems && !inserts.length) {
        return {
            children: newChildren,
            moves: null
        }
    }

    return {
        children: newChildren,
        moves: {
            removes: removes,
            inserts: inserts
        }
    }
}

function remove(arr, index, key) {
    arr.splice(index, 1)

    return {
        from: index,
        key: key
    }
}

function keyIndex(children) {
    var keys = {}
    var free = []
    var length = children.length

    for (var i = 0; i < length; i++) {
        var child = children[i]

        if (child.key) {
            keys[child.key] = i
        } else {
            free.push(i)
        }
    }

    return {
        keys: keys,     // A hash of key name to index
        free: free,     // An array of unkeyed item indices
    }
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"../vnode/handle-thunk":11,"../vnode/is-thunk":12,"../vnode/is-vnode":14,"../vnode/is-vtext":15,"../vnode/is-widget":16,"../vnode/vpatch":19,"./diff-props":21,"x-is-array":4}],23:[function(require,module,exports){
var VNode = require('virtual-dom/vnode/vnode');
var VText = require('virtual-dom/vnode/vtext');
var diff = require('virtual-dom/vtree/diff');
var patch = require('virtual-dom/vdom/patch');
var createElement = require('virtual-dom/vdom/create-element');
var isHook = require("virtual-dom/vnode/is-vhook");


Elm.Native.VirtualDom = {};
Elm.Native.VirtualDom.make = function(elm)
{
	elm.Native = elm.Native || {};
	elm.Native.VirtualDom = elm.Native.VirtualDom || {};
	if (elm.Native.VirtualDom.values)
	{
		return elm.Native.VirtualDom.values;
	}

	var Element = Elm.Native.Graphics.Element.make(elm);
	var Json = Elm.Native.Json.make(elm);
	var List = Elm.Native.List.make(elm);
	var Signal = Elm.Native.Signal.make(elm);
	var Utils = Elm.Native.Utils.make(elm);

	var ATTRIBUTE_KEY = 'UniqueNameThatOthersAreVeryUnlikelyToUse';



	// VIRTUAL DOM NODES


	function text(string)
	{
		return new VText(string);
	}

	function node(name)
	{
		return F2(function(propertyList, contents) {
			return makeNode(name, propertyList, contents);
		});
	}


	// BUILD VIRTUAL DOME NODES


	function makeNode(name, propertyList, contents)
	{
		var props = listToProperties(propertyList);

		var key, namespace;
		// support keys
		if (props.key !== undefined)
		{
			key = props.key;
			props.key = undefined;
		}

		// support namespace
		if (props.namespace !== undefined)
		{
			namespace = props.namespace;
			props.namespace = undefined;
		}

		// ensure that setting text of an input does not move the cursor
		var useSoftSet =
			(name === 'input' || name === 'textarea')
			&& props.value !== undefined
			&& !isHook(props.value);

		if (useSoftSet)
		{
			props.value = SoftSetHook(props.value);
		}

		return new VNode(name, props, List.toArray(contents), key, namespace);
	}

	function listToProperties(list)
	{
		var object = {};
		while (list.ctor !== '[]')
		{
			var entry = list._0;
			if (entry.key === ATTRIBUTE_KEY)
			{
				object.attributes = object.attributes || {};
				object.attributes[entry.value.attrKey] = entry.value.attrValue;
			}
			else
			{
				object[entry.key] = entry.value;
			}
			list = list._1;
		}
		return object;
	}



	// PROPERTIES AND ATTRIBUTES


	function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}

	function attribute(key, value)
	{
		return {
			key: ATTRIBUTE_KEY,
			value: {
				attrKey: key,
				attrValue: value
			}
		};
	}



	// NAMESPACED ATTRIBUTES


	function attributeNS(namespace, key, value)
	{
		return {
			key: key,
			value: new AttributeHook(namespace, key, value)
		};
	}

	function AttributeHook(namespace, key, value)
	{
		if (!(this instanceof AttributeHook))
		{
			return new AttributeHook(namespace, key, value);
		}

		this.namespace = namespace;
		this.key = key;
		this.value = value;
	}

	AttributeHook.prototype.hook = function (node, prop, prev)
	{
		if (prev
			&& prev.type === 'AttributeHook'
			&& prev.value === this.value
			&& prev.namespace === this.namespace)
		{
			return;
		}

		node.setAttributeNS(this.namespace, prop, this.value);
	};

	AttributeHook.prototype.unhook = function (node, prop, next)
	{
		if (next
			&& next.type === 'AttributeHook'
			&& next.namespace === this.namespace)
		{
			return;
		}

		node.removeAttributeNS(this.namespace, this.key);
	};

	AttributeHook.prototype.type = 'AttributeHook';



	// EVENTS


	function on(name, options, decoder, createMessage)
	{
		function eventHandler(event)
		{
			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				if (options.stopPropagation)
				{
					event.stopPropagation();
				}
				if (options.preventDefault)
				{
					event.preventDefault();
				}
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property('on' + name, eventHandler);
	}

	function SoftSetHook(value)
	{
		if (!(this instanceof SoftSetHook))
		{
			return new SoftSetHook(value);
		}

		this.value = value;
	}

	SoftSetHook.prototype.hook = function (node, propertyName)
	{
		if (node[propertyName] !== this.value)
		{
			node[propertyName] = this.value;
		}
	};



	// INTEGRATION WITH ELEMENTS


	function ElementWidget(element)
	{
		this.element = element;
	}

	ElementWidget.prototype.type = "Widget";

	ElementWidget.prototype.init = function init()
	{
		return Element.render(this.element);
	};

	ElementWidget.prototype.update = function update(previous, node)
	{
		return Element.update(node, previous.element, this.element);
	};

	function fromElement(element)
	{
		return new ElementWidget(element);
	}

	function toElement(width, height, html)
	{
		return A3(Element.newElement, width, height, {
			ctor: 'Custom',
			type: 'evancz/elm-html',
			render: render,
			update: update,
			model: html
		});
	}



	// RENDER AND UPDATE


	function render(model)
	{
		var element = Element.createNode('div');
		element.appendChild(createElement(model));
		return element;
	}

	function update(node, oldModel, newModel)
	{
		updateAndReplace(node.firstChild, oldModel, newModel);
		return node;
	}

	function updateAndReplace(node, oldModel, newModel)
	{
		var patches = diff(oldModel, newModel);
		var newNode = patch(node, patches);
		return newNode;
	}



	// LAZINESS


	function lazyRef(fn, a)
	{
		function thunk()
		{
			return fn(a);
		}
		return new Thunk(fn, [a], thunk);
	}

	function lazyRef2(fn, a, b)
	{
		function thunk()
		{
			return A2(fn, a, b);
		}
		return new Thunk(fn, [a,b], thunk);
	}

	function lazyRef3(fn, a, b, c)
	{
		function thunk()
		{
			return A3(fn, a, b, c);
		}
		return new Thunk(fn, [a,b,c], thunk);
	}

	function Thunk(fn, args, thunk)
	{
		/* public (used by VirtualDom.js) */
		this.vnode = null;
		this.key = undefined;

		/* private */
		this.fn = fn;
		this.args = args;
		this.thunk = thunk;
	}

	Thunk.prototype.type = "Thunk";
	Thunk.prototype.render = renderThunk;

	function shouldUpdate(current, previous)
	{
		if (current.fn !== previous.fn)
		{
			return true;
		}

		// if it's the same function, we know the number of args must match
		var cargs = current.args;
		var pargs = previous.args;

		for (var i = cargs.length; i--; )
		{
			if (cargs[i] !== pargs[i])
			{
				return true;
			}
		}

		return false;
	}

	function renderThunk(previous)
	{
		if (previous == null || shouldUpdate(this, previous))
		{
			return this.thunk();
		}
		else
		{
			return previous.vnode;
		}
	}


	return elm.Native.VirtualDom.values = Elm.Native.VirtualDom.values = {
		node: node,
		text: text,
		on: F4(on),

		property: F2(property),
		attribute: F2(attribute),
		attributeNS: F3(attributeNS),

		lazy: F2(lazyRef),
		lazy2: F3(lazyRef2),
		lazy3: F4(lazyRef3),

		toElement: F3(toElement),
		fromElement: fromElement,

		render: createElement,
		updateAndReplace: updateAndReplace
	};
};

},{"virtual-dom/vdom/create-element":6,"virtual-dom/vdom/patch":9,"virtual-dom/vnode/is-vhook":13,"virtual-dom/vnode/vnode":18,"virtual-dom/vnode/vtext":20,"virtual-dom/vtree/diff":22}]},{},[23]);

Elm.VirtualDom = Elm.VirtualDom || {};
Elm.VirtualDom.make = function (_elm) {
   "use strict";
   _elm.VirtualDom = _elm.VirtualDom || {};
   if (_elm.VirtualDom.values) return _elm.VirtualDom.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$VirtualDom = Elm.Native.VirtualDom.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var lazy3 = $Native$VirtualDom.lazy3;
   var lazy2 = $Native$VirtualDom.lazy2;
   var lazy = $Native$VirtualDom.lazy;
   var defaultOptions = {stopPropagation: false,preventDefault: false};
   var Options = F2(function (a,b) {    return {stopPropagation: a,preventDefault: b};});
   var onWithOptions = $Native$VirtualDom.on;
   var on = F3(function (eventName,decoder,toMessage) {    return A4($Native$VirtualDom.on,eventName,defaultOptions,decoder,toMessage);});
   var attributeNS = $Native$VirtualDom.attributeNS;
   var attribute = $Native$VirtualDom.attribute;
   var property = $Native$VirtualDom.property;
   var Property = {ctor: "Property"};
   var fromElement = $Native$VirtualDom.fromElement;
   var toElement = $Native$VirtualDom.toElement;
   var text = $Native$VirtualDom.text;
   var node = $Native$VirtualDom.node;
   var Node = {ctor: "Node"};
   return _elm.VirtualDom.values = {_op: _op
                                   ,text: text
                                   ,node: node
                                   ,toElement: toElement
                                   ,fromElement: fromElement
                                   ,property: property
                                   ,attribute: attribute
                                   ,attributeNS: attributeNS
                                   ,on: on
                                   ,onWithOptions: onWithOptions
                                   ,defaultOptions: defaultOptions
                                   ,lazy: lazy
                                   ,lazy2: lazy2
                                   ,lazy3: lazy3
                                   ,Options: Options};
};
Elm.Html = Elm.Html || {};
Elm.Html.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   if (_elm.Html.values) return _elm.Html.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var fromElement = $VirtualDom.fromElement;
   var toElement = $VirtualDom.toElement;
   var text = $VirtualDom.text;
   var node = $VirtualDom.node;
   var body = node("body");
   var section = node("section");
   var nav = node("nav");
   var article = node("article");
   var aside = node("aside");
   var h1 = node("h1");
   var h2 = node("h2");
   var h3 = node("h3");
   var h4 = node("h4");
   var h5 = node("h5");
   var h6 = node("h6");
   var header = node("header");
   var footer = node("footer");
   var address = node("address");
   var main$ = node("main");
   var p = node("p");
   var hr = node("hr");
   var pre = node("pre");
   var blockquote = node("blockquote");
   var ol = node("ol");
   var ul = node("ul");
   var li = node("li");
   var dl = node("dl");
   var dt = node("dt");
   var dd = node("dd");
   var figure = node("figure");
   var figcaption = node("figcaption");
   var div = node("div");
   var a = node("a");
   var em = node("em");
   var strong = node("strong");
   var small = node("small");
   var s = node("s");
   var cite = node("cite");
   var q = node("q");
   var dfn = node("dfn");
   var abbr = node("abbr");
   var time = node("time");
   var code = node("code");
   var $var = node("var");
   var samp = node("samp");
   var kbd = node("kbd");
   var sub = node("sub");
   var sup = node("sup");
   var i = node("i");
   var b = node("b");
   var u = node("u");
   var mark = node("mark");
   var ruby = node("ruby");
   var rt = node("rt");
   var rp = node("rp");
   var bdi = node("bdi");
   var bdo = node("bdo");
   var span = node("span");
   var br = node("br");
   var wbr = node("wbr");
   var ins = node("ins");
   var del = node("del");
   var img = node("img");
   var iframe = node("iframe");
   var embed = node("embed");
   var object = node("object");
   var param = node("param");
   var video = node("video");
   var audio = node("audio");
   var source = node("source");
   var track = node("track");
   var canvas = node("canvas");
   var svg = node("svg");
   var math = node("math");
   var table = node("table");
   var caption = node("caption");
   var colgroup = node("colgroup");
   var col = node("col");
   var tbody = node("tbody");
   var thead = node("thead");
   var tfoot = node("tfoot");
   var tr = node("tr");
   var td = node("td");
   var th = node("th");
   var form = node("form");
   var fieldset = node("fieldset");
   var legend = node("legend");
   var label = node("label");
   var input = node("input");
   var button = node("button");
   var select = node("select");
   var datalist = node("datalist");
   var optgroup = node("optgroup");
   var option = node("option");
   var textarea = node("textarea");
   var keygen = node("keygen");
   var output = node("output");
   var progress = node("progress");
   var meter = node("meter");
   var details = node("details");
   var summary = node("summary");
   var menuitem = node("menuitem");
   var menu = node("menu");
   return _elm.Html.values = {_op: _op
                             ,node: node
                             ,text: text
                             ,toElement: toElement
                             ,fromElement: fromElement
                             ,body: body
                             ,section: section
                             ,nav: nav
                             ,article: article
                             ,aside: aside
                             ,h1: h1
                             ,h2: h2
                             ,h3: h3
                             ,h4: h4
                             ,h5: h5
                             ,h6: h6
                             ,header: header
                             ,footer: footer
                             ,address: address
                             ,main$: main$
                             ,p: p
                             ,hr: hr
                             ,pre: pre
                             ,blockquote: blockquote
                             ,ol: ol
                             ,ul: ul
                             ,li: li
                             ,dl: dl
                             ,dt: dt
                             ,dd: dd
                             ,figure: figure
                             ,figcaption: figcaption
                             ,div: div
                             ,a: a
                             ,em: em
                             ,strong: strong
                             ,small: small
                             ,s: s
                             ,cite: cite
                             ,q: q
                             ,dfn: dfn
                             ,abbr: abbr
                             ,time: time
                             ,code: code
                             ,$var: $var
                             ,samp: samp
                             ,kbd: kbd
                             ,sub: sub
                             ,sup: sup
                             ,i: i
                             ,b: b
                             ,u: u
                             ,mark: mark
                             ,ruby: ruby
                             ,rt: rt
                             ,rp: rp
                             ,bdi: bdi
                             ,bdo: bdo
                             ,span: span
                             ,br: br
                             ,wbr: wbr
                             ,ins: ins
                             ,del: del
                             ,img: img
                             ,iframe: iframe
                             ,embed: embed
                             ,object: object
                             ,param: param
                             ,video: video
                             ,audio: audio
                             ,source: source
                             ,track: track
                             ,canvas: canvas
                             ,svg: svg
                             ,math: math
                             ,table: table
                             ,caption: caption
                             ,colgroup: colgroup
                             ,col: col
                             ,tbody: tbody
                             ,thead: thead
                             ,tfoot: tfoot
                             ,tr: tr
                             ,td: td
                             ,th: th
                             ,form: form
                             ,fieldset: fieldset
                             ,legend: legend
                             ,label: label
                             ,input: input
                             ,button: button
                             ,select: select
                             ,datalist: datalist
                             ,optgroup: optgroup
                             ,option: option
                             ,textarea: textarea
                             ,keygen: keygen
                             ,output: output
                             ,progress: progress
                             ,meter: meter
                             ,details: details
                             ,summary: summary
                             ,menuitem: menuitem
                             ,menu: menu};
};
Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   if (_elm.Html.Attributes.values) return _elm.Html.Attributes.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var attribute = $VirtualDom.attribute;
   var contextmenu = function (value) {    return A2(attribute,"contextmenu",value);};
   var property = $VirtualDom.property;
   var stringProperty = F2(function (name,string) {    return A2(property,name,$Json$Encode.string(string));});
   var $class = function (name) {    return A2(stringProperty,"className",name);};
   var id = function (name) {    return A2(stringProperty,"id",name);};
   var title = function (name) {    return A2(stringProperty,"title",name);};
   var accesskey = function ($char) {    return A2(stringProperty,"accessKey",$String.fromChar($char));};
   var dir = function (value) {    return A2(stringProperty,"dir",value);};
   var draggable = function (value) {    return A2(stringProperty,"draggable",value);};
   var dropzone = function (value) {    return A2(stringProperty,"dropzone",value);};
   var itemprop = function (value) {    return A2(stringProperty,"itemprop",value);};
   var lang = function (value) {    return A2(stringProperty,"lang",value);};
   var tabindex = function (n) {    return A2(stringProperty,"tabIndex",$Basics.toString(n));};
   var charset = function (value) {    return A2(stringProperty,"charset",value);};
   var content = function (value) {    return A2(stringProperty,"content",value);};
   var httpEquiv = function (value) {    return A2(stringProperty,"httpEquiv",value);};
   var language = function (value) {    return A2(stringProperty,"language",value);};
   var src = function (value) {    return A2(stringProperty,"src",value);};
   var height = function (value) {    return A2(stringProperty,"height",$Basics.toString(value));};
   var width = function (value) {    return A2(stringProperty,"width",$Basics.toString(value));};
   var alt = function (value) {    return A2(stringProperty,"alt",value);};
   var preload = function (value) {    return A2(stringProperty,"preload",value);};
   var poster = function (value) {    return A2(stringProperty,"poster",value);};
   var kind = function (value) {    return A2(stringProperty,"kind",value);};
   var srclang = function (value) {    return A2(stringProperty,"srclang",value);};
   var sandbox = function (value) {    return A2(stringProperty,"sandbox",value);};
   var srcdoc = function (value) {    return A2(stringProperty,"srcdoc",value);};
   var type$ = function (value) {    return A2(stringProperty,"type",value);};
   var value = function (value) {    return A2(stringProperty,"value",value);};
   var placeholder = function (value) {    return A2(stringProperty,"placeholder",value);};
   var accept = function (value) {    return A2(stringProperty,"accept",value);};
   var acceptCharset = function (value) {    return A2(stringProperty,"acceptCharset",value);};
   var action = function (value) {    return A2(stringProperty,"action",value);};
   var autocomplete = function (bool) {    return A2(stringProperty,"autocomplete",bool ? "on" : "off");};
   var autosave = function (value) {    return A2(stringProperty,"autosave",value);};
   var enctype = function (value) {    return A2(stringProperty,"enctype",value);};
   var formaction = function (value) {    return A2(stringProperty,"formAction",value);};
   var list = function (value) {    return A2(stringProperty,"list",value);};
   var minlength = function (n) {    return A2(stringProperty,"minLength",$Basics.toString(n));};
   var maxlength = function (n) {    return A2(stringProperty,"maxLength",$Basics.toString(n));};
   var method = function (value) {    return A2(stringProperty,"method",value);};
   var name = function (value) {    return A2(stringProperty,"name",value);};
   var pattern = function (value) {    return A2(stringProperty,"pattern",value);};
   var size = function (n) {    return A2(stringProperty,"size",$Basics.toString(n));};
   var $for = function (value) {    return A2(stringProperty,"htmlFor",value);};
   var form = function (value) {    return A2(stringProperty,"form",value);};
   var max = function (value) {    return A2(stringProperty,"max",value);};
   var min = function (value) {    return A2(stringProperty,"min",value);};
   var step = function (n) {    return A2(stringProperty,"step",n);};
   var cols = function (n) {    return A2(stringProperty,"cols",$Basics.toString(n));};
   var rows = function (n) {    return A2(stringProperty,"rows",$Basics.toString(n));};
   var wrap = function (value) {    return A2(stringProperty,"wrap",value);};
   var usemap = function (value) {    return A2(stringProperty,"useMap",value);};
   var shape = function (value) {    return A2(stringProperty,"shape",value);};
   var coords = function (value) {    return A2(stringProperty,"coords",value);};
   var challenge = function (value) {    return A2(stringProperty,"challenge",value);};
   var keytype = function (value) {    return A2(stringProperty,"keytype",value);};
   var align = function (value) {    return A2(stringProperty,"align",value);};
   var cite = function (value) {    return A2(stringProperty,"cite",value);};
   var href = function (value) {    return A2(stringProperty,"href",value);};
   var target = function (value) {    return A2(stringProperty,"target",value);};
   var downloadAs = function (value) {    return A2(stringProperty,"download",value);};
   var hreflang = function (value) {    return A2(stringProperty,"hreflang",value);};
   var media = function (value) {    return A2(stringProperty,"media",value);};
   var ping = function (value) {    return A2(stringProperty,"ping",value);};
   var rel = function (value) {    return A2(stringProperty,"rel",value);};
   var datetime = function (value) {    return A2(stringProperty,"datetime",value);};
   var pubdate = function (value) {    return A2(stringProperty,"pubdate",value);};
   var start = function (n) {    return A2(stringProperty,"start",$Basics.toString(n));};
   var colspan = function (n) {    return A2(stringProperty,"colSpan",$Basics.toString(n));};
   var headers = function (value) {    return A2(stringProperty,"headers",value);};
   var rowspan = function (n) {    return A2(stringProperty,"rowSpan",$Basics.toString(n));};
   var scope = function (value) {    return A2(stringProperty,"scope",value);};
   var manifest = function (value) {    return A2(stringProperty,"manifest",value);};
   var boolProperty = F2(function (name,bool) {    return A2(property,name,$Json$Encode.bool(bool));});
   var hidden = function (bool) {    return A2(boolProperty,"hidden",bool);};
   var contenteditable = function (bool) {    return A2(boolProperty,"contentEditable",bool);};
   var spellcheck = function (bool) {    return A2(boolProperty,"spellcheck",bool);};
   var async = function (bool) {    return A2(boolProperty,"async",bool);};
   var defer = function (bool) {    return A2(boolProperty,"defer",bool);};
   var scoped = function (bool) {    return A2(boolProperty,"scoped",bool);};
   var autoplay = function (bool) {    return A2(boolProperty,"autoplay",bool);};
   var controls = function (bool) {    return A2(boolProperty,"controls",bool);};
   var loop = function (bool) {    return A2(boolProperty,"loop",bool);};
   var $default = function (bool) {    return A2(boolProperty,"default",bool);};
   var seamless = function (bool) {    return A2(boolProperty,"seamless",bool);};
   var checked = function (bool) {    return A2(boolProperty,"checked",bool);};
   var selected = function (bool) {    return A2(boolProperty,"selected",bool);};
   var autofocus = function (bool) {    return A2(boolProperty,"autofocus",bool);};
   var disabled = function (bool) {    return A2(boolProperty,"disabled",bool);};
   var multiple = function (bool) {    return A2(boolProperty,"multiple",bool);};
   var novalidate = function (bool) {    return A2(boolProperty,"noValidate",bool);};
   var readonly = function (bool) {    return A2(boolProperty,"readOnly",bool);};
   var required = function (bool) {    return A2(boolProperty,"required",bool);};
   var ismap = function (value) {    return A2(boolProperty,"isMap",value);};
   var download = function (bool) {    return A2(boolProperty,"download",bool);};
   var reversed = function (bool) {    return A2(boolProperty,"reversed",bool);};
   var classList = function (list) {    return $class(A2($String.join," ",A2($List.map,$Basics.fst,A2($List.filter,$Basics.snd,list))));};
   var style = function (props) {
      return A2(property,
      "style",
      $Json$Encode.object(A2($List.map,function (_p0) {    var _p1 = _p0;return {ctor: "_Tuple2",_0: _p1._0,_1: $Json$Encode.string(_p1._1)};},props)));
   };
   var key = function (k) {    return A2(stringProperty,"key",k);};
   return _elm.Html.Attributes.values = {_op: _op
                                        ,key: key
                                        ,style: style
                                        ,$class: $class
                                        ,classList: classList
                                        ,id: id
                                        ,title: title
                                        ,hidden: hidden
                                        ,type$: type$
                                        ,value: value
                                        ,checked: checked
                                        ,placeholder: placeholder
                                        ,selected: selected
                                        ,accept: accept
                                        ,acceptCharset: acceptCharset
                                        ,action: action
                                        ,autocomplete: autocomplete
                                        ,autofocus: autofocus
                                        ,autosave: autosave
                                        ,disabled: disabled
                                        ,enctype: enctype
                                        ,formaction: formaction
                                        ,list: list
                                        ,maxlength: maxlength
                                        ,minlength: minlength
                                        ,method: method
                                        ,multiple: multiple
                                        ,name: name
                                        ,novalidate: novalidate
                                        ,pattern: pattern
                                        ,readonly: readonly
                                        ,required: required
                                        ,size: size
                                        ,$for: $for
                                        ,form: form
                                        ,max: max
                                        ,min: min
                                        ,step: step
                                        ,cols: cols
                                        ,rows: rows
                                        ,wrap: wrap
                                        ,href: href
                                        ,target: target
                                        ,download: download
                                        ,downloadAs: downloadAs
                                        ,hreflang: hreflang
                                        ,media: media
                                        ,ping: ping
                                        ,rel: rel
                                        ,ismap: ismap
                                        ,usemap: usemap
                                        ,shape: shape
                                        ,coords: coords
                                        ,src: src
                                        ,height: height
                                        ,width: width
                                        ,alt: alt
                                        ,autoplay: autoplay
                                        ,controls: controls
                                        ,loop: loop
                                        ,preload: preload
                                        ,poster: poster
                                        ,$default: $default
                                        ,kind: kind
                                        ,srclang: srclang
                                        ,sandbox: sandbox
                                        ,seamless: seamless
                                        ,srcdoc: srcdoc
                                        ,reversed: reversed
                                        ,start: start
                                        ,align: align
                                        ,colspan: colspan
                                        ,rowspan: rowspan
                                        ,headers: headers
                                        ,scope: scope
                                        ,async: async
                                        ,charset: charset
                                        ,content: content
                                        ,defer: defer
                                        ,httpEquiv: httpEquiv
                                        ,language: language
                                        ,scoped: scoped
                                        ,accesskey: accesskey
                                        ,contenteditable: contenteditable
                                        ,contextmenu: contextmenu
                                        ,dir: dir
                                        ,draggable: draggable
                                        ,dropzone: dropzone
                                        ,itemprop: itemprop
                                        ,lang: lang
                                        ,spellcheck: spellcheck
                                        ,tabindex: tabindex
                                        ,challenge: challenge
                                        ,keytype: keytype
                                        ,cite: cite
                                        ,datetime: datetime
                                        ,pubdate: pubdate
                                        ,manifest: manifest
                                        ,property: property
                                        ,attribute: attribute};
};
Elm.Html = Elm.Html || {};
Elm.Html.Events = Elm.Html.Events || {};
Elm.Html.Events.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Events = _elm.Html.Events || {};
   if (_elm.Html.Events.values) return _elm.Html.Events.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var keyCode = A2($Json$Decode._op[":="],"keyCode",$Json$Decode.$int);
   var targetChecked = A2($Json$Decode.at,_U.list(["target","checked"]),$Json$Decode.bool);
   var targetValue = A2($Json$Decode.at,_U.list(["target","value"]),$Json$Decode.string);
   var defaultOptions = $VirtualDom.defaultOptions;
   var Options = F2(function (a,b) {    return {stopPropagation: a,preventDefault: b};});
   var onWithOptions = $VirtualDom.onWithOptions;
   var on = $VirtualDom.on;
   var messageOn = F3(function (name,addr,msg) {    return A3(on,name,$Json$Decode.value,function (_p0) {    return A2($Signal.message,addr,msg);});});
   var onClick = messageOn("click");
   var onDoubleClick = messageOn("dblclick");
   var onMouseMove = messageOn("mousemove");
   var onMouseDown = messageOn("mousedown");
   var onMouseUp = messageOn("mouseup");
   var onMouseEnter = messageOn("mouseenter");
   var onMouseLeave = messageOn("mouseleave");
   var onMouseOver = messageOn("mouseover");
   var onMouseOut = messageOn("mouseout");
   var onBlur = messageOn("blur");
   var onFocus = messageOn("focus");
   var onSubmit = messageOn("submit");
   var onKey = F3(function (name,addr,handler) {    return A3(on,name,keyCode,function (code) {    return A2($Signal.message,addr,handler(code));});});
   var onKeyUp = onKey("keyup");
   var onKeyDown = onKey("keydown");
   var onKeyPress = onKey("keypress");
   return _elm.Html.Events.values = {_op: _op
                                    ,onBlur: onBlur
                                    ,onFocus: onFocus
                                    ,onSubmit: onSubmit
                                    ,onKeyUp: onKeyUp
                                    ,onKeyDown: onKeyDown
                                    ,onKeyPress: onKeyPress
                                    ,onClick: onClick
                                    ,onDoubleClick: onDoubleClick
                                    ,onMouseMove: onMouseMove
                                    ,onMouseDown: onMouseDown
                                    ,onMouseUp: onMouseUp
                                    ,onMouseEnter: onMouseEnter
                                    ,onMouseLeave: onMouseLeave
                                    ,onMouseOver: onMouseOver
                                    ,onMouseOut: onMouseOut
                                    ,on: on
                                    ,onWithOptions: onWithOptions
                                    ,defaultOptions: defaultOptions
                                    ,targetValue: targetValue
                                    ,targetChecked: targetChecked
                                    ,keyCode: keyCode
                                    ,Options: Options};
};
Elm.Random = Elm.Random || {};
Elm.Random.PCG = Elm.Random.PCG || {};
Elm.Random.PCG.make = function (_elm) {
   "use strict";
   _elm.Random = _elm.Random || {};
   _elm.Random.PCG = _elm.Random.PCG || {};
   if (_elm.Random.PCG.values) return _elm.Random.PCG.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Bitwise = Elm.Bitwise.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var listHelp = F4(function (list,n,generate,seed) {
      listHelp: while (true) if (_U.cmp(n,1) < 0) return {ctor: "_Tuple2",_0: $List.reverse(list),_1: seed}; else {
            var _p0 = generate(seed);
            var value = _p0._0;
            var newSeed = _p0._1;
            var _v0 = A2($List._op["::"],value,list),_v1 = n - 1,_v2 = generate,_v3 = newSeed;
            list = _v0;
            n = _v1;
            generate = _v2;
            seed = _v3;
            continue listHelp;
         }
   });
   var minInt = 0;
   var maxInt = 4294967295;
   var bit27 = 1.34217728e8;
   var bit53 = 9.007199254740992e15;
   var Seed = F2(function (a,b) {    return {ctor: "Seed",_0: a,_1: b};});
   var generate = F2(function (_p1,seed) {    var _p2 = _p1;return _p2._0(seed);});
   var Generator = function (a) {    return {ctor: "Generator",_0: a};};
   var list = F2(function (n,_p3) {    var _p4 = _p3;return Generator(function (seed) {    return A4(listHelp,_U.list([]),n,_p4._0,seed);});});
   var constant = function (value) {    return Generator(function (seed) {    return {ctor: "_Tuple2",_0: value,_1: seed};});};
   var map = F2(function (func,_p5) {
      var _p6 = _p5;
      return Generator(function (seed0) {    var _p7 = _p6._0(seed0);var a = _p7._0;var seed1 = _p7._1;return {ctor: "_Tuple2",_0: func(a),_1: seed1};});
   });
   var map2 = F3(function (func,_p9,_p8) {
      var _p10 = _p9;
      var _p11 = _p8;
      return Generator(function (seed0) {
         var _p12 = _p10._0(seed0);
         var a = _p12._0;
         var seed1 = _p12._1;
         var _p13 = _p11._0(seed1);
         var b = _p13._0;
         var seed2 = _p13._1;
         return {ctor: "_Tuple2",_0: A2(func,a,b),_1: seed2};
      });
   });
   var pair = F2(function (genA,genB) {    return A3(map2,F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),genA,genB);});
   var andMap = map2(F2(function (x,y) {    return x(y);}));
   var map3 = F4(function (func,_p16,_p15,_p14) {
      var _p17 = _p16;
      var _p18 = _p15;
      var _p19 = _p14;
      return Generator(function (seed0) {
         var _p20 = _p17._0(seed0);
         var a = _p20._0;
         var seed1 = _p20._1;
         var _p21 = _p18._0(seed1);
         var b = _p21._0;
         var seed2 = _p21._1;
         var _p22 = _p19._0(seed2);
         var c = _p22._0;
         var seed3 = _p22._1;
         return {ctor: "_Tuple2",_0: A3(func,a,b,c),_1: seed3};
      });
   });
   var map4 = F5(function (func,_p26,_p25,_p24,_p23) {
      var _p27 = _p26;
      var _p28 = _p25;
      var _p29 = _p24;
      var _p30 = _p23;
      return Generator(function (seed0) {
         var _p31 = _p27._0(seed0);
         var a = _p31._0;
         var seed1 = _p31._1;
         var _p32 = _p28._0(seed1);
         var b = _p32._0;
         var seed2 = _p32._1;
         var _p33 = _p29._0(seed2);
         var c = _p33._0;
         var seed3 = _p33._1;
         var _p34 = _p30._0(seed3);
         var d = _p34._0;
         var seed4 = _p34._1;
         return {ctor: "_Tuple2",_0: A4(func,a,b,c,d),_1: seed4};
      });
   });
   var map5 = F6(function (func,_p39,_p38,_p37,_p36,_p35) {
      var _p40 = _p39;
      var _p41 = _p38;
      var _p42 = _p37;
      var _p43 = _p36;
      var _p44 = _p35;
      return Generator(function (seed0) {
         var _p45 = _p40._0(seed0);
         var a = _p45._0;
         var seed1 = _p45._1;
         var _p46 = _p41._0(seed1);
         var b = _p46._0;
         var seed2 = _p46._1;
         var _p47 = _p42._0(seed2);
         var c = _p47._0;
         var seed3 = _p47._1;
         var _p48 = _p43._0(seed3);
         var d = _p48._0;
         var seed4 = _p48._1;
         var _p49 = _p44._0(seed4);
         var e = _p49._0;
         var seed5 = _p49._1;
         return {ctor: "_Tuple2",_0: A5(func,a,b,c,d,e),_1: seed5};
      });
   });
   var andThen = F2(function (_p50,callback) {
      var _p51 = _p50;
      return Generator(function (seed) {
         var _p52 = _p51._0(seed);
         var result = _p52._0;
         var newSeed = _p52._1;
         var _p53 = callback(result);
         var generateB = _p53._0;
         return generateB(newSeed);
      });
   });
   var filter = F2(function (predicate,generator) {
      return A2(andThen,generator,function (a) {    return predicate(a) ? constant(a) : A2(filter,predicate,generator);});
   });
   var Int64 = F2(function (a,b) {    return {ctor: "Int64",_0: a,_1: b};});
   var magicFactor = A2(Int64,1481765933,1284865837);
   _op[">>>"] = $Bitwise.shiftRightLogical;
   var add64 = F2(function (_p55,_p54) {
      var _p56 = _p55;
      var _p58 = _p56._1;
      var _p57 = _p54;
      var lo = A2(_op[">>>"],_p58 + _p57._1,0);
      var hi = A2(_op[">>>"],_p56._0 + _p57._0,0);
      var hi$ = _U.cmp(A2(_op[">>>"],lo,0),A2(_op[">>>"],_p58,0)) < 0 ? A2($Bitwise.or,hi + 1,0) : hi;
      return A2(Int64,hi$,lo);
   });
   _op["<<"] = $Bitwise.shiftLeft;
   _op["&"] = $Bitwise.and;
   var peel = function (_p59) {
      var _p60 = _p59;
      var _p62 = _p60._0._1;
      var _p61 = _p60._0._0;
      var rot = A2(_op[">>>"],_p61,27);
      var rot2 = A2(_op[">>>"],A2(_op["&"],A2(_op[">>>"],0 - rot,0),31),0);
      var xsLo = A2(_op[">>>"],A2($Bitwise.or,A2(_op[">>>"],_p62,18),A2(_op["<<"],_p61,14)),0);
      var xsLo$ = A2(_op[">>>"],A2($Bitwise.xor,xsLo,_p62),0);
      var xsHi = A2(_op[">>>"],_p61,18);
      var xsHi$ = A2(_op[">>>"],A2($Bitwise.xor,xsHi,_p61),0);
      var xorshifted = A2(_op[">>>"],A2($Bitwise.or,A2(_op[">>>"],xsLo$,27),A2(_op["<<"],xsHi$,5)),0);
      return A2(_op[">>>"],A2($Bitwise.or,A2(_op[">>>"],xorshifted,rot),A2(_op["<<"],xorshifted,rot2)),0);
   };
   var mul32 = F2(function (a,b) {
      var bl = A2(_op["&"],b,65535);
      var bh = A2(_op["&"],A2(_op[">>>"],b,16),65535);
      var al = A2(_op["&"],a,65535);
      var ah = A2(_op["&"],A2(_op[">>>"],a,16),65535);
      return A2($Bitwise.or,0,al * bl + A2(_op[">>>"],A2(_op["<<"],ah * bl + al * bh,16),0));
   });
   var mul64 = F2(function (_p64,_p63) {
      var _p65 = _p64;
      var _p68 = _p65._1;
      var _p66 = _p63;
      var _p67 = _p66._1;
      var lo = A2(_op[">>>"],A2(_op["&"],_p68,65535) * A2(_op["&"],_p67,65535),0);
      var c0 = A2(_op["&"],_p68,65535) * A2(_op[">>>"],A2(_op[">>>"],_p67,16),0);
      var c0$ = A2(_op[">>>"],A2(_op["<<"],c0,16),0);
      var lo$ = A2(_op[">>>"],lo + c0$,0);
      var c1 = A2(_op[">>>"],_p68,16) * A2(_op[">>>"],A2(_op["&"],_p67,65535),0);
      var hi = A2(_op[">>>"],_p68,16) * A2(_op[">>>"],_p67,16) + A2(_op[">>>"],A2(_op[">>>"],c0,16) + A2(_op[">>>"],c1,16),0);
      var hi$ = _U.cmp(A2(_op[">>>"],lo$,0),A2(_op[">>>"],c0$,0)) < 0 ? A2(_op[">>>"],hi + 1,0) : hi;
      var c1$ = A2(_op[">>>"],A2(_op["<<"],c1,16),0);
      var lo$$ = A2(_op[">>>"],lo$ + c1$,0);
      var hi$$ = _U.cmp(A2(_op[">>>"],lo$$,0),A2(_op[">>>"],c1$,0)) < 0 ? A2(_op[">>>"],hi$ + 1,0) : hi$;
      var hi$$$ = A2(_op[">>>"],hi$$ + A2(mul32,_p68,_p66._0),0);
      var hi$$$$ = A2(_op[">>>"],hi$$$ + A2(mul32,_p65._0,_p67),0);
      return A2(Int64,hi$$$$,lo$$);
   });
   var next = function (_p69) {
      var _p70 = _p69;
      var _p71 = _p70._1;
      var state1 = A2(mul64,_p70._0,magicFactor);
      var state2 = A2(add64,state1,_p71);
      return A2(Seed,state2,_p71);
   };
   var initialSeed2 = F2(function (stateHi,stateLo) {
      var incr = A2(Int64,335903614,4150755663);
      var zero = A2(Int64,0,0);
      var seed0 = A2(Seed,zero,incr);
      var _p72 = next(seed0);
      var state1 = _p72._0;
      var state2 = A2(add64,state1,A2(Int64,A2(_op[">>>"],stateHi,0),A2(_op[">>>"],stateLo,0)));
      return next(A2(Seed,state2,incr));
   });
   var initialSeed = initialSeed2(0);
   var integer = F2(function (max,seed0) {
      if (_U.eq(A2(_op["&"],max,max - 1),0)) return {ctor: "_Tuple2",_0: A2(_op[">>>"],A2(_op["&"],peel(seed0),max - 1),0),_1: next(seed0)}; else {
            var threshhold = A2(_op[">>>"],A2($Basics._op["%"],A2(_op[">>>"],0 - max,0),max),0);
            var accountForBias = function (seed) {
               accountForBias: while (true) {
                  var seedN = next(seed);
                  var x = peel(seed);
                  if (_U.cmp(x,threshhold) < 0) {
                        var _v28 = seedN;
                        seed = _v28;
                        continue accountForBias;
                     } else return {ctor: "_Tuple2",_0: A2($Basics._op["%"],x,max),_1: seedN};
               }
            };
            return accountForBias(seed0);
         }
   });
   var $int = F2(function (min,max) {
      return Generator(function (seed0) {
         if (_U.eq(min,max)) return {ctor: "_Tuple2",_0: min,_1: seed0}; else {
               var range = $Basics.abs(max - min) + 1;
               var _p73 = A2(integer,range,seed0);
               var i = _p73._0;
               var seed1 = _p73._1;
               return {ctor: "_Tuple2",_0: i + min,_1: seed1};
            }
      });
   });
   var bool = A2(map,F2(function (x,y) {    return _U.eq(x,y);})(1),A2($int,0,1));
   var $float = F2(function (min,max) {
      return Generator(function (seed0) {
         var range = $Basics.abs(max - min);
         var n0 = peel(seed0);
         var hi = $Basics.toFloat(A2(_op["&"],n0,67108863)) * 1.0;
         var seed1 = next(seed0);
         var n1 = peel(seed1);
         var lo = $Basics.toFloat(A2(_op["&"],n1,134217727)) * 1.0;
         var val = (hi * bit27 + lo) / bit53;
         var scaled = val * range + min;
         return {ctor: "_Tuple2",_0: scaled,_1: next(seed1)};
      });
   });
   var split = function (seed0) {
      var gen1 = A2($int,minInt,maxInt);
      var gen4 = A5(map4,F4(function (v0,v1,v2,v3) {    return {ctor: "_Tuple4",_0: v0,_1: v1,_2: v2,_3: v3};}),gen1,gen1,gen1,gen1);
      var _p74 = A2(generate,gen4,seed0);
      var a = _p74._0._0;
      var b = _p74._0._1;
      var c = _p74._0._2;
      var d = _p74._0._3;
      var seed1 = _p74._1;
      var dOdd = A2(_op[">>>"],A2($Bitwise.or,d,1),0);
      var seed2 = A2(Seed,A2(Int64,a,b),A2(Int64,c,dOdd));
      return {ctor: "_Tuple2",_0: next(seed1),_1: next(seed2)};
   };
   var fastForward = F2(function (delta0,_p75) {
      var _p76 = _p75;
      var _p78 = _p76._1;
      var zero = A2(Int64,0,0);
      var one = A2(Int64,0,1);
      var helper = F6(function (accMult,accPlus,curMult,curPlus,delta,repeat) {
         helper: while (true) {
            var newDelta = A2(_op[">>>"],delta,1);
            var curMult$ = A2(mul64,curMult,curMult);
            var curPlus$ = A2(mul64,A2(add64,curMult,one),curPlus);
            var deltaOdd = _U.eq(A2(_op["&"],delta,1),1);
            var accMult$ = deltaOdd ? A2(mul64,accMult,curMult) : accMult;
            var accPlus$ = deltaOdd ? A2(add64,A2(mul64,accPlus,curMult),curPlus) : accPlus;
            if (_U.eq(newDelta,0)) if (_U.cmp(delta0,0) < 0 && repeat) {
                     var _v30 = accMult$,_v31 = accPlus$,_v32 = curMult$,_v33 = curPlus$,_v34 = -1,_v35 = false;
                     accMult = _v30;
                     accPlus = _v31;
                     curMult = _v32;
                     curPlus = _v33;
                     delta = _v34;
                     repeat = _v35;
                     continue helper;
                  } else return {ctor: "_Tuple2",_0: accMult$,_1: accPlus$}; else {
                  var _v36 = accMult$,_v37 = accPlus$,_v38 = curMult$,_v39 = curPlus$,_v40 = newDelta,_v41 = repeat;
                  accMult = _v36;
                  accPlus = _v37;
                  curMult = _v38;
                  curPlus = _v39;
                  delta = _v40;
                  repeat = _v41;
                  continue helper;
               }
         }
      });
      var _p77 = A6(helper,one,zero,magicFactor,_p78,delta0,true);
      var accMultFinal = _p77._0;
      var accPlusFinal = _p77._1;
      var state1 = A2(add64,accPlusFinal,A2(mul64,accMultFinal,_p76._0));
      return A2(Seed,state1,_p78);
   });
   return _elm.Random.PCG.values = {_op: _op
                                   ,bool: bool
                                   ,$int: $int
                                   ,$float: $float
                                   ,list: list
                                   ,pair: pair
                                   ,map: map
                                   ,map2: map2
                                   ,map3: map3
                                   ,map4: map4
                                   ,map5: map5
                                   ,andMap: andMap
                                   ,filter: filter
                                   ,constant: constant
                                   ,andThen: andThen
                                   ,minInt: minInt
                                   ,maxInt: maxInt
                                   ,generate: generate
                                   ,initialSeed2: initialSeed2
                                   ,initialSeed: initialSeed
                                   ,split: split
                                   ,fastForward: fastForward};
};
Elm.Rnd = Elm.Rnd || {};
Elm.Rnd.make = function (_elm) {
   "use strict";
   _elm.Rnd = _elm.Rnd || {};
   if (_elm.Rnd.values) return _elm.Rnd.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Random$PCG = Elm.Random.PCG.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var refreshCol = function (rnd) {
      var _p0 = rnd.colRnd(rnd.seed);
      var nextCol = _p0._0;
      var seed$ = _p0._1;
      return _U.update(rnd,{seed: seed$,col: nextCol});
   };
   var refreshRow = function (rnd) {
      var _p1 = rnd.rowRnd(rnd.seed);
      var nextRow = _p1._0;
      var seed$ = _p1._1;
      return _U.update(rnd,{seed: seed$,row: nextRow});
   };
   var refreshCoinFlip = function (rnd) {
      var _p2 = A2($Random$PCG.generate,A2($Random$PCG.$int,1,2),rnd.seed);
      var headOrTail = _p2._0;
      var seed$ = _p2._1;
      return _U.update(rnd,{seed: seed$,heads: _U.eq(headOrTail,1)});
   };
   var refresh = function (rnd) {
      var _p3 = rnd.rowRnd(rnd.seed);
      var nextRow = _p3._0;
      var seed2 = _p3._1;
      var _p4 = rnd.colRnd(seed2);
      var nextCol = _p4._0;
      var seed3 = _p4._1;
      var _p5 = A2($Random$PCG.generate,A2($Random$PCG.$int,1,2),seed3);
      var headOrTail = _p5._0;
      var seed4 = _p5._1;
      return _U.update(rnd,{seed: seed4,row: nextRow,col: nextCol,heads: _U.eq(headOrTail,1)});
   };
   var randInt = F2(function (rnd,max) {    return $Basics.fst(A2($Random$PCG.generate,A2($Random$PCG.$int,0,max - 1),rnd.seed));});
   var nextSeed = function (rnd) {    return refresh(rnd).seed;};
   var createGridRnd = F3(function (rows,cols,initSeed) {
      return {seed: initSeed
             ,row: -1
             ,col: -1
             ,heads: false
             ,rowRnd: $Random$PCG.generate(A2($Random$PCG.$int,0,rows - 1))
             ,colRnd: $Random$PCG.generate(A2($Random$PCG.$int,0,cols - 1))};
   });
   var GridRnd = F6(function (a,b,c,d,e,f) {    return {seed: a,row: b,col: c,heads: d,rowRnd: e,colRnd: f};});
   return _elm.Rnd.values = {_op: _op
                            ,GridRnd: GridRnd
                            ,createGridRnd: createGridRnd
                            ,nextSeed: nextSeed
                            ,randInt: randInt
                            ,refresh: refresh
                            ,refreshCoinFlip: refreshCoinFlip
                            ,refreshRow: refreshRow
                            ,refreshCol: refreshCol};
};
Elm.Mask = Elm.Mask || {};
Elm.Mask.make = function (_elm) {
   "use strict";
   _elm.Mask = _elm.Mask || {};
   if (_elm.Mask.values) return _elm.Mask.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Rnd = Elm.Rnd.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var fromImage = F2(function (_p0,flags) {
      var _p1 = _p0;
      var _p3 = _p1._1;
      var _p2 = _p1._0;
      var rowBools = function (row) {    var start = row * _p2;var end = start + _p2;return A3($Array.slice,start,end,flags);};
      return {rows: _p3,cols: _p2,bits: A2($Array.initialize,_p3,rowBools)};
   });
   var fromTxt = function (lines) {
      var validLines = A2($List.map,$String.trim,A2($List.filter,function (l) {    return $Basics.not($String.isEmpty(l));},lines));
      var rows = $List.length(validLines);
      var cols = $String.length(A2($Maybe.withDefault,"",$List.head(validLines)));
      var linesArr = $Array.fromList(validLines);
      var rowBools = function (row) {
         var rowStr = A2($Array.get,row,linesArr);
         var cols = $String.toList(A2($Maybe.withDefault,"",rowStr));
         return $Array.fromList(A2($List.map,function (c) {    return !_U.eq(c,_U.chr("X"));},cols));
      };
      return {rows: rows,cols: cols,bits: A2($Array.initialize,rows,rowBools)};
   };
   var count = function (mask) {
      var addCols = function (rowbits) {
         return A3($Array.foldl,F2(function (x,y) {    return x + y;}),0,A2($Array.map,function (b) {    return b ? 1 : 0;},rowbits));
      };
      return A3($Array.foldl,F2(function (x,y) {    return x + y;}),0,A2($Array.map,addCols,mask.bits));
   };
   var set = F3(function (mask,_p4,isOn) {
      var _p5 = _p4;
      var _p7 = _p5._0;
      var _p6 = A2($Array.get,_p7,mask.bits);
      if (_p6.ctor === "Just") {
            return _U.update(mask,{bits: A3($Array.set,_p7,A3($Array.set,_p5._1,isOn,_p6._0),mask.bits)});
         } else {
            return mask;
         }
   });
   var mset = F2(function (mask,vals) {
      var setone = F2(function (e,mask$) {    return A3(set,mask$,$Basics.fst(e),$Basics.snd(e));});
      return A3($List.foldl,setone,mask,vals);
   });
   var get = F3(function (mask,row,col) {
      var _p8 = A2($Array.get,row,mask.bits);
      if (_p8.ctor === "Just") {
            var _p9 = A2($Array.get,col,_p8._0);
            if (_p9.ctor === "Just") {
                  return _p9._0;
               } else {
                  return false;
               }
         } else {
            return false;
         }
   });
   var createMask = F2(function (cols,rows) {
      var bits = A2($Array.initialize,rows,function (n) {    return A2($Array.repeat,cols,true);});
      return {rows: rows,cols: cols,bits: bits};
   });
   var maxGetRandomLocationTries = 10000;
   var getRandomLoc = F3(function (mask,rnd,itr) {
      getRandomLoc: while (true) if (A3(get,mask,rnd.row,rnd.col)) return {ctor: "_Tuple2",_0: rnd.row,_1: rnd.col}; else if (_U.cmp(itr,
         maxGetRandomLocationTries) > 0) return {ctor: "_Tuple2",_0: 0,_1: 0}; else {
               var _v5 = mask,_v6 = $Rnd.refresh(rnd),_v7 = itr + 1;
               mask = _v5;
               rnd = _v6;
               itr = _v7;
               continue getRandomLoc;
            }
   });
   var randomLocation = F2(function (mask,rnd) {    return A3(getRandomLoc,mask,rnd,1);});
   var Mask = F3(function (a,b,c) {    return {rows: a,cols: b,bits: c};});
   return _elm.Mask.values = {_op: _op
                             ,Mask: Mask
                             ,maxGetRandomLocationTries: maxGetRandomLocationTries
                             ,createMask: createMask
                             ,get: get
                             ,set: set
                             ,mset: mset
                             ,count: count
                             ,randomLocation: randomLocation
                             ,getRandomLoc: getRandomLoc
                             ,fromTxt: fromTxt
                             ,fromImage: fromImage};
};
Elm.Cell = Elm.Cell || {};
Elm.Cell.make = function (_elm) {
   "use strict";
   _elm.Cell = _elm.Cell || {};
   if (_elm.Cell.values) return _elm.Cell.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var cellToString = function (cell) {
      return A2($Basics._op["++"],
      "(",
      A2($Basics._op["++"],$Basics.toString(cell.row),A2($Basics._op["++"],", ",A2($Basics._op["++"],$Basics.toString(cell.col),")"))));
   };
   var hasLinks = function (cell) {    return $Basics.not($Set.isEmpty(cell.links));};
   var isLinked = F2(function (cell1,cell2) {    return A2($Set.member,cell2.id,cell1.links);});
   var isMasked = function (cell) {    var _p0 = cell;if (_p0.ctor === "Nothing") {    return true;} else {    return _p0._0.masked;}};
   var linked = function (cell) {    return cell.links;};
   var isNilCell = function (cell) {    return _U.eq(cell.row,-1) && _U.eq(cell.col,-1);};
   var isNilCellID = function (_p1) {    var _p2 = _p1;return _U.eq(_p2._0,-1) && _U.eq(_p2._1,-1);};
   var createCellID = F2(function (a,b) {    return {ctor: "_Tuple2",_0: a,_1: b};});
   var createCell = F2(function (row,col) {    return {id: A2(createCellID,row,col),row: row,col: col,masked: false,links: $Set.empty};});
   var createMaskedCell = F2(function (row,col) {    var cell = A2(createCell,row,col);return _U.update(cell,{masked: true});});
   var createNilCell = A2(createCell,-1,-1);
   var BaseCell = F5(function (a,b,c,d,e) {    return {id: a,row: b,col: c,masked: d,links: e};});
   return _elm.Cell.values = {_op: _op
                             ,BaseCell: BaseCell
                             ,createCell: createCell
                             ,createMaskedCell: createMaskedCell
                             ,createNilCell: createNilCell
                             ,createCellID: createCellID
                             ,isNilCellID: isNilCellID
                             ,isNilCell: isNilCell
                             ,linked: linked
                             ,isMasked: isMasked
                             ,isLinked: isLinked
                             ,hasLinks: hasLinks
                             ,cellToString: cellToString};
};
Elm.Arithmetic = Elm.Arithmetic || {};
Elm.Arithmetic.make = function (_elm) {
   "use strict";
   _elm.Arithmetic = _elm.Arithmetic || {};
   if (_elm.Arithmetic.values) return _elm.Arithmetic.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var isEven = function (x) {    return _U.eq(A2($Basics._op["%"],x,2),0);};
   var isOdd = function (x) {    return $Basics.not(isEven(x));};
   return _elm.Arithmetic.values = {_op: _op,isEven: isEven,isOdd: isOdd};
};
Elm.GridCell = Elm.GridCell || {};
Elm.GridCell.make = function (_elm) {
   "use strict";
   _elm.GridCell = _elm.GridCell || {};
   if (_elm.GridCell.values) return _elm.GridCell.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var toPolarCell = function (cell) {
      var _p0 = cell;
      switch (_p0.ctor)
      {case "PolarCellTag": return _p0._0;
         case "RectCellTag": return {ctor: "_Tuple2",_0: _p0._0,_1: {ctor: "_Tuple2",_0: {ctor: "_Tuple2",_0: -1,_1: -1},_1: $Set.empty}};
         case "HexCellTag": return {ctor: "_Tuple2",_0: _p0._0,_1: {ctor: "_Tuple2",_0: {ctor: "_Tuple2",_0: -1,_1: -1},_1: $Set.empty}};
         default: return {ctor: "_Tuple2",_0: _p0._0,_1: {ctor: "_Tuple2",_0: {ctor: "_Tuple2",_0: -1,_1: -1},_1: $Set.empty}};}
   };
   var base = function (gc) {
      var _p1 = gc;
      switch (_p1.ctor)
      {case "RectCellTag": return _p1._0;
         case "PolarCellTag": return _p1._0._0;
         case "HexCellTag": return _p1._0;
         default: return _p1._0;}
   };
   var toRectCell = function (cell) {    return base(cell);};
   var maybeGridCellToMaybeCell = function (cell) {    return A2($Maybe.map,base,cell);};
   var filterGridCells = F2(function (fn,cells) {    return A2($List.filter,function (_p2) {    return fn(base(_p2));},cells);});
   var isValidCell = function (cell) {    var _p3 = cell;if (_p3.ctor === "Nothing") {    return false;} else {    return true;}};
   var id = function (gc) {
      var _p4 = gc;
      switch (_p4.ctor)
      {case "RectCellTag": return _p4._0.id;
         case "PolarCellTag": return _p4._0._0.id;
         case "HexCellTag": return _p4._0.id;
         default: return _p4._0.id;}
   };
   var row = function (gc) {    return $Basics.fst(id(gc));};
   var col = function (gc) {    return $Basics.snd(id(gc));};
   var TriangleCellTag = function (a) {    return {ctor: "TriangleCellTag",_0: a};};
   var HexCellTag = function (a) {    return {ctor: "HexCellTag",_0: a};};
   var PolarCellTag = function (a) {    return {ctor: "PolarCellTag",_0: a};};
   var cellToPolarCell = function (base) {
      return PolarCellTag({ctor: "_Tuple2",_0: base,_1: {ctor: "_Tuple2",_0: {ctor: "_Tuple2",_0: -1,_1: -1},_1: $Set.empty}});
   };
   var setInwardCell = F2(function (cell,inward) {
      var _p5 = toPolarCell(inward);
      var ic = _p5._0;
      var icid = _p5._1._0;
      var _p6 = toPolarCell(cell);
      var c = _p6._0;
      var cid = _p6._1._0;
      var links = _p6._1._1;
      return PolarCellTag({ctor: "_Tuple2",_0: c,_1: {ctor: "_Tuple2",_0: ic.id,_1: links}});
   });
   var addOutwardLink = F2(function (parentCell,outwardCell) {
      var _p7 = toPolarCell(outwardCell);
      var cell = _p7._0;
      var cid = _p7._1._0;
      var clinks = _p7._1._1;
      var _p8 = toPolarCell(parentCell);
      var pcell = _p8._0;
      var pcid = _p8._1._0;
      var pclinks = _p8._1._1;
      var newLinks = A2($Set.insert,cell.id,pclinks);
      return PolarCellTag({ctor: "_Tuple2",_0: pcell,_1: {ctor: "_Tuple2",_0: pcid,_1: newLinks}});
   });
   var RectCellTag = function (a) {    return {ctor: "RectCellTag",_0: a};};
   var maybeGridCellToGridCell = function (cell) {
      var _p9 = cell;
      if (_p9.ctor === "Nothing") {
            return RectCellTag($Cell.createNilCell);
         } else {
            switch (_p9._0.ctor)
            {case "RectCellTag": return RectCellTag(_p9._0._0);
               case "PolarCellTag": return PolarCellTag(_p9._0._0);
               case "HexCellTag": return HexCellTag(_p9._0._0);
               default: return TriangleCellTag(_p9._0._0);}
         }
   };
   var maybeGridCellToCell = function (cell) {    return base(maybeGridCellToGridCell(cell));};
   return _elm.GridCell.values = {_op: _op
                                 ,RectCellTag: RectCellTag
                                 ,PolarCellTag: PolarCellTag
                                 ,HexCellTag: HexCellTag
                                 ,TriangleCellTag: TriangleCellTag
                                 ,id: id
                                 ,row: row
                                 ,col: col
                                 ,isValidCell: isValidCell
                                 ,base: base
                                 ,cellToPolarCell: cellToPolarCell
                                 ,toRectCell: toRectCell
                                 ,toPolarCell: toPolarCell
                                 ,setInwardCell: setInwardCell
                                 ,addOutwardLink: addOutwardLink
                                 ,maybeGridCellToCell: maybeGridCellToCell
                                 ,maybeGridCellToMaybeCell: maybeGridCellToMaybeCell
                                 ,maybeGridCellToGridCell: maybeGridCellToGridCell
                                 ,filterGridCells: filterGridCells};
};
Elm.ListUtils = Elm.ListUtils || {};
Elm.ListUtils.make = function (_elm) {
   "use strict";
   _elm.ListUtils = _elm.ListUtils || {};
   if (_elm.ListUtils.values) return _elm.ListUtils.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var indicesOf = F2(function (thing,things) {
      return A2($List.map,
      $Basics.fst,
      A2($List.filter,
      function (_p0) {
         var _p1 = _p0;
         return _U.eq(_p1._1,thing);
      },
      A2($List.indexedMap,F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),things)));
   });
   var firstIndexOf = F2(function (thing,things) {    return A2($Maybe.withDefault,-1,$List.minimum(A2(indicesOf,thing,things)));});
   return _elm.ListUtils.values = {_op: _op,indicesOf: indicesOf,firstIndexOf: firstIndexOf};
};
Elm.GridUtils = Elm.GridUtils || {};
Elm.GridUtils.make = function (_elm) {
   "use strict";
   _elm.GridUtils = _elm.GridUtils || {};
   if (_elm.GridUtils.values) return _elm.GridUtils.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $List = Elm.List.make(_elm),
   $ListUtils = Elm.ListUtils.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Random$PCG = Elm.Random.PCG.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Rnd = Elm.Rnd.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var smooshMaybes = function (maybes) {
      var cellToList = function (cell) {    var _p0 = cell;if (_p0.ctor === "Just") {    return _U.list([_p0._0]);} else {    return _U.list([]);}};
      return $List.concat(A2($List.map,cellToList,maybes));
   };
   var indexOfCell = F2(function (cell,cells) {    var id = $GridCell.id(cell);return A2($ListUtils.firstIndexOf,id,A2($List.map,$GridCell.id,cells));});
   var sampleCell = F2(function (sample,rnd) {
      var _p1 = A2($Random$PCG.generate,A2($Random$PCG.$int,0,$List.length(sample) - 1),rnd.seed);
      var rand = _p1._0;
      var seed = _p1._1;
      return A2($Array.get,rand,$Array.fromList(sample));
   });
   return _elm.GridUtils.values = {_op: _op,sampleCell: sampleCell,indexOfCell: indexOfCell,smooshMaybes: smooshMaybes};
};
Elm.Grid = Elm.Grid || {};
Elm.Grid.make = function (_elm) {
   "use strict";
   _elm.Grid = _elm.Grid || {};
   if (_elm.Grid.values) return _elm.Grid.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $Mask = Elm.Mask.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Rnd = Elm.Rnd.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var cellBackgroundColor = F2(function (grid,cell) {    return $Color.white;});
   var cellToAscii = F2(function (grid,cell) {    return cell.masked ? "M" : " ";});
   var toTitle = function (grid) {
      return A2($Basics._op["++"],$Basics.toString(grid.rows),A2($Basics._op["++"]," X ",A2($Basics._op["++"],$Basics.toString(grid.cols)," Grid")));
   };
   var gridIndex = F3(function (grid,row,col) {    return grid.cols * row + col;});
   var cellIndex = F2(function (grid,cell) {    var rc = $GridCell.base(cell);return grid.cols * rc.row + rc.col;});
   var size = function (grid) {    return $Mask.count(grid.mask);};
   var gridCellsToBaseCells = function (gridcells) {    return A2($List.map,$GridCell.base,gridcells);};
   var rowCells = F2(function (grid,row) {    return $Array.toList(A2($Maybe.withDefault,$Array.empty,A2($Array.get,row,grid.cells)));});
   var rowMatcher = F2(function (cell,row) {    return _U.eq($GridCell.row(cell),row);});
   var gridCellID = function (gc) {    return $GridCell.id(gc);};
   var filterNeighbors2 = F4(function (neighborsFn,pred,grid,cell) {    return A2($List.filter,pred,A2(neighborsFn,grid,cell));});
   var toValidCell = function (cell) {    var _p0 = cell;if (_p0.ctor === "Just") {    return _p0._0;} else {    return $Cell.createNilCell;}};
   var getCell = F3(function (grid,row,col) {
      if (_U.cmp(row,grid.rows) > -1 || (_U.cmp(row,0) < 0 || _U.cmp(col,0) < 0)) return $Maybe.Nothing; else {
            var rowCells = A2($Maybe.withDefault,$Array.empty,A2($Array.get,row,grid.cells));
            var sampleCell = A2($Array.get,0,rowCells);
            var _p1 = sampleCell;
            _v1_3: do {
               if (_p1.ctor === "Just") {
                     switch (_p1._0.ctor)
                     {case "RectCellTag": return _U.cmp(col,grid.cols) > -1 || _p1._0._0.masked ? $Maybe.Nothing : A2($Array.get,col,rowCells);
                        case "PolarCellTag": if (_p1._0._0.ctor === "_Tuple2") {
                                var rowLen = $Array.length(rowCells);
                                return A2($Array.get,A2($Basics._op["%"],col,rowLen),rowCells);
                             } else {
                                break _v1_3;
                             }
                        case "HexCellTag": return _U.cmp(col,grid.cols) > -1 || _p1._0._0.masked ? $Maybe.Nothing : A2($Array.get,col,rowCells);
                        default: break _v1_3;}
                  } else {
                     break _v1_3;
                  }
            } while (false);
            return $Maybe.Nothing;
         }
   });
   var north = F2(function (grid,cell) {    return A3(getCell,grid,cell.row - 1,cell.col);});
   var south = F2(function (grid,cell) {    return A3(getCell,grid,cell.row + 1,cell.col);});
   var west = F2(function (grid,cell) {    return A3(getCell,grid,cell.row,cell.col - 1);});
   var east = F2(function (grid,cell) {    return A3(getCell,grid,cell.row,cell.col + 1);});
   var neighbors = F2(function (grid,cell) {
      var _p2 = cell;
      if (_p2.ctor === "RectCellTag") {
            var _p3 = _p2._0;
            var e = A2(east,grid,_p3);
            var w = A2(west,grid,_p3);
            var s = A2(south,grid,_p3);
            var n = A2(north,grid,_p3);
            return $GridUtils.smooshMaybes(_U.list([n,s,w,e]));
         } else {
            return _U.list([]);
         }
   });
   var center = function (grid) {    return $GridCell.maybeGridCellToCell(A3(getCell,grid,grid.rows / 2 | 0,grid.cols / 2 | 0));};
   var randomCell = function (grid) {
      var _p4 = A2($Mask.randomLocation,grid.mask,grid.rnd);
      var row = _p4._0;
      var col = _p4._1;
      return A3(getCell,grid,row,col);
   };
   var cellIdToCell = F2(function (grid,cellid) {
      var col = $Basics.snd(cellid);
      var row = $Basics.fst(cellid);
      return $GridCell.maybeGridCellToGridCell(A3(getCell,grid,row,col));
   });
   var linkedCells = F2(function (grid,cell) {    var base = $GridCell.base(cell);return A2($List.map,cellIdToCell(grid),$Set.toList(base.links));});
   var cellsListToCellGrid = function (cells) {
      var rowMax = A2($Maybe.withDefault,0,$List.maximum(A2($List.map,$GridCell.row,cells)));
      return A2($Array.initialize,
      rowMax + 1,
      function (row) {
         return $Array.fromList(A2($List.filter,function (c) {    return _U.eq($GridCell.row(c),row);},cells));
      });
   };
   var cellsList = function (cells) {    return $List.concat($Array.toList(A2($Array.map,$Array.toList,cells)));};
   var deadEnds = function (grid) {
      return A2($List.filter,function (c) {    return _U.eq($List.length($Set.toList(c.links)),1);},gridCellsToBaseCells(cellsList(grid.cells)));
   };
   var linkCellsHelper = F4(function (grid,cell,cellToLinkId,bidi) {
      var linkCell = F2(function (cell1,id) {    return _U.update(cell1,{links: A2($Set.insert,id,cell1.links)});});
      var linker = function (c) {    return _U.eq(c.id,cell.id) ? A2(linkCell,c,cellToLinkId) : bidi && _U.eq(c.id,cellToLinkId) ? A2(linkCell,c,cell.id) : c;};
      var linkMatched = function (c) {
         var _p5 = c;
         switch (_p5.ctor)
         {case "RectCellTag": return $GridCell.RectCellTag(linker(_p5._0));
            case "PolarCellTag": return $GridCell.PolarCellTag({ctor: "_Tuple2",_0: linker(_p5._0._0),_1: _p5._0._1});
            case "HexCellTag": return $GridCell.HexCellTag(linker(_p5._0));
            default: return $GridCell.TriangleCellTag(linker(_p5._0));}
      };
      return _U.update(grid,{cells: cellsListToCellGrid(A2($List.map,linkMatched,cellsList(grid.cells)))});
   });
   var linkCells = F4(function (grid,cell,cell2,bidi) {
      var base = $GridCell.base(cell);
      var c2Id = gridCellID(cell2);
      return A4(linkCellsHelper,grid,base,c2Id,bidi);
   });
   var painter = F3(function (cellPainter,grid,cellSize) {
      var cellToRect = function (cell) {    return $Graphics$Collage.square($Basics.toFloat(cellSize));};
      var cellBackground = F2(function (style,cell) {
         var halfSize = $Basics.toFloat(cellSize) / 2.0;
         var rectcell = $GridCell.base(cell);
         var bgRect = A2($Graphics$Collage.filled,A2(cellPainter,grid,cell),cellToRect(rectcell));
         var cx = $Basics.toFloat(rectcell.col * cellSize) + halfSize;
         var cy = $Basics.toFloat($Basics.negate(rectcell.row) * cellSize) - halfSize;
         return A2($Graphics$Collage.move,{ctor: "_Tuple2",_0: cx,_1: cy},bgRect);
      });
      var maybeVisibleLine = F2(function (style,_p6) {    var _p7 = _p6;return _p7._0 ? _U.list([A2($Graphics$Collage.traced,style,_p7._1)]) : _U.list([]);});
      var cellWalls = F2(function (style,gridcell) {
         var cell = $GridCell.base(gridcell);
         var x1 = $Basics.toFloat(cell.col * cellSize);
         var y1 = $Basics.toFloat($Basics.negate(cell.row) * cellSize);
         var x2 = $Basics.toFloat((cell.col + 1) * cellSize);
         var y2 = $Basics.toFloat($Basics.negate(cell.row + 1) * cellSize);
         return cell.masked ? _U.list([]) : A2($List.concatMap,
         maybeVisibleLine(style),
         _U.list([{ctor: "_Tuple2"
                  ,_0: $Basics.not($GridCell.isValidCell(A2(north,grid,cell)))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: x1,_1: y1},{ctor: "_Tuple2",_0: x2,_1: y1})}
                 ,{ctor: "_Tuple2"
                  ,_0: $Basics.not($GridCell.isValidCell(A2(west,grid,cell)))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: x1,_1: y1},{ctor: "_Tuple2",_0: x1,_1: y2})}
                 ,{ctor: "_Tuple2"
                  ,_0: $Basics.not(A2($Cell.isLinked,cell,$GridCell.maybeGridCellToCell(A2(east,grid,cell))))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: x2,_1: y1},{ctor: "_Tuple2",_0: x2,_1: y2})}
                 ,{ctor: "_Tuple2"
                  ,_0: $Basics.not(A2($Cell.isLinked,cell,$GridCell.maybeGridCellToCell(A2(south,grid,cell))))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: x1,_1: y2},{ctor: "_Tuple2",_0: x2,_1: y2})}]));
      });
      var paintCell = function (cell) {
         var style = _U.update($Graphics$Collage.defaultLine,{width: 2});
         return $Graphics$Collage.group(A2($List._op["::"],A2(cellBackground,style,cell),A2(cellWalls,style,cell)));
      };
      var drawables = A2($List.map,paintCell,cellsList(grid.cells));
      var imgHeight = cellSize * grid.rows;
      var oy = $Basics.toFloat(imgHeight) / 2.0;
      var imgWidth = cellSize * grid.cols;
      var ox = $Basics.toFloat($Basics.negate(imgWidth)) / 2.0;
      return A3($Graphics$Collage.collage,
      imgWidth,
      imgHeight,
      _U.list([A2($Graphics$Collage.move,{ctor: "_Tuple2",_0: ox,_1: oy},$Graphics$Collage.group(drawables))]));
   });
   var toAscii = F2(function (grid,cellViewer) {
      var cellToString = F2(function (cell,ascii) {
         var curbottom = ascii.bottom;
         var curtop = ascii.top;
         var south_boundary = A2($Cell.isLinked,cell,$GridCell.maybeGridCellToCell(A2(south,grid,cell))) ? "   " : "---";
         var east_boundary = A2($Cell.isLinked,cell,$GridCell.maybeGridCellToCell(A2(east,grid,cell))) ? " " : "|";
         var body = A2($Basics._op["++"]," ",A2($Basics._op["++"],A2(cellViewer,grid,cell)," "));
         return _U.update(ascii,
         {top: A2($Basics._op["++"],curtop,A2($Basics._op["++"],body,east_boundary))
         ,bottom: A2($Basics._op["++"],curbottom,A2($Basics._op["++"],south_boundary,"+"))});
      });
      var rowToStrings = function (row) {
         var baseCells = gridCellsToBaseCells(A2(rowCells,grid,row));
         var rowascii = {top: "|",bottom: "+"};
         var finalascii = A3($List.foldl,cellToString,rowascii,baseCells);
         return A2($Basics._op["++"],finalascii.top,A2($Basics._op["++"],"\n",A2($Basics._op["++"],finalascii.bottom,"\n")));
      };
      return A2($Basics._op["++"],
      "+",
      A2($Basics._op["++"],
      A2($String.repeat,grid.cols,"---+"),
      A2($Basics._op["++"],"\n",$String.concat(A2($List.map,rowToStrings,_U.range(0,grid.rows - 1))))));
   });
   var toElement = F4(function (grid,gridPainter,cellPainter,cellSize) {    return A3(gridPainter,cellPainter,grid,cellSize);});
   var makeCells = function (mask) {
      var createMaskedCell = F2(function (row,col) {
         return A3($Mask.get,mask,row,col) ? $GridCell.RectCellTag(A2($Cell.createCell,row,col)) : $GridCell.RectCellTag(A2($Cell.createMaskedCell,row,col));
      });
      var makeRow = F2(function (row,cols) {    return A2($Array.initialize,mask.cols,function (n) {    return A2(createMaskedCell,row,n);});});
      return A2($Array.initialize,mask.rows,function (n) {    return A2(makeRow,n,mask.cols);});
   };
   var update = function (grid) {    return _U.update(grid,{cells: grid.cellMaker(grid.mask)});};
   var updateRnd = function (grid) {    return _U.update(grid,{rnd: $Rnd.refresh(grid.rnd)});};
   var createGridFromMask = F3(function (mask,initSeed,cellMaker) {
      return {rows: mask.rows
             ,cols: mask.cols
             ,cells: cellMaker(mask)
             ,cellMaker: cellMaker
             ,rnd: A3($Rnd.createGridRnd,mask.rows,mask.cols,initSeed)
             ,mask: mask
             ,maximum: 0
             ,dists: _U.list([])};
   });
   var createGrid = F4(function (rows,cols,initSeed,cellMaker) {
      var mask$ = A2($Mask.createMask,rows,cols);
      return A3(createGridFromMask,mask$,initSeed,cellMaker);
   });
   var RowAscii = F2(function (a,b) {    return {top: a,bottom: b};});
   return _elm.Grid.values = {_op: _op
                             ,RowAscii: RowAscii
                             ,createGrid: createGrid
                             ,createGridFromMask: createGridFromMask
                             ,updateRnd: updateRnd
                             ,update: update
                             ,makeCells: makeCells
                             ,toElement: toElement
                             ,toAscii: toAscii
                             ,painter: painter
                             ,cellsList: cellsList
                             ,cellsListToCellGrid: cellsListToCellGrid
                             ,getCell: getCell
                             ,toValidCell: toValidCell
                             ,north: north
                             ,south: south
                             ,west: west
                             ,east: east
                             ,center: center
                             ,randomCell: randomCell
                             ,neighbors: neighbors
                             ,filterNeighbors2: filterNeighbors2
                             ,deadEnds: deadEnds
                             ,gridCellID: gridCellID
                             ,linkCellsHelper: linkCellsHelper
                             ,linkCells: linkCells
                             ,linkedCells: linkedCells
                             ,rowMatcher: rowMatcher
                             ,rowCells: rowCells
                             ,gridCellsToBaseCells: gridCellsToBaseCells
                             ,size: size
                             ,cellIndex: cellIndex
                             ,gridIndex: gridIndex
                             ,cellIdToCell: cellIdToCell
                             ,toTitle: toTitle
                             ,cellToAscii: cellToAscii
                             ,cellBackgroundColor: cellBackgroundColor};
};
Elm.PolarGrid = Elm.PolarGrid || {};
Elm.PolarGrid.make = function (_elm) {
   "use strict";
   _elm.PolarGrid = _elm.PolarGrid || {};
   if (_elm.PolarGrid.values) return _elm.PolarGrid.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $List = Elm.List.make(_elm),
   $Mask = Elm.Mask.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Rnd = Elm.Rnd.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var randomCell = function (grid) {
      var grid$ = $Grid.updateRnd(grid);
      var randRow = grid$.rnd.row;
      var rowLen = $List.length(A2($Grid.rowCells,grid,randRow));
      var randCol = A2($Rnd.randInt,grid$.rnd,rowLen);
      return A3($Grid.getCell,grid$,randRow,randCol);
   };
   var size = function (grid) {    return $List.length($Grid.cellsList(grid.cells));};
   var maybeGridCellToMaybePolarCell = function (cell) {    return A2($Maybe.map,$GridCell.toPolarCell,cell);};
   var toCellList = function (cell) {
      var _p0 = cell;
      if (_p0.ctor === "Nothing") {
            return _U.list([]);
         } else {
            return _U.list([$GridCell.PolarCellTag(_p0._0)]);
         }
   };
   var toValidCell = function (cell) {
      var _p1 = cell;
      if (_p1.ctor === "Just") {
            return _p1._0;
         } else {
            return {ctor: "_Tuple2",_0: $Cell.createNilCell,_1: {ctor: "_Tuple2",_0: {ctor: "_Tuple2",_0: -1,_1: -1},_1: $Set.empty}};
         }
   };
   var polarCellsToGridCells = function (cells) {    return A2($List.map,function (c) {    return $GridCell.PolarCellTag(c);},cells);};
   var gridCellsToPolarCells = function (gridcells) {    return A2($List.map,$GridCell.toPolarCell,gridcells);};
   var center = function (grid) {    return $GridCell.maybeGridCellToGridCell(A3($Grid.getCell,grid,0,0));};
   var outwardCells = F2(function (grid,outward) {    var outwardIds = $Set.toList(outward);return A2($List.map,$Grid.cellIdToCell(grid),outwardIds);});
   var counterClockwiseCell = F2(function (grid,cell) {    return maybeGridCellToMaybePolarCell(A3($Grid.getCell,grid,cell.row,cell.col - 1));});
   var clockwiseCell = F2(function (grid,cell) {    return maybeGridCellToMaybePolarCell(A3($Grid.getCell,grid,cell.row,cell.col + 1));});
   var neighbors = F2(function (grid,cell) {
      var _p2 = cell;
      if (_p2.ctor === "PolarCellTag" && _p2._0.ctor === "_Tuple2" && _p2._0._1.ctor === "_Tuple2") {
            var _p4 = _p2._0._1._0;
            var _p3 = _p2._0._0;
            var outward = A2(outwardCells,grid,_p2._0._1._1);
            var inward = $Cell.isNilCellID(_p4) ? _U.list([]) : _U.list([A2($Grid.cellIdToCell,grid,_p4)]);
            var ccw = toCellList(A2(counterClockwiseCell,grid,_p3));
            var cw = toCellList(A2(clockwiseCell,grid,_p3));
            return A2($List.append,$List.concat(_U.list([cw,ccw,inward])),outward);
         } else {
            return _U.list([]);
         }
   });
   var painter = F3(function (cellPainter,grid,cellSize) {
      var radius = grid.rows * cellSize;
      var circleForm = A2($Graphics$Collage.outlined,$Graphics$Collage.defaultLine,$Graphics$Collage.circle($Basics.toFloat(radius)));
      var wall = $Color.black;
      var imgSize = 2 * grid.rows * cellSize;
      var center = $Basics.toFloat(imgSize) / 2;
      var cellLines = function (gc) {
         var _p5 = $GridCell.toPolarCell(gc);
         var cell = _p5._0;
         var inward = _p5._1._0;
         var outwards = _p5._1._1;
         var theta = 2 * $Basics.pi / $Basics.toFloat($List.length(A2($Grid.rowCells,grid,cell.row)));
         var innerRadius = $Basics.toFloat(cell.row * cellSize);
         var outerRadius = $Basics.toFloat((cell.row + 1) * cellSize);
         var thetaCcw = $Basics.toFloat(cell.col) * theta;
         var ax = center + innerRadius * $Basics.cos(thetaCcw);
         var ay = center + innerRadius * $Basics.sin(thetaCcw);
         var bx = center + outerRadius * $Basics.cos(thetaCcw);
         var by = center + outerRadius * $Basics.sin(thetaCcw);
         var midTheta = ($Basics.toFloat(cell.col) + 0.5) * theta;
         var midX = center + outerRadius * $Basics.cos(midTheta);
         var midY = center + outerRadius * $Basics.sin(midTheta);
         var thetaCw = $Basics.toFloat(cell.col + 1) * theta;
         var cx = center + innerRadius * $Basics.cos(thetaCw);
         var cy = center + innerRadius * $Basics.sin(thetaCw);
         var dx = center + outerRadius * $Basics.cos(thetaCw);
         var dy = center + outerRadius * $Basics.sin(thetaCw);
         var filled = A2($Graphics$Collage.filled,
         A2(cellPainter,grid,gc),
         $Graphics$Collage.polygon(_U.list([{ctor: "_Tuple2",_0: ax,_1: ay}
                                           ,{ctor: "_Tuple2",_0: bx,_1: by}
                                           ,{ctor: "_Tuple2",_0: midX,_1: midY}
                                           ,{ctor: "_Tuple2",_0: dx,_1: dy}
                                           ,{ctor: "_Tuple2",_0: cx,_1: cy}])));
         var linkedInward = A2($Cell.isLinked,cell,$Basics.fst($GridCell.toPolarCell(A2($Grid.cellIdToCell,grid,inward))));
         var line1 = $Basics.not(linkedInward) ? _U.list([A2($Graphics$Collage.segment,
         {ctor: "_Tuple2",_0: ax,_1: ay},
         {ctor: "_Tuple2",_0: cx,_1: cy})]) : _U.list([]);
         var linkedCw = A2($Cell.isLinked,cell,$Basics.fst(toValidCell(A2(clockwiseCell,grid,cell))));
         var line2 = $Basics.not(linkedCw) ? _U.list([A2($Graphics$Collage.segment,
         {ctor: "_Tuple2",_0: cx,_1: cy},
         {ctor: "_Tuple2",_0: dx,_1: dy})]) : _U.list([]);
         return A2($List._op["::"],filled,A2($List.map,$Graphics$Collage.traced($Graphics$Collage.defaultLine),$List.concat(_U.list([line1,line2]))));
      };
      var drawables = A2($List.concatMap,
      cellLines,
      A2($List.filter,function (c) {    return _U.cmp($Basics.fst($GridCell.toPolarCell(c)).row,0) > 0;},$Grid.cellsList(grid.cells)));
      var forms = A2($List._op["::"],
      circleForm,
      _U.list([A2($Graphics$Collage.move,{ctor: "_Tuple2",_0: $Basics.negate(center),_1: $Basics.negate(center)},$Graphics$Collage.group(drawables))]));
      return A3($Graphics$Collage.collage,imgSize + 1,imgSize + 1,forms);
   });
   var configureCells = F3(function (rows,cols,incells) {
      var rowLength = F2(function (row,cells) {
         return $List.length(A2($List.filter,function (c) {    return _U.eq($GridCell.toRectCell(c).row,row);},cells));
      });
      var configurer = F2(function (gc,work) {
         var _p6 = $GridCell.toPolarCell(gc);
         var cell = _p6._0;
         var rowLen = A2(rowLength,cell.row,work.cells);
         var divLen = A2(rowLength,cell.row - 1,work.cells);
         var ratio = $Basics.toFloat(rowLen) / $Basics.toFloat(divLen);
         var pcol = $Basics.floor($Basics.toFloat(cell.col) / ratio);
         var parent = $GridCell.maybeGridCellToGridCell($List.head(A2($List.filter,
         function (c) {
            var rc = $GridCell.toRectCell(c);
            return _U.eq(rc.row,cell.row - 1) && _U.eq(rc.col,pcol);
         },
         work.cells)));
         var parent$ = A2($GridCell.addOutwardLink,parent,gc);
         var cell$ = A2($GridCell.setInwardCell,gc,parent$);
         var newCells = A2($List.map,
         function (c) {
            var pcId = $GridCell.id(c);
            return _U.eq(pcId,$GridCell.id(parent$)) ? parent$ : _U.eq(pcId,$GridCell.id(cell$)) ? cell$ : c;
         },
         work.cells);
         return _U.update(work,{cells: newCells});
      });
      var cellList = $Grid.cellsList(incells);
      var res = {cells: cellList,rows: rows,cols: cols};
      var result = A3($List.foldl,configurer,res,A2($List.filter,function (c) {    return _U.cmp($GridCell.toRectCell(c).row,0) > 0;},cellList));
      return $Grid.cellsListToCellGrid(result.cells);
   });
   var ConfigStep = F3(function (a,b,c) {    return {cells: a,rows: b,cols: c};});
   var makeCells = function (mask) {
      var nrows = mask.rows;
      var rowHeight = 1 / $Basics.toFloat(nrows);
      var rows = A2($Array.initialize,nrows,function (r) {    return $Array.empty;});
      var rows$ = A3($Array.set,0,$Array.fromList(_U.list([$GridCell.cellToPolarCell(A2($Cell.createCell,0,0))])),rows);
      var makeCellRows = F2(function (res,row) {
         makeCellRows: while (true) if (_U.cmp(row,nrows) > -1) return res; else {
               var prevCount = $Array.length(A2($Maybe.withDefault,$Array.empty,A2($Array.get,row - 1,res)));
               var radius = $Basics.toFloat(row) / $Basics.toFloat(nrows);
               var circumference = 2 * $Basics.pi * radius;
               var estCellWidth = circumference / $Basics.toFloat(prevCount);
               var ratio = $Basics.round(estCellWidth / rowHeight);
               var ncells = prevCount * ratio;
               var rowCells = A2($Array.initialize,ncells,function (a) {    return $GridCell.cellToPolarCell(A2($Cell.createCell,row,a));});
               var res$ = A3($Array.set,row,rowCells,res);
               var _v3 = res$,_v4 = row + 1;
               res = _v3;
               row = _v4;
               continue makeCellRows;
            }
      });
      var acells = A2(makeCellRows,rows$,1);
      return A3(configureCells,nrows,mask.cols,acells);
   };
   return _elm.PolarGrid.values = {_op: _op
                                  ,makeCells: makeCells
                                  ,ConfigStep: ConfigStep
                                  ,configureCells: configureCells
                                  ,clockwiseCell: clockwiseCell
                                  ,counterClockwiseCell: counterClockwiseCell
                                  ,outwardCells: outwardCells
                                  ,center: center
                                  ,gridCellsToPolarCells: gridCellsToPolarCells
                                  ,polarCellsToGridCells: polarCellsToGridCells
                                  ,toValidCell: toValidCell
                                  ,toCellList: toCellList
                                  ,maybeGridCellToMaybePolarCell: maybeGridCellToMaybePolarCell
                                  ,size: size
                                  ,randomCell: randomCell
                                  ,neighbors: neighbors
                                  ,painter: painter};
};
Elm.HexGrid = Elm.HexGrid || {};
Elm.HexGrid.make = function (_elm) {
   "use strict";
   _elm.HexGrid = _elm.HexGrid || {};
   if (_elm.HexGrid.values) return _elm.HexGrid.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Arithmetic = Elm.Arithmetic.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $Mask = Elm.Mask.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var cellBackgroundColor = F2(function (grid,gridcell) {    return A3($Color.rgb,255,255,255);});
   var HexVertices = F7(function (a,b,c,d,e,f,g) {    return {x_fw: a,x_nw: b,x_ne: c,x_fe: d,y_n: e,y_m: f,y_s: g};});
   var getCell = F3(function (grid,row,col) {    return A3($Grid.getCell,grid,row,col);});
   var south = F2(function (grid,cell) {    return A3(getCell,grid,cell.row + 1,cell.col);});
   var north = F2(function (grid,cell) {    return A3(getCell,grid,cell.row - 1,cell.col);});
   var southDiag = function (cell) {    return $Arithmetic.isEven(cell.col) ? cell.row : cell.row + 1;};
   var southwest = F2(function (grid,cell) {    return A3(getCell,grid,southDiag(cell),cell.col - 1);});
   var southeast = F2(function (grid,cell) {    return A3(getCell,grid,southDiag(cell),cell.col + 1);});
   var northDiag = function (cell) {    return $Arithmetic.isEven(cell.col) ? cell.row - 1 : cell.row;};
   var northwest = F2(function (grid,cell) {    return A3(getCell,grid,northDiag(cell),cell.col - 1);});
   var northeast = F2(function (grid,cell) {    return A3(getCell,grid,northDiag(cell),cell.col + 1);});
   var neighbors = F2(function (grid,gc) {
      var _p0 = gc;
      if (_p0.ctor === "HexCellTag") {
            var _p1 = _p0._0;
            return $GridUtils.smooshMaybes(_U.list([A2(northwest,grid,_p1)
                                                   ,A2(north,grid,_p1)
                                                   ,A2(northeast,grid,_p1)
                                                   ,A2(southwest,grid,_p1)
                                                   ,A2(south,grid,_p1)
                                                   ,A2(southeast,grid,_p1)]));
         } else {
            return _U.crashCase("HexGrid",
            {start: {line: 81,column: 5},end: {line: 92,column: 91}},
            _p0)("Illegal call to HexGrid.neighbors with non-HexCellTag type cell");
         }
   });
   var painter = F3(function (cellPainter,grid,cellSize) {
      var maybeVisibleLine = F2(function (style,_p3) {    var _p4 = _p3;return _p4._0 ? _U.list([A2($Graphics$Collage.traced,style,_p4._1)]) : _U.list([]);});
      var cellWalls = F3(function (style,gc,vx) {
         var cell = $GridCell.base(gc);
         return cell.masked ? _U.list([]) : A2($List.concatMap,
         maybeVisibleLine(style),
         _U.list([{ctor: "_Tuple2"
                  ,_0: $Basics.not($GridCell.isValidCell(A2(southwest,grid,cell)))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: vx.x_fw,_1: vx.y_m},{ctor: "_Tuple2",_0: vx.x_nw,_1: vx.y_s})}
                 ,{ctor: "_Tuple2"
                  ,_0: $Basics.not($GridCell.isValidCell(A2(northwest,grid,cell)))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: vx.x_fw,_1: vx.y_m},{ctor: "_Tuple2",_0: vx.x_nw,_1: vx.y_n})}
                 ,{ctor: "_Tuple2"
                  ,_0: $Basics.not($GridCell.isValidCell(A2(north,grid,cell)))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: vx.x_nw,_1: vx.y_n},{ctor: "_Tuple2",_0: vx.x_ne,_1: vx.y_n})}
                 ,{ctor: "_Tuple2"
                  ,_0: $Basics.not(A2($Cell.isLinked,cell,$GridCell.maybeGridCellToCell(A2(northeast,grid,cell))))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: vx.x_ne,_1: vx.y_n},{ctor: "_Tuple2",_0: vx.x_fe,_1: vx.y_m})}
                 ,{ctor: "_Tuple2"
                  ,_0: $Basics.not(A2($Cell.isLinked,cell,$GridCell.maybeGridCellToCell(A2(southeast,grid,cell))))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: vx.x_fe,_1: vx.y_m},{ctor: "_Tuple2",_0: vx.x_ne,_1: vx.y_s})}
                 ,{ctor: "_Tuple2"
                  ,_0: $Basics.not(A2($Cell.isLinked,cell,$GridCell.maybeGridCellToCell(A2(south,grid,cell))))
                  ,_1: A2($Graphics$Collage.segment,{ctor: "_Tuple2",_0: vx.x_ne,_1: vx.y_s},{ctor: "_Tuple2",_0: vx.x_nw,_1: vx.y_s})}]));
      });
      var cellBackground = F2(function (gc,vx) {
         var ngon = $Graphics$Collage.polygon(_U.list([{ctor: "_Tuple2",_0: vx.x_fw,_1: vx.y_m}
                                                      ,{ctor: "_Tuple2",_0: vx.x_nw,_1: vx.y_n}
                                                      ,{ctor: "_Tuple2",_0: vx.x_ne,_1: vx.y_n}
                                                      ,{ctor: "_Tuple2",_0: vx.x_fe,_1: vx.y_m}
                                                      ,{ctor: "_Tuple2",_0: vx.x_ne,_1: vx.y_s}
                                                      ,{ctor: "_Tuple2",_0: vx.x_nw,_1: vx.y_s}]));
         var color = A2(cellPainter,grid,gc);
         var outline = $Graphics$Collage.solid(color);
         return $Graphics$Collage.group(_U.list([A2($Graphics$Collage.filled,color,ngon),A2($Graphics$Collage.outlined,outline,ngon)]));
      });
      var wall = $Color.black;
      var background = $Color.white;
      var width = cellSize * 2;
      var bsize = cellSize * $Basics.sqrt(3) / 2;
      var height = bsize * 2;
      var imgHeight = $Basics.round(height * $Basics.toFloat(grid.rows) + bsize + 0.5);
      var oy = $Basics.negate($Basics.toFloat(imgHeight)) / 2.0;
      var asize = cellSize / 2;
      var imgWidth = $Basics.round(3 * asize * $Basics.toFloat(grid.cols) + asize + 0.5);
      var ox = $Basics.negate($Basics.toFloat(imgWidth)) / 2.0;
      var paintCell = function (gc) {
         var dl = $Graphics$Collage.defaultLine;
         var style = _U.update(dl,{width: 3});
         var cell = $GridCell.base(gc);
         var cx = cellSize + 3 * cell.col * $Basics.round(asize);
         var cy = bsize + $Basics.toFloat(cell.row) * height;
         var cy$ = $Basics.round($Arithmetic.isOdd(cell.col) ? cy + bsize : cy);
         var vertices = {x_fw: $Basics.toFloat(cx) - $Basics.toFloat(cellSize)
                        ,x_nw: $Basics.toFloat(cx) - asize
                        ,x_ne: $Basics.toFloat(cx) + asize
                        ,x_fe: $Basics.toFloat(cx) + $Basics.toFloat(cellSize)
                        ,y_n: $Basics.toFloat(cy$) - bsize
                        ,y_m: $Basics.toFloat(cy$)
                        ,y_s: $Basics.toFloat(cy$) + bsize};
         return $Graphics$Collage.group(A2($List._op["::"],A2(cellBackground,gc,vertices),A3(cellWalls,style,gc,vertices)));
      };
      var drawables = A2($List.map,paintCell,$Grid.cellsList(grid.cells));
      var forms = _U.list([A2($Graphics$Collage.move,{ctor: "_Tuple2",_0: ox,_1: oy},$Graphics$Collage.group(drawables))]);
      return A3($Graphics$Collage.collage,imgWidth + 1,imgHeight + 1,forms);
   });
   var configureCells = F3(function (rows,cols,incells) {    return incells;});
   var makeCells = function (mask) {
      var createMaskedCell = F2(function (row,col) {
         return A3($Mask.get,mask,row,col) ? $GridCell.HexCellTag(A2($Cell.createCell,row,col)) : $GridCell.HexCellTag(A2($Cell.createMaskedCell,row,col));
      });
      var makeRow = F2(function (row,cols) {    return A2($Array.initialize,mask.cols,function (n) {    return A2(createMaskedCell,row,n);});});
      var acells = A2($Array.initialize,mask.rows,function (n) {    return A2(makeRow,n,mask.cols);});
      return A3(configureCells,mask.rows,mask.cols,acells);
   };
   return _elm.HexGrid.values = {_op: _op
                                ,makeCells: makeCells
                                ,configureCells: configureCells
                                ,northDiag: northDiag
                                ,southDiag: southDiag
                                ,northwest: northwest
                                ,north: north
                                ,northeast: northeast
                                ,southwest: southwest
                                ,south: south
                                ,southeast: southeast
                                ,getCell: getCell
                                ,neighbors: neighbors
                                ,HexVertices: HexVertices
                                ,painter: painter
                                ,cellBackgroundColor: cellBackgroundColor};
};
Elm.TriangleGrid = Elm.TriangleGrid || {};
Elm.TriangleGrid.make = function (_elm) {
   "use strict";
   _elm.TriangleGrid = _elm.TriangleGrid || {};
   if (_elm.TriangleGrid.values) return _elm.TriangleGrid.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Arithmetic = Elm.Arithmetic.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $Mask = Elm.Mask.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var west = F2(function (grid,cell) {    return A2($Grid.west,grid,cell);});
   var east = F2(function (grid,cell) {    return A2($Grid.east,grid,cell);});
   var upright = function (_p0) {    var _p1 = _p0;return $Arithmetic.isEven(_p1.row + _p1.col);};
   var north = F2(function (grid,cell) {    return upright(cell) ? $Maybe.Nothing : A3($Grid.getCell,grid,cell.row - 1,cell.col);});
   var south = F2(function (grid,cell) {    return upright(cell) ? A3($Grid.getCell,grid,cell.row + 1,cell.col) : $Maybe.Nothing;});
   var neighbors = F2(function (grid,gc) {
      var _p2 = gc;
      if (_p2.ctor === "TriangleCellTag") {
            var _p3 = _p2._0;
            return $GridUtils.smooshMaybes(_U.list([A2(north,grid,_p3),A2(south,grid,_p3),A2(west,grid,_p3),A2(east,grid,_p3)]));
         } else {
            return _U.crashCase("TriangleGrid",
            {start: {line: 61,column: 5},end: {line: 70,column: 96}},
            _p2)("Illegal call to HexGrid.neighbors with non-TriangleCellTag type cell");
         }
   });
   var configureCells = F3(function (rows,cols,incells) {    return incells;});
   var makeCells = function (mask) {
      var createMaskedCell = F2(function (row,col) {
         return A3($Mask.get,mask,row,col) ? $GridCell.TriangleCellTag(A2($Cell.createCell,row,col)) : $GridCell.TriangleCellTag(A2($Cell.createMaskedCell,
         row,
         col));
      });
      var makeRow = F2(function (row,cols) {    return A2($Array.initialize,mask.cols,function (n) {    return A2(createMaskedCell,row,n);});});
      var acells = A2($Array.initialize,mask.rows,function (n) {    return A2(makeRow,n,mask.cols);});
      return A3(configureCells,mask.rows,mask.cols,acells);
   };
   return _elm.TriangleGrid.values = {_op: _op
                                     ,makeCells: makeCells
                                     ,configureCells: configureCells
                                     ,upright: upright
                                     ,north: north
                                     ,south: south
                                     ,east: east
                                     ,west: west
                                     ,neighbors: neighbors};
};
Elm.AldousBroder = Elm.AldousBroder || {};
Elm.AldousBroder.make = function (_elm) {
   "use strict";
   _elm.AldousBroder = _elm.AldousBroder || {};
   if (_elm.AldousBroder.values) return _elm.AldousBroder.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $PolarGrid = Elm.PolarGrid.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Trampoline = Elm.Trampoline.make(_elm);
   var _op = {};
   var walkRandomly = F4(function (grid,neighborsFn,cell,unvisited) {
      if (_U.eq(unvisited,0)) return $Trampoline.Done(grid); else {
            var grid$ = $Grid.updateRnd(grid);
            var sample = A2(neighborsFn,grid$,cell);
            var gcneighbor = $GridCell.maybeGridCellToGridCell(A2($GridUtils.sampleCell,sample,grid.rnd));
            var neighbor = $GridCell.base(gcneighbor);
            if ($Basics.not($Cell.hasLinks(neighbor))) {
                  var grid$$ = A4($Grid.linkCells,grid$,cell,gcneighbor,true);
                  return $Trampoline.Continue(function (_p0) {    var _p1 = _p0;return A4(walkRandomly,grid$$,neighborsFn,gcneighbor,unvisited - 1);});
               } else return $Trampoline.Continue(function (_p2) {    var _p3 = _p2;return A4(walkRandomly,grid$,neighborsFn,gcneighbor,unvisited);});
         }
   });
   var on = F3(function (startCellFn,neighborsFn,grid) {
      var startCell = $GridCell.maybeGridCellToGridCell(startCellFn(grid));
      var grid$ = $Grid.updateRnd(grid);
      var gridSize = function () {
         var _p4 = startCell;
         if (_p4.ctor === "PolarCellTag") {
               return $PolarGrid.size(grid$);
            } else {
               return $Grid.size(grid$);
            }
      }();
      return $Trampoline.trampoline(A4(walkRandomly,grid$,neighborsFn,startCell,gridSize - 1));
   });
   return _elm.AldousBroder.values = {_op: _op,on: on};
};
Elm.BinaryTree = Elm.BinaryTree || {};
Elm.BinaryTree.make = function (_elm) {
   "use strict";
   _elm.BinaryTree = _elm.BinaryTree || {};
   if (_elm.BinaryTree.values) return _elm.BinaryTree.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var on = F3(function (startCellFn,neighborsFn,grid) {
      var getRandomNeighbor = F2(function (grid$,cell) {
         var acell = $GridCell.base(cell);
         var gcneighbors = $GridUtils.smooshMaybes(_U.list([A2($Grid.north,grid$,acell),A2($Grid.east,grid$,acell)]));
         if ($List.isEmpty(gcneighbors)) return $Maybe.Nothing; else {
               var _p0 = A2($GridUtils.sampleCell,gcneighbors,grid$.rnd);
               if (_p0.ctor === "Nothing") {
                     return $Maybe.Nothing;
                  } else {
                     return $Maybe.Just(_p0._0);
                  }
            }
      });
      var processCell = F2(function (cell,grid) {
         var grid$ = $Grid.updateRnd(grid);
         var neighbor = A2(getRandomNeighbor,grid,cell);
         var _p1 = neighbor;
         if (_p1.ctor === "Nothing") {
               return grid$;
            } else {
               return A4($Grid.linkCells,grid$,cell,_p1._0,true);
            }
      });
      return A3($List.foldl,processCell,grid,$Grid.cellsList(grid.cells));
   });
   return _elm.BinaryTree.values = {_op: _op,on: on};
};
Elm.Distances = Elm.Distances || {};
Elm.Distances.make = function (_elm) {
   "use strict";
   _elm.Distances = _elm.Distances || {};
   if (_elm.Distances.values) return _elm.Distances.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var cells = function (dists) {    return $Dict.keys(dists.cells);};
   var max = function (dists) {
      var maxDist = $List.reverse(A2($List.sortBy,$Basics.snd,$Dict.toList(dists.cells)));
      var _p0 = $List.head(maxDist);
      if (_p0.ctor === "Just") {
            return _p0._0;
         } else {
            return {ctor: "_Tuple2",_0: dists.root.id,_1: 0};
         }
   };
   var add = F3(function (dists,cell,dist) {    return _U.update(dists,{cells: A3($Dict.insert,cell.id,dist,dists.cells)});});
   var lookup = F2(function (dists,cell) {    return A2($Maybe.withDefault,-1,A2($Dict.get,cell.id,dists.cells));});
   var init = function (cell) {    return {root: cell,cells: A2($Dict.singleton,cell.id,0)};};
   var Distances = F2(function (a,b) {    return {root: a,cells: b};});
   return _elm.Distances.values = {_op: _op,Distances: Distances,init: init,lookup: lookup,add: add,max: max,cells: cells};
};
Elm.Dijkstra = Elm.Dijkstra || {};
Elm.Dijkstra.make = function (_elm) {
   "use strict";
   _elm.Dijkstra = _elm.Dijkstra || {};
   if (_elm.Dijkstra.values) return _elm.Dijkstra.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Distances = Elm.Distances.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var cellDistances = F2(function (grid,cell) {
      var acc = {grid: grid,dists: $Distances.init(cell),frontier: _U.list([cell]),newFrontier: _U.list([])};
      var scanCell = F3(function (cell,linked,diter) {
         if ($Basics.not(_U.eq(A2($Distances.lookup,diter.dists,linked),-1))) return diter; else {
               var curDist = A2($Distances.lookup,diter.dists,cell);
               return _U.update(diter,
               {newFrontier: A2($List.append,diter.newFrontier,_U.list([linked])),dists: A3($Distances.add,diter.dists,linked,curDist + 1)});
            }
      });
      var scanCellLinks = F2(function (cell,diter) {
         return A3($List.foldl,scanCell(cell),diter,$Grid.gridCellsToBaseCells(A2($Grid.linkedCells,diter.grid,$GridCell.RectCellTag(cell))));
      });
      var scanFrontier = function (diter) {    var res = A3($List.foldl,scanCellLinks,diter,diter.frontier);return _U.update(res,{frontier: res.newFrontier});};
      var frontierAcc = function (diter) {
         frontierAcc: while (true) if ($List.isEmpty(diter.frontier)) return diter; else {
               var acc = _U.update(diter,{newFrontier: _U.list([])});
               var _v0 = scanFrontier(acc);
               diter = _v0;
               continue frontierAcc;
            }
      };
      return function (_) {
         return _.dists;
      }(frontierAcc(acc));
   });
   var DijkstraIter = F4(function (a,b,c,d) {    return {dists: a,grid: b,frontier: c,newFrontier: d};});
   return _elm.Dijkstra.values = {_op: _op,DijkstraIter: DijkstraIter,cellDistances: cellDistances};
};
Elm.IntToBaseX = Elm.IntToBaseX || {};
Elm.IntToBaseX.make = function (_elm) {
   "use strict";
   _elm.IntToBaseX = _elm.IntToBaseX || {};
   if (_elm.IntToBaseX.values) return _elm.IntToBaseX.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var digitMap = "0123456789abcdefghijklmnopqrstuvwxyz";
   var lookupDigitChar = function (n) {    return A3($String.slice,n,n + 1,digitMap);};
   var toBaseX = F2(function (num,base) {
      var convert = F2(function (str,v) {
         convert: while (true) if (_U.eq(v,0)) return str; else {
               var c = A2($Basics._op["++"],lookupDigitChar(A2($Basics._op["%"],v,base)),str);
               var _v0 = c,_v1 = v / base | 0;
               str = _v0;
               v = _v1;
               continue convert;
            }
      });
      if (_U.cmp(base,2) < 0 || _U.cmp(36,base) < 0) return A2($Basics._op["++"],"illegal radix ",$Basics.toString(base)); else if (_U.eq(num,0)) return "0";
         else {
               var num$ = _U.cmp(num,0) < 0 ? $Basics.negate(num) : num;
               var res = A2(convert,"",num$);
               return _U.cmp(num,0) < 0 ? A2($Basics._op["++"],"-",res) : res;
            }
   });
   return _elm.IntToBaseX.values = {_op: _op,digitMap: digitMap,lookupDigitChar: lookupDigitChar,toBaseX: toBaseX};
};
Elm.DistanceGrid = Elm.DistanceGrid || {};
Elm.DistanceGrid.make = function (_elm) {
   "use strict";
   _elm.DistanceGrid = _elm.DistanceGrid || {};
   if (_elm.DistanceGrid.values) return _elm.DistanceGrid.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dijkstra = Elm.Dijkstra.make(_elm),
   $Distances = Elm.Distances.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $IntToBaseX = Elm.IntToBaseX.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var cellToAscii = F2(function (dgrid,cell) {
      var dist = A2($Distances.lookup,dgrid.dists,cell);
      return _U.eq(dist,-1) ? A2($Grid.cellToAscii,dgrid,cell) : A2($IntToBaseX.toBaseX,dist,36);
   });
   var viewDistances = function (dgrid) {    return A2($Grid.toAscii,dgrid,cellToAscii);};
   var distances = F2(function (grid,root) {    return A2($Dijkstra.cellDistances,grid,root);});
   var createGrid = F2(function (grid,root) {    var cellDistances = A2(distances,grid,root);return _U.update(grid,{dists: cellDistances});});
   var pathTo = F3(function (grid,root,goal) {
      var current = goal;
      var dgrid = A2(createGrid,grid,root);
      var breadcrumbs = A3($Distances.add,$Distances.init(root),current,A2($Distances.lookup,dgrid.dists,current));
      var walkPath = F2(function (xpbreadcrumbs,xpcurrent) {
         walkPath: while (true) if (_U.eq(xpcurrent.id,root.id)) return xpbreadcrumbs; else {
               var currentDistance = A2($Distances.lookup,dgrid.dists,xpcurrent);
               var links = $Grid.gridCellsToBaseCells(A2($Grid.linkedCells,grid,$GridCell.RectCellTag(xpcurrent)));
               var res = A2($List.filter,function (neighbor) {    return _U.cmp(A2($Distances.lookup,dgrid.dists,neighbor),currentDistance) < 0;},links);
               if ($List.isEmpty(res)) return xpbreadcrumbs; else {
                     var neighbor = $Grid.toValidCell($List.head(res));
                     var ixpbreadcrumbs = A3($Distances.add,xpbreadcrumbs,neighbor,A2($Distances.lookup,dgrid.dists,neighbor));
                     var _v0 = ixpbreadcrumbs,_v1 = neighbor;
                     xpbreadcrumbs = _v0;
                     xpcurrent = _v1;
                     continue walkPath;
                  }
            }
      });
      return A2(walkPath,breadcrumbs,current);
   });
   var longestPath = F2(function (grid,root) {
      var dgrid = A2(createGrid,grid,root);
      var _p0 = $Distances.max(dgrid.dists);
      var cellId = _p0._0;
      var foo = _p0._1;
      var newStartCell = $GridCell.toRectCell(A2($Grid.cellIdToCell,grid,cellId));
      var dgrid$ = A2(createGrid,grid,newStartCell);
      var _p1 = $Distances.max(dgrid$.dists);
      var goalId = _p1._0;
      var foo$ = _p1._1;
      var goal = $GridCell.toRectCell(A2($Grid.cellIdToCell,grid,goalId));
      return A3(pathTo,grid,newStartCell,goal);
   });
   return _elm.DistanceGrid.values = {_op: _op
                                     ,createGrid: createGrid
                                     ,distances: distances
                                     ,cellToAscii: cellToAscii
                                     ,viewDistances: viewDistances
                                     ,pathTo: pathTo
                                     ,longestPath: longestPath};
};
Elm.ColoredGrid = Elm.ColoredGrid || {};
Elm.ColoredGrid.make = function (_elm) {
   "use strict";
   _elm.ColoredGrid = _elm.ColoredGrid || {};
   if (_elm.ColoredGrid.values) return _elm.ColoredGrid.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $DistanceGrid = Elm.DistanceGrid.make(_elm),
   $Distances = Elm.Distances.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var cellBackgroundColor = F2(function (grid,gridcell) {
      var cell = $GridCell.toRectCell(gridcell);
      var distance = A2($Distances.lookup,grid.dists,cell);
      var intensity = $Basics.toFloat(grid.maximum - distance) / $Basics.toFloat(grid.maximum);
      var dark = $Basics.round(255 * intensity);
      var bright = $Basics.round(128 + 127 * intensity);
      return _U.cmp(distance,0) < 0 ? A3($Color.rgb,255,255,255) : A3($Color.rgb,dark,bright,dark);
   });
   var createGrid = F2(function (grid,root) {
      var grid$ = A2($DistanceGrid.createGrid,grid,root);
      var _p0 = $Distances.max(grid$.dists);
      var farthest = _p0._0;
      var max = _p0._1;
      return _U.update(grid$,{maximum: max});
   });
   return _elm.ColoredGrid.values = {_op: _op,createGrid: createGrid,cellBackgroundColor: cellBackgroundColor};
};
Elm.HuntAndKill = Elm.HuntAndKill || {};
Elm.HuntAndKill.make = function (_elm) {
   "use strict";
   _elm.HuntAndKill = _elm.HuntAndKill || {};
   if (_elm.HuntAndKill.values) return _elm.HuntAndKill.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $List$Extra = Elm.List.Extra.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Trampoline = Elm.Trampoline.make(_elm);
   var _op = {};
   var hunt = F2(function (grid,neighborsFn) {
      var visitedNeighbors = function (cell) {
         return A4($Grid.filterNeighbors2,neighborsFn,function (c) {    return $Cell.hasLinks($GridCell.toRectCell(c));},grid,cell);
      };
      var huntUnvisitedNeighbor = function (gcell) {
         return $Basics.not($List.isEmpty(visitedNeighbors(gcell))) && $Basics.not($Cell.hasLinks($GridCell.toRectCell(gcell)));
      };
      var huntedCell = A2($List$Extra.find,huntUnvisitedNeighbor,$Grid.cellsList(grid.cells));
      var _p0 = huntedCell;
      if (_p0.ctor === "Nothing") {
            return {ctor: "_Tuple2",_0: grid,_1: $GridCell.RectCellTag($Cell.createNilCell)};
         } else {
            var _p1 = _p0._0;
            var linked = $GridCell.maybeGridCellToGridCell(A2($GridUtils.sampleCell,visitedNeighbors(_p1),grid.rnd));
            return {ctor: "_Tuple2",_0: A4($Grid.linkCells,$Grid.updateRnd(grid),_p1,linked,true),_1: _p1};
         }
   });
   var walkRandomly = F3(function (grid,gcell,neighborsFn) {
      var cell = $GridCell.toRectCell(gcell);
      if ($Cell.isNilCell(cell)) return $Trampoline.Done(grid); else {
            var unvisitedNeighbors = A4($Grid.filterNeighbors2,
            neighborsFn,
            function (c) {
               return $Basics.not($Cell.hasLinks($GridCell.toRectCell(c)));
            },
            grid,
            gcell);
            if ($Basics.not($List.isEmpty(unvisitedNeighbors))) {
                  var neighbor = $GridCell.maybeGridCellToGridCell(A2($GridUtils.sampleCell,unvisitedNeighbors,grid.rnd));
                  var grid$ = A4($Grid.linkCells,grid,gcell,neighbor,true);
                  var grid$$ = $Grid.updateRnd(grid$);
                  return $Trampoline.Continue(function (_p2) {    var _p3 = _p2;return A3(walkRandomly,grid$$,neighbor,neighborsFn);});
               } else {
                  var _p4 = A2(hunt,grid,neighborsFn);
                  var grid$ = _p4._0;
                  var current = _p4._1;
                  return $Trampoline.Continue(function (_p5) {    var _p6 = _p5;return A3(walkRandomly,grid$,current,neighborsFn);});
               }
         }
   });
   var on = F3(function (startCellFn,neighborsFn,grid) {
      var startCell = $GridCell.maybeGridCellToGridCell(startCellFn(grid));
      var grid$ = $Grid.updateRnd(grid);
      return $Trampoline.trampoline(A3(walkRandomly,grid$,startCell,neighborsFn));
   });
   return _elm.HuntAndKill.values = {_op: _op,on: on};
};
Elm.Sidewinder = Elm.Sidewinder || {};
Elm.Sidewinder.make = function (_elm) {
   "use strict";
   _elm.Sidewinder = _elm.Sidewinder || {};
   if (_elm.Sidewinder.values) return _elm.Sidewinder.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var on = F3(function (startCellFn,neighborsFn,grid) {
      var processCell = F2(function (cell,rowState) {
         var grid$ = $Grid.updateRnd(rowState.grid);
         var basecell = $GridCell.base(cell);
         var atEasternBoundary = $Basics.not($GridCell.isValidCell(A2($Grid.east,rowState.grid,basecell)));
         var atNorthernBoundary = $Basics.not($GridCell.isValidCell(A2($Grid.north,rowState.grid,basecell)));
         var shouldCloseOut = atEasternBoundary || $Basics.not(atNorthernBoundary) && grid$.rnd.heads;
         var run$ = A2($List._op["::"],cell,rowState.run);
         if (shouldCloseOut) {
               var grid$$ = $Grid.updateRnd(grid$);
               var member = $GridCell.maybeGridCellToCell(A2($GridUtils.sampleCell,run$,grid$.rnd));
               var northern = A2($Grid.north,grid$,member);
               return $GridCell.isValidCell(northern) ? {run: _U.list([])
                                                        ,grid: A4($Grid.linkCells,
                                                        grid$$,
                                                        $GridCell.RectCellTag(member),
                                                        $GridCell.maybeGridCellToGridCell(northern),
                                                        true)} : {run: _U.list([]),grid: grid$$};
            } else return _U.update(rowState,
            {run: run$,grid: A4($Grid.linkCells,grid$,cell,$GridCell.maybeGridCellToGridCell(A2($Grid.east,grid$,basecell)),true)});
      });
      var processRow = F2(function (row,curGrid) {
         var state = {run: _U.list([]),grid: curGrid};
         return function (_) {
            return _.grid;
         }(A3($List.foldl,processCell,state,A2($Grid.rowCells,curGrid,row)));
      });
      return A3($List.foldl,processRow,grid,$List.reverse(_U.range(0,grid.rows - 1)));
   });
   var RowState = F2(function (a,b) {    return {run: a,grid: b};});
   return _elm.Sidewinder.values = {_op: _op,RowState: RowState,on: on};
};
Elm.Wilsons = Elm.Wilsons || {};
Elm.Wilsons.make = function (_elm) {
   "use strict";
   _elm.Wilsons = _elm.Wilsons || {};
   if (_elm.Wilsons.values) return _elm.Wilsons.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Trampoline = Elm.Trampoline.make(_elm);
   var _op = {};
   var carvePassage = function (rwp) {
      var pathArr = $Array.fromList(rwp.path);
      var carve = F2(function (index,rwp) {
         var nextcell = $GridCell.maybeGridCellToGridCell(A2($Array.get,index + 1,pathArr));
         var icell = $GridCell.maybeGridCellToGridCell(A2($Array.get,index,pathArr));
         var grid$ = A4($Grid.linkCells,rwp.grid,icell,nextcell,true);
         var icellId = $GridCell.id(icell);
         var unvisited$ = A2($GridCell.filterGridCells,function (e) {    return $Basics.not(_U.eq(e.id,icellId));},rwp.unvisited);
         return _U.update(rwp,{grid: grid$,unvisited: unvisited$});
      });
      return A3($List.foldl,carve,rwp,_U.range(0,$List.length(rwp.path) - 2));
   };
   var loopErasedRandomWalk = F2(function (rwp,neighborsFn) {
      loopErasedRandomWalk: while (true) if ($Basics.not(A2($List.member,rwp.cell,rwp.unvisited))) return carvePassage(rwp); else {
            var grid = $Grid.updateRnd(rwp.grid);
            var gccell = $GridCell.maybeGridCellToGridCell(A2($GridUtils.sampleCell,A2(neighborsFn,rwp.grid,rwp.cell),rwp.grid.rnd));
            var position = A2($GridUtils.indexOfCell,gccell,rwp.path);
            if (_U.cmp(position,0) > -1) {
                  var _v0 = _U.update(rwp,{grid: grid,cell: gccell,path: A2($List.take,position + 1,rwp.path)}),_v1 = neighborsFn;
                  rwp = _v0;
                  neighborsFn = _v1;
                  continue loopErasedRandomWalk;
               } else {
                  var _v2 = _U.update(rwp,{grid: grid,cell: gccell,path: $List.concat(_U.list([rwp.path,_U.list([gccell])]))}),_v3 = neighborsFn;
                  rwp = _v2;
                  neighborsFn = _v3;
                  continue loopErasedRandomWalk;
               }
         }
   });
   var work = F3(function (grid,unvisited,neighborsFn) {
      if ($List.isEmpty(unvisited)) return $Trampoline.Done(grid); else {
            var cell = $GridCell.maybeGridCellToGridCell(A2($GridUtils.sampleCell,unvisited,grid.rnd));
            var rwp = A2(loopErasedRandomWalk,{grid: $Grid.updateRnd(grid),cell: cell,path: _U.list([cell]),unvisited: unvisited},neighborsFn);
            return $Trampoline.Continue(function (_p0) {    var _p1 = _p0;return A3(work,rwp.grid,rwp.unvisited,neighborsFn);});
         }
   });
   var on = F3(function (startCellFn,neighborsFn,grid) {
      var grid$ = $Grid.updateRnd(grid);
      var startCell = $GridCell.maybeGridCellToGridCell(startCellFn(grid));
      var unvisited = A2($GridCell.filterGridCells,function (e) {    return $Basics.not(_U.eq(e.id,$GridCell.id(startCell)));},$Grid.cellsList(grid.cells));
      return $Trampoline.trampoline(A3(work,grid$,unvisited,neighborsFn));
   });
   var RandomWalkPath = F4(function (a,b,c,d) {    return {grid: a,cell: b,path: c,unvisited: d};});
   return _elm.Wilsons.values = {_op: _op,on: on};
};
Elm.RecursiveBacktracker = Elm.RecursiveBacktracker || {};
Elm.RecursiveBacktracker.make = function (_elm) {
   "use strict";
   _elm.RecursiveBacktracker = _elm.RecursiveBacktracker || {};
   if (_elm.RecursiveBacktracker.values) return _elm.RecursiveBacktracker.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $GridUtils = Elm.GridUtils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Trampoline = Elm.Trampoline.make(_elm);
   var _op = {};
   var walkRandomly = F3(function (grid,neighborsFn,stack) {
      if ($List.isEmpty(stack)) return $Trampoline.Done(grid); else {
            var current = $GridCell.maybeGridCellToGridCell($List.head(stack));
            var neighbors = A4($Grid.filterNeighbors2,neighborsFn,function (c) {    return $Basics.not($Cell.hasLinks($GridCell.base(c)));},grid,current);
            if ($List.isEmpty(neighbors)) return $Trampoline.Continue(function (_p0) {
                  var _p1 = _p0;
                  return A3(walkRandomly,grid,neighborsFn,A2($Maybe.withDefault,_U.list([]),$List.tail(stack)));
               }); else {
                  var neighbor = $GridCell.maybeGridCellToGridCell(A2($GridUtils.sampleCell,neighbors,grid.rnd));
                  var grid$ = A4($Grid.linkCells,grid,current,neighbor,true);
                  var grid$$ = $Grid.updateRnd(grid$);
                  return $Trampoline.Continue(function (_p2) {
                     var _p3 = _p2;
                     return A3(walkRandomly,grid$$,neighborsFn,A2($List._op["::"],neighbor,stack));
                  });
               }
         }
   });
   var on = F3(function (startCellFn,neighborsFn,grid) {
      var gcell = $GridCell.maybeGridCellToGridCell(startCellFn(grid));
      var grid$ = $Grid.updateRnd(grid);
      return $Trampoline.trampoline(A3(walkRandomly,grid$,neighborsFn,_U.list([gcell])));
   });
   return _elm.RecursiveBacktracker.values = {_op: _op,on: on};
};
Elm.Maze = Elm.Maze || {};
Elm.Maze.make = function (_elm) {
   "use strict";
   _elm.Maze = _elm.Maze || {};
   if (_elm.Maze.values) return _elm.Maze.values;
   var _U = Elm.Native.Utils.make(_elm),
   $AldousBroder = Elm.AldousBroder.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $BinaryTree = Elm.BinaryTree.make(_elm),
   $ColoredGrid = Elm.ColoredGrid.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $DistanceGrid = Elm.DistanceGrid.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $GridCell = Elm.GridCell.make(_elm),
   $HexGrid = Elm.HexGrid.make(_elm),
   $Html = Elm.Html.make(_elm),
   $HuntAndKill = Elm.HuntAndKill.make(_elm),
   $List = Elm.List.make(_elm),
   $Mask = Elm.Mask.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $PolarGrid = Elm.PolarGrid.make(_elm),
   $RecursiveBacktracker = Elm.RecursiveBacktracker.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Sidewinder = Elm.Sidewinder.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $TriangleGrid = Elm.TriangleGrid.make(_elm),
   $Wilsons = Elm.Wilsons.make(_elm);
   var _op = {};
   var algToString = function (algType) {
      var _p0 = algType;
      switch (_p0.ctor)
      {case "NoOp": return "None";
         case "BinaryTree": return "Binary Tree";
         case "Sidewinder": return "Sidewinder";
         case "AldousBroder": return "Aldous-Broder";
         case "Wilsons": return "Wilsons";
         case "HuntAndKill": return "Hunt - Kill";
         default: return "Recursive Backtracker";}
   };
   var viewDistances = function (maze) {
      var root = $Grid.center(maze.grid);
      var dgrid = A2($DistanceGrid.createGrid,maze.grid,root);
      return A2($Html.div,
      _U.list([]),
      _U.list([A2($Html.br,_U.list([]),_U.list([])),A2($Html.pre,_U.list([]),_U.list([$Html.text($DistanceGrid.viewDistances(dgrid))]))]));
   };
   var setMask = F2(function (maze,mask) {
      var grid$ = A3($Grid.createGridFromMask,mask,maze.grid.rnd.seed,maze.grid.cellMaker);
      return _U.update(maze,{grid: maze.generator(grid$)});
   });
   var update = function (maze) {    var grid$ = $Grid.update(maze.grid);return _U.update(maze,{grid: maze.generator(grid$)});};
   var genAlg = F2(function (algName,shape) {
      var neighborFn = function () {
         var _p1 = shape;
         switch (_p1.ctor)
         {case "Polar": return $PolarGrid.neighbors;
            case "Hex": return $HexGrid.neighbors;
            case "Triangle": return $TriangleGrid.neighbors;
            default: return $Grid.neighbors;}
      }();
      var randCellFn = function () {    var _p2 = shape;if (_p2.ctor === "Polar") {    return $PolarGrid.randomCell;} else {    return $Grid.randomCell;}}();
      var _p3 = algName;
      switch (_p3.ctor)
      {case "NoOp": return $Basics.identity;
         case "BinaryTree": return A2($BinaryTree.on,randCellFn,neighborFn);
         case "Sidewinder": return A2($Sidewinder.on,randCellFn,neighborFn);
         case "AldousBroder": return A2($AldousBroder.on,randCellFn,neighborFn);
         case "Wilsons": return A2($Wilsons.on,randCellFn,neighborFn);
         case "HuntAndKill": return A2($HuntAndKill.on,randCellFn,neighborFn);
         default: return A2($RecursiveBacktracker.on,randCellFn,neighborFn);}
   });
   var init = F6(function (algType,width,height,seed,shape,display) {
      var cellGenFn = function () {
         var _p4 = shape;
         switch (_p4.ctor)
         {case "Rect": return $Grid.makeCells;
            case "Polar": return $PolarGrid.makeCells;
            case "Hex": return $HexGrid.makeCells;
            default: return $TriangleGrid.makeCells;}
      }();
      var mask = A2($Mask.createMask,width,height);
      var grid$ = A3($Grid.createGridFromMask,mask,seed,cellGenFn);
      return {grid: grid$,generator: A2(genAlg,algType,shape),alg: algType,shape: shape,display: display};
   });
   var updateSize = F3(function (maze,width,height) {    return A6(init,maze.alg,width,height,maze.grid.rnd.seed,maze.shape,maze.display);});
   var updateView = F2(function (maze,displayType) {    return A6(init,maze.alg,maze.grid.cols,maze.grid.rows,maze.grid.rnd.seed,maze.shape,displayType);});
   var cellSize = 30;
   var view = function (maze) {
      var gridHtml = function () {
         var _p5 = maze.display;
         if (_p5.ctor === "Ascii") {
               return A2($Html.pre,_U.list([]),_U.list([$Html.text(A2($Grid.toAscii,maze.grid,$Grid.cellToAscii))]));
            } else {
               var _p6 = maze.shape;
               switch (_p6.ctor)
               {case "Rect": var root = $Grid.center(maze.grid);
                    var coloredGrid = A2($ColoredGrid.createGrid,maze.grid,root);
                    return $Html.fromElement(A4($Grid.toElement,coloredGrid,$Grid.painter,$ColoredGrid.cellBackgroundColor,cellSize));
                  case "Polar": var _p7 = $GridCell.toPolarCell($PolarGrid.center(maze.grid));
                    var root = _p7._0;
                    var coloredGrid = A2($ColoredGrid.createGrid,maze.grid,root);
                    return $Html.fromElement(A4($Grid.toElement,coloredGrid,$PolarGrid.painter,$ColoredGrid.cellBackgroundColor,cellSize));
                  case "Hex": var root = $Grid.center(maze.grid);
                    var coloredGrid = A2($ColoredGrid.createGrid,maze.grid,root);
                    return $Html.fromElement(A4($Grid.toElement,coloredGrid,$HexGrid.painter,$ColoredGrid.cellBackgroundColor,cellSize));
                  default: return $Html.text("TRIANGLE MAZE HERE");}
            }
      }();
      return A2($Html.div,
      _U.list([]),
      _U.list([$Html.text(A2($Basics._op["++"],algToString(maze.alg)," algorithm"))
              ,A2($Html.br,_U.list([]),_U.list([]))
              ,$Html.text(A2($Basics._op["++"],$Basics.toString(maze.grid.cols),A2($Basics._op["++"]," X ",$Basics.toString(maze.grid.rows))))
              ,A2($Html.br,_U.list([]),_U.list([]))
              ,$Html.text(A2($Basics._op["++"],$Basics.toString($List.length($Grid.deadEnds(maze.grid)))," deadends"))
              ,gridHtml
              ,A2($Html.br,_U.list([]),_U.list([]))]));
   };
   var Maze = F5(function (a,b,c,d,e) {    return {grid: a,alg: b,shape: c,display: d,generator: e};});
   var Triangle = {ctor: "Triangle"};
   var Hex = {ctor: "Hex"};
   var Polar = {ctor: "Polar"};
   var Rect = {ctor: "Rect"};
   var shapes = _U.list([{ctor: "_Tuple2",_0: Rect,_1: "Rectangular"}
                        ,{ctor: "_Tuple2",_0: Polar,_1: "Polar"}
                        ,{ctor: "_Tuple2",_0: Hex,_1: "Hexagonal"}
                        ,{ctor: "_Tuple2",_0: Triangle,_1: "Triangle"}]);
   var Colored = {ctor: "Colored"};
   var Ascii = {ctor: "Ascii"};
   var displays = _U.list([{ctor: "_Tuple2",_0: Ascii,_1: "ASCII"},{ctor: "_Tuple2",_0: Colored,_1: "Colored"}]);
   var AlgAttr = F2(function (a,b) {    return {alg: a,name: b};});
   var RecursiveBacktracker = {ctor: "RecursiveBacktracker"};
   var HuntAndKill = {ctor: "HuntAndKill"};
   var Wilsons = {ctor: "Wilsons"};
   var AldousBroder = {ctor: "AldousBroder"};
   var Sidewinder = {ctor: "Sidewinder"};
   var BinaryTree = {ctor: "BinaryTree"};
   var NoOp = {ctor: "NoOp"};
   var defaultAlgorithm = NoOp;
   var algorithms = function (shape) {
      var allAlgs = _U.list([{alg: AldousBroder,name: algToString(AldousBroder)}
                            ,{alg: Wilsons,name: algToString(Wilsons)}
                            ,{alg: RecursiveBacktracker,name: algToString(RecursiveBacktracker)}]);
      var rectAlgs = _U.list([{alg: BinaryTree,name: algToString(BinaryTree)}
                             ,{alg: Sidewinder,name: algToString(Sidewinder)}
                             ,{alg: HuntAndKill,name: algToString(HuntAndKill)}]);
      var algs = _U.list([{alg: NoOp,name: algToString(NoOp)}]);
      var _p8 = shape;
      if (_p8.ctor === "Polar") {
            return $List.concat(_U.list([algs,allAlgs]));
         } else {
            return $List.concat(_U.list([algs,rectAlgs,allAlgs]));
         }
   };
   var algByName = function (str) {
      var res = $List.head(A2($List.filter,function (a) {    return _U.eq(a.name,str);},algorithms(Rect)));
      var _p9 = res;
      if (_p9.ctor === "Just") {
            return _p9._0.alg;
         } else {
            return A2(_U.crash("Maze",{start: {line: 233,column: 17},end: {line: 233,column: 28}}),"Unknown algorithm",BinaryTree);
         }
   };
   return _elm.Maze.values = {_op: _op
                             ,NoOp: NoOp
                             ,BinaryTree: BinaryTree
                             ,Sidewinder: Sidewinder
                             ,AldousBroder: AldousBroder
                             ,Wilsons: Wilsons
                             ,HuntAndKill: HuntAndKill
                             ,RecursiveBacktracker: RecursiveBacktracker
                             ,AlgAttr: AlgAttr
                             ,Ascii: Ascii
                             ,Colored: Colored
                             ,Rect: Rect
                             ,Polar: Polar
                             ,Hex: Hex
                             ,Triangle: Triangle
                             ,Maze: Maze
                             ,defaultAlgorithm: defaultAlgorithm
                             ,displays: displays
                             ,shapes: shapes
                             ,cellSize: cellSize
                             ,init: init
                             ,genAlg: genAlg
                             ,update: update
                             ,updateSize: updateSize
                             ,updateView: updateView
                             ,setMask: setMask
                             ,view: view
                             ,viewDistances: viewDistances
                             ,algorithms: algorithms
                             ,algToString: algToString
                             ,algByName: algByName};
};
Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $List = Elm.List.make(_elm),
   $Mask = Elm.Mask.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Maze = Elm.Maze.make(_elm),
   $Random$PCG = Elm.Random.PCG.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Time = Elm.Time.make(_elm);
   var _op = {};
   var openFromPNGFile = Elm.Native.Port.make(_elm).inboundSignal("openFromPNGFile",
   "Main.PngData",
   function (v) {
      return typeof v === "object" && "width" in v && "height" in v && "blackFlags" in v ? {_: {}
                                                                                           ,width: typeof v.width === "number" && isFinite(v.width) && Math.floor(v.width) === v.width ? v.width : _U.badPort("an integer",
                                                                                           v.width)
                                                                                           ,height: typeof v.height === "number" && isFinite(v.height) && Math.floor(v.height) === v.height ? v.height : _U.badPort("an integer",
                                                                                           v.height)
                                                                                           ,blackFlags: typeof v.blackFlags === "object" && v.blackFlags instanceof Array ? Elm.Native.Array.make(_elm).fromJSArray(v.blackFlags.map(function (v) {
                                                                                              return typeof v === "boolean" ? v : _U.badPort("a boolean (true or false)",
                                                                                              v);
                                                                                           })) : _U.badPort("an array",
                                                                                           v.blackFlags)} : _U.badPort("an object with fields `width`, `height`, `blackFlags`",
      v);
   });
   var openFromTextFile = Elm.Native.Port.make(_elm).inboundSignal("openFromTextFile",
   "String",
   function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _U.badPort("a string",v);
   });
   var outputFromFilePNG = Elm.Native.Port.make(_elm).outboundSignal("outputFromFilePNG",
   function (v) {
      return {width: v.width,height: v.height,blackFlags: Elm.Native.Array.make(_elm).toJSArray(v.blackFlags).map(function (v) {    return v;})};
   },
   openFromPNGFile);
   var outputFromFileAscii = Elm.Native.Port.make(_elm).outboundSignal("outputFromFileAscii",
   function (v) {
      return Elm.Native.List.make(_elm).toArray(v).map(function (v) {    return v;});
   },
   A2($Signal.map,$String.lines,openFromTextFile));
   var startTime = Elm.Native.Port.make(_elm).inbound("startTime","Float",function (v) {    return typeof v === "number" ? v : _U.badPort("a number",v);});
   var startTimeSeed = $Random$PCG.initialSeed($Basics.round(startTime));
   var displayFromString = function (str) {    return _U.eq(str,"ASCII") ? $Maze.Ascii : $Maze.Colored;};
   var update = F2(function (action,model) {
      var _p0 = action;
      switch (_p0.ctor)
      {case "NoOp": return model;
         case "Refresh": return _U.update(model,{maze: $Maze.update(model.maze)});
         case "UpdateWidth": var maze$ = A3($Maze.updateSize,
           model.maze,
           A2($Maybe.withDefault,model.maze.grid.cols,$Result.toMaybe($String.toInt(_p0._0))),
           model.maze.grid.rows);
           return _U.update(model,{maze: maze$});
         case "UpdateHeight": var maze$ = A3($Maze.updateSize,
           model.maze,
           model.maze.grid.cols,
           A2($Maybe.withDefault,model.maze.grid.rows,$Result.toMaybe($String.toInt(_p0._0))));
           return _U.update(model,{maze: maze$});
         case "SelectAlg": var maze = model.maze;
           var maze$ = A6($Maze.init,$Maze.algByName(_p0._0),maze.grid.cols,maze.grid.rows,maze.grid.rnd.seed,maze.shape,maze.display);
           return _U.update(model,{maze: $Maze.update(maze$)});
         case "SelectView": var maze$ = A2($Maze.updateView,model.maze,_p0._0);
           return _U.update(model,{maze: maze$});
         case "SelectShape": var maze = model.maze;
           var maze$ = A6($Maze.init,maze.alg,maze.grid.cols,maze.grid.rows,maze.grid.rnd.seed,_p0._0,maze.display);
           return _U.update(model,{maze: maze$});
         case "LoadAsciiMask": var mask = $Mask.fromTxt(_p0._0);
           return _U.update(model,{maze: A2($Maze.setMask,model.maze,mask)});
         case "LoadImageMask": var _p1 = _p0._0;
           var mask = A2($Mask.fromImage,{ctor: "_Tuple2",_0: _p1.width,_1: _p1.height},_p1.blackFlags);
           return _U.update(model,{maze: A2($Maze.setMask,model.maze,mask)});
         default: return model;}
   });
   var LoadImageMask = function (a) {    return {ctor: "LoadImageMask",_0: a};};
   var LoadAsciiMask = function (a) {    return {ctor: "LoadAsciiMask",_0: a};};
   var SelectShape = function (a) {    return {ctor: "SelectShape",_0: a};};
   var SelectView = function (a) {    return {ctor: "SelectView",_0: a};};
   var SelectAlg = function (a) {    return {ctor: "SelectAlg",_0: a};};
   var UpdateHeight = function (a) {    return {ctor: "UpdateHeight",_0: a};};
   var UpdateWidth = function (a) {    return {ctor: "UpdateWidth",_0: a};};
   var Refresh = {ctor: "Refresh"};
   var Tick = function (a) {    return {ctor: "Tick",_0: a};};
   var tick = A2($Signal.map,function (dt) {    return Tick(dt);},$Time.fps(16));
   var NoOp = {ctor: "NoOp"};
   var actions = $Signal.mailbox(NoOp);
   var userInput = $Signal.mergeMany(_U.list([A2($Signal.map,LoadAsciiMask,outputFromFileAscii)
                                             ,A2($Signal.map,LoadImageMask,outputFromFilePNG)
                                             ,actions.signal
                                             ,tick]));
   var PngData = F3(function (a,b,c) {    return {width: a,height: b,blackFlags: c};});
   var AppState = F3(function (a,b,c) {    return {maze: a,seedInitialized: b,seed: c};});
   var initShape = $Maze.Rect;
   var shapeFromString = function (str) {
      var s = A2($List.filter,function (e) {    return _U.eq($Basics.snd(e),str);},$Maze.shapes);
      var _p2 = $List.head(s);
      if (_p2.ctor === "Nothing") {
            return initShape;
         } else {
            return $Basics.fst(_p2._0);
         }
   };
   var view = F2(function (address,model) {
      var maze = model.maze;
      var shapeToOption = function (opt) {
         return A2($Html.option,_U.list([$Html$Attributes.selected(_U.eq($Basics.fst(opt),model.maze.shape))]),_U.list([$Html.text($Basics.snd(opt))]));
      };
      var viewToOption = function (opt) {
         return A2($Html.option,_U.list([$Html$Attributes.selected(_U.eq($Basics.fst(opt),model.maze.display))]),_U.list([$Html.text($Basics.snd(opt))]));
      };
      var algToOptions = function (attr) {
         return A2($Html.option,_U.list([$Html$Attributes.selected(_U.eq(attr.alg,model.maze.alg))]),_U.list([$Html.text(attr.name)]));
      };
      var selectShape = A3($Html$Events.on,
      "change",
      $Html$Events.targetValue,
      function (val) {
         return A2($Signal.message,address,SelectShape(shapeFromString(val)));
      });
      var selectView = A3($Html$Events.on,
      "change",
      $Html$Events.targetValue,
      function (val) {
         return A2($Signal.message,address,SelectView(displayFromString(val)));
      });
      var selectAlg = A3($Html$Events.on,"change",$Html$Events.targetValue,function (val) {    return A2($Signal.message,address,SelectAlg(val));});
      return A2($Html.div,
      _U.list([$Html$Attributes.id("main")]),
      _U.list([A2($Html.header,_U.list([]),_U.list([A2($Html.h1,_U.list([]),_U.list([$Html.text("Amaze Mazes")]))]))
              ,$Maze.view(maze)
              ,$Html.text("width X height")
              ,A2($Html.br,_U.list([]),_U.list([]))
              ,A2($Html.input,
              _U.list([$Html$Attributes.$class("sizeInput")
                      ,$Html$Attributes.value($Basics.toString(maze.grid.cols))
                      ,A3($Html$Events.on,"input",$Html$Events.targetValue,function (_p3) {    return A2($Signal.message,address,UpdateWidth(_p3));})]),
              _U.list([]))
              ,$Html.text(" X ")
              ,A2($Html.input,
              _U.list([$Html$Attributes.$class("sizeInput")
                      ,$Html$Attributes.value($Basics.toString(maze.grid.rows))
                      ,A3($Html$Events.on,"input",$Html$Events.targetValue,function (_p4) {    return A2($Signal.message,address,UpdateHeight(_p4));})]),
              _U.list([]))
              ,A2($Html.br,_U.list([]),_U.list([]))
              ,A2($Html.select,_U.list([selectAlg]),A2($List.map,algToOptions,$Maze.algorithms(maze.shape)))
              ,A2($Html.select,_U.list([selectView]),A2($List.map,viewToOption,$Maze.displays))
              ,A2($Html.select,_U.list([selectShape]),A2($List.map,shapeToOption,$Maze.shapes))
              ,A2($Html.button,_U.list([A2($Html$Events.onClick,address,Refresh)]),_U.list([$Html.text("REFRESH")]))
              ,A2($Html.br,_U.list([]),_U.list([]))
              ,$Html.text("Ascii Mask file: ")
              ,A2($Html.input,_U.list([$Html$Attributes.type$("file"),$Html$Attributes.id("maskfileinput")]),_U.list([]))
              ,A2($Html.footer,_U.list([]),_U.list([]))]));
   });
   var initDisplay = $Maze.Colored;
   var initHeight = 8;
   var initWidth = 8;
   var initialModel = {maze: A6($Maze.init,$Maze.defaultAlgorithm,initWidth,initHeight,startTimeSeed,initShape,initDisplay)
                      ,seedInitialized: false
                      ,seed: $Random$PCG.initialSeed(45)};
   var model = A3($Signal.foldp,update,initialModel,userInput);
   var main = A2($Signal.map,view(actions.address),model);
   return _elm.Main.values = {_op: _op
                             ,initWidth: initWidth
                             ,initHeight: initHeight
                             ,initDisplay: initDisplay
                             ,initShape: initShape
                             ,AppState: AppState
                             ,PngData: PngData
                             ,NoOp: NoOp
                             ,Tick: Tick
                             ,Refresh: Refresh
                             ,UpdateWidth: UpdateWidth
                             ,UpdateHeight: UpdateHeight
                             ,SelectAlg: SelectAlg
                             ,SelectView: SelectView
                             ,SelectShape: SelectShape
                             ,LoadAsciiMask: LoadAsciiMask
                             ,LoadImageMask: LoadImageMask
                             ,update: update
                             ,view: view
                             ,displayFromString: displayFromString
                             ,shapeFromString: shapeFromString
                             ,main: main
                             ,userInput: userInput
                             ,model: model
                             ,initialModel: initialModel
                             ,actions: actions
                             ,tick: tick
                             ,startTimeSeed: startTimeSeed};
};
