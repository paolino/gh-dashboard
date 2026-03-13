var _terminals = {};

export const attachTerminal = (elemId) => (wsUrl) => () => {
  // Clean up existing terminal for this ID
  if (_terminals[elemId]) {
    try { _terminals[elemId].ws.close(); } catch (_) {}
    try { _terminals[elemId].term.dispose(); } catch (_) {}
    delete _terminals[elemId];
  }

  // Defer to next frame so the DOM element exists after Halogen render
  requestAnimationFrame(() => {
    var container = document.getElementById(elemId);
    if (!container) return;

    var term = new Terminal({
      cursorBlink: true,
      fontSize: 13,
      theme: {
        background: '#1a1a2e',
        foreground: '#e0e0e0',
        cursor: '#e94560'
      }
    });
    var fitAddon = new FitAddon.FitAddon();
    term.loadAddon(fitAddon);
    term.open(container);
    fitAddon.fit();

    var ws = new WebSocket(wsUrl);
    ws.binaryType = 'arraybuffer';

    ws.onopen = () => {
      term.focus();
      var msg = '\x01' + term.cols + ';' + term.rows;
      ws.send(new TextEncoder().encode(msg));
    };

    ws.onmessage = (ev) => {
      if (ev.data instanceof ArrayBuffer) {
        term.write(new Uint8Array(ev.data));
      } else {
        term.write(ev.data);
      }
    };

    ws.onclose = () => {
      term.write('\r\n\x1b[90m[disconnected]\x1b[0m\r\n');
    };

    term.onData((data) => {
      if (ws.readyState === WebSocket.OPEN) {
        ws.send(new TextEncoder().encode(data));
      }
    });

    term.onResize(() => {
      if (ws.readyState === WebSocket.OPEN) {
        var msg = '\x01' + term.cols + ';' + term.rows;
        ws.send(new TextEncoder().encode(msg));
      }
    });

    // Re-fit on window resize
    var resizeHandler = () => fitAddon.fit();
    window.addEventListener('resize', resizeHandler);

    _terminals[elemId] = { term, ws, fitAddon, resizeHandler };
  });
};

export const destroyTerminal = (elemId) => () => {
  if (_terminals[elemId]) {
    try { _terminals[elemId].ws.close(); } catch (_) {}
    try {
      window.removeEventListener('resize', _terminals[elemId].resizeHandler);
    } catch (_) {}
    try { _terminals[elemId].term.dispose(); } catch (_) {}
    delete _terminals[elemId];
  }
};
