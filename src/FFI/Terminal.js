var _terminals = {};

export const attachTerminal = (elemId) => (launchKey) => (wsUrl) => () => {
  // If terminal is still attached to the same DOM
  // element, skip (avoid reconnect flicker)
  if (_terminals[elemId]) {
    var existing = document.getElementById(elemId);
    if (existing && existing === _terminals[elemId].container) {
      return;
    }
  }
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
        background: '#0f1525',
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

    var resizeHandler = () => fitAddon.fit();
    window.addEventListener('resize', resizeHandler);

    // Drag-to-resize handle
    var handle = document.querySelector(
      '[data-terminal="' + elemId + '"]'
    );
    if (handle) {
      handle.addEventListener('mousedown', (e) => {
        e.preventDefault();
        var startY = e.clientY;
        var startH = container.offsetHeight;
        var onMove = (ev) => {
          var newH = Math.max(150, startH + ev.clientY - startY);
          container.style.height = newH + 'px';
          fitAddon.fit();
        };
        var onUp = () => {
          document.removeEventListener('mousemove', onMove);
          document.removeEventListener('mouseup', onUp);
          fitAddon.fit();
        };
        document.addEventListener('mousemove', onMove);
        document.addEventListener('mouseup', onUp);
      });
    }

    _terminals[elemId] = { term, ws, fitAddon, resizeHandler, launchKey, container };
  });
};

// Destroy all terminals whose container element
// is no longer in the DOM, after next animation frame.
// Returns a promise of the launch keys cleaned up.
export const destroyOrphanedTerminals = () => {
  var orphanedKeys = [];
  var orphanedIds = [];
  for (var id in _terminals) {
    if (!document.getElementById(id)) {
      orphanedKeys.push(_terminals[id].launchKey);
      orphanedIds.push(id);
    }
  }
  for (var i = 0; i < orphanedIds.length; i++) {
    var oid = orphanedIds[i];
    try { _terminals[oid].ws.close(); } catch (_) {}
    try {
      window.removeEventListener(
        'resize', _terminals[oid].resizeHandler
      );
    } catch (_) {}
    try { _terminals[oid].term.dispose(); } catch (_) {}
    delete _terminals[oid];
  }
  return orphanedKeys;
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
