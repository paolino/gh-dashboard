// --- Crypto helpers (AES-256-GCM) ---

const te = new TextEncoder();
const td = new TextDecoder();

function toBase64(buf) {
  return btoa(String.fromCharCode(...new Uint8Array(buf)));
}

function fromBase64(str) {
  return Uint8Array.from(atob(str), (c) => c.charCodeAt(0));
}

// Generate or load a random AES key for at-rest encryption.
async function getLocalKey() {
  const stored = localStorage.getItem(
    "gh-dashboard-crypto-key"
  );
  if (stored) {
    return crypto.subtle.importKey(
      "jwk",
      JSON.parse(stored),
      { name: "AES-GCM" },
      true,
      ["encrypt", "decrypt"]
    );
  }
  const key = await crypto.subtle.generateKey(
    { name: "AES-GCM", length: 256 },
    true,
    ["encrypt", "decrypt"]
  );
  const jwk = await crypto.subtle.exportKey("jwk", key);
  localStorage.setItem(
    "gh-dashboard-crypto-key",
    JSON.stringify(jwk)
  );
  return key;
}

// Encrypt with the local random key (at-rest).
// Returns base64(iv12 + ciphertext).
async function encryptLocal(plaintext) {
  const key = await getLocalKey();
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const ct = await crypto.subtle.encrypt(
    { name: "AES-GCM", iv },
    key,
    te.encode(plaintext)
  );
  const buf = new Uint8Array(iv.length + ct.byteLength);
  buf.set(iv, 0);
  buf.set(new Uint8Array(ct), iv.length);
  return toBase64(buf);
}

// Decrypt with the local random key (at-rest).
async function decryptLocal(blob) {
  const key = await getLocalKey();
  const data = fromBase64(blob);
  const iv = data.slice(0, 12);
  const ct = data.slice(12);
  const pt = await crypto.subtle.decrypt(
    { name: "AES-GCM", iv },
    key,
    ct
  );
  return td.decode(pt);
}

// Derive an AES key from a passphrase via PBKDF2.
async function deriveKey(passphrase, salt) {
  const km = await crypto.subtle.importKey(
    "raw",
    te.encode(passphrase),
    "PBKDF2",
    false,
    ["deriveKey"]
  );
  return crypto.subtle.deriveKey(
    {
      name: "PBKDF2",
      salt,
      iterations: 100000,
      hash: "SHA-256",
    },
    km,
    { name: "AES-GCM", length: 256 },
    false,
    ["encrypt", "decrypt"]
  );
}

// Encrypt with a user passphrase (for export).
// Returns base64(salt16 + iv12 + ciphertext).
async function encryptPassphrase(plaintext, passphrase) {
  const salt = crypto.getRandomValues(new Uint8Array(16));
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const key = await deriveKey(passphrase, salt);
  const ct = await crypto.subtle.encrypt(
    { name: "AES-GCM", iv },
    key,
    te.encode(plaintext)
  );
  const buf = new Uint8Array(
    salt.length + iv.length + ct.byteLength
  );
  buf.set(salt, 0);
  buf.set(iv, salt.length);
  buf.set(new Uint8Array(ct), salt.length + iv.length);
  return toBase64(buf);
}

// Decrypt with a user passphrase (for import).
async function decryptPassphrase(blob, passphrase) {
  const data = fromBase64(blob);
  const salt = data.slice(0, 16);
  const iv = data.slice(16, 28);
  const ct = data.slice(28);
  const key = await deriveKey(passphrase, salt);
  const pt = await crypto.subtle.decrypt(
    { name: "AES-GCM", iv },
    key,
    ct
  );
  return td.decode(pt);
}

// --- Token storage (encrypted at rest) ---

export const saveTokenEncrypted = (token) => () =>
  encryptLocal(token).then((enc) =>
    localStorage.setItem("gh-dashboard-token", enc)
  );

export const loadTokenEncrypted = () => {
  const raw = localStorage.getItem("gh-dashboard-token");
  if (!raw) return Promise.resolve("");
  return decryptLocal(raw).catch(() => {
    // Migration: plaintext token from before encryption.
    // Re-encrypt and store, return the original value.
    return encryptLocal(raw).then((enc) => {
      localStorage.setItem("gh-dashboard-token", enc);
      return raw;
    });
  });
};

// --- Settings keys (non-token) ---

const KEYS = [
  "gh-dashboard-repos",
  "gh-dashboard-hidden",
  "gh-dashboard-dark-theme",
  "gh-dashboard-issue-labels",
  "gh-dashboard-pr-labels",
  "gh-dashboard-view",
];

// --- Export / Import ---

export const exportStorage = () => {
  (async () => {
    const data = {};
    for (const key of KEYS) {
      const val = localStorage.getItem(key);
      if (val !== null) data[key] = val;
    }
    // Include token encrypted with a passphrase
    const rawToken = localStorage.getItem(
      "gh-dashboard-token"
    );
    if (rawToken) {
      let token;
      try {
        token = await decryptLocal(rawToken);
      } catch {
        // Pre-encryption plaintext token
        token = rawToken;
      }
      const passphrase = prompt(
        "Enter a passphrase to protect your token in the export file:"
      );
      if (passphrase) {
        data["gh-dashboard-token"] =
          await encryptPassphrase(token, passphrase);
      }
    }
    const blob = new Blob(
      [JSON.stringify(data, null, 2)],
      { type: "application/json" }
    );
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = "gh-dashboard-settings.json";
    a.click();
    URL.revokeObjectURL(url);
  })();
};

export const importStorage = () => {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = ".json,application/json";
  input.addEventListener("change", async () => {
    const file = input.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = async () => {
      try {
        const data = JSON.parse(reader.result);
        for (const key of KEYS) {
          if (key in data) {
            localStorage.setItem(key, data[key]);
          }
        }
        // Decrypt token with passphrase, re-encrypt for
        // local storage
        if ("gh-dashboard-token" in data) {
          const passphrase = prompt(
            "Enter the passphrase to decrypt your token:"
          );
          if (passphrase) {
            try {
              const token = await decryptPassphrase(
                data["gh-dashboard-token"],
                passphrase
              );
              const enc = await encryptLocal(token);
              localStorage.setItem(
                "gh-dashboard-token",
                enc
              );
            } catch {
              alert(
                "Wrong passphrase or corrupted token data"
              );
            }
          }
        }
        location.reload();
      } catch {
        alert("Invalid settings file");
      }
    };
    reader.readAsText(file);
  });
  input.click();
};
