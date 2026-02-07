const KEYS = [
  "gh-dashboard-repos",
  "gh-dashboard-hidden",
  "gh-dashboard-dark-theme",
  "gh-dashboard-issue-labels",
  "gh-dashboard-pr-labels",
];

export const exportStorage = () => {
  const data = {};
  for (const key of KEYS) {
    const val = localStorage.getItem(key);
    if (val !== null) data[key] = val;
  }
  const blob = new Blob([JSON.stringify(data, null, 2)], {
    type: "application/json",
  });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = "gh-dashboard-settings.json";
  a.click();
  URL.revokeObjectURL(url);
};

export const importStorage = () => {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = ".json,application/json";
  input.addEventListener("change", () => {
    const file = input.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = () => {
      try {
        const data = JSON.parse(reader.result);
        for (const key of KEYS) {
          if (key in data) {
            localStorage.setItem(key, data[key]);
          }
        }
        location.reload();
      } catch (_) {
        alert("Invalid settings file");
      }
    };
    reader.readAsText(file);
  });
  input.click();
};
