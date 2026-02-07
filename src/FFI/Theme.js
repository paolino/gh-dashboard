export const setBodyTheme = (dark) => () => {
  document.body.classList.toggle("light-theme", !dark);
};
