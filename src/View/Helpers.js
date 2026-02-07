export const parseMarkdownImpl = (md) => {
  if (typeof marked !== "undefined" && marked.parse) {
    return marked.parse(md);
  }
  return md;
};
