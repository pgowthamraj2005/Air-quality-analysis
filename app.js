document.querySelectorAll('.bar-chart i').forEach((bar) => {
  const height = bar.style.height;
  bar.style.height = '0';
  requestAnimationFrame(() => { bar.style.height = height; });
});
