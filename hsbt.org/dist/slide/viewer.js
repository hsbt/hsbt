import * as pdfjsLib from "https://cdn.jsdelivr.net/npm/pdfjs-dist@6.5.438/build/pdf.min.mjs";

const PDFJS = "https://cdn.jsdelivr.net/npm/pdfjs-dist@6.5.438/";
pdfjsLib.GlobalWorkerOptions.workerSrc = `${PDFJS}build/pdf.worker.min.mjs`;

const viewer = document.querySelector(".viewer");
const stage = viewer.querySelector(".stage");
const counter = viewer.querySelector(".counter");
const prev = viewer.querySelector(".prev");
const next = viewer.querySelector(".next");

const doc = await pdfjsLib.getDocument({
  url: viewer.dataset.pdf,
  cMapUrl: `${PDFJS}cmaps/`,
  standardFontDataUrl: `${PDFJS}standard_fonts/`,
  wasmUrl: `${PDFJS}wasm/`,
  iccUrl: `${PDFJS}iccs/`,
}).promise;

let current = 0;
let task = null;

function pageFromHash() {
  return parseInt(location.hash.slice(1), 10) || 1;
}

// The canvas may be handed to pdf.js's rendering worker, so every render
// draws into a fresh one and swaps it in only once it is complete.
async function render() {
  const number = current;
  const page = await doc.getPage(number);
  if (number !== current) return;

  const { width, height } = page.getViewport({ scale: 1 });
  const scale = Math.min(stage.clientWidth / width, stage.clientHeight / height);
  const ratio = window.devicePixelRatio || 1;
  const viewport = page.getViewport({ scale: scale * ratio });

  const canvas = document.createElement("canvas");
  canvas.width = Math.floor(viewport.width);
  canvas.height = Math.floor(viewport.height);
  canvas.style.width = `${Math.floor(width * scale)}px`;
  canvas.style.height = `${Math.floor(height * scale)}px`;

  task?.cancel();
  task = page.render({ canvas, viewport });
  try {
    await task.promise;
  } catch (e) {
    if (e instanceof pdfjsLib.RenderingCancelledException) return;
    throw e;
  }
  stage.replaceChildren(canvas);
}

function show(number) {
  number = Math.min(Math.max(number, 1), doc.numPages);
  if (number === current) return;
  current = number;
  counter.textContent = `${current} / ${doc.numPages}`;
  prev.disabled = current === 1;
  next.disabled = current === doc.numPages;
  history.replaceState(null, "", current === 1 ? location.pathname : `#${current}`);
  render();
}

prev.addEventListener("click", () => show(current - 1));
next.addEventListener("click", () => show(current + 1));

const fullscreen = viewer.querySelector(".fullscreen");
fullscreen.hidden = !document.fullscreenEnabled;
fullscreen.addEventListener("click", () => {
  if (document.fullscreenElement) document.exitFullscreen();
  else viewer.requestFullscreen();
});

document.addEventListener("keydown", (e) => {
  if (e.altKey || e.ctrlKey || e.metaKey) return;
  switch (e.key) {
    case "ArrowRight": case "ArrowDown": case "PageDown": case " ": case "Enter": case "n":
      show(current + 1); break;
    case "ArrowLeft": case "ArrowUp": case "PageUp": case "Backspace": case "p":
      show(current - 1); break;
    case "Home": show(1); break;
    case "End": show(doc.numPages); break;
    case "f": fullscreen.click(); break;
    default: return;
  }
  e.preventDefault();
});

// A swipe turns the page in its direction, and a tap or click turns it
// back on the left half of the slide and forward on the right half.
let startX = null;
stage.addEventListener("pointerdown", (e) => { startX = e.clientX; });
stage.addEventListener("pointerup", (e) => {
  if (startX === null) return;
  const dx = e.clientX - startX;
  startX = null;
  if (Math.abs(dx) > 40) {
    show(current + (dx < 0 ? 1 : -1));
  } else {
    const { left, width } = stage.getBoundingClientRect();
    show(current + (e.clientX - left < width / 2 ? -1 : 1));
  }
});

let resizing = false;
window.addEventListener("resize", () => {
  if (resizing) return;
  resizing = true;
  requestAnimationFrame(() => { resizing = false; render(); });
});
window.addEventListener("hashchange", () => show(pageFromHash()));

show(pageFromHash());
