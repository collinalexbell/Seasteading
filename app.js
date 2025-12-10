import * as THREE from "https://unpkg.com/three@0.160.0/build/three.module.js";

// --------------------
// Session / helpers
// --------------------
const sessionId = (() => {
  const key = "seasteadSessionId";
  let s = localStorage.getItem(key);
  if (!s) {
    s = Math.random().toString(36).slice(2) + Date.now().toString(36);
    localStorage.setItem(key, s);
  }
  return s;
})();

const apiBase = `http://${location.hostname}:8081`;
async function api(path, method = "GET", body) {
  const opts = {
    method,
    headers: {
      "X-Session": sessionId
    }
  };
  if (body) {
    opts.headers["Content-Type"] = "application/json";
    opts.body = JSON.stringify(body);
  }
  const res = await fetch(`${apiBase}${path}`, opts);
  if (!res.ok) throw new Error(`${method} ${path} failed`);
  return res.json();
}

// --------------------
// Defaults
// --------------------
const defaultCards = [
  { id: "Realm", bullets: ["Should move its own crypto coin on its own servers (partial).", "Each user is their own realm.", "Mentions: Realm (multiuser? Gaston? Ant?).", "Producing Greendale with water fountains."], pos: { x: -260, y: 80 } },
  { id: "Hardware / Events", bullets: ["MMO events (IRL & online).", "VR Theater.", "Story/Adventure & storyline (see map) (scribbled).", "Bank of Seastead (illegible).", "Architecture grid doodle."], pos: { x: -60, y: -20 } },
  { id: "Floorplan Tile", bullets: ["Circular hub with corridors and rooms.", "Rooms, Lake of Purity, Pillar, Religious (diamond).", "Mass hall (partial), Open to Future, hallway/pipes."], pos: { x: 180, y: 60 } },
  { id: "Lake Map", bullets: ["Blue grid as deepsea cables across the lake.", "Square tiles placed at grid cells, movable/anchorable.", "Clusters near east/center; one south tile."], pos: { x: 40, y: -140 } }
];

// Lake outline
const lakePoints = [
  [0.04, 0.55], [0.12, 0.40], [0.24, 0.32], [0.36, 0.27],
  [0.50, 0.23], [0.66, 0.24], [0.82, 0.30], [0.93, 0.38],
  [0.88, 0.44], [0.78, 0.46], [0.70, 0.50], [0.60, 0.54],
  [0.50, 0.56], [0.40, 0.53], [0.32, 0.58], [0.22, 0.63],
  [0.12, 0.67], [0.06, 0.64]
];
const lakeSize = { w: 180000, h: 110000 };
const gridStep = 12000;
const lakePolyRot = lakePoints.map(([x, y]) => {
  const px = x * lakeSize.w - lakeSize.w / 2;
  const py = y * lakeSize.h - lakeSize.h / 2;
  return { x: -py, y: px }; // rotate 90°
});
const lakeBounds = {
  minX: Math.min(...lakePolyRot.map(p => p.x)),
  maxX: Math.max(...lakePolyRot.map(p => p.x)),
  minY: Math.min(...lakePolyRot.map(p => p.y)),
  maxY: Math.max(...lakePolyRot.map(p => p.y))
};

// --------------------
// State
// --------------------
let cards = [...defaultCards];
let tiles = [];
let strokes = [];
let cardMeshes = [];
let cardOpacity = 1;
let selectedDrawCard = null;
let drawMode = false;
const drawState = { active: false, stroke: null };
const ship = { x: 0, y: 0, heading: 0, speed: 8000, aground: false };
let shipMesh = null;
const defaultShipProgram = `// Ship program runs on the MMO server.\n// This code will be uploaded; the server executes fn(ship, dt, env).\n// ship: { x, y, heading, speed }\n// dt: seconds since last frame\n// env: { pointInLake(x,y), lakeBounds, log(...) }\nconst cruise = 12000;\nship.heading += 0.3 * dt;\nif (!env.pointInLake(ship.x, ship.y)) {\n  ship.heading += Math.PI * 0.6;\n}\nreturn { heading: ship.heading, speed: cruise };`;
let shipProgramCode = defaultShipProgram;
const shipEnv = {
  pointInLake,
  lakeBounds,
  aground: () => ship.aground
};
let runOnClose = true;
let stateData = {};
let shipStateDirty = false;
let shipStateTimer = null;
let ws = null;
let wsConnected = false;
let wsLastSend = 0;
let wsSendIntervalMs = 200;
const pendingControl = { heading: 0, speed: 0, has: false };
const initialRoll = (() => {
  // simple deterministic pseudo-roll from session id
  let seed = 0;
  for (const ch of sessionId) seed = (seed * 31 + ch.charCodeAt(0)) >>> 0;
  const roll = () => ((seed = (1664525 * seed + 1013904223) >>> 0) % 6) + 1;
  return { str: roll() + roll() + roll(), dex: roll() + roll() + roll(), int: roll() + roll() + roll() };
})();

// --------------------
// Load/save helpers
// --------------------
async function loadAll() {
  try {
    const [t, c, d, s] = await Promise.allSettled([
      api("/tiles"),
      api("/cards"),
      api("/draw"),
      api("/state")
    ]);
    if (c.status === "fulfilled" && Array.isArray(c.value.cards)) cards = c.value.cards;
    if (t.status === "fulfilled" && Array.isArray(t.value.tiles)) tiles = t.value.tiles;
    if (d.status === "fulfilled" && Array.isArray(d.value.strokes)) strokes = d.value.strokes;
    if (s.status === "fulfilled" && typeof s.value === "object" && s.value !== null) {
      stateData = s.value;
      if (typeof stateData.shipProgram === "string" && stateData.shipProgram.trim()) {
        shipProgramCode = stateData.shipProgram;
      }
      if (stateData.ship && typeof stateData.ship === "object") {
        ship.x = Number(stateData.ship.x) || 0;
        ship.y = Number(stateData.ship.y) || 0;
        ship.heading = Number(stateData.ship.heading) || 0;
        if (typeof stateData.ship.speed === "number") ship.speed = stateData.ship.speed;
        if (typeof stateData.ship.aground === "boolean") ship.aground = stateData.ship.aground;
      }
      if (typeof stateData.shipRunOnClose === "boolean") runOnClose = stateData.shipRunOnClose;
    }
  } catch (e) {
    console.warn("loadAll", e);
  }
  if (!tiles.length) {
    tiles = cards.map(c => ({ id: c.id, x: c.pos.x, y: c.pos.y }));
  }
}
async function saveTiles() {
  try { await api("/tiles", "POST", { tiles }); } catch (e) { console.warn("saveTiles", e); }
}
async function saveCards() {
  try { await api("/cards", "POST", { cards }); } catch (e) { console.warn("saveCards", e); }
}
async function saveDraw() {
  try { await api("/draw", "POST", { strokes }); } catch (e) { console.warn("saveDraw", e); }
}
async function saveState() {
  stateData = {
    ...stateData,
    shipProgram: shipProgramCode,
    shipRunOnClose: runOnClose,
    ship: { x: ship.x, y: ship.y, heading: ship.heading, speed: ship.speed, aground: ship.aground }
  };
  try { await api("/state", "POST", stateData); } catch (e) { console.warn("saveState", e); }
}
async function registerUser(payload) {
  try { await api("/register", "POST", payload); } catch (e) { console.warn("register", e); }
}

// --------------------
// Three.js scene
// --------------------
const scene = new THREE.Scene();
scene.background = new THREE.Color("#060b13");
const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setPixelRatio(devicePixelRatio);
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

const aspect = window.innerWidth / window.innerHeight;
const viewSize = Math.max(lakeSize.h * 0.6, 900);
const camera = new THREE.OrthographicCamera(
  (-viewSize * aspect) / 2,
  (viewSize * aspect) / 2,
  viewSize / 2,
  -viewSize / 2,
  0.1,
  2000
);
camera.position.set(0, 0, 1000);
const camTarget = new THREE.Vector3(0, 0, 0);
camera.lookAt(camTarget);
scene.add(new THREE.AmbientLight(0xffffff, 0.7));

function buildLakeShape(poly) {
  const shape = new THREE.Shape();
  poly.forEach((p, i) => i === 0 ? shape.moveTo(p.x, p.y) : shape.lineTo(p.x, p.y));
  shape.closePath();
  return shape;
}
function buildGridSegments(poly, step, vertical = true) {
  const minX = Math.min(...poly.map(p => p.x));
  const maxX = Math.max(...poly.map(p => p.x));
  const minY = Math.min(...poly.map(p => p.y));
  const maxY = Math.max(...poly.map(p => p.y));
  const verts = [];
  if (vertical) {
    for (let x = Math.ceil(minX / step) * step; x <= maxX; x += step) {
      const ys = [];
      for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
        const a = poly[j], b = poly[i];
        if ((a.x <= x && b.x >= x) || (a.x >= x && b.x <= x)) {
          if (a.x == b.x) continue;
          const t = (x - a.x) / (b.x - a.x);
          ys.push(a.y + t * (b.y - a.y));
        }
      }
      ys.sort((a, b) => a - b);
      for (let k = 0; k + 1 < ys.length; k += 2) verts.push(x, ys[k], 0, x, ys[k + 1], 0);
    }
  } else {
    for (let y = Math.ceil(minY / step) * step; y <= maxY; y += step) {
      const xs = [];
      for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
        const a = poly[j], b = poly[i];
        if ((a.y <= y && b.y >= y) || (a.y >= y && b.y <= y)) {
          if (a.y == b.y) continue;
          const t = (y - a.y) / (b.y - a.y);
          xs.push(a.x + t * (b.x - a.x));
        }
      }
      xs.sort((a, b) => a - b);
      for (let k = 0; k + 1 < xs.length; k += 2) verts.push(xs[k], y, 0, xs[k + 1], y, 0);
    }
  }
  return verts;
}
function makeGridLines(poly, step) {
  const verts = new Float32Array([
    ...buildGridSegments(poly, step, true),
    ...buildGridSegments(poly, step, false)
  ]);
  const geom = new THREE.BufferGeometry();
  geom.setAttribute("position", new THREE.BufferAttribute(verts, 3));
  return new THREE.LineSegments(
    geom,
    new THREE.LineBasicMaterial({ color: 0x7ca9da, opacity: 0.6, transparent: true, linewidth: 1 })
  );
}
const lakeGroup = new THREE.Group();
lakeGroup.add(new THREE.Mesh(new THREE.ShapeGeometry(buildLakeShape(lakePolyRot)), new THREE.MeshBasicMaterial({ color: "#0e1f33" })));
lakeGroup.add(new THREE.LineLoop(
  new THREE.BufferGeometry().setFromPoints(lakePolyRot.map(p => new THREE.Vector3(p.x, p.y, 1))),
  new THREE.LineBasicMaterial({ color: 0xc8d7e1, linewidth: 2, transparent: true, opacity: 0.8 })
));
lakeGroup.add(makeGridLines(lakePolyRot, gridStep));
scene.add(lakeGroup);

// --------------------
// Ship
// --------------------
function createShipMesh() {
  const geom = new THREE.BufferGeometry();
  const size = 6000;
  const verts = new Float32Array([
    0, size * 0.9, 0,
    -size * 0.6, -size * 0.5, 0,
    size * 0.6, -size * 0.5, 0
  ]);
  geom.setAttribute("position", new THREE.BufferAttribute(verts, 3));
  geom.setIndex([0,1,2]);
  geom.computeVertexNormals();
  const mat = new THREE.MeshBasicMaterial({ color: 0xff8c42, transparent: true, opacity: 0.92 });
  const mesh = new THREE.Mesh(geom, mat);
  mesh.position.set(ship.x, ship.y, 12);
  mesh.userData.type = "ship";
  return mesh;
}
function ensureShip() {
  if (!shipMesh) {
    shipMesh = createShipMesh();
    scene.add(shipMesh);
  }
}
function applyShipToMesh() {
  if (!shipMesh) return;
  shipMesh.position.set(ship.x, ship.y, 12);
  shipMesh.rotation.z = ship.heading - Math.PI / 2;
}
async function uploadShipProgram(code) {
  try { await api("/program", "POST", { shipId: "ship", code }); }
  catch (e) { console.warn("uploadShipProgram", e); }
}

function compileShipProgram(code) {
  // Store and upload; actual execution happens on server runner.
  shipProgramCode = code;
  markShipStateDirty();
  uploadShipProgram(code);
  return true;
}
function markShipStateDirty() {
  shipStateDirty = true;
  if (!shipStateTimer) {
    shipStateTimer = setTimeout(async () => {
      shipStateTimer = null;
      if (shipStateDirty) {
        shipStateDirty = false;
        await saveState();
      }
    }, 1200);
  }
}

// --------------------
// Cards / tiles
// --------------------
const cardMaterial = (tex) => new THREE.MeshBasicMaterial({ map: tex, transparent: true, opacity: cardOpacity });
function makeCardTexture(card) {
  const c = document.createElement("canvas"); c.width = 512; c.height = 512;
  const ctx = c.getContext("2d");
  ctx.fillStyle = "rgba(20,25,35,0.92)";
  ctx.fillRect(0, 0, c.width, c.height);
  ctx.strokeStyle = "rgba(255,255,255,0.1)";
  ctx.lineWidth = 6;
  ctx.strokeRect(8, 8, c.width - 16, c.height - 16);
  ctx.fillStyle = "#f4d37a";
  ctx.font = "bold 36px 'IBM Plex Sans', sans-serif";
  ctx.textBaseline = "top";
  ctx.fillText(card.id, 24, 20);
  ctx.fillStyle = "#eaf2ff";
  ctx.font = "26px 'IBM Plex Sans', sans-serif";
  const wrap = (text, x, y, maxWidth, lineHeight) => {
    const words = text.split(" "); let line = "";
    for (const w of words) {
      const test = line + w + " ";
      if (ctx.measureText(test).width > maxWidth) { ctx.fillText(line, x, y); line = w + " "; y += lineHeight; }
      else line = test;
    }
    ctx.fillText(line, x, y); return y + lineHeight;
  };
  let y = 80;
  card.bullets.forEach(b => {
    ctx.beginPath(); ctx.arc(30, y + 12, 6, 0, Math.PI * 2); ctx.fill();
    y = wrap(b, 50, y, 420, 30) + 6;
  });
  return new THREE.CanvasTexture(c);
}
function makeCardMesh(card) {
  const tex = makeCardTexture(card);
  const mesh = new THREE.Mesh(new THREE.PlaneGeometry(220, 220), cardMaterial(tex));
  mesh.position.set(card.pos.x ?? 0, card.pos.y ?? 0, 10);
  mesh.userData.card = card;
  return mesh;
}
function rebuildCards() {
  cardMeshes.forEach(m => scene.remove(m));
  cardMeshes = cards.map(c => {
    if (!tiles.find(t => t.id === c.id)) tiles.push({ id: c.id, x: c.pos?.x ?? 0, y: c.pos?.y ?? 0 });
    const m = makeCardMesh(c);
    const t = tiles.find(t => t.id === c.id);
    if (t) m.position.set(t.x, t.y, 10);
    scene.add(m);
    return m;
  });
  renderCardsOverlay();
  refreshDrawCardOptions();
}
function updateCardOpacity(value) {
  cardOpacity = value;
  cardMeshes.forEach(m => m.material.opacity = cardOpacity);
}

// --------------------
// Input / drag / pan
// --------------------
const dragState = { active: null, offset: new THREE.Vector3() };
const panState = { active: false };
const raycaster = new THREE.Raycaster();
const pointer = new THREE.Vector2();
function updatePointer(event) {
  const rect = renderer.domElement.getBoundingClientRect();
  pointer.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
  pointer.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
}
function pointInLake(x, y) {
  let inside = false;
  for (let i = 0, j = lakePolyRot.length - 1; i < lakePolyRot.length; j = i++) {
    const xi = lakePolyRot[i].x, yi = lakePolyRot[i].y;
    const xj = lakePolyRot[j].x, yj = lakePolyRot[j].y;
    const intersect = ((yi > y) !== (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi + 1e-9) + xi);
    if (intersect) inside = !inside;
  }
  return inside;
}
const input = { pointerDown: [], pointerMove: [], pointerUp: [] };
const addInput = (arr, fn) => arr.push(fn);
const dispatch = (arr, e) => arr.forEach(fn => fn(e));

addInput(input.pointerDown, (e) => {
  updatePointer(e);
  raycaster.setFromCamera(pointer, camera);
  const targets = [...cardMeshes];
  if (shipMesh) targets.push(shipMesh);
  const hits = raycaster.intersectObjects(targets);
  if (hits.length && !drawMode) {
    const hit = hits[0];
    if (hit.object.userData.type === "ship") {
      openShipEditor();
      dragState.active = null;
      panState.active = false;
      return;
    }
    dragState.active = hit.object;
    dragState.offset.copy(hit.point).sub(hit.object.position);
    panState.active = false;
  } else if (!drawMode) {
    panState.active = true;
    panState.last = { x: e.clientX, y: e.clientY };
  }
});
addInput(input.pointerMove, (e) => {
  if (drawMode) return;
  if (dragState.active) {
    updatePointer(e);
    raycaster.setFromCamera(pointer, camera);
    const planeZ = new THREE.Plane(new THREE.Vector3(0, 0, 1), 0);
    const pos = new THREE.Vector3();
    raycaster.ray.intersectPlane(planeZ, pos);
    pos.sub(dragState.offset);
    if (pointInLake(pos.x, pos.y)) dragState.active.position.set(pos.x, pos.y, 10);
  } else if (panState.active) {
    const dx = e.clientX - panState.last.x;
    const dy = e.clientY - panState.last.y;
    panState.last = { x: e.clientX, y: e.clientY };
    const worldPerPxX = (camera.right - camera.left) / (renderer.domElement.clientWidth * camera.zoom);
    const worldPerPxY = (camera.top - camera.bottom) / (renderer.domElement.clientHeight * camera.zoom);
    camTarget.x -= dx * worldPerPxX;
    camTarget.y += dy * worldPerPxY;
    camera.position.x -= dx * worldPerPxX;
    camera.position.y += dy * worldPerPxY;
    camera.lookAt(camTarget);
  }
});
addInput(input.pointerUp, () => {
  if (dragState.active) {
    const t = tiles.find(t => t.id === dragState.active.userData.card.id);
    if (t) { t.x = dragState.active.position.x; t.y = dragState.active.position.y; saveTiles(); }
  }
  dragState.active = null; panState.active = false;
});

renderer.domElement.addEventListener("pointerdown", e => dispatch(input.pointerDown, e));
window.addEventListener("pointermove", e => dispatch(input.pointerMove, e));
window.addEventListener("pointerup", e => dispatch(input.pointerUp, e));

// --------------------
// Draw overlay
// --------------------
const drawCanvas = document.createElement("canvas");
drawCanvas.id = "drawCanvas";
drawCanvas.style.position = "fixed";
drawCanvas.style.left = "0"; drawCanvas.style.top = "0";
drawCanvas.style.width = "100%"; drawCanvas.style.height = "100%";
drawCanvas.style.pointerEvents = "none";
drawCanvas.style.zIndex = "1";
document.body.appendChild(drawCanvas);
const dctx = drawCanvas.getContext("2d");
function resizeDrawCanvas() {
  drawCanvas.width = window.innerWidth * devicePixelRatio;
  drawCanvas.height = window.innerHeight * devicePixelRatio;
  dctx.scale(devicePixelRatio, devicePixelRatio);
  redrawStrokes();
}
function redrawStrokes() {
  dctx.setTransform(1,0,0,1,0,0);
  dctx.clearRect(0,0,drawCanvas.width, drawCanvas.height);
  if (!selectedDrawCard) return;
  dctx.setTransform(devicePixelRatio,0,0,devicePixelRatio,0,0);
  strokes.filter(s => s.cardId === selectedDrawCard).forEach(s => {
    if (!s.points || s.points.length < 2) return;
    dctx.strokeStyle = s.color || "#ffcc00";
    dctx.lineWidth = s.width || 3;
    dctx.globalAlpha = s.alpha ?? 0.6;
    dctx.beginPath();
    const first = s.points[0];
    dctx.moveTo(first.x, first.y);
    for (let i=1;i<s.points.length;i++) {
      const p = s.points[i];
      dctx.lineTo(p.x, p.y);
    }
    dctx.stroke();
  });
  dctx.globalAlpha = 1;
}
function startDraw(e) {
  if (!drawMode || !selectedDrawCard) return;
  drawState.active = true;
  drawState.stroke = {
    cardId: selectedDrawCard,
    color: document.getElementById("drawColor").value,
    alpha: parseFloat(document.getElementById("drawAlpha").value),
    width: 3,
    points: []
  };
  drawCanvas.style.pointerEvents = "auto";
  addDrawPoint(e);
}
function addDrawPoint(e) {
  if (!drawState.active || !drawState.stroke) return;
  const rect = drawCanvas.getBoundingClientRect();
  drawState.stroke.points.push({ x: e.clientX - rect.left, y: e.clientY - rect.top });
  redrawStrokesWithCurrent();
}
function endDraw() {
  if (drawState.active && drawState.stroke && drawState.stroke.points.length) {
    strokes.push(drawState.stroke);
    saveDraw();
  }
  drawState.active = false;
  drawState.stroke = null;
  redrawStrokes();
  drawCanvas.style.pointerEvents = drawMode ? "auto" : "none";
}
function redrawStrokesWithCurrent() {
  redrawStrokes();
  if (drawState.stroke && drawState.stroke.points.length > 1) {
    const s = drawState.stroke;
    dctx.strokeStyle = s.color;
    dctx.lineWidth = s.width;
    dctx.globalAlpha = s.alpha;
    dctx.beginPath();
    const first = s.points[0];
    dctx.moveTo(first.x, first.y);
    for (let i=1;i<s.points.length;i++) dctx.lineTo(s.points[i].x, s.points[i].y);
    dctx.stroke();
    dctx.globalAlpha = 1;
  }
}
drawCanvas.addEventListener("pointerdown", (e) => { if (drawMode) { startDraw(e); e.preventDefault(); }});
drawCanvas.addEventListener("pointermove", (e) => { if (drawMode && drawState.active) { addDrawPoint(e); e.preventDefault(); }});
drawCanvas.addEventListener("pointerup", (e) => { if (drawMode) { endDraw(); e.preventDefault(); }});
drawCanvas.addEventListener("pointerleave", () => { if (drawMode) endDraw(); });

// --------------------
// UI / overlay
// --------------------
function renderCardsOverlay() {
  const overlay = document.getElementById("overlay");
  overlay.innerHTML = "";
  cards.forEach(c => {
    const div = document.createElement("div");
    div.className = "card";
    div.dataset.cardId = c.id;
    div.innerHTML = `<h3>${c.id}</h3><ul>${c.bullets.map(b => `<li>${b}</li>`).join("")}</ul>`;
    div.addEventListener("click", () => focusOnCard(c.id));
    overlay.appendChild(div);
  });
}
function refreshDrawCardOptions() {
  const sel = document.getElementById("drawCard");
  sel.innerHTML = "";
  cards.forEach(c => {
    const opt = document.createElement("option");
    opt.value = c.id; opt.textContent = c.id;
    sel.appendChild(opt);
  });
  selectedDrawCard = sel.value || (cards[0]?.id ?? null);
}

document.getElementById("reset").onclick = () => {
  tiles = cards.map(c => ({ id: c.id, x: c.pos?.x ?? 0, y: c.pos?.y ?? 0 }));
  rebuildCards();
  saveTiles();
};
document.getElementById("shuffle").onclick = () => {
  cards.forEach((c, i) => {
    const cols = 10, rows = 6;
    const gx = (Math.random() * cols - cols/2) * gridStep * 0.5;
    const gy = (Math.random() * rows - rows/2) * gridStep * 0.5;
    tiles[i] = { id: c.id, x: gx, y: gy };
  });
  rebuildCards();
  saveTiles();
};
document.getElementById("opacity").addEventListener("input", (e) => {
  updateCardOpacity(parseFloat(e.target.value));
});
document.getElementById("addCard").onclick = () => {
  const title = document.getElementById("newCardTitle").value.trim() || "New Card";
  const id = document.getElementById("newCardId").value.trim() || title;
  const bullets = document.getElementById("newCardBullets").value.split(",").map(s => s.trim()).filter(Boolean);
  const newCard = { id, bullets: bullets.length ? bullets : ["..."], pos: { x: 0, y: 0 } };
  cards.push(newCard);
  tiles.push({ id, x: 0, y: 0 });
  rebuildCards();
  saveCards();
  saveTiles();
  refreshDrawCardOptions();
};
document.getElementById("drawCard").addEventListener("change", (e) => {
  selectedDrawCard = e.target.value;
  redrawStrokes();
});
document.getElementById("toggleDraw").onclick = () => {
  drawMode = !drawMode;
  drawCanvas.style.pointerEvents = drawMode ? "auto" : "none";
};
document.getElementById("regRoll").value = JSON.stringify(initialRoll);
document.getElementById("registerBtn").onclick = () => {
  const user = document.getElementById("regUser").value.trim() || "anon";
  const pub = document.getElementById("regPub").value.trim();
  const sig = document.getElementById("regSig").value.trim();
  const roll = document.getElementById("regRoll").value.trim() || JSON.stringify(initialRoll);
  const payload = { user, pub, sig, roll, session: sessionId };
  registerUser(payload);
  alert("Registration submitted");
};

function focusOnCard(cardId) {
  const mesh = cardMeshes.find(m => m.userData.card.id === cardId);
  if (!mesh) return;
  const target = mesh.position.clone();
  camTarget.set(target.x, target.y, 0);
  camera.position.set(target.x, target.y, 1000);
  camera.lookAt(camTarget);
  camera.zoom = 80;
  camera.updateProjectionMatrix();
}

// Zoom (wheel + pinch)
function applyZoom(mult) {
  camera.zoom = THREE.MathUtils.clamp(camera.zoom * mult, 0.05, 100.0);
  camera.updateProjectionMatrix();
}
renderer.domElement.addEventListener("wheel", (e) => {
  if (drawMode) return;
  e.preventDefault();
  const mult = 1 - e.deltaY * 0.001;
  applyZoom(mult);
}, { passive: false });
const touch = { pinching: false, lastDist: 0 };
function touchDistance(touches) {
  const dx = touches[0].clientX - touches[1].clientX;
  const dy = touches[0].clientY - touches[1].clientY;
  return Math.hypot(dx, dy);
}
renderer.domElement.addEventListener("touchstart", (e) => {
  if (e.touches.length === 2) {
    touch.pinching = true;
    touch.lastDist = touchDistance(e.touches);
  }
}, { passive: true });
renderer.domElement.addEventListener("touchmove", (e) => {
  if (touch.pinching && e.touches.length === 2) {
    e.preventDefault();
    const dist = touchDistance(e.touches);
    const mult = 1 + (dist - touch.lastDist) / 400;
    applyZoom(mult);
    touch.lastDist = dist;
  }
}, { passive: false });
renderer.domElement.addEventListener("touchend", () => { touch.pinching = false; }, { passive: true });

// Resize
function resize() {
  const aspect = window.innerWidth / window.innerHeight;
  camera.left = (-viewSize * aspect) / 2;
  camera.right = (viewSize * aspect) / 2;
  camera.top = viewSize / 2;
  camera.bottom = -viewSize / 2;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
  resizeDrawCanvas();
}
window.addEventListener("resize", resize);

function animate() {
  const now = performance.now();
  const dt = Math.min((now - animate.lastTime) / 1000, 0.1);
  animate.lastTime = now;
  applyShipToMesh();
  requestAnimationFrame(animate);
  renderer.render(scene, camera);
}
animate.lastTime = performance.now();

// --------------------
// Init
// --------------------
(async function init() {
  await loadAll();
  ensureShip();
  compileShipProgram(shipProgramCode);
  rebuildCards();
  updateCardOpacity(1);
  refreshDrawCardOptions();
  redrawStrokes();
  resize();
  shipCodeEl.value = shipProgramCode;
  runOnCloseEl.checked = runOnClose;
  connectRealtime();
  animate();
})();

// --------------------
// Ship editor UI
// --------------------
const shipEditor = document.getElementById("shipEditor");
const shipCodeEl = document.getElementById("shipCode");
const runOnCloseEl = document.getElementById("runOnClose");
document.getElementById("closeShipEditor").onclick = () => closeShipEditor(true);
document.getElementById("runShipCode").onclick = () => {
  if (compileShipProgram(shipCodeEl.value)) {
    applyShipToMesh();
    saveState();
  }
};
runOnCloseEl.addEventListener("change", (e) => { runOnClose = e.target.checked; markShipStateDirty(); });

function openShipEditor() {
  shipCodeEl.value = shipProgramCode;
  shipEditor.style.display = "block";
}
function closeShipEditor(shouldRun) {
  shipEditor.style.display = "none";
  if (shouldRun && runOnClose && shipCodeEl.value !== shipProgramCode) {
    compileShipProgram(shipCodeEl.value);
    saveState();
  }
}
window.addEventListener("blur", () => {
  if (shipStateDirty) saveState();
});
window.addEventListener("keydown", (e) => {
  if (e.key === "Escape" && shipEditor.style.display === "block") {
    closeShipEditor(false);
  }
});

// --------------------
// Realtime (WebSocket) with server for aground checks
// --------------------
function connectRealtime() {
  const url = `ws://${location.hostname}:8081/ws`;
  try {
    ws = new WebSocket(url);
  } catch (e) {
    console.warn("ws connect failed", e);
    return;
  }
  ws.onopen = () => { wsConnected = true; };
  ws.onclose = () => { wsConnected = false; ws = null; setTimeout(connectRealtime, 1200); };
  ws.onerror = () => { wsConnected = false; ws = null; };
  ws.onmessage = (e) => {
    try {
      const data = JSON.parse(e.data);
      if (data.ship) {
        ship.x = typeof data.ship.x === "number" ? data.ship.x : ship.x;
        ship.y = typeof data.ship.y === "number" ? data.ship.y : ship.y;
        ship.heading = typeof data.ship.heading === "number" ? data.ship.heading : ship.heading;
        ship.speed = typeof data.ship.speed === "number" ? data.ship.speed : ship.speed;
        ship.aground = !!data.ship.aground;
      }
    } catch (err) {
      console.warn("ws message parse", err);
    }
  };
}

function sendRealtimeCommand(heading, speed, nowMs) {
  if (!wsConnected || !ws) return;
  if (nowMs - wsLastSend < wsSendIntervalMs) return;
  wsLastSend = nowMs;
  const payload = JSON.stringify({ control: { heading, speed } });
  ws.send(payload);
}

// Input -> server ws
const keyState = {};
function sendInputUpdate(code, pressed) {
  if (!wsConnected || !ws) return;
  const payload = JSON.stringify({ input: { keys: { [code]: pressed } } });
  ws.send(payload);
}
window.addEventListener("keydown", (e) => {
  if (e.repeat) return;
  keyState[e.code] = true;
  sendInputUpdate(e.code, true);
});
window.addEventListener("keyup", (e) => {
  keyState[e.code] = false;
  sendInputUpdate(e.code, false);
});
