import * as THREE from "https://unpkg.com/three@0.160.0/build/three.module.js";

// Card data (transcribed)
const cards = [
  {
    id: "Realm",
    bullets: [
      "Should move its own crypto coin on its own servers (partial).",
      "Each user is their own realm.",
      "Mentions: Realm (multiuser? Gaston? Ant?).",
      "Producing Greendale with water fountains."
    ],
    pos: { x: -260, y: 80 }
  },
  {
    id: "Hardware / Events",
    bullets: [
      "MMO events (IRL & online).",
      "VR Theater.",
      "Story/Adventure & storyline (see map) (scribbled).",
      "Bank of Seastead (illegible).",
      "Architecture grid doodle."
    ],
    pos: { x: -60, y: -20 }
  },
  {
    id: "Floorplan Tile",
    bullets: [
      "Circular hub with corridors and rooms.",
      "Rooms, Lake of Purity, Pillar, Religious (diamond).",
      "Mass hall (partial), Open to Future, hallway/pipes."
    ],
    pos: { x: 180, y: 60 }
  },
  {
    id: "Lake Map",
    bullets: [
      "Blue grid as deepsea cables across the lake.",
      "Square tiles placed at grid cells, movable/anchorable.",
      "Clusters near east/center; one south tile."
    ],
    pos: { x: 40, y: -140 }
  }
];

// Lake Superior-like silhouette (normalized 0..1), simplified
const lakePoints = [
  [0.04, 0.55], [0.12, 0.40], [0.24, 0.32], [0.36, 0.27],
  [0.50, 0.23], [0.66, 0.24], [0.82, 0.30], [0.93, 0.38],
  [0.88, 0.44], [0.78, 0.46], [0.70, 0.50], [0.60, 0.54],
  [0.50, 0.56], [0.40, 0.53], [0.32, 0.58], [0.22, 0.63],
  [0.12, 0.67], [0.06, 0.64]
];

// Scale and rotation
const lakeSize = { w: 180000, h: 110000 };
const gridStep = 12000;
const lakePoly = lakePoints.map(([x, y]) => ({
  x: x * lakeSize.w - lakeSize.w / 2,
  y: y * lakeSize.h - lakeSize.h / 2
}));
// Rotate lake 90 degrees (z+)
const rotate90 = (p) => ({ x: -p.y, y: p.x });
const lakePolyRot = lakePoly.map(rotate90);

// --- Persistence client ---
const apiBase = `http://${location.hostname}:8081`;
async function loadTiles() {
  try {
    const res = await fetch(`${apiBase}/tiles`, { method: "GET" });
    if (!res.ok) throw new Error("GET failed");
    const data = await res.json();
    if (data && Array.isArray(data.tiles)) return data.tiles;
  } catch (e) {
    console.warn("loadTiles failed", e);
  }
  return null;
}

async function saveTiles(tiles) {
  try {
    await fetch(`${apiBase}/tiles`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ tiles })
    });
  } catch (e) {
    console.warn("saveTiles failed", e);
  }
}

// Three.js setup
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

// Lights
scene.add(new THREE.AmbientLight(0xffffff, 0.7));

// Lake shape/group
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
          if (a.x === b.x) continue;
          const t = (x - a.x) / (b.x - a.x);
          const y = a.y + t * (b.y - a.y);
          ys.push(y);
        }
      }
      ys.sort((a, b) => a - b);
      for (let k = 0; k + 1 < ys.length; k += 2) {
        verts.push(x, ys[k], 0, x, ys[k + 1], 0);
      }
    }
  } else {
    for (let y = Math.ceil(minY / step) * step; y <= maxY; y += step) {
      const xs = [];
      for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
        const a = poly[j], b = poly[i];
        if ((a.y <= y && b.y >= y) || (a.y >= y && b.y <= y)) {
          if (a.y === b.y) continue;
          const t = (y - a.y) / (b.y - a.y);
          const x = a.x + t * (b.x - a.x);
          xs.push(x);
        }
      }
      xs.sort((a, b) => a - b);
      for (let k = 0; k + 1 < xs.length; k += 2) {
        verts.push(xs[k], y, 0, xs[k + 1], y, 0);
      }
    }
  }
  return verts;
}

function makeGridLines(poly, step) {
  const vVerts = buildGridSegments(poly, step, true);
  const hVerts = buildGridSegments(poly, step, false);
  const verts = new Float32Array([...vVerts, ...hVerts]);
  const geom = new THREE.BufferGeometry();
  geom.setAttribute("position", new THREE.BufferAttribute(verts, 3));
  return new THREE.LineSegments(
    geom,
    new THREE.LineBasicMaterial({
      color: 0x7ca9da,
      opacity: 0.6,
      transparent: true,
      linewidth: 1
    })
  );
}

const lakeGroup = new THREE.Group();
const lakeShape = buildLakeShape(lakePolyRot);
const lakeMesh = new THREE.Mesh(
  new THREE.ShapeGeometry(lakeShape),
  new THREE.MeshBasicMaterial({ color: "#0e1f33" })
);
lakeGroup.add(lakeMesh);

const lakeOutline = new THREE.LineLoop(
  new THREE.BufferGeometry().setFromPoints(lakePolyRot.map(p => new THREE.Vector3(p.x, p.y, 1))),
  new THREE.LineBasicMaterial({ color: 0xc8d7e1, linewidth: 2, transparent: true, opacity: 0.8 })
);
lakeGroup.add(lakeOutline);

const lakeGrid = makeGridLines(lakePolyRot, gridStep);
lakeGroup.add(lakeGrid);

scene.add(lakeGroup);

// Cards
const cardMeshes = [];
const dragState = { active: null, offset: new THREE.Vector3() };
const panState = { active: false };
const raycaster = new THREE.Raycaster();
const pointer = new THREE.Vector2();

function makeCardTexture(card) {
  const c = document.createElement("canvas");
  c.width = 512; c.height = 512;
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
    const words = text.split(" ");
    let line = "";
    for (const w of words) {
      const test = line + w + " ";
      if (ctx.measureText(test).width > maxWidth) {
        ctx.fillText(line, x, y);
        line = w + " ";
        y += lineHeight;
      } else {
        line = test;
      }
    }
    ctx.fillText(line, x, y);
    return y + lineHeight;
  };
  let y = 80;
  card.bullets.forEach(b => {
    ctx.beginPath();
    ctx.arc(30, y + 12, 6, 0, Math.PI * 2);
    ctx.fill();
    y = wrap(b, 50, y, 420, 30) + 6;
  });
  return new THREE.CanvasTexture(c);
}

function makeCardMesh(card) {
  const tex = makeCardTexture(card);
  const mesh = new THREE.Mesh(
    new THREE.PlaneGeometry(220, 220),
    new THREE.MeshBasicMaterial({ map: tex, transparent: true })
  );
  mesh.position.set(card.pos.x, card.pos.y, 10);
  mesh.userData.card = card;
  return mesh;
}

cards.forEach(c => {
  const m = makeCardMesh(c);
  cardMeshes.push(m);
  scene.add(m);
});

async function applyLoadedTiles() {
  const loaded = await loadTiles();
  if (loaded) {
    loaded.forEach(t => {
      const mesh = cardMeshes.find(m => m.userData.card.id === t.id);
      if (mesh && typeof t.x === "number" && typeof t.y === "number") {
        mesh.position.set(t.x, t.y, 10);
      }
    });
  }
}

function pointInLake(x, y) {
  let inside = false;
  for (let i = 0, j = lakePolyRot.length - 1; i < lakePolyRot.length; j = i++) {
    const xi = lakePolyRot[i].x, yi = lakePolyRot[i].y;
    const xj = lakePolyRot[j].x, yj = lakePolyRot[j].y;
    const intersect = ((yi > y) !== (yj > y)) &&
      (x < (xj - xi) * (y - yi) / (yj - yi + 1e-9) + xi);
    if (intersect) inside = !inside;
  }
  return inside;
}

// Input routing
const input = {
  pointerDown: [],
  pointerMove: [],
  pointerUp: []
};
function addInput(handlerList, fn) { handlerList.push(fn); }
function dispatch(list, evt) { list.forEach(fn => fn(evt)); }

// Drag handlers
addInput(input.pointerDown, (e) => {
  updatePointer(e);
  raycaster.setFromCamera(pointer, camera);
  const intersects = raycaster.intersectObjects(cardMeshes);
  if (intersects.length) {
    const hit = intersects[0];
    dragState.active = hit.object;
    dragState.offset.copy(hit.point).sub(hit.object.position);
    panState.active = false;
  } else {
    panState.active = true;
    panState.last = { x: e.clientX, y: e.clientY };
  }
});
addInput(input.pointerMove, (e) => {
  if (!dragState.active) return;
  updatePointer(e);
  raycaster.setFromCamera(pointer, camera);
  const planeZ = new THREE.Plane(new THREE.Vector3(0, 0, 1), 0);
  const pos = new THREE.Vector3();
  raycaster.ray.intersectPlane(planeZ, pos);
  pos.sub(dragState.offset);
  if (pointInLake(pos.x, pos.y)) {
    dragState.active.position.set(pos.x, pos.y, 10);
  }
});
addInput(input.pointerMove, (e) => {
  if (!panState.active || dragState.active) return;
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
});
addInput(input.pointerUp, () => {
  if (dragState.active) scheduleSave();
  dragState.active = null;
  panState.active = false;
});

function currentTilePositions() {
  return cardMeshes.map(m => ({
    id: m.userData.card.id,
    x: m.position.x,
    y: m.position.y
  }));
}

function scheduleSave() {
  if (scheduleSave._timer) clearTimeout(scheduleSave._timer);
  scheduleSave._timer = setTimeout(() => {
    saveTiles(currentTilePositions());
  }, 200);
}

function updatePointer(event) {
  const rect = renderer.domElement.getBoundingClientRect();
  pointer.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
  pointer.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
}

renderer.domElement.addEventListener("pointerdown", (e) => dispatch(input.pointerDown, e));
window.addEventListener("pointermove", (e) => dispatch(input.pointerMove, e));
window.addEventListener("pointerup", (e) => dispatch(input.pointerUp, e));

// HUD buttons
document.getElementById("reset").onclick = () => {
  cards.forEach((c, i) => {
    const m = cardMeshes[i];
    m.position.set(c.pos.x, c.pos.y, 10);
  });
};
document.getElementById("shuffle").onclick = () => {
  cards.forEach((c, i) => {
    let tries = 0;
    while (tries < 50) {
      const x = (Math.random() - 0.5) * lakeSize.w * 0.9;
      const y = (Math.random() - 0.5) * lakeSize.h * 0.9;
      if (pointInLake(x, y)) {
        cardMeshes[i].position.set(x, y, 10);
        break;
      }
      tries++;
    }
  });
};

// Overlay cards with click-to-focus
function focusOnCard(cardId) {
  const mesh = cardMeshes.find(m => m.userData.card.id === cardId);
  if (!mesh) return;
  const target = mesh.position.clone();
  camTarget.set(target.x, target.y, 0);
  camera.position.set(target.x, target.y, 1000);
  camera.lookAt(camTarget);
  camera.zoom = 80; // deeper close-up
  camera.updateProjectionMatrix();
  scheduleSave(); // also persist any manual move before focusing
}

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

// Zoom (wheel + pinch)
function applyZoom(mult) {
  camera.zoom = THREE.MathUtils.clamp(camera.zoom * mult, 0.05, 100.0);
  camera.updateProjectionMatrix();
}
renderer.domElement.addEventListener("wheel", (e) => {
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

function resize() {
  const aspect = window.innerWidth / window.innerHeight;
  camera.left = (-viewSize * aspect) / 2;
  camera.right = (viewSize * aspect) / 2;
  camera.top = viewSize / 2;
  camera.bottom = -viewSize / 2;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
}
window.addEventListener("resize", resize);

function animate() {
  requestAnimationFrame(animate);
  renderer.render(scene, camera);
}

renderCardsOverlay();
applyLoadedTiles().then(() => {
  resize();
  animate();
});
