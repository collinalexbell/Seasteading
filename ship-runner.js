#!/usr/bin/env node
// Ship runner: executes user-supplied JS programs in a sandbox at fixed ticks.
// Protocol: JSON lines over UNIX socket (default /tmp/ship_runner.sock).
// Commands:
//   {"cmd":"set_program","shipId":"S","code":"..."} -> {ok:true}|{error:""}
//   {"cmd":"set_state","shipId":"S","ship":{x,y,heading,speed}} -> {ok:true}
//   {"cmd":"set_tick_rate","hz":20} -> {ok:true}
//   {"cmd":"subscribe"} -> stream of {"event":"tick","shipId":...,"control":{heading,speed},"ship":{...}}
//
// Runner ticks at HZ (default 20), merges latest ship state per ship, executes program in vm with:
//   fn(ship, dt, env) where env = {pointInLake(x,y), lakeBounds, log}
// Returns control intent {heading, speed}; applied to ship state.

const fs = require("fs");
const net = require("net");
const vm = require("vm");
const path = require("path");

const sockPath = process.env.SHIP_RUNNER_SOCK || "/tmp/ship_runner.sock";
let tickHz = Number(process.env.SHIP_RUNNER_HZ) || 20;
if (tickHz < 1 || tickHz > 240) tickHz = 20;

const lakePoints = [
  [0.04, 0.55], [0.12, 0.40], [0.24, 0.32], [0.36, 0.27],
  [0.50, 0.23], [0.66, 0.24], [0.82, 0.30], [0.93, 0.38],
  [0.88, 0.44], [0.78, 0.46], [0.70, 0.50], [0.60, 0.54],
  [0.50, 0.56], [0.40, 0.53], [0.32, 0.58], [0.22, 0.63],
  [0.12, 0.67], [0.06, 0.64]
];
const lakeSize = { w: 180000, h: 110000 };
const lakePoly = (() => {
  const pts = lakePoints.map(([x, y]) => {
    const px = x * lakeSize.w - lakeSize.w / 2;
    const py = y * lakeSize.h - lakeSize.h / 2;
    return { x: -py, y: px }; // rotate 90°
  });
  const bounds = {
    minX: Math.min(...pts.map(p => p.x)),
    maxX: Math.max(...pts.map(p => p.x)),
    minY: Math.min(...pts.map(p => p.y)),
    maxY: Math.max(...pts.map(p => p.y))
  };
  return { pts, bounds };
})();
function pointInLake(x, y) {
  let inside = false;
  const pts = lakePoly.pts;
  for (let i = 0, j = pts.length - 1; i < pts.length; j = i++) {
    const xi = pts[i].x, yi = pts[i].y;
    const xj = pts[j].x, yj = pts[j].y;
    const intersect = ((yi > y) !== (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi + 1e-9) + xi);
    if (intersect) inside = !inside;
  }
  return inside;
}

const defaultProgram = `// ship: {x,y,heading,speed}, dt in seconds, env: {pointInLake,lakeBounds,log}
const cruise = 12000;
ship.heading += 0.3 * dt;
if (!env.pointInLake(ship.x, ship.y)) {
  ship.heading += Math.PI * 0.6;
}
return { heading: ship.heading, speed: cruise };`;

const ships = new Map(); // shipId -> {state, program, script, lastControl, inputKeys}
const subscribers = new Set();

function compileProgram(code) {
  return new vm.Script(`(function(ship, dt, env) { ${code}\n})`);
}

function loadProgramFile(shipId) {
  try {
    const p = path.join(process.cwd(), "game_code", `${shipId}.js`);
    if (fs.existsSync(p)) {
      const code = fs.readFileSync(p, "utf8");
      const rec = getShip(shipId);
      rec.program = code;
      rec.script = compileProgram(code);
      console.log(`[runner] loaded program for ${shipId} from ${p}`);
    }
  } catch (e) {
    console.warn("[runner] loadProgramFile", e);
  }
}

function getShip(id) {
  if (!ships.has(id)) {
    ships.set(id, {
      state: { x: 0, y: 0, heading: 0, speed: 0 },
      program: defaultProgram,
      script: compileProgram(defaultProgram),
      lastControl: { heading: 0, speed: 0 },
      inputKeys: {}
    });
    loadProgramFile(id);
  }
  return ships.get(id);
}

function clampShip(ship) {
  const m = 12000 * 0.8;
  let clamped = false;
  if (ship.x < lakePoly.bounds.minX - m) { ship.x = lakePoly.bounds.minX - m; clamped = true; }
  if (ship.x > lakePoly.bounds.maxX + m) { ship.x = lakePoly.bounds.maxX + m; clamped = true; }
  if (ship.y < lakePoly.bounds.minY - m) { ship.y = lakePoly.bounds.minY - m; clamped = true; }
  if (ship.y > lakePoly.bounds.maxY + m) { ship.y = lakePoly.bounds.maxY + m; clamped = true; }
  if (!pointInLake(ship.x, ship.y) || clamped) {
    ship.speed = 0;
  }
}

function runProgram(shipId, dt) {
  const rec = getShip(shipId);
  const ship = rec.state;
  if (!rec.inputKeys) rec.inputKeys = {};
  const context = vm.createContext({
    ship,
    dt,
    env: {
      pointInLake,
      lakeBounds: lakePoly.bounds,
      log: (...args) => {},
      inputKeys: rec.inputKeys
    }
  });
  let control = rec.lastControl;
  try {
    const fn = rec.script.runInContext(context, { timeout: 10 });
    const res = fn(ship, dt, context.env);
    if (res && typeof res === "object") {
      if (typeof res.heading === "number") control.heading = res.heading;
      if (typeof res.speed === "number") control.speed = res.speed;
    }
  } catch (e) {
    // keep last control on error
  }
  clampShip(ship);
  rec.lastControl = control;
  ship.heading = control.heading ?? ship.heading;
  ship.speed = control.speed ?? ship.speed;
  return { ship: { ...ship }, control: { ...control } };
}

// Tick loop
let lastTick = Date.now();
function tick() {
  const now = Date.now();
  const dt = Math.min((now - lastTick) / 1000, 0.2);
  lastTick = now;
  for (const [shipId, rec] of ships.entries()) {
    const result = runProgram(shipId, dt);
    for (const ws of subscribers) {
      ws.write(JSON.stringify({ event: "tick", shipId, ...result }) + "\n");
    }
  }
}
setInterval(tick, Math.round(1000 / tickHz));

function handleLine(sock, line) {
  let msg;
  try { msg = JSON.parse(line); } catch { return; }
  const respond = (obj) => sock.write(JSON.stringify(obj) + "\n");
  switch (msg.cmd) {
    case "set_program": {
      const shipId = msg.shipId || "ship";
      const code = msg.code || defaultProgram;
      try {
        const script = compileProgram(code);
        const rec = getShip(shipId);
        rec.program = code;
        rec.script = script;
        respond({ ok: true });
      } catch (e) {
        respond({ ok: false, error: e.message });
      }
      break;
    }
    case "set_state": {
      const shipId = msg.shipId || "ship";
      const rec = getShip(shipId);
      rec.state = Object.assign({}, rec.state, msg.ship || {});
      respond({ ok: true });
      break;
    }
    case "step": {
      const shipId = msg.shipId || "ship";
      const rec = getShip(shipId);
      if (msg.ship && typeof msg.ship === "object") {
        rec.state = Object.assign({}, rec.state, msg.ship);
      }
      if (msg.input && msg.input.keys && typeof msg.input.keys === "object") {
        rec.inputKeys = msg.input.keys;
      } else {
        rec.inputKeys = rec.inputKeys || {};
      }
      const dt = typeof msg.dt === "number" ? msg.dt : 0.05;
      const result = runProgram(shipId, dt);
      respond({ ok: true, ship: result.ship, control: result.control });
      break;
    }
    case "set_tick_rate": {
      const hz = Number(msg.hz);
      if (hz > 0 && hz <= 240) {
        tickHz = hz;
        respond({ ok: true, hz });
      } else {
        respond({ ok: false, error: "invalid hz" });
      }
      break;
    }
    case "subscribe": {
      subscribers.add(sock);
      respond({ ok: true });
      break;
    }
    default:
      respond({ ok: false, error: "unknown cmd" });
  }
}

function startServer() {
  if (fs.existsSync(sockPath)) fs.unlinkSync(sockPath);
  const server = net.createServer((socket) => {
    let buffer = "";
    socket.on("data", (chunk) => {
      buffer += chunk.toString("utf8");
      let idx;
      while ((idx = buffer.indexOf("\n")) >= 0) {
        const line = buffer.slice(0, idx).trim();
        buffer = buffer.slice(idx + 1);
        if (line) handleLine(socket, line);
      }
    });
    socket.on("close", () => subscribers.delete(socket));
    socket.on("error", () => subscribers.delete(socket));
  });
  server.listen(sockPath, () => {
    fs.chmodSync(sockPath, 0o666);
    console.log(`ship-runner listening on ${sockPath} @ ${tickHz} hz`);
  });
}

startServer();
