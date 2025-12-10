// Player ship program (runs on server). Uses WASD keys via env.inputKeys.
// ship: { x, y, heading, speed }
// dt: seconds
// env: { inputKeys, pointInLake(x,y), lakeBounds, log(...) }
const keys = env.inputKeys || {};
const turn = 2.5; // radians/sec
let h = ship.heading;
let speed = ship.speed;
if (keys.KeyA) h -= turn * dt;
if (keys.KeyD) h += turn * dt;
if (keys.KeyW) speed = 20000;
else if (keys.KeyS) speed = -10000;
else speed *= 0.92;
if (!env.pointInLake(ship.x, ship.y)) {
  h += Math.PI * 0.6;
  speed = 0;
}
return { heading: h, speed };