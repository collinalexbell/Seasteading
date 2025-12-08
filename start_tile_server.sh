#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC="${ROOT}/server.cpp"
BIN="${ROOT}/server"
LOG="${ROOT}/server.log"
PIDFILE="${ROOT}/server.pid"
PORT="${PORT:-8081}"

echo "[tile-server] root: ${ROOT}"

if [[ -f "${PIDFILE}" ]]; then
  OLD_PID="$(cat "${PIDFILE}")"
  if kill -0 "${OLD_PID}" 2>/dev/null; then
    echo "[tile-server] stopping old server pid ${OLD_PID}"
    kill "${OLD_PID}" || true
    sleep 0.2
  fi
fi

echo "[tile-server] building ${SRC}"
g++ -std=c++17 -O2 -pipe "${SRC}" -o "${BIN}"

echo "[tile-server] starting on port ${PORT}"
cd "${ROOT}"
nohup "${BIN}" >> "${LOG}" 2>&1 &
NEW_PID=$!
echo "${NEW_PID}" > "${PIDFILE}"
echo "[tile-server] pid ${NEW_PID}, log ${LOG}"
