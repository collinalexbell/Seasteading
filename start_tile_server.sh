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
if command -v g++ >/dev/null 2>&1; then
  if ! g++ -std=c++17 -O2 -pipe -pthread "${SRC}" -lssl -lcrypto -o "${BIN}"; then
    if [[ -x "${BIN}" ]]; then
      echo "[tile-server] build failed; using existing binary ${BIN}"
    else
      echo "[tile-server] build failed and no existing binary; aborting"
      exit 1
    fi
  fi
else
  if [[ -x "${BIN}" ]]; then
    echo "[tile-server] g++ not found; using existing binary ${BIN}"
  else
    echo "[tile-server] g++ not found and no existing binary; aborting"
    exit 1
  fi
fi

echo "[tile-server] starting on port ${PORT}"
cd "${ROOT}"
if command -v setsid >/dev/null 2>&1; then
  setsid nohup "${BIN}" >> "${LOG}" 2>&1 < /dev/null &
else
  nohup "${BIN}" >> "${LOG}" 2>&1 < /dev/null &
fi
NEW_PID=$!
echo "${NEW_PID}" > "${PIDFILE}"
echo "[tile-server] pid ${NEW_PID}, log ${LOG}"
