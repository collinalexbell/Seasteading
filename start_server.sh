#!/usr/bin/env bash
set -euo pipefail

PORT=${PORT:-8000}
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="${ROOT}/server.log"
PIDFILE="${ROOT}/server_http.pid"

echo "Starting server on port ${PORT} from ${ROOT} ..."
cd "${ROOT}"

if [[ -f "${PIDFILE}" ]]; then
  OLD_PID="$(cat "${PIDFILE}")"
  if kill -0 "${OLD_PID}" 2>/dev/null; then
    echo "Stopping old HTTP server pid ${OLD_PID}"
    kill "${OLD_PID}" || true
    sleep 0.2
  fi
fi

if command -v setsid >/dev/null 2>&1; then
  setsid nohup python -m http.server "${PORT}" --bind 0.0.0.0 >>"${LOG}" 2>&1 < /dev/null &
else
  nohup python -m http.server "${PORT}" --bind 0.0.0.0 >>"${LOG}" 2>&1 < /dev/null &
fi
PID=$!
echo "${PID}" > "${PIDFILE}"
echo "PID ${PID}. Logs: ${LOG} (pidfile ${PIDFILE})"
