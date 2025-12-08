#!/usr/bin/env bash
set -euo pipefail

PORT=${PORT:-8000}
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="${ROOT}/server.log"

echo "Starting server on port ${PORT} from ${ROOT} ..."
cd "${ROOT}"
nohup python -m http.server "${PORT}" --bind 0.0.0.0 >>"${LOG}" 2>&1 &
PID=$!
echo "PID ${PID}. Logs: ${LOG}"
