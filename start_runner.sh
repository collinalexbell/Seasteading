#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOCK="${SOCK:-/tmp/ship_runner.sock}"
HZ="${HZ:-20}"
PIDFILE="${PIDFILE:-${ROOT}/runner.pid}"
LOGFILE="${LOGFILE:-${ROOT}/runner.log}"

cd "${ROOT}"
if [[ -S "${SOCK}" ]]; then
  rm -f "${SOCK}"
fi

SHIP_RUNNER_SOCK="${SOCK}" SHIP_RUNNER_HZ="${HZ}" nohup node "${ROOT}/ship-runner.js" >"${LOGFILE}" 2>&1 &
echo $! > "${PIDFILE}"
echo "[runner] started pid $(cat "${PIDFILE}") log ${LOGFILE} sock ${SOCK}"
