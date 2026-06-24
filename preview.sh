#!/usr/bin/env bash
# Preview the site locally with live reload.
# Requires Quarto (not installed on this machine): https://quarto.org/docs/get-started/
set -euo pipefail
cd "$(dirname "$0")"
quarto preview .
