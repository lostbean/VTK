#!/usr/bin/env bash
set -euo pipefail

echo "=== Format Check ==="
nix fmt -- --ci

echo ""
echo "=== Cabal Update ==="
nix develop --command cabal update

echo ""
echo "=== Build (-Wall -Werror) ==="
nix develop --command cabal build --allow-newer --ghc-options="-Wall -Werror"

echo ""
echo "No test suite — VTK is integration-tested by downstream packages."

echo ""
echo "=== CI Passed ==="
