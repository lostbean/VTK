#!/usr/bin/env bash
set -euo pipefail

echo "=== Formatting ==="
nix fmt

echo ""
echo "=== Build (-Wall -Werror) ==="
nix develop --command cabal build --allow-newer --ghc-options="-Wall -Werror"

echo ""
echo "No test suite — VTK is integration-tested by downstream packages."

echo ""
echo "=== All Checks Passed ==="
