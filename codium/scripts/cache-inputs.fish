CACHIX_CACHE=$1

nix flake archive --json | \
    jq -r '.path,(.inputs|to_entries[].value.path)' | \
    cachix-wrapped push $CACHIX_CACHE