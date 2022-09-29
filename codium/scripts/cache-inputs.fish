nix flake archive --json | \
    jq -r '.path,(.inputs|to_entries[].value.path)' | \
    cachix push $CACHIX_CACHE