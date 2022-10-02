nix flake archive

# set temp directory for outputs for packages
set -q PATHS_FOR_PACKAGES || set PATHS_FOR_PACKAGES __paths_for_packages
set t $( nix flake show --json | jq -r --arg cur_sys "$CURRENT_SYSTEM" '.packages[$cur_sys]|(try keys[] catch "")' )


if test -n "$t";
    printf "%s\n" $t | xargs -I {} nix build --print-out-paths .#{} > $PATHS_FOR_PACKAGES
    cat $PATHS_FOR_PACKAGES | xargs -I {} nix-store -qR {} | cachix-wrapped push $CACHIX_CACHE;
    rm $PATHS_FOR_PACKAGES
end