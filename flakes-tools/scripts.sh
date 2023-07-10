# Set default location of a temp dir to save devshell profiles to

CURRENT_SYSTEM="$(nix eval --impure --raw --expr 'builtins.currentSystem')"
RANDOM_CACHE_NAME="$(basename "$(mktemp -t cache.XXXXXXXXX)")"
CACHE_DIRECTORY="${CACHE_DIRECTORY:-"/nix/var/nix/profiles/cache"}"
NIX_CACHE_PROFILE="${NIX_CACHE_PROFILE:-"$CACHE_DIRECTORY/$RANDOM_CACHE_NAME"}"

mkdir -p "$(dirname "$NIX_CACHE_PROFILE")"

saveDevshells () {
    doPushToCachix="$1"

    # get the names of devshells
    t="$( nix flake show --json \
            | jq -r --arg cur_sys "$CURRENT_SYSTEM" '.devShells[$cur_sys]|(try keys[] catch "")' \
            | xargs -I {} printf "devShells.$CURRENT_SYSTEM.{}\n" )"

    # save profiles for these devshells so that they're not garbage collected
    printf "%s\n" $t | xargs -I {} nix profile install .#{} --profile "$NIX_CACHE_PROFILE"
    
    if [ "$doPushToCachix" = true ] ; then
        # push profiles for these devshells
        printf "%s\n" $t | xargs -I {} cachix push "$CACHIX_CACHE" "$NIX_CACHE_PROFILE"
    fi
}

saveInputs () {
    doPushToCachix="$1"

    # get inputs
    t="$( nix flake archive --json | jq -r '.path,(.inputs|to_entries[].value.path)' )"

    # save profiles for these devshells so that they're not garbage collected
    printf "%s\n" $t | xargs -I {} nix profile install {} --profile "$NIX_CACHE_PROFILE"

    if [ "$doPushToCachix" = true ]; then
        # push inputs
        printf "%s\n" $t | cachix push "$CACHIX_CACHE"
    fi
}

savePackages () {
    doPushToCachix="$1"

    t="$( nix flake show --json \
            | jq -r --arg cur_sys "$CURRENT_SYSTEM" '.packages[$cur_sys]|(try keys[] catch "")' \
            | xargs -I {} printf "packages.$CURRENT_SYSTEM.{}\n" )"

    printf "%s\n" "$t" | xargs -I {} nix profile install .#{} --profile "$NIX_CACHE_PROFILE"

    if [ "$doPushToCachix" = true ] ; then
        printf "%s\n" "$t" | xargs -I {} nix path-info --recursive .#{} | cachix push "$CACHIX_CACHE"
    fi
}

saveAll () {
    doPushToCachix="$1"
    saveDevshells "$doPushToCachix"
    saveInputs "$doPushToCachix"
    savePackages "$doPushToCachix"
}