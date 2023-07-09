# Set default location of a temp dir to save devshell profiles to

CURRENT_SYSTEM="${CURRENT_SYSTEM:-}"

save-devshells () {
    doPushToCachix="$1"

    PROFILES_FOR_DEVSHELLS="${PROFILES_FOR_DEVSHELLS:-$(mktemp -d -t devshells-XXXXXXXXXX)}"
    mkdir -p $PROFILES_FOR_DEVSHELLS

    # get the names of devshells
    t="$( nix flake show --json \
            | jq -r --arg cur_sys "$CURRENT_SYSTEM" '.devShells[$cur_sys]|(try keys[] catch "")' \
            | xargs -I {} printf "devShells.$CURRENT_SYSTEM.{}\n" )"

    # save profiles for these devshells so that they're not garbage collected
    printf "%s\n" $t | xargs -I {} nix profile install .#{} --profile "$PROFILES_FOR_DEVSHELLS/{}"
    
    if [ "$doPushToCachix" = true ] ; then
        # push profiles for these devshells
        printf "%s\n" $t | xargs -I {} cachix push "$CACHIX_CACHE" "$PROFILES_FOR_DEVSHELLS/{}"
    fi
}

save-inputs () {
    doPushToCachix="$1"

    # get inputs
    t="$( nix flake archive --json | jq -r '.path,(.inputs|to_entries[].value.path)' )"
    
    PROFILES_FOR_INPUTS="${PROFILES_FOR_INPUTS:-$(mktemp -d -t inputs-XXXXXXXXXX)}"

    # save profiles for these devshells so that they're not garbage collected
    printf "%s\n" $t | xargs -I {} nix profile install {} --profile "$PROFILES_FOR_INPUTS/{}"

    if [ "$doPushToCachix" = true ]; then
        # push inputs
        printf "%s\n" $t | cachix push "$CACHIX_CACHE"
    fi
}

save-packages () {
    doPushToCachix="$1"

    # set temp directory for outputs for packages
    PROFILES_FOR_PACKAGES="${PROFILES_FOR_PACKAGES:-$(mktemp -d -t packages-XXXXXXXXXX)}"

    t="$( nix flake show --json \
            | jq -r --arg cur_sys "$CURRENT_SYSTEM" '.packages[$cur_sys]|(try keys[] catch "")' \
            | xargs -I {} printf "packages.$CURRENT_SYSTEM.{}\n" )"

    printf "%s\n" "$t" | xargs -I {} nix profile install .#{} --profile "$PROFILES_FOR_PACKAGES/{}"

    if [ "$doPushToCachix" = true ] ; then
        printf "%s\n" "$t" | xargs -I {} nix path-info --recursive .#{} | cachix push "$CACHIX_CACHE"
    fi
}

save-all () {
    doPushToCachix="$1"
    save-devshells "$doPushToCachix"
    save-inputs "$doPushToCachix"
    save-packages "$doPushToCachix"
}