# Set default location of a temp dir to save devshell profiles to
set -q PROFILES_FOR_DEVSHELLS || set PROFILES_FOR_DEVSHELLS $(mktemp -d -t devshells-XXXXXXXXXX)
# get the names of devshells
set t $( nix flake show --json | jq -r --arg cur_sys "$CURRENT_SYSTEM" '.devShells[$cur_sys]|(try keys[] catch "")' )


if test -n "$t"
    # save profiles for these devshells
    printf "%s\n" $t | xargs -I {} nix develop .#{} --profile $PROFILES_FOR_DEVSHELLS/{}
    # push profiles for these devshells
    printf "%s\n" $t | xargs -I {} cachix push $CACHIX_CACHE $PROFILES_FOR_DEVSHELLS/{}
end

# remove the temporary dir
rm -rf $PROFILES_FOR_DEVSHELLS