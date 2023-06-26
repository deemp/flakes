# Set default location of a temp dir to save devshell profiles to
set -q PROFILES_FOR_DEVSHELLS || set PROFILES_FOR_DEVSHELLS $(mkdir tmp && mktemp -d -t devshells-XXXXXXXXXX -p tmp)
mkdir -p $PROFILES_FOR_DEVSHELLS

# get the names of devshells
set t $( nix flake show --json | jq -r --arg cur_sys "$CURRENT_SYSTEM" '.devShells[$cur_sys]|(try keys[] catch "")' )

if test -n "$t";
    # build devshells
    printf "%s\n" $t | xargs -I {} nix build --profile $PROFILES_FOR_DEVSHELLS/{} .#{}
    printf "%s\n" $t | xargs -I {} nix develop --profile $PROFILES_FOR_DEVSHELLS/{} .#{}
end