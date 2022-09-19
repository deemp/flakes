{
  inputs = {
    inputs.url = "github:br4ch1st0chr0n3/flakes?dir=inputs";
    nixpkgs.follows = "inputs/nixpkgs";
    flake-utils.follows = "inputs/flake-utils";
    my-codium.follows = "inputs/my-codium";
    flake-compat.follows = "inputs/flake-compat";
    hls.follows = "inputs/haskell-language-server";
    gitignore.follows = "inputs/gitignore";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , my-codium
    , flake-compat
    , hls
    , gitignore
    , inputs
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "902";
      inherit (my-codium.tools.${system})
        writeSettingsJson
        settingsNix
        codium
        toList
        shellTools
        toolsGHC
        ;
      inherit (toolsGHC ghcVersion) hls stack callCabal staticExecutable;

      # should come together
      manager = [ (staticExecutable "manager" ./manager) (pkgs.hpack) ];

      writeSettings = writeSettingsJson
        {
          inherit (settingsNix) haskell todo-tree files editor gitlens git nix-ide workbench;
        };

      tools = (
        toList {
          inherit (shellTools) nix haskell;
        }) ++ [ stack hls manager ];

      codiumWithSettings = pkgs.mkShell {
        buildInputs = [ writeSettings codium ];
        shellHook = ''
          write-settings-json
          codium .
        '';
      };
    in
    {
      devShells =
        {
          default = pkgs.mkShell {
            name = "dev-tools";
            buildInputs = tools;
            shellHook = ''
              source <(manager --bash-completion-script `which manager`)
            '';
          };

          codium = codiumWithSettings;
        };

      stack-shell = { ghcVersion }:

        pkgs.haskell.lib.buildStackProject {
          name = "nix-managed-stack-shell";

          ghc = pkgs.haskell.compiler.${ghcVersion};

          buildInputs = [
            pkgs.zlib
          ];
        };
    });

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      https://br4ch1st0chr0n3-nix-managed.cachix.org
      "https://br4ch1st0chr0n3-flakes.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "br4ch1st0chr0n3-nix-managed.cachix.org-1:sDKsfgu5fCCxNwVhZg+AWeGvbLlEtZoyzkSNKRM/KAo="
      "br4ch1st0chr0n3-flakes.cachix.org-1:Dyc2yLlRIkdbq8CtfOe24QQhQVduQaezkyV8J9RhuZ8="
    ];
  };
}
