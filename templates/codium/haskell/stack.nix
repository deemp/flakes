{ ghcVersion }: (import ./.).outputs.stack-shell.${builtins.currentSystem} { version = ghcVersion; }
