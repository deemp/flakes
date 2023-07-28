src: (import (import ./source-flake).outputs.flake-compat { inherit src; }).defaultNix
