let
  name = "Nanhu-V5";
  # latest 24.05 nixpkgs
  pkgs = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "190c31a89e5eec80dd6604d7f9e5af3802a58a13";
    hash = "sha256-K5DJ2LpPqht7K76bsxetI+YHhGGRyVteTPRQaIIKJpw=";
  }) {};
  ccache_dir = toString ./. + "/.ccache";
  ccache14Stdenv = pkgs.ccacheStdenv.override {
    stdenv = pkgs.gcc14Stdenv;
    extraConfig = ''
      export CCACHE_COMPRESS=1
      export CCACHE_DIR="${ccache_dir}"
      export CCACHE_UMASK=007
      if [ ! -d "$CCACHE_DIR" ]; then
        echo "====="
        echo "Directory '$CCACHE_DIR' does not exist"
        echo "Please create it with:"
        echo "  mkdir -m0770 '$CCACHE_DIR'"
        echo "====="
        exit 1
      fi
      if [ ! -w "$CCACHE_DIR" ]; then
        echo "====="
        echo "Directory '$CCACHE_DIR' is not accessible for user $(whoami)"
        echo "Please verify its access permissions"
        echo "====="
        exit 1
      fi
    '';
  };
  ccacheMkShell = pkgs.mkShell.override {
    stdenv = ccache14Stdenv;
  };
in ccacheMkShell {
  inherit name;

  buildInputs = let
    h_content = builtins.toFile "h_content" ''
      # ${pkgs.lib.toUpper "${name} usage tips"}

      * `make init`
      * ``make emu -j`nproc` ``
    '';
    _h_ = pkgs.writeShellScriptBin "h" ''
      ${pkgs.glow}/bin/glow ${h_content}
    '';
    mill_0_11_8 = (import (pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "05bbf675397d5366259409139039af8077d695ce";
      sha256 = "1r26vjqmzgphfnby5lkfihz6i3y70hq84bpkwd43qjjvgxkcyki0";
    }){}).mill;
  in [
    _h_
    mill_0_11_8
    pkgs.espresso
    pkgs.verilator
    pkgs.python3
    # libs
    pkgs.sqlite
    pkgs.zlib
    pkgs.zstd
  ];

  shellHook = let
    circt_1_62_0 = (import (pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "771b079bb84ac2395f3a24a5663ac8d1495c98d3";
      sha256 = "0l1l9ms78xd41xg768pkb6xym200zpf4zjbv4kbqbj3z7rzvhpb7";
    }){}).circt;
  in ''
    h
    export CHISEL_FIRTOOL_PATH=${circt_1_62_0}/bin/
    export NOOP_HOME=$(realpath .)
    mkdir -m0770 -p ${ccache_dir}
  '';
}
