{ ... } @ args:
let
  deterload = import (let
    owner = "OpenXiangShan";
    repo = "Deterload";
    rev = "d55d11d11a524fc78a262ba0a4121010e75881c5";
    sha256 = "02arflm10d96xsy2mz9gmbld7jp53jawkp128mcgkb5lxns7ny5x";
  in builtins.fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    inherit sha256;
  }) args;
in {
  inherit (deterload) spec2006 openblas;
}
