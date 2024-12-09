# pin nixpkgs to latest 24.11
{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/4dc2fc4e62dbf62b84132fe526356fbac7b03541.tar.gz";
    sha256 = "10sm236ix9v0qaih2pvfdzw8vjg1w4z11fjkkrzknn4x98gnaa8n";
  }) {}
, owner ? "OpenXiangShan-Nanhu"
, repo ? "Nanhu-V5"
, spec2006-src ? throw ''
    Please specify the <spec2006-src>:
      nix-build deterunner.nix --arg spec2006-src <spec2006-src>
  ''
, github_token ? throw ''
    Please specify the <github_token> that accessing ${repo} repo,
    <github_token> beginning with github_pat_..., the command like below:
      nix-build deterunner.nix --argstr github_token <github_token>
    To generate the github token, click here:
      https://github.com/settings/tokens?type=beta
    More info about how github token works, see:
      https://docs.github.com/en/rest/actions/self-hosted-runners?apiVersion=2022-11-28#create-a-registration-token-for-a-repository
      > The fine-grained token must have the following permission set:
      > "Administration" repository permissions (write)
  ''
}: let
  ccache-dir = "/tmp/${repo}/ccache";
  deterunner = import (pkgs.fetchFromGitHub {
    owner = "xieby1";
    repo = "Deterunner";
    rev = "d3683a8d905814c9249dc0d2657ba8c8bdc5b221";
    hash = "sha256-/0ITNS2cPbBMsqECqE723vA02ksMj38mCTCZg0zZMD0=";
  }) {
    inherit pkgs;
    extraPodmanOpts = [
      "-v ${spec2006-src}:/${builtins.baseNameOf spec2006-src}:ro"
      "-v ${ccache-dir}:/ccache"
    ];
    extraPkgsInPATH = [pkgs.git];
  };
  run-ephemeral = pkgs.writeShellScriptBin "deterunner-${repo}" ''
    mkdir -p ${ccache-dir}
    resp=$(curl -L \
      -X POST \
      -H "Accept: application/vnd.github+json" \
      -H "Authorization: Bearer ${github_token}" \
      -H "X-GitHub-Api-Version: 2022-11-28" \
      Https://api.github.com/repos/${owner}/${repo}/actions/runners/registration-token)
    # https://unix.stackexchange.com/questions/13466/can-grep-output-only-specified-groupings-that-match
    echo resp=$resp
    runner_token=$(echo $resp | grep -oP '"token":\s*"\K[^"]*')
    echo runner_token=$runner_token
    ${deterunner} \
      --labels 'self-hosted,Linux,X64,nix,spec2006,ccache' \
      --ephemeral \
      --url https://github.com/${owner}/${repo} \
      --token $runner_token
  '';
in run-ephemeral
