{ pkgs ? import <nixpkgs> {}
, owner ? "OpenXiangShan-Nanhu"
, repo ? "Nanhu-V5"
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
  deterunner = import (pkgs.fetchFromGitHub {
    owner = "xieby1";
    repo = "Deterunner";
    rev = "346cec0cbade8f00de49c5b41806872ea7070249";
    hash = "sha256-m/IzUZuktg3f8UmaeazBQvBBwT7WJA+RkRPcsmsE8rQ=";
  }) {
    inherit pkgs;
    extraPkgsInPATH = [pkgs.git];
  };
  run-ephemeral = pkgs.writeShellScriptBin "deterunner-${repo}" ''
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
      --labels 'self-hosted,Linux,X64,nix' \
      --ephemeral \
      --url https://github.com/${owner}/${repo} \
      --token $runner_token
  '';
in run-ephemeral
