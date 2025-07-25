{
  description = "cljdocset - A tool to generate a cljdoc set from a Clojure project";
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # tracks nixpkgs unstable branch
    flakelight.url = "github:nix-community/flakelight";
    clj-nix.url = "github:jlesquembre/clj-nix";
    clj-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    {
      self,
      flakelight,
      clj-nix,
      ...
    }:
    flakelight ./. {
      packages = {
        bb =
          pkgs:
          clj-nix.packages.${pkgs.system}.mkBabashka {
            withFeatures = [
              "jdbc"
              "sqlite"
            ];
          };
        cljdocset =
          pkgs:
          pkgs.writeShellApplication (
            let
              script = ./spdx;
            in
            {
              name = "spdx";
              runtimeInputs = [
                pkgs.babashka
                pkgs.git
              ];
              text = ''
                exec ${pkgs.babashka}/bin/bb ${script} $@
              '';
              checkPhase = ''
                ${pkgs.clj-kondo}/bin/clj-kondo --config '{:linters {:namespace-name-mismatch {:level :off}}}' --lint ${script}
              '';
            }
          );
      };

      app = pkgs: {
        meta.description = "cljdocset - A tool to generate a cljdoc set from a Clojure project";
        type = "app";
        program = "${self.packages.${pkgs.system}.default}/bin/spdx";
      };
      devShell.packages =
        pkgs: with pkgs; [
          clj-kondo
          babashka
          git
          self.packages.${pkgs.system}.default
        ];

      flakelight.builtinFormatters = false;
      formatters = pkgs: {
        "*.nix" = "${pkgs.nixfmt}/bin/nixfmt";
        "*.clj" = "${pkgs.cljfmt}/bin/cljfmt fix";
      };
    };
}
