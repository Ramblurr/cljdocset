{
  description = "a tool to generate a docset from the cljdoc of a Clojure project";
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # tracks nixpkgs unstable branch
    devenv.url = "github:ramblurr/nix-devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    clj-nix.url = "github:jlesquembre/clj-nix";
    clj-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    {
      self,
      devenv,
      clj-nix,
      ...
    }@inputs:
    devenv.lib.mkFlake ./. {
      inherit inputs;
      packages = {
        babashka =
          pkgs:
          clj-nix.packages.${pkgs.system}.mkBabashka {
            withFeatures = [
              "jdbc"
              "sqlite"
            ];
          };
        default =
          pkgs: pkgs.callPackage ./package.nix { babashka = self.packages.${pkgs.system}.babashka; };
      };

      app = pkgs: {
        meta.description = "a tool to generate a docset from the cljdoc of a Clojure project";
        type = "app";
        program = "${self.packages.${pkgs.system}.default}/bin/cljdocset";
      };
      devShell.packages =
        pkgs: with pkgs; [
          clj-kondo
          cljfmt
          git
          self.packages.${pkgs.system}.babashka
        ];

      treefmtConfig = {
        programs = {
          nixfmt.enable = true;
          cljfmt.enable = true;
        };
        settings.global.excludes = [ ".clj-kondo/**" ];
      };
    };
}
