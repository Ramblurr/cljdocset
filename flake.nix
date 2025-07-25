{
  description = "a tool to generate a docset from the cljdoc of a Clojure project";
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
        babashka =
          pkgs:
          clj-nix.packages.${pkgs.system}.mkBabashka {
            withFeatures = [
              "jdbc"
              "sqlite"
            ];
          };
        cljdocset =
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
          #self.packages.${pkgs.system}.cljdocset
          self.packages.${pkgs.system}.babashka
        ];

      flakelight.builtinFormatters = false;
      formatters = pkgs: {
        "*.nix" = "${pkgs.nixfmt}/bin/nixfmt";
        "*.clj" = "${pkgs.cljfmt}/bin/cljfmt fix";
      };
    };
}
