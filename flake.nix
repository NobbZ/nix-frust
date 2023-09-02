# SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
#
# SPDX-License-Identifier: CC0-1.0
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";

    rust-overlay.url = "github:oxalica/rust-overlay";

    parts.url = "github:hercules-ci/flake-parts";

    cargo2nix.url = "github:cargo2nix/cargo2nix";
    cargo2nix.inputs.nixpkgs.follows = "nixpkgs";
    cargo2nix.inputs.rust-overlay.follows = "rust-overlay";
  };

  outputs = {parts, ...} @ inputs:
    parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux"];

      perSystem = {
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        pkgsWithOverlays = (inputs.nixpkgs.legacyPackages.${system}.extend inputs.rust-overlay.overlays.default).extend inputs.cargo2nix.overlays.default;
        rustVersion = "1.71.0";
        rust = pkgs.rust-bin.stable.${rustVersion}.default;
      in {
        _module.args.pkgs = pkgsWithOverlays;

        formatter = pkgs.alejandra;

        # legacyPackages.nix-frustPkgsBuilder = pkgs.rustBuilder.makePackageSet {
        #   inherit rustVersion;
        #   packageFun = import "${inputs.self}/Cargo.nix";
        # };

        legacyPackages.helpers.testrunner = pkgs.writeShellScriptBin "testrunner" ''
          ${pkgs.inotify-tools}/bin/inotifywait -m -r -e close_write,moved_to --format '%w%f' src | \
            while read dir action file; do
              cargo test --release -- --format=terse
              cargo doc
            done
        '';

        # packages.nix-frust = (self'.legacyPackages.janitorPkgsBuilder.workspace.janitor {}).bin;
        # packages.default = self'.packages.nix-frust;

        devShells.default = pkgs.mkShell {
          packages = builtins.attrValues {
            inherit (pkgs) cargo-nextest cargo-audit cargo-deny cargo-tarpaulin rust-analyzer;
            inherit (pkgs) nil pre-commit reuse;
            inherit (self'.legacyPackages.helpers) testrunner;
            inherit (inputs'.cargo2nix.packages) cargo2nix;
            inherit rust;
          };
        };
      };
    };
}
