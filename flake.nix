# noether.el --- A modeline which plays hide and seek  -*- lexical-binding: t; -*-
#
# Copyright (c) 2023-2025 Sameer Rahmani <lxsameer@gnu.org>
#
# Author: Sameer Rahmani <lxsameer@gnu.org>
# URL: https://devheroes.codes/lxsameer/noether
# Version: 0.1.0
# Keywords: frames, modeline
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
{
  description = "A frame work to create frames that updates based on certain events";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/442d407992384ed9c0e6d352de75b69079904e4e";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs = { self, nixpkgs, flake-parts, ... }@inputs: flake-parts.lib.mkFlake { inherit inputs; } (
    {
      systems = [
        "aarch64-darwin"
        "riscv64-linux"
        "riscv32-linux"
        "x86_64-linux"
        "x86_64-windows"
      ];

      perSystem = { config, pkgs, system, ... }:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ inputs.emacs-overlay.overlays.package ];
          };
          emacs = pkgs.emacs29.override
            { } // (pkgs.lib.optionalAttrs (pkgs.stdenv.isLinux) {
            # Gtk causes a flickering issue on WM mode
            withGTK3 = false;
            toolkit = "lucid";
          });


          noether = pkgs.emacsPackages.trivialBuild {
            pname = "noether";
            version = "0.3.0";
            buildInputs = [ pkgs.emacsPackages.posframe ];

            src = ./.;
          };

          emacsPkgs = (pkgs.emacsPackagesFor emacs).withPackages (epkgs: [
            epkgs.posframe
            noether
          ]);

          test-template = ./test-noether.el.in;

          test-noether = pkgs.writeShellApplication {
            name = "test-noether";
            runtimeInputs = [ emacsPkgs ];

            text = ''
              ${emacsPkgs}/bin/emacs -Q -l ${test-template} "$@"
            '';
          };

        in
        {
          packages.default = noether;
          devShells.default = pkgs.mkShell {
            nativeBuildInputs = [ noether emacsPkgs test-noether ];
            buildInputs = [ noether emacsPkgs ];
          };

          apps.default = {
            type = "app";
            program = "${test-noether}/bin/test-noether";
          };
        };
    });
}
