{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  inherit ghc;
  name = "xmonad";
  buildInputs = [
    xorg.libX11
    xorg.libXext
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXrender
    xorg.libXScrnSaver
    xorg.libXft
    pkgconfig
  ];
}
