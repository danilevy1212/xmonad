{ ghc }: with (import <nixpkgs> {});

# FIXME https://stackoverflow.com/questions/27713707/nix-shell-how-to-specify-a-custom-environment-variable
# Use `XMONAD_CONFIG_HOME` and etc... https://wiki.archlinux.org/index.php/XDG_Base_Directory.

haskell.lib.buildStackProject {
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
