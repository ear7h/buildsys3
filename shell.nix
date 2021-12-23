with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "buildsys3";

  buildInputs = [ zlib.dev ];

  # set the LD_LIBRARY_PATH so that the program can find wayland gui libs
  shellHook = ''
    exec zsh
  '';
}

