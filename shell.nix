#
# Initiate the environment by running:
#
# nix-shell --pure shell.nix
#
# Then run the Linux build instructions. If you omit `--pure` above,
# problems may arise, such as a different compiler (e.g. clang)
# being used from the ambient environment.
#

with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "felix-lang";
  buildInputs = [
    gcc
    git
    gmp
    ocaml-ng.ocamlPackages_4_06.ocaml
    python36Full
    SDL2
  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib
    export PATH=$PATH:$(pwd)/build/release/host/bin/
    export FLX_INSTALL_DIR=$PWD/build/release
    #
    # So far my attempts at getting the build script to recognize
    # sdl-config are not working (I also tried an alias).
    # The package db needs to be modified; see discussion on
    # https://github.com/felix-lang/felix/pull/126
    # 
  '';
}
