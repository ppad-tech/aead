{
  description = "A pure Haskell AEAD-ChaCha20-Poly1305 construction.";

  inputs = {
    ppad-base16 = {
      type = "git";
      url  = "git://git.ppad.tech/base16.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-chacha = {
      type = "git";
      url  = "git://git.ppad.tech/chacha.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-base16.follows = "ppad-base16";
    };
    ppad-poly1305 = {
      type = "git";
      url  = "git://git.ppad.tech/poly1305.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-base16.follows = "ppad-base16";
    };
    ppad-nixpkgs = {
      type = "git";
      url  = "git://git.ppad.tech/nixpkgs.git";
      ref  = "master";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs
            , ppad-base16
            , ppad-chacha, ppad-poly1305
            }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-aead";

        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib;
        llvm  = pkgs.llvmPackages_15.llvm;

        poly1305 = ppad-poly1305.packages.${system}.default;
        poly1305-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag poly1305 "llvm")
            [ llvm ];

        hpkgs = pkgs.haskell.packages.ghc981.extend (new: old: {
          ppad-base16 = ppad-base16.packages.${system}.default;
          ppad-chacha = ppad-chacha.packages.${system}.default;
          ppad-poly1305 = poly1305-llvm;
          ${lib} = new.callCabal2nixWithOptions lib ./. "--enable-profiling" {
            ppad-poly1305 = new.ppad-poly1305;
          };
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
      in
        {
          packages.default = hpkgs.${lib};

          devShells.default = hpkgs.shellFor {
            packages = p: [
              (hlib.doBenchmark p.${lib})
            ];

            buildInputs = [
              cabal
              cc
              llvm
            ];

            doBenchmark = true;

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
              echo "llc:   $(${llvm}/bin/llc --version | head -2 | tail -1)"
            '';
          };
        }
      );
}

