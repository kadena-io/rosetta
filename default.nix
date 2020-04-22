{ kpkgs ? import ./deps/kpkgs {}
}:
let pkgs = kpkgs.pkgs;
    haskellPackages = kpkgs.rp.ghc8_6;
in
haskellPackages.developPackage {
  name = builtins.baseNameOf ./.;
  root = kpkgs.gitignoreSource ./.;

  overrides = self: super: with pkgs.haskell.lib; {
    # Don't run a package's test suite
    # foo = dontCheck super.foo;
    #
    # Don't enforce package's version constraints
    # bar = doJailbreak super.bar;
    #
    # Get a specific hackage version straight from hackage. Unlike the above
    # callHackage approach, this will always succeed if the version is on
    # hackage. The downside is that you have to specify the hash manually.
    # aeson = callHackageDirect {
    #   pkg = "aeson";
    #   ver = "1.4.2.0";
    #   sha256 = "0qcczw3l596knj9s4ha07wjspd9wkva0jv4734sv3z3vdad5piqh";
    # } {};
    #
    # To discover more functions that can be used to modify haskell
    # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
    # <TAB> to get a tab-completed list of functions.
  };
  source-overrides = {
    # Use a specific hackage version using callHackage. Only works if the
    # version you want is in the version of all-cabal-hashes that you have.
    # bytestring = "0.10.8.1";
    #
    # Use a particular commit from github
    # parsec = pkgs.fetchFromGitHub
    #   { owner = "hvr";
    #     repo = "parsec";
    #     rev = "c22d391c046ef075a6c771d05c612505ec2cd0c3";
    #     sha256 = "0phar79fky4yzv4hq28py18i4iw779gp5n327xx76mrj7yj87id3";
    #   };
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
  });
}
