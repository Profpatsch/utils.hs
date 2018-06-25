((import <nixpkgs> {}).haskellPackages.override {
  overrides = self:
    super:
      {
        my-pkg = let
          buildDepends = with self;
          [
            base
            directory
            filepath
            fsnotify
            optparse-generic
            process
            protolude
            regex-tdfa
          ];
        in super.mkDerivation {
          pname = "pkg-env";
          src = "/dev/null";
          version = "none";
          license = "none";
          inherit buildDepends;
          buildTools = with self;
          [
            ghcid
            cabal-install
            hpack
            hscolour
            (hoogleLocal {
              packages = buildDepends;
            })
          ];
        };
      };
}).my-pkg.env
