((import <nixpkgs> {}).haskellPackages.override {
  overrides = self: super: {
        my-pkg = let
          buildDepends = with self; [
            protolude
            hnix
            ansi-wl-pprint
            data-fix
          ];
          in super.mkDerivation {
            pname = "pkg-env";
            src = "/dev/null";
            version = "none";
            license = "none";
            inherit buildDepends;
            buildTools = with self; [
              ghcid
              cabal-install
              hpack
              (hoogleLocal {
                packages = buildDepends;
              })
            ];
          };
      };
}).my-pkg.env

