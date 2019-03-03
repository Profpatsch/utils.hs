let
  vuizvuiPkgs = import ../pin.nix;

in vuizvuiPkgs.profpatsch.haskellPackages.shellFor (haskellPkgs: {
  buildDepends = with haskellPkgs; [
    protolude
    hnix
    prettyprinter
    data-fix
  ];
})
