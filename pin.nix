let

  nixpkgsPath = builtins.fetchTarball {
    # nixpkgs 2019-03-01
    url = "https://github.com/NixOS/nixpkgs/archive/81bf897e294a27b59b73f6a76942b1405923e99e.tar.gz";
    sha256 = "03syn2jrwrpb5ghvz3y1wa5qrkpbxyy99q903qq62l27idmzxg4n";
  };

  vuizvuiPath = builtins.fetchTarball {
    # vuizvui 2019-03-03
    url = "https://github.com/openlab-aux/vuizvui/archive/5f3a96fff93f7c995d25ef01589fd3d36c659cf1.tar.gz";
    sha256 = "184850zy03d4nz9jrf4l9c7kcqx6ryf9i39az4l8d32k71h50r7x";
  };

  vuizvuiPkgs = import "${toString vuizvuiPath}/pkgs" {
    pkgs = import (toString nixpkgsPath) {};
  };

in vuizvuiPkgs
