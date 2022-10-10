{
  description = "NixOS configuration of phga";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pianoteq.url = "github:qhga/nix-pianoteq7";
  };

  outputs = inputs@{ nixpkgs, home-manager, pianoteq, ... }:
    let
      user = "phga";
      dotf = "/home/${user}/.dotfiles";
    in {
      nixosConfigurations = {
        hisoka = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./systems
            ./systems/clients
            ./systems/clients/hisoka
            home-manager.nixosModules.home-manager {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.phga = ./users/phga;
                extraSpecialArgs = { inherit user dotf; };
              };
            }
          ];
	        specialArgs = {
       	    inherit user dotf pianoteq;
          };
        };
      };
    };
}
