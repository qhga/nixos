{
  description = "NixOS configuration of phga";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, home-manager, ... }:
    let
      user = "phga";
      dotf = "/home/${user}/.dotfiles";
    in {
      nixosConfigurations = {
        dotfiles = builtins.toString ./.;
        hisoka = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./system/configuration.nix
            home-manager.nixosModules.home-manager {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.phga = ./users/phga/home.nix;
                extraSpecialArgs = { inherit user dotf; };
              };
            }
          ];
	        specialArgs = {
       	    inherit user dotf;
          };
        };
      };
    };
}
