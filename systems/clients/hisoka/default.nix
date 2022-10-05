{ config, lib, pkgs, user, dotf, ... }:

{

  imports = [
    ./hardware-configuration.nix
  ];

  # To use the latest kernel
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  # To pin the kernel to a specific version
  # The kernel packages are here: https://cdn.kernel.org/pub/linux/kernel
  # The sha256 can be found here: https://cdn.kernel.org/pub/linux/kernel/v5.x/sha256sums.asc
  # I found the mirror url here:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchurl/mirrors.nix#L132
  # This could take some time since it seems like we compile the kernel ourself
  # Approximately 40 minutes on my current workstation
  boot.kernelPackages = pkgs.linuxPackagesFor (pkgs.linux_5_19.override {
    argsOverride = rec {
      src = pkgs.fetchurl {
        url = "mirror://kernel/linux/kernel/v5.x/linux-${version}.tar.xz";
        sha256 = "616308795a952a6a39b4c74807c33916850eb7166d8ed7c9a87a1ba55d7487ce";
      };
      version = "5.19.8";
      modDirVersion = "5.19.8";
    };
  });

  # Define Hostname
  networking.hostName = "hisoka";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  hardware = {
    opengl.enable = true;
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
    cpu.intel.updateMicrocode = true;
  };

  # Cuda support !THIS RECOMPILES ANYTHING WITH CUDA SUPPORT!
  # nixpkgs.config = {
  #   cudaSupport = true;
  # };

  # nixpkgs.overlays = [ blender-bin.overlay ];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs = {
    steam.enable = true;
  };

  environment.systemPackages = with pkgs; [
    legendary-gl
  ];

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}