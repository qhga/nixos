# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, user, dotf, ... }:

{
  # Enable flakes & auto gc after 15 days
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 15d";
    };
  };


  imports = [
    ./hardware-configuration.nix
    # ../modules/editors/emacs
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # To use the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # To pin the kernel to a specific version
  # The kernel packages are here: https://cdn.kernel.org/pub/linux/kernel
  # The sha256 can be found here: https://cdn.kernel.org/pub/linux/kernel/v5.x/sha256sums.asc
  # I found the mirror url here:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchurl/mirrors.nix#L132
  # This could take some time since it seems like we compile the kernel ourself
  # Approximately 40 minutes on my current workstation
  # boot.kernelPackages = pkgs.linuxPackagesFor (pkgs.linux_5_19.override {
  #   argsOverride = rec {
  #     src = pkgs.fetchurl {
  #       url = "mirror://kernel/linux/kernel/v5.x/linux-${version}.tar.xz";
  #       sha256 = "616308795a952a6a39b4c74807c33916850eb7166d8ed7c9a87a1ba55d7487ce";
  #     };
  #     version = "5.19.8";
  #     modDirVersion = "5.19.8";
  #   };
  # });

  # Define Hostname
  # networking.hostName = "hisoka";
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };
  nixpkgs.config.allowUnfree = true;

  services = {
  };

  virtualisation.docker.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.phga = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ];
  };

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    vim git curl pass
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;
  networking.nameservers = [ "1.1.1.1" "8.8.8.8" ];

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
