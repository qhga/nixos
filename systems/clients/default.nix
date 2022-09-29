# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, user, dotf, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.config.allowUnfree = true;

  fonts.fonts = with pkgs; [ hack-font unifont unifont_upper freefont_ttf font-awesome ];

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    vim git
    # Utils
    curl inetutils
    # Filesystem
    ntfs3g exfat
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "qt";
    };
  };


  location = {
    provider = "manual";
    latitude = 48.7;
    longitude = 11.6;
  };

  # Enable the X11 windowing system.
  services = {
    picom.enable = true; # Default settings are sensible
    blueman.enable = true;
    redshift = {
      enable = true;
      temperature = {
        day = 5700;
        night = 3450;
      };
    };
    xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "caps:escape";
      videoDrivers = [ "nvidia" ];
      displayManager = {
        defaultSession = "none+bspwm";
        lightdm.enable = true;
        lightdm.greeters = {
          mini.enable = true;
          mini.user = "phga";
          gtk.cursorTheme = {
            name = "Bibata-Original-Amber";
            package = pkgs.bibata-cursors;
            size = 16;
          };
        };
      };
      windowManager.bspwm.enable = true;
      wacom.enable = true;
    };
    syncthing = {
      enable = true;
      inherit user;
      dataDir = "/home/${user}";
      configDir = "/home/${user}/.config/syncthing";
    };
    pipewire = {
      enable = true;
      wireplumber.enable = true;
      pulse.enable = true;
      jack.enable = true;
      alsa.enable = true;
    };
  };

  virtualisation.docker.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.phga = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ];
  };

  security = {
    # PAM configuration for yubikey
    # Run scripts/post-install to generate hmac secrets in ~/.yubico/
    pam = {
      yubico = {
        enable = true;
        mode = "challenge-response";
        control = "sufficient";
      };
    };
    # Sudo timeout
    sudo = {
      extraConfig = "Defaults:${user} timestamp_timeout=30\n";
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;
  networking.nameservers = [ "1.1.1.1" "10.10.10.2" ];

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