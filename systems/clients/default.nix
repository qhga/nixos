# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, user, dotf, ... }:

{
  # Extra options
  nix.settings = {
    # Used for https://github.com/nix-community/nix-direnv
    keep-outputs = true;
    keep-derivations = true;
  };

  # NFS shares
  fileSystems."/home/${user}/shares/naz" = {
    device = "10.10.10.3:/";
    fsType = "nfs";
    options = [ "x-systemd.automount" "noauto" "x-systemd.idle-timeout=3600" ];
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.config = {
    allowUnfree = true;
  };

  fonts.fonts = with pkgs; [ hack-font unifont unifont_upper freefont_ttf font-awesome ];

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    vim
    # Utils
    curl inetutils at libnotify
    # Filesystem
    ntfs3g exfat
    # Theme
    adwaita-qt
    gnome.adwaita-icon-theme
    feh
  ];

  environment.etc = {
    "xdg/gtk-2.0/gtkrc".text = ''
        gtk-theme-name = "Adwaita-dark"
        gtk-icon-theme-name = "Adwaita"
      '';

    "xdg/gtk-3.0/settings.ini".text = ''
        [Settings]
        gtk-theme-name = Adwaita-dark
        gtk-application-prefer-dark-theme = true
        gtk-icon-theme-name = Adwaita
      '';
  };

  qt5 = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs = {
    dconf.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "qt";
    };
    git = {
      enable = true;
      config = {
        init = {
          defaultBranch = "main";
        };
        user = {
          name = "phga";
          email = "phga@posteo.de";
          signingKey = "5249548AA705F019";
        };
        commit.gpgsign = true;
      };
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
      displayManager = {
        defaultSession = "none+bspwm";
        lightdm.enable = true;
        lightdm.greeters = {
          mini = {
            enable = true;
            extraConfig = ''
                [greeter]
                user = phga
                password-alignment = left
                password-label-text = Yubikey | Password:

                [greeter-hotkeys]
                mod-key = meta
                restart-key = R
                shutdown-key = S
                suspend-key = U
                hibernate-key = H
                session-key = s

                [greeter-theme]
                background-image = ""
                text-color = "#C3C9E9"
                error-color = "#ED474A"
                background-color = "#020E18"
                window-color = "#0D1F2D"
                border-color = "#1E3141"
                password-color = "#FDAA3A"
                password-background-color = "#0D1F2D"
                password-border-color = "#1E3141"
            '';
          };
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
    udev.extraRules = ''
        ACTION=="add",SUBSYSTEM=="usb",ATTRS{idVendor}=="056a",ATTRS{idProduct}=="033e",RUN+="${pkgs.bash}/bin/bash ${dotf}/config/wacom/at.sh"
      '';
    atd.enable = true;
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

  xdg.mime.defaultApplications = {
    "text/html" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/about" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/unknown" = "org.qutebrowser.qutebrowser.desktop";
  };

  virtualisation = {
    docker.enable = true;
    virtualbox.host.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.phga = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "vboxusers" ];
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
  networking.nameservers = [ "10.10.10.2" "1.1.1.1" ];

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
