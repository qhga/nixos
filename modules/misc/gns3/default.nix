{ config, pkgs, user, dotf, ... }:
{
  environment.systemPackages = with pkgs; [
    ubridge gns3-server dynamips gns3-gui vpcs wireshark
  ];

  users.users.phga.extraGroups = [ "ubridge" ];

  # Required for ubridge to work with gns3
  security.wrappers.ubridge = {
    source = "${pkgs.ubridge}/bin/ubridge";
    owner = "root";
    group = "root";
    capabilities = "cap_net_admin,cap_net_raw=ep";
    permissions = "u+rx,g+rx,o+x";
  };

  virtualisation.libvirtd.enable = true;

}