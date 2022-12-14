#!/usr/bin/env bash
# Author: Philip Gaber <phga@posteo.de>

############################################################################
# NOTE: While testing with a VM make sure to enable EFI boot in VM options #
############################################################################

while getopts "n:d:e:s:t:" opt;
do case $opt in
       n) HNAME=${OPTARG} ;;
       d) DEV=${OPTARG} ;;
       e) ENC=${OPTARG} ;;
       s) SWAP_SIZE=${OPTARG} ;;
       t) SYSTEM_TYPE=${OPTARG} ;;
       \?) echo "-$OPTARG is not valid" >&2 && exit ;;
   esac
done

init() {
    sleep 1
    echo ""
    echo "+---------------------------+"
    echo "| Just Some Short Questions |"
    echo "+---------------------------+"
    echo ""
    # System type

    [ -z "$SYSTEM_TYPE" ] && echo -e "Which type of system is this? \n1: Client\n2: Server\n3: Testing (VM)\n" && read -p "Type the corresponding number: " SYSTEM_TYPE
    SYSTEM_FOLDER="testing" # Default
    [ "$SYSTEM_TYPE" == "1" ] && SYSTEM_FOLDER="clients"
    [ "$SYSTEM_TYPE" == "2" ] && SYSTEM_FOLDER="servers"
    [ "$SYSTEM_TYPE" == "3" ] && SYSTEM_FOLDER="testing"
    # Hostname
    [ -z "$HNAME" ] && read -p "Hostname: " HNAME
    # Show some possible disks
    [ -z "$DEV" ] && lsblk -nrpo "name,size,model" &&
        read -p "Provide installation medium (e.g. sda, nvme0n1): " DEV
    [[ "$DEV" =~ sd[a-z] ]] && SUF="1-2" && MODE="SATA"
    [[ "$DEV" =~ nvme[0-9]n[0-9] ]] && SUF="p1-2" && MODE="NVME"
    # Swap size
    [ -z "$SWAP_SIZE" ] && read -p "Specify the size for the SWAP file SIZE{M|G} (16G, 1M): " SWAP_SIZE
    SWAP_UNIT=${SWAP_SIZE: -1}
    SWAP_SIZE=${SWAP_SIZE:: -1}
    [ "$SWAP_UNIT" == "G" ] && SWAP_SIZE=$(($SWAP_SIZE * 1000))
    # Encrytion
    [ -z "$ENC" ] && read -p "Do you want the root partition to be encrypted (y/n)? " ENC
    [ "$ENC" == "y" ] && ENC=true || ENC=false

    echo "+---------------------+"
    echo "|   NixOS Bootstrap   |"
    echo "+---------------------+"
    echo    "SYSTEM      = $SYSTEM_TYPE -> $SYSTEM_FOLDER"
    echo    "HOSTNAME    = $HNAME"
    echo    "DEVICEPARTS = $DEV$SUF"
    echo    "SWAP_SIZE   = $SWAP_SIZE Megabyte"
    echo    "ENCRYPTION  = $ENC"
    read -p "Do you want to continue with these values (y/n)? " cont
    [ ! "$cont" == "y" ] && unset SYSTEM_TYPE HNAME DEV ENC SWAP_SIZE && init
    echo "Let's GOOOO"
}

crypt_prepare_disk() {
    echo "Preparing disk for encryption. This could take a while..."
    set +e
    cryptsetup open --type plain -d /dev/urandom $1 wipe_me
    dd if=/dev/zero of=/dev/mapper/wipe_me bs=1M status=progress
    cryptsetup close wipe_me
    set -e
}

crypt_create_fs() {
    cryptsetup -y -v luksFormat $1
    cryptsetup open $1 root
    mkfs.ext4 -L NIXROOT /dev/mapper/root
}

# stop on error
set -e

# initialize important values
init

[ "$ENC" == true ] && crypt_prepare_disk "/dev/$DEV"

# All values set, start bootstrapping
gdisk /dev/$DEV <<EOF
o
y
n


+1G
ef00
n



8304
w
y

EOF

[ $MODE == "NVME" ] && suffix="p" || suffix=""

if [ "$ENC" == true ]; then
    echo "MAKING CHANGES ON /dev/${DEV}${suffix}1-2"
    mkfs.fat -I -F 32 -n NIXBOOT "/dev/${DEV}${suffix}1" # -I bc of virtual mappings
    crypt_create_fs "/dev/${DEV}${suffix}2"
else
    echo "MAKING CHANGES ON /dev/${DEV}${suffix}1-2"
    mkfs.fat -F 32 -n NIXBOOT "/dev/${DEV}${suffix}1"
    mkfs.ext4 -L NIXROOT "/dev/${DEV}${suffix}2"
fi

# ROOT_UUID=$(blkid | grep -Po '/dev/'"${DEV}${suffix}"'3.* UUID="\K[0-9a-f-]+')

mount -L NIXROOT /mnt # Same as mount /dev/disk/by-label/NIXROOT
mkdir -p /mnt/boot
mount -L NIXBOOT /mnt/boot # EFI

nixos-generate-config --root /mnt

HWC_PATH="/mnt/etc/nixos/hardware-configuration.nix"
GIT_PATH="/root/nixos"
GIT_HWC_PATH="$GIT_PATH/systems/$SYSTEM_FOLDER/$HNAME/hardware-configuration.nix"

# Create an entry for the swapfile in hardware-configuration.nix
sed -i "s/swapDevices.*$/swapDevices = \[\{device = \"\/swapfile\"; size = $SWAP_SIZE;\}\];/" $HWC_PATH

nix-shell -p git --command "git config --global user.email phga@posteo.de"
nix-shell -p git --command "git config --global user.name phga"
nix-shell -p git --command "git clone https://g.phga.de/phga/nixos.git $GIT_PATH"
# Copy the hardware-configuration.nix into the repo if it not exists
[ ! -f $GIT_HWC_PATH ] &&
    cp $HWC_PATH $GIT_HWC_PATH &&
    nix-shell -p git --command "cd $GIT_PATH && git add $GIT_HWC_PATH && git commit -m 'Added new system $HNAME'"

# Install git?
nix-shell -p git --command "nixos-install --flake '$GIT_PATH#$HNAME'"

# The ROOT password is set by the nix-install command (Last step)

echo "BEFORE REBOOTING: If you want push the changes to the git repository!"
echo "Otherwise, the hardware-configuration.nix file needs to added manually later on!"
echo "This can be done like this: cp /etc/nixos/hardware-configuration.nix ~/.dotfiles/...."