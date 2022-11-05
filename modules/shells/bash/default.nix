{ config, pkgs, user, dotf, ... }:
let
  t = (import ../../themes).shanty;
in {
  programs = {
    bash = {
      enable = true;
      historyFileSize = 100000;
      historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
      sessionVariables = {
        EDITOR = "emacsclient";
        _JAVA_AWT_WM_NONREPARENTING = 1;
      };
      initExtra = ''
        C1="\[$(tput setaf 1)\]"
        C2="\[$(tput setaf 2)\]"
        RS="\[$(tput sgr0)\]"
        [ -z $TERM_FROM_EMACS ] && [ ! -z $EMACS_SHELL ] && RS="\033[0;10m"
        PS1=" $C1\w$RS\n $C2⤷$RS "
        eval "$(direnv hook bash)"
        . ${config.programs.fzf.package}/share/fzf/key-bindings.bash
      '';
      # Any Shell (even non interactive)
      # bashrcExtra = ''
      # '';
      # Login Shell
      # profileExtra = '''';
      shellAliases = {
        bye="shutdown now"                                                        ;
        ls="ls --color=auto --group-directories-first";
        ll="ls -alSFhv";
        la="ls -AG";
        l="ls -CF";
        lb="lsblk -o 'name,fstype,label,fssize,fsavail,fsuse%,mountpoint'";
        grep="grep --color=auto";
        rm="rm -I --preserve-root";
        monitor="${dotf}/config/bspwm/monitor";
        switch_yubi="gpg-connect-agent 'scd serialno' 'learn --force' /bye";
        lp="lp -h 10.10.10.3:631 -d HLL2350DW";
        copy="rsync -avhW --no-compress --progress --ignore-errors";
        java_new_project="${dotf}/scripts/java/create_project";
        docker_stop_all="docker stop $(docker ps -q)";
        youtube-dl="yt-dlp";
        pnt="${dotf}/scripts/pnt";
      };
    };
  };

  # home.file.".bashrc".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/bash/bashrc";
  # home.file.".bash_aliases".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/bash/bash_aliases";

}