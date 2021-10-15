# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"
#PATH="$HOME/node_modules/.bin:$PATH"
# PATH="/opt/gcc-arm-none-eabi-7-2018-q2-update/bin:$PATH"
PATH="/opt/gcc-arm-none-eabi-10-2020-q4-major/bin:$PATH"

PATH="/home/pekka/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/pekka/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/pekka/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/pekka/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/pekka/perl5"; export PERL_MM_OPT;

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/tools/blackbox/bin:$PATH"
export PATH="/opt/sdcc/bin:$PATH"
export PATH="$HOME/SiliconLabs/SimplicityStudio_v5/developer/adapter_packs/commander:$PATH"
