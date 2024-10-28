# tools
mkdir ~/tools && pushd ~/tools
git clone https://github.com/alacritty/alacritty.git
git clone https://github.com/pyenv/pyenv.git ~/.pyenv
git clone https://git.savannah.gnu.org/git/emacs.git
curl -sS https://starship.rs/install.sh | sh
popd

# extra packages and build deps
sudo apt install fonts-powerline
sudo apt install git
sudo apt install btop
sudo apt install curl
sudo apt install fzf
sudo apt install fonts-powerline
sudo apt install git
sudo apt install btop
sudo apt install show
sudo apt install stow
sudo apt install build-essentials
sudo apt install build-essential
sudo apt install automake
sudo apt install texinfo
sudo apt install libx11-dev
sudo apt install libgtk-4-dev
sudo apt install libgtk-3-dev
sudo apt install libxpm-dev
sudo apt install libgif7
sudo apt install libgif-dev
sudo apt install gnutls
sudo apt install libgnutls28-dev
sudo apt install libncurses-dev
sudo apt install eza
sudo apt install cmake
sudo apt install libtool
sudo apt install libtool-bin
sudo apt install awesome
sudo apt install nodejs
sudo apt install libtree-sitter-dev
sudo apt install libxml2-dev
sudo apt install libgccjit0
sudo apt install apt-transport-https ca-certificates curl gnupg-agent software-properties-common
sudo apt install libjansson4 libjansson-dev
sudo apt install libgccjit-14-dev
sudo apt install libgccjit-13-dev
sudo apt install librsvg2-dev
sudo apt install libsqlite3-dev
sudo apt install rofi

# emacs
pushd ~/tools/emacs
make -j12 bootstrap
sudo make install
popd

# dotfiles
pushd ~/
git clone git@github.com:vpje/.dotfiles.git
cd .dotfiles && stow *
popd

# alacritty: install rust, compile and symlink executable to PATH (~/bin/)

# zsh plugins
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
