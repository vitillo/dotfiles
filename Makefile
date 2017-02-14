ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
install: install-vim install-git install-hg install-awesome install-tmux install-zsh install-ssh install-emacs

install-vim:
	rm -rf ~/.vim ~/.vimrc
	-ln -s $(ROOT_DIR)/.vim ~/.vim
	-ln -s $(ROOT_DIR)/.vimrc ~/.vimrc
	vim -c ":BundleInstall" -c ":q" -c ":q"

install-zsh:
	-ln -s $(ROOT_DIR)/.zshrc ~/.zshrc
	-wget --no-check-certificate http://install.ohmyz.sh -O - | sh

install-git:
	-ln -s $(ROOT_DIR)/.gitconfig ~/.gitconfig
	-ln -s $(ROOT_DIR)/.gitignore ~/.gitignore

install-hg:
	-ln -s $(ROOT_DIR)/.hgrc ~/.hgrc

install-awesome:
	-ln -s $(ROOT_DIR)/.xinitrc ~/.xinitrc

	mkdir -p ~/.config/terminator
	-ln -s $(ROOT_DIR)/.config/terminator/config ~/.config/terminator/config
	-ln -s $(ROOT_DIR)/.config/awesome ~/.config/awesome

install-tmux:
	-ln -s $(ROOT_DIR)/.tmux.conf ~/.tmux.conf
	-ln -s $(ROOT_DIR)/.ssh/rc ~/.ssh/

install-ssh:
	mkdir -p ~/.ssh
	-ln -s $(ROOT_DIR)/ssh_config ~/.ssh/config

install-emacs:
	mkdir -p ~/.emacs.d
	mkdir -p ~/.lein
	git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d
	-ln -s $(ROOT_DIR)/.spacemacs ~/.spacemacs
	-ln -s $(ROOT_DIR)/.lein/profiles.clj ~/.lein/profiles.clj
	emacs --batch -l ~/.emacs.d/init.el
