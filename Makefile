install: install-vim install-git install-hg install-awesome install-tmux install-zsh install-ssh install-emacs

install-vim:
	rm -rf ~/.vim ~/.vimrc
	-ln -s $(PWD)/.vim ~/.vim
	-ln -s $(PWD)/.vimrc ~/.vimrc

install-zsh:
	wget --no-check-certificate https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh
	bash install.sh
	-ln -s $(PWD)/.zshrc ~/.zshrc

install-git:
	-ln -s $(PWD)/.gitconfig ~/.gitconfig
	-ln -s $(PWD)/.gitignore ~/.gitignore

install-hg:
	-ln -s $(PWD)/.hgrc ~/.hgrc

install-awesome:
	-ln -s $(PWD)/.wallpaper.png ~/.wallpaper.png
	-ln -s $(PWD)/.conkyrc ~/.conkyrc

	mkdir -p ~/.config/terminator
	-ln -s $(PWD)/.config/terminator/config ~/.config/terminator/config
	-ln -s $(PWD)/.config/awesome ~/.config/awesome

install-tmux:
	-ln -s $(PWD)/.tmux.conf ~/.tmux.conf

install-ssh:
	mkdir -p ~/.ssh
	-ln -s $(PWD)/ssh_config ~/.ssh/config

install-emacs:
	mkdir -p ~/.emacs.d
	mkdir -p ~/.lein
	-ln -s $(PWD)/.emacs.d/init.el ~/.emacs.d/init.el
	-ln -s $(PWD)/.lein/profiles.clj ~/.lein/profiles.clj
