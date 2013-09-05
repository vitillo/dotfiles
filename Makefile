install: install-vim install-git install-awesome install-tmux install-zsh

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -si $(PWD)/.vim ~/.vim
	ln -si $(PWD)/.vimrc ~/.vimrc

install-zsh:
	wget --no-check-certificate https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | sh
	ln -si $(PWD)/.zshrc ~/.zshrc

install-git:
	ln -si $(PWD)/.gitconfig ~/.gitconfig
	ln -si $(PWD)/.gitignore ~/.gitignore

install-awesome:
	ln -si $(PWD)/.wallpaper.png ~/.wallpaper.png
	ln -si $(PWD)/.conkyrc ~/.conkyrc

	mkdir -p ~/.config/terminator
	ln -si $(PWD)/.config/terminator/config ~/.config/terminator/config
	ln -si $(PWD)/.config/awesome ~/.config/awesome

install-tmux:
	ln -si $(PWD)/.tmux.conf ~/.tmux.conf
