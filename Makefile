install: install-vim install-bash install-git install-spectrwm install-tmux install-zsh

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -si $(PWD)/.vim ~/.vim
	ln -si $(PWD)/.vimrc ~/.vimrc

install-bash:
	ln -si $(PWD)/.bashrc ~/.bashrc

install-zsh:
	wget --no-check-certificate https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | sh
	ln -si $(PWD)/.zshrc ~/.zshrc

install-git:
	ln -si $(PWD)/.gitconfig ~/.gitconfig

install-spectrwm:
	ln -si $(PWD)/.spectrwm.conf ~/.spectrwm.conf
	ln -si $(PWD)/.wallpaper.png ~/.wallpaper.png
	ln -si $(PWD)/.xinitrc ~/.xinitrc
	ln -si $(PWD)/.conkyrc ~/.conkyrc

	mkdir -p ~/.config/terminator
	ln -si $(PWD)/.config/terminator/config ~/.config/terminator/config
	ln -si $(PWD)/.config/awesome ~/.config/awesome

install-tmux:
	ln -si $(PWD)/.tmux.conf ~/.tmux.conf
