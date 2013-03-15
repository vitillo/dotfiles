install: install-vim install-bash install-git install-gtk install-spectrwm

install-vim:
	rm -rfI ~/.vim ~/.vimrc
	ln -si $(PWD)/.vim ~/.vim
	ln -si $(PWD)/.vimrc ~/.vimrc

install-bash:
	ln -si $(PWD)/.bashrc ~/.bashrc

install-git:
	ln -si $(PWD)/.gitconfig ~/.gitconfig

install-gtk:
	ln -si $(PWD)/.gtkrc-2.0.mine ~/.gtkrc-2.0.mine

install-spectrwm:
	ln -si $(PWD)/.spectrwm.conf ~/.spectrwm.conf
	ln -si $(PWD)/.wallpaper.png ~/.wallpaper.png
	ln -si $(PWD)/.xinitrc ~/.xinitrc

	mkdir -p ~/.config/terminator
	ln -si $(PWD)/.config/terminator/config ~/.config/terminator/config
