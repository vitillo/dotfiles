install: install-vim install-bash install-git

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -s $(PWD)/.vim ~/.vim
	ln -s $(PWD)/.vimrc ~/.vimrc

install-bash:
	rm -f ~/.bashrc
	ln -s $(PWD)/.bashrc ~/.bashrc

install-git:
	rm -f ~/.gitconfig
	ln -s $(PWD)/.gitconfig ~/.gitconfig
