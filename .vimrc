set nocompatible
syntax on
filetype on
filetype plugin on
filetype indent on

set backspace=indent,eol,start
set hidden
set title
set mouse=a
set mousehide
set virtualedit=onemore
set history=1000

set gdefault
set autochdir
set autoread
set autowrite
set enc=utf-8
set scrolloff=5

set wildmenu
set wildmode=list:longest
set visualbell
set cursorline
set ttyfast
set completeopt+=longest

set ignorecase
set smartcase
set incsearch
set showmatch

set backspace=indent,eol,start  " Backspace for dummies"
set linespace=0                 " No extra spaces between rows"
set winminheight=0              " Windows can be 0 line high"

" Show trailing whitespace
set listchars=tab:\ \ ,trail:.
set list

set tags=./tags;

" Command-line configuration
set cmdheight=2
set laststatus=2
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ 
set showcmd

set backupdir^=~/.vim/_backup//    " where to put backup files.
set directory^=~/.vim/_temp//      " where to put swap files.

if v:version > 702
  set relativenumber
  set undofile
endif

" Font settings for gvim
if has("gui_running")
  set guifont=Anonymous\ Pro\ 13
endif

" Automatically resize splits when resizing MacVim window
autocmd VimResized * wincmd =

" Yank text to the clipboard
if has ('x') && has ('gui') " On Linux use + register for copy-paste
  set clipboard=unnamedplus
elseif has ('gui')          " On mac and Windows, use * register for copy-paste
  set clipboard=unnamed
endif

" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested botright cwindow
autocmd QuickFixCmdPost    l* nested botright lwindow

" sudo write this
cmap W! w !sudo tee % >/dev/null

" Keymappings
nnoremap Y y$
nnoremap ; :
nnoremap j gj
nnoremap k gk
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <F2> :SaveSession<CR>
nnoremap <F3> :OpenSession<CR> 

" Visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" Fix home and end keybindings for screen, particularly on mac
" - for some reason this fixes the arrow keys too. huh.
map [F $
imap [F $
map [H g0
imap [H g0

" Leader keymappings
let mapleader = ","
nnoremap <leader>w <C-w>v<C-w>l
nnoremap <leader>o :NERDTreeToggle<CR>
nnoremap <leader>b :TagbarToggle<CR>
nnoremap <leader>c :silent make\|redraw!\|cc<CR>
nnoremap <leader>e :CtrlPBuffer<CR>
nnoremap <leader>g :Gstatus<CR>
nnoremap <leader>s :silent Ggrep 
nnoremap <leader>q :cclose<CR> :lclose<CR>

" Handy to edit foreign code
:nmap <leader>t :set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>
:nmap <leader>T :set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>
:nmap <leader>m :set noexpandtab tabstop=8 shiftwidth=8 softtabstop=8<CR>

" make configuration
:let &makeprg = 'if [ -f Makefile ]; then make; else make -C ..; fi'

" Pathogen configuration
au BufEnter * :syntax sync fromstart
call pathogen#infect()

" Vundle configuration
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'mileszs/ack.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'Raimondi/delimitMate'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'tpope/vim-fugitive'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-session'
Bundle 'altercation/vim-colors-solarized'
Bundle 'scrooloose/syntastic'
Bundle 'majutsushi/tagbar'
Bundle 'mbbill/undotree'
Bundle 'bling/vim-airline'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'Valloric/YouCompleteMe'
Bundle 'tpope/vim-surround'
Bundle 'godlygeek/tabular'
Bundle 'tpope/vim-unimpaired'
Bundle 'terryma/vim-multiple-cursors'
Bundle "pangloss/vim-javascript"
Bundle "maxbrunsfeld/vim-yankstack"
Bundle "tpope/vim-repeat"

" yankstack configuration
let g:yankstack_map_keys = 0
nmap <leader>y <Plug>yankstack_substitute_newer_paste
nmap <leader>e <Plug>yankstack_substitute_older_paste

" solarized configuration
set background=dark
let g:solarized_termtrans = 1
let g:solarized_termcolors = 256
let g:solarized_contrast = "high"
colorscheme solarized

" tagbar configuration
let g:tagbar_width = 30
let g:tagbar_compact = 1
let g:tagbar_singleclick = 1
let g:tagbar_autoshowtag = 1

" NERDTree configuration
let NERDTreeChDirMode=2
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" session configuration
let g:session_autosave = 'no'
let g:session_autoload = 'no'

" syntastic configuration
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_always_populate_loc_list= 1
let g:syntastic_c_check_header          = 0
let g:syntastic_c_compiler_options      = ' -Wextra -Wall'
let g:syntastic_c_remove_include_errors = 1
let g:syntastic_cpp_compiler_options = ' -Wextra -Wall -std=c++0x'

" undotree configuration
nnoremap <leader>u :UndotreeToggle<CR>
let g:undotree_SetFocusWhenToggle=1

" ctrlp configuration
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_custom_ignore = '\v\~$|\.(o|swp|pyc|wav|mp3|ogg|blend)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])|__init__\.py'
let g:ctrlp_working_path_mode = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_switch_buffer = 0

" airline configuration
let g:airline_theme='solarized'
let g:airline_left_sep='›'
let g:airline_right_sep='‹'

" ISPC
au BufNewFile,BufRead *.ispc setlocal ft=cpp cindent shiftwidth=2

" C, C++ 
" ------
au FileType c,cpp setlocal cindent shiftwidth=2
au FileType c,cpp let b:syntastic_checkers = ['gcc']
let c_no_curly_error=1 " C++11 support

" Cuda
au BufNewFile,BufRead *.cu setlocal ft=cpp cindent shiftwidth=2

" OpenCL
au BufNewFile,BufRead *.cl setlocal ft=cpp cindent shiftwidth=2

" Python
" ------
" make Python follow PEP8 for whitespace ( http://www.python.org/dev/peps/pep-0008/ )
au FileType python setlocal softtabstop=4 tabstop=4 shiftwidth=4

" HTML
" ----
autocmd FileType html setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" Javascript
" ----------
autocmd FileType javascript setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd BufNewFile,BufRead *.json setlocal ft=javascript
let javascript_enable_domhtmlcss=1

" Java
" ----
autocmd FileType java setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
let java_highlight_java_lang_ids=1
let java_highlight_functions="style"

" CSS
" ---
autocmd FileType css setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" HTML
" ----
autocmd FileType html setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" CMake
" -----
autocmd BufNewFile,BufRead CMakeLists.txt setlocal ft=cmake
