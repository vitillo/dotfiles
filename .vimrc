set nocompatible
syntax on
filetype on
filetype plugin on
filetype indent on

set backspace=indent,eol,start
set hidden
set title
set mouse=a
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

set tags=./tags;

" Command-line configuration
set cmdheight=2
set laststatus=2
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ 
set showcmd

if v:version > 702
  set relativenumber
  set undofile
endif

" Font settings for gvim
if has("gui_running")
  if has("gui_gtk2")
    set guifont=Inconsolata\ 9
  elseif has("gui_win32")
    set guifont=Consolas:h11:cANSI
  endif
endif

" Keymappings
nnoremap Y y$
nnoremap ; :
nnoremap j gj
nnoremap k gk
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Leader keymappings
let mapleader = ","
nnoremap <leader>w <C-w>v<C-w>l
nnoremap <leader>o :NERDTreeToggle<CR>
nnoremap <leader>b :TagbarToggl<CR> 
nnoremap <leader>m :make<CR>:make -C../<CR>:make -C../../<CR>:make -C../../../<CR>
nnoremap <leader>e :BufExplorer<CR>
nnoremap <leader>g :Gstatus<CR>
map <leader>d <plug>NERDCommenterToggle
map <F2> :SaveSession<CR>
map <F3> :OpenSession<CR> 

au BufEnter * :syntax sync fromstart
call pathogen#infect()

if has("gui_running")
  set background=dark
  colorscheme solarized
endif

" Tagbar configuration
let g:tagbar_width = 30
let g:tagbar_compact = 1
let g:tagbar_singleclick = 1
let g:tagbar_autoshowtag = 1

" NERDTree configuration
let NERDTreeChDirMode=2
"autocmd vimenter * NERDTree
"autocmd vimenter * wincmd l

" Session configuration
let g:session_autosave = 'no'
let g:session_autoload = 'no'

" Syntastic configuration
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_always_populate_loc_list=1

" Gundo configuration
nnoremap <F5> :GundoToggle<CR>
let g:gundo_right = 1

" ISPC
au BufNewFile,BufRead *.ispc set filetype=ispc cindent shiftwidth=2 syntax=cpp

" C, C++ 
" ------
au filetype c,cpp setlocal cindent shiftwidth=2 "| imap <buffer> <Tab> <Esc>cc
let c_no_curly_error=1 " C++11 support

" Cuda
autocmd BufNewFile,BufRead *.cu setlocal ft=cpp

" Python
" ------
autocmd FileType python setlocal expandtab shiftwidth=4 tabstop=8
\ formatoptions+=croq softtabstop=4 smartindent
\ cinwords=if,elif,else,for,while,try,except,finally,def,class,with
\ | imap <silent> <buffer> . .<C-X><C-O>

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

" cmake
" -----
autocmd BufNewFile,BufRead CMakeLists.txt setlocal ft=cmake

" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested botright cwindow
autocmd QuickFixCmdPost    l* nested botright lwindow

" Run helptags on all plugins
:Helptags

" sudo write this
cmap W! w !sudo tee % >/dev/null
