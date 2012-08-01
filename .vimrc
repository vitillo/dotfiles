set nocompatible
syntax on
filetype on
filetype plugin on

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
set undofile

set ignorecase
set smartcase
set incsearch
set showmatch

" Command-line configuration
set cmdheight=2
set laststatus=2
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ 
set showcmd
set relativenumber

" Keymappings
nnoremap Y y$
nnoremap ; :
nnoremap j gj
nnoremap k gk
nnoremap s S
inoremap jj <ESC>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Leader keymappings
let mapleader = ","
nnoremap <leader>w <C-w>v<C-w>l
nnoremap <leader>o :NERDTreeToggle<CR>
nnoremap <leader>b :TagbarToggl<CR> 
nnoremap <leader>s :ConqueTerm bash<CR>
nnoremap <leader>m :make<CR>
nnoremap <leader>e :BufExplorer<CR>
map <leader>d <plug>NERDCommenterToggle
map <F2> :SaveSession<CR>
map <F3> :OpenSession<CR> 

au BufEnter * :syntax sync fromstart
call pathogen#infect()

colorscheme wombat

" ConqueTerm configuration
let g:ConqueTerm_SessionSupport = 1
let g:ConqueTerm_ReadUnfocsed = 1
let g:ConqueTerm_PromptRegex = '^\w\+@[0-9A-Za-z_.-]\+:[0-9A-Za-z_./\~,:-]\+\$'

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

" Supertab configuration
let g:SuperTabClosePreviewOnPopupClose = 1

" C, C++ 
" ------
au filetype c,cpp setlocal cindent shiftwidth=2 " | imap <buffer> <Tab> <Esc>cc

" Python
" ------
autocmd FileType python setlocal expandtab shiftwidth=4 tabstop=8
\ formatoptions+=croq softtabstop=4 smartindent
\ cinwords=if,elif,else,for,while,try,except,finally,def,class,with

" Javascript
" ----------
autocmd FileType javascript setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
autocmd BufNewFile,BufRead *.json setlocal ft=javascript
let javascript_enable_domhtmlcss=1

" CSS
" ---
autocmd FileType css setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

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
