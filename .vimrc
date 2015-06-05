set nocompatible
syntax on
filetype on
filetype plugin on
filetype indent on

set hidden
set title
set mouse=a
set mousehide
set virtualedit=onemore
set history=1000
set report=0

set gdefault
set autoread
set autowrite
set enc=utf-8

set wildmenu
set wildmode=list:longest
set visualbell
set cursorline
set ttyfast
set noerrorbells
set completeopt+=longest
set splitbelow " New window goes below
set splitright " New windows goes right

set ignorecase
set smartcase
set incsearch
set showmatch

set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
set winminheight=0              " Windows can be 0 line high
set scrolloff=5
set nowrap
set nostartofline

" Show trailing whitespace
set listchars=tab:\ \ ,trail:◦,extends:»,precedes:«
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
  "set relativenumber
  set undofile
endif

" Font settings for gvim
if has("gui_running")
  set guioptions-=e
  set guifont=Anonymous\ Pro\ 13
endif

" Automatically resize splits when resizing MacVim window
autocmd VimResized * wincmd =

" Yank text to the clipboard
if $TMUX == ''
  if has("unix")
    if system("uname") == "Linux\n"
      set clipboard=unnamedplus
    else
      set clipboard=unnamed
    endif
  else
    set clipboard=unnamed
  endif
endif

" Remove non-active fugitive buffers
autocmd BufReadPost fugitive://* set bufhidden=delete

" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested botright cwindow
autocmd QuickFixCmdPost    l* nested botright lwindow

" sudo write this
cmap W! w !sudo tee % >/dev/null

" Clear last search
map <silent> // <Esc>:noh<CR>

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

" Better mark jumping (line + col)
nnoremap ' `

" Speed up viewport scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Fix home and end keybindings for screen, particularly on mac
map [F $
imap [F $
map [H g0
imap [H g0

" Fix page up and down
map <PageUp> <C-U>
map <PageDown> <C-D>
imap <PageUp> <C-O><C-U>
imap <PageDown> <C-O><C-D>

" When typing a string, your quotes auto complete. Move past the quote
" while still in insert mode by hitting Ctrl-a. Example:
"
" type 'foo<c-a>
"
" the first quote will autoclose so you'll get 'foo' and hitting <c-a> will
" put the cursor right after the quote
imap <C-a> <esc>ea

" Leader keymappings
let mapleader = ","
nnoremap <leader>o :NERDTreeToggle<CR>
nnoremap <leader>z :TagbarToggle<CR>
nnoremap <leader>m :Make<CR>
nnoremap <leader>e :CtrlPBuffer<CR>
nnoremap <leader>g :Gstatus<CR>
nnoremap <leader>s :silent Ggrep 
nnoremap <leader>q :cclose<CR> :lclose<CR>
nnoremap <leader>d :Dash<CR>

" Use numbers to pick the tab you want
nmap <leader>1 :tabn 1<cr>
nmap <leader>2 :tabn 2<cr>
nmap <leader>3 :tabn 3<cr>
nmap <leader>4 :tabn 4<cr>
nmap <leader>5 :tabn 5<cr>
nmap <leader>6 :tabn 6<cr>
nmap <leader>7 :tabn 7<cr>
nmap <leader>8 :tabn 8<cr>
nmap <leader>9 :tabn 9<cr>

" Handy to edit foreign code
:nmap <leader>t2 :set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>
:nmap <leader>t4 :set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>
:nmap <leader>t8 :set noexpandtab tabstop=8 shiftwidth=8 softtabstop=8<CR>

" Paste toggle
set pastetoggle=<leader>p
nmap <leader>p :set invpaste paste?<CR>

" Strip all trailing whitespace in file
function! StripWhitespace ()
    exec ':%s/ \+$//gc'
endfunction
nmap ,s :call StripWhitespace ()<CR>

" make configuration
:let &makeprg = 'if [ -f Makefile ]; then make; else make -C ..; fi'

" Pathogen configuration
au BufEnter * :syntax sync fromstart
call pathogen#infect()

" Vundle configuration
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Use tab to cycle through autocompletion options
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<C-g>u\<Tab>"

Bundle "maxbrunsfeld/vim-yankstack"
Bundle "pangloss/vim-javascript"
Bundle "tpope/vim-repeat"
Bundle 'Lokaltog/vim-easymotion'
Bundle 'Raimondi/delimitMate'
Bundle 'bling/vim-airline'
Bundle 'chrisbra/csv.vim'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'gcmt/taboo.vim'
Bundle 'gmarik/vundle'
Bundle 'godlygeek/tabular'
Bundle 'guns/vim-clojure-static'
Bundle 'guns/vim-sexp'
Bundle 'kien/ctrlp.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'majutsushi/tagbar'
Bundle 'mbbill/undotree'
Bundle 'mileszs/ack.vim'
Bundle 'othree/html5.vim'
Bundle 'saltstack/salt-vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'tpope/vim-classpath'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-fireplace'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-sensible'
Bundle 'tpope/vim-sexp-mappings-for-regular-people'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'vim-scripts/LargeFile'
Bundle 'vitillo/vim-colors-solarized'
Bundle 'vitillo/vimux'
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-session'
Bundle 'rizzatti/funcoo.vim'
Bundle 'rizzatti/dash.vim'
Bundle 'Shougo/neocomplcache.vim'
Bundle 'jnurmine/Zenburn'

" Neocomplcache configuration
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_force_overwrite_completefunc = 1

if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif

let g:neocomplcache_omni_patterns['clojure'] = '\w\|-\|\.\|+\|*\|/'

" fireplace configuration
nmap cqr :Require<CR>

" Rainbow configuration
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['darkgreen',   'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]

au VimEnter * silent! RainbowParenthesesToggle
au Syntax * silent! RainbowParenthesesLoadRound
"au Syntax * RainbowParenthesesLoadSquare
"au Syntax * RainbowParenthesesLoadBraces

" Vimux configuration
nmap <leader>vp :VimuxPromptCommand<CR>
nmap <Leader>vi :VimuxInspectRunner<CR>
nmap <Leader>vq :VimuxCloseRunner<CR>
nmap <Leader>vx :VimuxInterruptRunner<CR>
nmap <leader>l :VimuxRunLastCommand<CR>

" Taboo configuration
let g:taboo_tab_format = " %N %f "

" Tabular configuration
nmap <Leader>a= :Tabularize /=<CR>
vmap <Leader>a= :Tabularize /=<CR>
nmap <Leader>a: :Tabularize /:\zs/l0l1<CR>
vmap <Leader>a: :Tabularize /:\zs/l0l1<CR>
nmap <Leader>a, :Tabularize /,\zs/l0l1<CR>
vmap <Leader>a, :Tabularize /,\zs/l0l1<CR>

" yankstack configuration
let g:yankstack_map_keys = 0
nmap <leader>j <Plug>yankstack_substitute_newer_paste
nmap <leader>k <Plug>yankstack_substitute_older_paste

if has('gui_running')
	" solarized configuration
	set background=dark
	set t_Co=16
	let g:solarized_termtrans = 1
	let g:solarized_termcolors = 16
	let g:solarized_contrast = "high"
	silent! colorscheme solarized
else
	silent! colors zenburn
endif

" tagbar configuration
let g:tagbar_sort = 0
let g:tagbar_width = 30
let g:tagbar_compact = 1
let g:tagbar_singleclick = 1
let g:tagbar_autoshowtag = 1

" NERDTree configuration
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let g:NERDTreeWinSize = 30
let NERDTreeChDirMode=2
let g:nerdtree_tabs_focus_on_files = 1
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" session configuration
let g:session_autosave = 'no'
let g:session_autoload = 'no'

" syntastic configuration
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_auto_jump=0
let g:syntastic_always_populate_loc_list= 1
let g:syntastic_c_check_header          = 0
let g:syntastic_c_compiler_options      = ' -Wextra -Wall -std=c99'
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
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'

" airline configuration
" let g:airline_theme='solarized'
let g:airline_left_sep='›'
let g:airline_right_sep='‹'
let g:airline#extensions#tagbar#enabled = 0
let g:airline#extensions#whitespace#checks = [ 'trailing' ]

" Clojure
let g:ycm_filetype_blacklist = {'clojure': 0}

" Vim-clojure-static: Correctly indent compojure and korma macros, etc.
let g:clojure_fuzzy_indent_patterns = "with.*,def.*,let.*,send.*,if.*,when.*artition"
let g:clojure_fuzzy_indent_patterns .= ",GET,POST,PUT,PATCH,DELETE,context"          " Compojure
let g:clojure_fuzzy_indent_patterns .= ",clone-for"                                  " Enlive
let g:clojure_fuzzy_indent_patterns .= ",select.*,insert.*,update.*,delete.*,with.*" " Korma
let g:clojure_fuzzy_indent_patterns .= ",fact,facts"                                 " Midje
let g:clojure_fuzzy_indent_patterns .= ",up,down,alter,table"                        " Lobos
let g:clojure_fuzzy_indent_patterns .= ",check,match,url-of-form"                    " Misc

" ISPC
au BufNewFile,BufRead *.ispc setlocal ft=cpp cindent shiftwidth=2

" C, C++ 
" ------
au FileType c,cpp setlocal cindent shiftwidth=2
au FileType c,cpp let b:syntastic_checkers = [] " gcc and ycm are too slow...
let c_no_curly_error=1 " C++11 support

" Cuda
au BufNewFile,BufRead *.cu setlocal ft=cpp cindent shiftwidth=2

" OpenCL
au BufNewFile,BufRead *.cl setlocal ft=cpp cindent shiftwidth=2

" UPC
au BufNewFile,BufRead *.upc setlocal ft=c cindent shiftwidth=2

" Python
" ------
" make Python follow PEP8 for whitespace ( http://www.python.org/dev/peps/pep-0008/ )
au FileType python setlocal expandtab softtabstop=4 tabstop=4 shiftwidth=4

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
