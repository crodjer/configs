"" Enviornment Variables
if has('nvim')
    let vim_config_dir = "~/.config/nvim"
else
    let vim_config_dir = "~/.vim"
endif
let plug_path = join([vim_config_dir, "autoload/plug.vim"], "/")
let plug_source = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'


"" Basic behaviour
set noswapfile            "disable swapfiles
set hidden                "hide buffers when not displayed
set textwidth=80          "maximum width of text that can be inserted
set nofoldenable          "dont fold by default
set clipboard=unnamedplus "use system cliboard
set mouse-=a

" Format options
set formatoptions-=o    "disable auto comment leader insertion with o/O
set formatoptions+=c    "enable auto wrapping and formatting in comments
set formatoptions-=t    "dislable autowrapping using textwidth
set undofile

" Indentation / syntax highlighting
syntax enable
filetype plugin on
filetype indent on
set autoindent
set smartindent


"commandline configuration
set showcmd                 "display incomplete commands
set wildmode=list           "make cmdline tab completion similar to bash
set wildmenu                "enable C-n and C-p to scroll through matches
"stuff to ignore when tab completing
set wildignore=*.o,*~,*.pyc,*.hi


"" Looks
colorscheme default
set colorcolumn=+0          "mark the ideal max text width
set rnu                     "show relative line numbers
set showmode                "show current mode down the bottom
set laststatus=2
set statusline=%y                   " File type
set statusline+=\ %r%w              " Readonly / Preview flags
set statusline+=\ %f%*              " File path
set statusline+=%m                  " Modified flag
set statusline+=%#warningmsg#
set statusline+=%*
set statusline+=%=                  " Right alignment separator
set statusline+=%l/%L%*             " Line number / Total lines
set statusline+=\|%c                " Column number
set statusline+=\ [%p%%]            " Percent through lines

set ruler
highlight ColorColumn ctermbg='LightGrey'
highlight Pmenu ctermbg='LightGrey'
highlight PmenuSel ctermbg='White'

"display tabs and trailing spaces
set list
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅

"" Handling whitespaces

set expandtab                   "use spaces for tabs and set it to 4 spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4
set nowrap                      "dont wrap lines
set backspace=indent,eol,start  "backspace through everything in insert mode

"" Searching
set hlsearch        "highlight search by default
set incsearch       "incremental search
set ignorecase      "ignore cases while searching
set smartcase       "consider case for search patterns with uppercase letters

"" Mappings

"Set comma as my leader
let mapleader = ","

"restore messed up vim
map <F8> :redraw! \| :noh \| <cr><c-w>=

"<C-l> - Clear the highlight as well as redraw
nnoremap <C-L> :nohls<CR><C-L>

"`#` should follow neighbouring indentation
inoremap # X<BS>#

"reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

"use w!! to save with root permissions
cmap w!! %!sudo tee > /dev/null %


"" Load plugins
if empty(glob(plug_path))
    echo "Installing plug..."
    execute 'silent !curl -fLo ' . plug_path ' --create-dirs ' . plug_source
endif

silent! call plug#begin()
" General plugins
if (has( 'python' ) || has( 'python3' )) && v:version > 703
    Plug 'Valloric/YouCompleteMe'
endif
Plug 'scrooloose/syntastic'
Plug 'mileszs/ack.vim'

" Language plugins
Plug 'rust-lang/rust.vim'
Plug 'cespare/vim-toml'
Plug 'pangloss/vim-javascript', { 'for': ['js', 'jsx', 'json']}
Plug 'mxw/vim-jsx', { 'for': ['js', 'jsx']}
Plug 'elmcast/elm-vim'
Plug 'guns/vim-clojure-static', { 'for': ['clojure', 'edn'] }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Done loading plugins
call plug#end()


"" Plugin configurations
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

if executable('ag')
    let g:ackprg = 'ag --vimgrep'
endif

"" Language configurations

" JS/JSX
let g:jsx_ext_required = 0
let g:syntastic_javascript_checkers = ['eslint']
autocmd FileType javascript setlocal sw=2 sts=2 et

" Elm
autocmd FileType elm setlocal sw=2 sts=2 et

" Clojure
autocmd FileType clojure nnoremap <buffer> <leader>e :Eval<cr>
autocmd FileType clojure nnoremap <buffer> <leader>l :%Eval<cr>

" Crontab
autocmd FileType crontab setlocal backupcopy=yes
