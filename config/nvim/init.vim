"" Basic behaviour
set noswapfile          "disable swapfiles
set hidden              "hide buffers when not displayed
set textwidth=80        "maximum width of text that can be inserted
set nofoldenable        "dont fold by default

" Format options
set formatoptions-=o    "disable auto comment leader insertion with o/O
set formatoptions-=t    "dislable autowrapping using textwidth
set formatoptions+=c    "enable auto wrapping and formatting in comments
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

set statusline=%y                   " File type
set statusline+=\ %r%w              " Readonly / Preview flags
set statusline+=\ %f%*              " File path
set statusline+=%m                  " Modified flag
set statusline+=%=                  " Right alignment separator
set statusline+=%l/%L%*             " Line number / Total lines
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

imap ( ()<left>
imap { {}<left>
imap [ []<left>

"reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

"use w!! to save with root permissions
cmap w!! %!sudo tee > /dev/null %

"" Load plugins
call plug#begin()
" General plugins
Plug 'Valloric/YouCompleteMe'

" Language plugins
Plug 'rust-lang/rust.vim'
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
call plug#end()
