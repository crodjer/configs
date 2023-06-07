scriptencoding utf-8

"" Environment Variables
if has('nvim')
    let vim_config_dir = '~/.config/nvim'
else
    let vim_config_dir = '~/.vim'
endif

let plug_path = join([vim_config_dir, 'autoload/plug.vim'], '/')
let plug_source = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

"" Basic behaviour
set noswapfile              "disable swapfiles
set hidden                  "hide buffers when not displayed
set textwidth=80            "maximum width of text that can be inserted
set nofoldenable            "don't fold by default
set clipboard+=unnamedplus  "use system clipboard
set mouse-=a
set cursorline
set updatetime=500

" Format options
set formatoptions-=o    "disable auto comment leader insertion with o/O
set formatoptions+=c    "enable auto wrapping and formatting in comments
set formatoptions-=t    "disable autowrapping using textwidth
set undofile

" Indentation / syntax highlighting
syntax enable
filetype plugin on
filetype indent on
set autoindent
set smartindent
runtime plugins/matchit.vim

"command line configuration
set showcmd                 "display incomplete commands
set wildmode=list           "make cmd line tab completion similar to bash
set wildmenu                "enable C-n and C-p to scroll through matches
"stuff to ignore when tab completing
set wildignore=*.o,*~,*.pyc,*.hi,*.class

"" Looks
set background=light
set colorcolumn=+1                      "mark the ideal max text width
set relativenumber                      "show relative line numbers
set number                              "show absolute current line number
set showmode                            "show current mode down the bottom
set laststatus=2

set ruler
highlight Normal guibg=#fdf6e3 ctermbg=None
highlight SpellBad cterm=underline gui=underline guisp=Grey
highlight rubyDefine ctermbg=None
" highlight ColorColumn ctermbg=LightGrey
highlight SignColumn ctermbg=None

"display tabs and trailing spaces
set list
set listchars=tab:\ \ ,trail:⋅,nbsp:⋅

"disable paste mode
set nopaste
augroup paste
  autocmd InsertLeave * set nopaste
augroup END

"" Handling whitespace
set expandtab                   "use spaces for tabs and set it to 4 spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4
set nowrap                      "don't wrap lines
set backspace=indent,eol,start  "backspace through everything in insert mode

"" Searching
set hlsearch        "highlight search by default
set incsearch       "incremental search
set ignorecase      "ignore cases while searching
set smartcase       "consider case for search patterns with uppercase letters


"" Mappings
"Set comma as my leader
let mapleader = ','

"Open file relative to current file
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

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

"toggle spell check
nnoremap <leader>z :setlocal spell! spelllang=en<CR>

"edit re-load config file
nnoremap <leader>ce :e $MYVIMRC<CR>
nnoremap <leader>cs :so $MYVIMRC<CR>

"close preview windows
nnoremap <leader>pc :pclose<CR>

"go into paste mode
nnoremap <leader>pm :set paste<CR>a

"" Custom functions

" Custom commands
" Strip white space!
command! STW %s/\s\+$//e

"" Load plugins
if empty(glob(plug_path))
    echo 'Installing plug...'
    execute 'silent !curl -fLo ' . plug_path ' --create-dirs ' . plug_source
endif

silent! call plug#begin()

" General plugins
Plug 'itchyny/lightline.vim'
Plug 'Raimondi/delimitMate'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'preservim/tagbar'
Plug 'morhetz/gruvbox'

Plug 'dense-analysis/ale'
Plug 'maximbaz/lightline-ale'

" Language plugins
Plug 'plasticboy/vim-markdown'          , { 'for': 'markdown' }
Plug 'rust-lang/rust.vim'               , { 'for': 'rust'}
Plug 'cespare/vim-toml'                 , { 'for': 'toml'}
Plug 'ekalinin/Dockerfile.vim'          , { 'for': 'Dockerfile' }
Plug 'Vimjas/vim-python-pep8-indent'    , { 'for': 'python' }
Plug 'ledger/vim-ledger'                , { 'for': 'dat' }
Plug 'guns/vim-sexp'                    , { 'for': 'clojure' }
Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': 'clojure' }
Plug 'liquidz/vim-iced'                 , { 'for': 'clojure', 'branch': 'main' }
Plug 'hashivim/vim-terraform'           , { 'for': 'tf' }
Plug 'udalov/kotlin-vim'                , { 'for': 'kt' }

" Done loading plugins
call plug#end()

"" Plugin configurations

" Gruvbox
let g:gruvbox_termcolors=16
colorscheme gruvbox

" Lightline
let g:lightline = {}

" Ale
let g:ale_lint_on_text_changed = 'never'
" let g:ale_open_list = 1
let g:ale_sign_error = 'x'
let g:ale_sign_warning = '!'
let g:ale_sign_column_always = 0
let g:ale_hover_cursor=1
let g:ale_set_balloons=1
let g:ale_hover_to_floating_preview = 1
let g:ale_floating_window_border = ['│', '─', '╭', '╮', '╯', '╰']

set omnifunc=ale#completion#OmniFunc
let g:ale_completion_enabled = 1
let g:ale_completion_autoimport = 1
let g:ale_fixers = {
            \ '*': ['remove_trailing_lines', 'trim_whitespace']
            \ }
" let g:ale_linters = {
"             \ 'python': ['pylsp', 'pylint'],
"             \ 'rust': ['analyzer', 'rls', 'cargo'],
"             \ 'typescript': ['tsserver']
"             \ }

nmap <silent> <leader>aj :ALENext<cr>
nmap <silent> <leader>ak :ALEPrevious<cr>
nmap <silent> <leader>ah :ALEHover<cr>
nmap <silent> <leader>d :ALEHover<cr>
nmap <silent> <leader>ag :ALEGoToDefinition<cr>
nmap <silent> <leader>ar :ALEFindReferences<cr>
nmap <silent> <leader>aj :ALEImport<cr>
nmap <silent> <leader>af :ALEFix<cr>

augroup CloseLoclistWindowGroup
    autocmd!
    autocmd QuitPre * if empty(&buftype) | lclose | endif
augroup END

let g:lightline.component_expand = {
            \  'linter_checking': 'lightline#ale#checking',
            \  'linter_infos': 'lightline#ale#infos',
            \  'linter_warnings': 'lightline#ale#warnings',
            \  'linter_errors': 'lightline#ale#errors',
            \  'linter_ok': 'lightline#ale#ok',
            \ }
let g:lightline.component_type = {
      \     'linter_checking': 'right',
      \     'linter_infos': 'tabsel',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'tabsel',
      \ }

let g:lightline#ale#indicator_checking = ' '
let g:lightline#ale#indicator_infos = '  '
let g:lightline#ale#indicator_warnings = ' '
let g:lightline#ale#indicator_errors = ' '
let g:lightline#ale#indicator_ok = ' '

"" Language configurations

" JS/JSX
" let g:jsx_ext_required = 1
augroup javascript
    autocmd FileType javascript setlocal sw=2 sts=2 et
    autocmd FileType typescript* setlocal sw=2 sts=2 et
augroup END

" Clojure
let g:iced_enable_default_key_mappings = v:true
augroup clojure
    autocmd FileType clojure nnoremap <buffer> <leader>ec :IcedInstantConnect babashka<cr>
augroup END

" Crontab
augroup crontab
    autocmd FileType crontab setlocal backupcopy=yes
augroup END


" FZF/Skim
augroup fzf
    autocmd! FileType fzf
    autocmd  FileType fzf set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
augroup END
nnoremap <leader>s :GFiles<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>h :History<CR>

" Git commit
augroup git
    autocmd FileType gitcommit setlocal spell spelllang=en
augroup END

" Go
augroup go
    autocmd FileType go setlocal noet ts=2 sw=2 sts=2 ai
augroup END

" Java

" HTML
augroup html
    autocmd FileType html* setlocal noet ts=2 sw=2 sts=2 ai
augroup END

"" Ledger
let g:ledger_default_commodity = '₹ '
augroup ledger
    autocmd FileType ledger inoremap <silent> <Tab> <C-R>=ledger#autocomplete_and_align()<CR>
    autocmd FileType ledger inoremap <silent> <Esc> <Esc>:LedgerAlign<CR>
    autocmd FileType ledger vnoremap <silent> <Tab> :LedgerAlign<CR>
    autocmd FileType ledger nnoremap <silent> <Tab> :LedgerAlign<CR>
    autocmd FileType ledger noremap { ?^\d<CR>
    autocmd FileType ledger noremap } /^\d<CR>
augroup END

" Lightline
let g:lightline.active = {
    \ 'left': [ [ 'mode', 'paste' ],
	\           [ 'readonly', 'filename', 'modified' ] ],
	\ 'right': [ [ 'lineinfo' ],
	\            [ 'percent' ],
	\            [ 'filetype' ],
    \            [ 'linter_checking', 'linter_errors', 'linter_warnings',
    \              'linter_infos', 'linter_ok' ]] }

call lightline#init()
call lightline#colorscheme()
call lightline#update()

" Markdown
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'javascript']
let g:vim_markdown_new_list_item_indent = 2
augroup markdown
    autocmd FileType markdown,rst setlocal sw=2 sts=2 et textwidth=70 conceallevel=0
    autocmd FileType markdown,rst,text setlocal spell spelllang=en wrap
augroup END

" Python

" Ruby
augroup ruby
    autocmd FileType ruby setlocal sw=2 sts=2
augroup END

" Rust
let g:rustfmt_autosave = 1

augroup rust
    autocmd FileType rust setlocal textwidth=80
    autocmd FileType rust map <buffer> <leader>rt :RustTest<CR>
    autocmd FileType rust nmap <buffer> K :ALEHover<CR>
augroup END

" XML
augroup xml
    autocmd FileType xml setlocal iskeyword+=.,-
augroup END

" YAML
augroup yaml
    autocmd FileType yaml setlocal sw=2 sts=2
augroup END

" Tagbar
let g:tagbar_width = 30
nnoremap <leader>t :TagbarToggle<CR>

try
    " Load any local overrides, if any.
    source ~/.local.vim
catch
    " No such file? No problem; just ignore it.
endtry
