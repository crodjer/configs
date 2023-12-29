scriptencoding utf-8

"" Environment Variables
if has('nvim')
    let vim_config_dir = '~/.config/nvim'
else
    let vim_config_dir = '~/.vim'
endif

"" Basic behaviour
set noswapfile              "disable swapfiles
set hidden                  "hide buffers when not displayed
set textwidth=80            "maximum width of text that can be inserted
" set nofoldenable            "don't fold by default
set foldmethod=syntax       "syntax based folding
set foldlevel=1
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
set shiftwidth=2
set tabstop=2
set expandtab
set autoindent
set smartindent
runtime plugins/matchit.vim

"command line configuration
set showcmd                     "display incomplete commands
set wildmenu                    "enable C-n and C-p to scroll through matches
set wildmode=full:lastused      "make cmd line tab completion similar to bash
set wildoptions=fuzzy           "use fuzzy matching to find completions
"stuff to ignore when tab completing
set wildignore=*.o,*~,*.pyc,*.hi,*.class

set colorcolumn=+1                      "mark the ideal max text width
set relativenumber                      "show relative line numbers
set number                              "show absolute current line number
set showmode                            "show current mode down the bottom
set laststatus=2

set ruler

colorscheme noctu
highlight ColorColumn cterm=reverse
highlight Pmenu cterm=bold,italic

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
nnoremap <leader>ms :setlocal spell! spelllang=en<CR>

"edit re-load config file
nnoremap <leader>ce :e $MYVIMRC<CR>
nnoremap <leader>cs :so $MYVIMRC<CR>

"close preview windows
nnoremap <leader>x :pclose<CR>

"go into paste mode
nnoremap <leader>mp :set paste<CR>a

"" Custom functions

" Custom commands
" Strip white space!
command! STW %s/\s\+$//e

"" Load plugins
let plug_path = join([vim_config_dir, 'autoload/plug.vim'], '/')
let plug_source = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

if empty(glob(plug_path))
    echo 'Installing plug...'
    execute 'silent !curl -fLo ' . plug_path ' --create-dirs ' . plug_source
endif

silent! call plug#begin()

" General plugins
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'preservim/tagbar'
Plug 'dense-analysis/ale'
Plug 'maximbaz/lightline-ale'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'pearofducks/ansible-vim'
Plug 'jamessan/vim-gnupg'

" Language plugins
Plug 'plasticboy/vim-markdown'          , { 'for': 'markdown' }
Plug 'Vimjas/vim-python-pep8-indent'    , { 'for': 'python'}
Plug 'rust-lang/rust.vim'               , { 'for': 'rust'}
Plug 'ekalinin/Dockerfile.vim'          , { 'for': 'Dockerfile' }
Plug 'ledger/vim-ledger'                , { 'for': 'dat' }
Plug 'hashivim/vim-terraform'           , { 'for': 'tf' }
Plug 'cuducos/yaml.nvim'                , { 'for': 'yaml' }
Plug 'pearofducks/ansible-vim'          , { 'for': 'yaml' }
Plug 'imsnif/kdl.vim'                   , { 'for': 'kdl' }
Plug 'Olical/conjure'                   , { 'for': ['clojure', 'guile', 'scheme'] }

" Done loading plugins
call plug#end()

"" Plugin configurations

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

nmap <silent> <leader>n <Plug>(ale_next)
nmap <silent> <leader>p <Plug>(ale_previous)
nmap <silent> <leader>N <Plug>(ale_next_wrap_error)
nmap <silent> <leader>P <Plug>(ale_previous_wrap_error)
nmap <silent> <leader>d :ALEHover<cr>
nmap <silent> <leader>D :ALEGoToDefinition<cr>
nmap <silent> <leader>r :ALEFindReferences<cr>
nmap <silent> <leader>j :ALEImport<cr>
nmap <silent> <leader>F :ALEFix<cr>

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

" Ansible
augroup yaml.ansible
    au BufRead,BufNewFile */plays/*.yaml set filetype=yaml.ansible
augroup END

" JS/JSX
" let g:jsx_ext_required = 1

" Cmdline
:cnoremap <C-A> <Home>
:cnoremap <C-F> <Right>
:cnoremap <C-B> <Left>
:cnoremap <Esc>b <S-Left>
:cnoremap <Esc>f <S-Right>

" Clojure
let g:iced_enable_default_key_mappings = v:true
augroup clojure
    autocmd FileType clojure nnoremap <buffer> <leader>C :IcedInstantConnect babashka<cr>
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
nnoremap <leader>c :Command<CR>
" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Git commit
augroup git
    autocmd FileType gitcommit setlocal spell spelllang=en
augroup END

" Java
let g:ale_java_checkstyle_config = 'checkstyle-rules.xml'

" HTML

"" Ledger
let g:ledger_default_commodity = '₹ '
augroup ledger
    autocmd FileType ledger inoremap <silent> <Tab> <C-R>=ledger#autocomplete_and_align()<CR>
    " autocmd FileType ledger inoremap <silent> <Esc> <Esc>:LedgerAlign<CR>
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
    autocmd FileType markdown,rst setlocal textwidth=70 conceallevel=0 nofoldenable
    autocmd FileType markdown,rst,text setlocal spell spelllang=en
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

" Tagbar
let g:tagbar_width = 30
nnoremap <leader>t :TagbarToggle<CR>

try
    " Load any local overrides, if any.
    source ~/.local.vim
catch
    " No such file? No problem; just ignore it.
endtry

" XML
augroup xml
    autocmd FileType xml setlocal iskeyword+=.,-
augroup END

" YAML
augroup yaml
    autocmd!
    autocmd BufEnter *.yaml,*.yml setlocal indentkeys-=0#
    autocmd FileType yaml setlocal foldmethod=indent foldlevel=2
augroup END
