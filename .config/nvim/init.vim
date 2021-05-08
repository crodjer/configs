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
set clipboard=unnamedplus   "use system clipboard
set mouse-=a
set cursorline
set signcolumn=yes:1

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
if has('nvim')
    runtime plugins/matchit.vim
else
    runtime macros/matchit.vim
end

"command line configuration
set showcmd                 "display incomplete commands
set wildmode=list           "make cmd line tab completion similar to bash
set wildmenu                "enable C-n and C-p to scroll through matches
"stuff to ignore when tab completing
set wildignore=*.o,*~,*.pyc,*.hi,*.class

"" Looks
set background=light
colorscheme tango
set colorcolumn=+1                      "mark the ideal max text width
set relativenumber                      "show relative line numbers
set number                              "show absolute current line number
set showmode                            "show current mode down the bottom
set laststatus=2

set ruler
highlight Normal guibg=#fdf6e3 ctermbg=None
highlight SpellBad cterm=underline gui=underline guisp=Grey
highlight rubyDefine ctermbg=None
" highlight ColorColumn ctermbg=240
highlight SignColumn ctermbg=None

"display tabs and trailing spaces
set list
set listchars=tab:\ \ ,trail:⋅,nbsp:⋅

"disable paste mode
set nopaste
augroup paste
  autocmd InsertLeave * set nopaste
augroup END

"" GUI
set guifont=Ubuntu\ Mono:h15

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

"auto insert matching pair
" inoremap { {}<Esc>i
" inoremap ( ()<Esc>i
" inoremap [ []<Esc>i

"edit re-load config file
nnoremap <leader>ce :e $MYVIMRC<CR>
nnoremap <leader>cs :so $MYVIMRC<CR>

"" Custom functions

" Custom commands
command! STW %s/\s\+$//e


"" Load plugins
if empty(glob(plug_path))
    echo 'Installing plug...'
    execute 'silent !curl -fLo ' . plug_path ' --create-dirs ' . plug_source
endif

silent! call plug#begin()

" General plugins
" Plug 'scrooloose/syntastic'
Plug 'dense-analysis/ale'
Plug 'itchyny/lightline.vim'
Plug 'maximbaz/lightline-ale'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'preservim/tagbar'

" Language plugins
Plug 'plasticboy/vim-markdown'       , { 'for': ['markdown', 'md', 'mkd'] }
Plug 'rust-lang/rust.vim'            , { 'for': ['rust']}
Plug 'racer-rust/vim-racer'          , { 'for': ['rust']}
Plug 'cespare/vim-toml'              , { 'for': ['toml']}
Plug 'pangloss/vim-javascript'       , { 'for': ['js', 'jsx', 'json']}
Plug 'ekalinin/Dockerfile.vim'       , { 'for': 'Dockerfile' }
Plug 'kchmck/vim-coffee-script'      , { 'for': 'coffee' }
Plug 'Vimjas/vim-python-pep8-indent' , { 'for': 'python' }
Plug 'leafgarland/typescript-vim'    , { 'for': ['ts'] }
Plug 'tpope/vim-fireplace'           , { 'for': ['clojure', 'clj'] }
Plug 'ElmCast/elm-vim'               , { 'for': ['elm'] }
Plug 'artur-shaik/vim-javacomplete2' , { 'for': ['java'] }
Plug 'ledger/vim-ledger'             , { 'for': ['dat'] }

" Done loading plugins
call plug#end()

"" Plugin configurations
let g:AutoPairsFlyMode = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Ack
if executable('rg')
    let g:ackprg = 'rg --vimgrep'
elseif executable('ag')
    let g:ackprg = 'ag --vimgrep'
endif

" Ale
let g:ale_lint_on_text_changed = 'never'
" let g:ale_open_list = 1
let g:ale_sign_error = 'x'
let g:ale_sign_warning = '!'
let g:ale_sign_column_always = 1

nmap <silent> <leader>aj :ALENext<cr>
nmap <silent> <leader>ak :ALEPrevious<cr>
nmap <silent> <leader>ag :ALEGoToDefinition<cr>
augroup CloseLoclistWindowGroup
  autocmd!
  autocmd QuitPre * if empty(&buftype) | lclose | endif
augroup END

let g:lightline = {}

let g:lightline.component_expand = {
      \  'linter_checking': 'lightline#ale#checking',
      \  'linter_infos': 'lightline#ale#infos',
      \  'linter_warnings': 'lightline#ale#warnings',
      \  'linter_errors': 'lightline#ale#errors',
      \  'linter_ok': 'lightline#ale#ok',
      \ }
let g:lightline.component_type = {
      \     'linter_checking': 'raw',
      \     'linter_infos': 'raw',
      \     'linter_warnings': 'raw',
      \     'linter_errors': 'raw',
      \     'linter_ok': 'raw',
      \ }
let g:lightline#ale#indicator_checking = '⌛ '
let g:lightline#ale#indicator_infos = 'ℹ️  '
let g:lightline#ale#indicator_warnings = '⚠️  '
let g:lightline#ale#indicator_errors = '❌ '
let g:lightline#ale#indicator_ok = '✅ '

"" Language configurations

" JS/JSX
let g:jsx_ext_required = 1
let g:syntastic_javascript_checkers = ['eslint']
augroup javascript
    autocmd FileType javascript setlocal sw=2 sts=2 et
augroup END

" Clojure
augroup clojure
    autocmd FileType clojure nnoremap <buffer> <leader>l :%Eval<cr>
augroup END

" Crontab
augroup crontab
    autocmd FileType crontab setlocal backupcopy=yes
augroup END

" Coffee
augroup coffee
    autocmd FileType coffee setlocal sw=2 sts=2 et
    let g:tagbar_type_coffee = {
        \ 'ctagstype' : 'coffee',
        \ 'kinds'     : [
            \ 'c:classes',
            \ 'm:methods',
            \ 'f:functions',
            \ 'v:variables',
            \ 'f:fields',
        \ ]
    \ }
    autocmd FileType coffee setlocal sw=2 sts=2 et foldmethod=indent foldnestmax=3
augroup END

" FZF
augroup fzf
    autocmd! FileType fzf
    autocmd  FileType fzf set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
augroup END
nnoremap <leader>s :GFiles<CR>
nnoremap <leader>as :FZF<CR>
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
augroup java
    autocmd FileType java setlocal sw=4 sts=4 et omnifunc=javacomplete#Complete
augroup END
let g:syntastic_java_javac_executable= '*.jar'

" Ledger
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
    autocmd FileType markdown,rst setlocal sw=2 sts=2 et textwidth=70 conceallevel=2
    autocmd FileType markdown,rst,text setlocal spell spelllang=en nowrap
augroup END

" Python
let g:syntastic_python_checkers = ['pylint']

" Ruby
augroup ruby
    autocmd FileType ruby setlocal sw=2 sts=2
augroup END

" Rust
let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1
let g:rustfmt_autosave = 1
augroup rust
    autocmd FileType rust setlocal textwidth=80
    autocmd FileType rust map <buffer> <leader>rt :RustTest<CR>
augroup END

" YAML
augroup yaml
    autocmd FileType yaml setlocal sw=2 sts=2
augroup END

" Tagbar
let g:tagbar_width = 30
nnoremap <leader>t :TagbarToggle<CR>
