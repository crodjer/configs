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

"" GUI
set guifont=SourceCodePro:h10

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

"close preview windows
nnoremap <leader>pm :set paste<CR>a

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
Plug 'itchyny/lightline.vim'
Plug 'Raimondi/delimitMate'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'preservim/tagbar'
Plug 'morhetz/gruvbox'

if has('nvim-0.5')
    Plug 'neovim/nvim-lspconfig'
else
    Plug 'dense-analysis/ale'
    Plug 'maximbaz/lightline-ale'
endif

" Language plugins
Plug 'plasticboy/vim-markdown'       , { 'for': ['markdown', 'md', 'mkd'] }
Plug 'rust-lang/rust.vim'            , { 'for': ['rust']}
Plug 'cespare/vim-toml'              , { 'for': ['toml']}
Plug 'pangloss/vim-javascript'       , { 'for': ['js', 'jsx', 'json']}
Plug 'ekalinin/Dockerfile.vim'       , { 'for': 'Dockerfile' }
Plug 'Vimjas/vim-python-pep8-indent' , { 'for': 'python' }
Plug 'ledger/vim-ledger'             , { 'for': ['dat'] }

" Done loading plugins
call plug#end()

"" Plugin configurations

" Gruvbox
let g:gruvbox_termcolors=16
colorscheme gruvbox

" Lightline
let g:lightline = {}

" LSP Config / Ale
if has('nvim-0.5')
    lua << EOF
    local nvim_lsp = require('lspconfig')

    -- Use an on_attach function to only map the following keys
    -- after the language server attaches to the current buffer
    local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    --Enable completion triggered by <c-x><c-o>
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = { noremap=true, silent=true }

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
    buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
    buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { "rust_analyzer" }
for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup { on_attach = on_attach }
end
EOF
else
    let g:ale_lint_on_text_changed = 'never'
    " let g:ale_open_list = 1
    let g:ale_sign_error = 'x'
    let g:ale_sign_warning = '!'
    let g:ale_sign_column_always = 1

    set omnifunc=ale#completion#OmniFunc
    let g:ale_completion_enabled = 1
    let g:ale_completion_autoimport = 1
    let g:ale_fixers = {
                \ '*': ['remove_trailing_lines', 'trim_whitespace']
                \ }
    let g:ale_linters = {
                \ 'python': ['pyls', 'pylint'],
                \ 'rust': ['analyzer', 'rls', 'cargo'],
                \ }
    let g:ale_use_global_executables = 1

    nmap <silent> <leader>aj :ALENext<cr>
    nmap <silent> <leader>ak :ALEPrevious<cr>
    nmap <silent> <leader>ah :ALEHover<cr>
    nmap <silent> <leader>ag :ALEGoToDefinition<cr>
    nmap <silent> <leader>ar :ALEFindReferences<cr>

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
endif

"" Language configurations

" JS/JSX
let g:jsx_ext_required = 1
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
    autocmd FileType markdown,rst,text setlocal spell spelllang=en nowrap
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
augroup END

" YAML
augroup yaml
    autocmd FileType yaml setlocal sw=2 sts=2
augroup END

" Tagbar
let g:tagbar_width = 30
nnoremap <leader>t :TagbarToggle<CR>
