" Stoic Vim
"""""""""""
" A minimalist Neovim configuration, primarily in Vimscript.
" For simple configuration, Vimscript is simply much more ergonomic than Lua.

" Plugins
"""""""""
" Initialization of the core (and only) plugins
let s:plugins = ['junegunn/fzf', 'junegunn/fzf.vim', 'neovim/nvim-lspconfig']
let s:plugins_path = stdpath('config') . '/pack/vendor/start'

for s:repo in s:plugins
  let s:name = fnamemodify(s:repo, ':t')
  let s:dest = s:plugins_path . '/' . s:name
  if !isdirectory(s:dest)
    execute '!git clone --depth 1 https://github.com/' . s:repo . ' ' . s:dest
  endif
endfor

" Update plugins
command! PlugUpdate for d in split(glob(s:plugins_path . '/*'), '\n') |
                  \   execute '!git -C ' . d . ' pull -q' |
                  \ endfor

" Colors
"""""""""
set notermguicolors
set background=light
colorscheme vim

" We don't need a highlight on the SignColumn
highlight SignColumn ctermbg=none cterm=bold
highlight ColorColumn ctermbg=116
" highlight Error cterm=undercurl ctermbg=none ctermfg=red
highlight SpellBad ctermbg=none cterm=undercurl
highlight SpellRare ctermbg=none cterm=underdotted

" The floating hint highlight is too light. Match with `Info` instead.
highlight link DiagnosticFloatingHint DiagnosticFloatingInfo
" This is a nicer color for menu highlights.
highlight Pmenu ctermbg=116
highlight PmenuSel ctermbg=0 ctermfg=116 cterm=bold

" Options
""""""""""
set number relativenumber
set clipboard=unnamedplus
set ignorecase smartcase
set undofile
set list

set spelllang=en
set spellfile=~/.config/nvim/spell/en.utf-8.add

set textwidth=80 colorcolumn=+1
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab

" File path. Use LineNr highlight group.
set statusline=\ %f%m\ %=
" LSP
set statusline+=%{get(b:,'lsp_status','')}
" File type, percentage in file, lines/total lines:column
set statusline+=\ \ %Y\ \ %p%%\ \ %l/%L:%c\      " Don't trim space on end.

" Allow custom configuration per directory (.nvimrc, .nvim.lua)
set exrc secure

" File Types
""""""""""""
filetype indent plugin on

" Configure file type specific config in:
" neovim/.config/nvim/after/ftplugin/<ft>.[vim|lua]

" Bindings and Functions
""""""""""""""""""""""""
let mapleader = ","
let maplocalleader = ","

nnoremap <Leader>$ :source $MYVIMRC<CR>"

autocmd TermOpen * startinsert
command! Trw execute '%s/\s\+$//e'

" Plugins Configurations
"""""""""""""""""""""""
" Fzf
nmap <leader>b :Buffers<CR>
nmap <leader>f :Files<CR>
nmap <leader>g :GFiles<CR>
nmap <leader>h :History<CR>
nmap <leader>m :Marks<CR>
nmap <leader>sl :Rg<CR>
nmap <leader>ss :History/<CR>

" LSP
lua << END
vim.lsp.enable('ansiblels')
vim.lsp.enable('denols')
vim.lsp.config('vtsls', {
    cmd = { 'bun', 'vtsls', '--stdio' },
    root_markers = {"package.json"},
})
vim.lsp.enable('ty')
vim.lsp.enable('vtsls')
vim.lsp.enable('rubocop')
vim.lsp.enable('ruby_lsp')
vim.lsp.enable('rust_analyzer')
vim.lsp.enable('clojure_lsp')
vim.lsp.enable('gleam')

-- Show diagnostics for the current line
vim.keymap.set(
  "n", "<leader>d", function ()
    local opts = { focusable = true, border = "single", source = "always" }
    vim.diagnostic.open_float(nil, opts)
  end,
  { noremap = true, silent = true, desc = "Show line diagnostics" }
)
vim.keymap.set(
  "n", "<leader>ca", vim.lsp.buf.code_action,
  { noremap = true, silent = true, desc = "LSP Code Actions" }
)
vim.keymap.set(
  "n", "<leader>cf", vim.lsp.buf.format,
  { noremap = true, silent = true, desc = "LSP Formatting" }
)

--  Update `lsp_status` for use in status line
vim.api.nvim_create_autocmd({ "LspAttach", "LspDetach" }, {
  callback = function(args)
    local clients = vim.lsp.get_clients({ bufnr = args.buf })
    local names = vim.iter(clients):map(function(c) return c.name end):join(", ")
    vim.api.nvim_buf_set_var(args.buf, "lsp_status", names)
  end
})
END

" Autocommands
""""""""""""""

" LSP
augroup LspStatuslineUpdate
  autocmd!
  autocmd User LspProgressUpdate redrawstatus!
  autocmd User LspAttach redrawstatus!
  autocmd User LspDetach redrawstatus!
augroup END

" Spell Check
augroup SpellCheck
  autocmd FileType markdown setlocal spell
  autocmd FileType gitcommit setlocal spell
  autocmd FileType text setlocal spell
augroup END

" Ansible
au BufRead,BufNewFile */plays/**.y*ml set filetype=yaml.ansible

" Rust
let g:rustfmt_autosave = 1
augroup rust
  autocmd FileType rust set tw=80
augroup END

" Jrnl
autocmd BufNewFile,BufReadPre *.jrnl
      \ setlocal filetype=gitcommit |   " Mail like editor with spell check!
      \ setlocal shada= noswapfile noundofile nobackup nowritebackup |
      \ setlocal noshelltemp history=0 nomodeline secure
