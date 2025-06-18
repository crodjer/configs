" Stoic Vim
"""""""""""
" A minimalist Neovim configuration, primarily in Vimscript.
" For simple configuration, Vimscript is simply much more ergonomic than Lua.

" Colors
"""""""""
set notermguicolors
set background=light
colorscheme vim

" Options
""""""""""
set number relativenumber
set clipboard=unnamedplus
set undofile

set textwidth=80 colorcolumn=+1
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab

" File path. Use LineNr highlight group.
set statusline=\ %f%m\ %=
" LSP
set statusline+=%{luaeval('lspStatus()')}
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
vim.lsp.enable('denols')
vim.lsp.enable('ruby_lsp')
vim.lsp.enable('rust_analyzer')
vim.lsp.enable('lua-language-server')

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

function lspStatus()
  local clients = vim.lsp.get_clients({ bufnr = vim.api.nvim_get_current_buf() })
  if #clients == 0 then
    return ""
  end

  return vim.iter(clients):map(function(c) 
    return c.server_info.name
  end):join(", ")
end
END

augroup LspStatuslineUpdate
  autocmd!
  autocmd User LspProgressUpdate redrawstatus!
  autocmd User LspAttach redrawstatus!
  autocmd User LspDetach redrawstatus!
augroup END
