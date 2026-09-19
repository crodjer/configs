" Stoic Vim
"""""""""""
" A minimalist Neovim configuration, primarily in Vimscript.
" For simple configuration, Vimscript is simply much more ergonomic than Lua.


" Plugins
"""""""""
" Install plugins
let s:plugins = [
      \ 'https://github.com/junegunn/fzf',
      \ 'https://github.com/junegunn/fzf.vim',
      \ 'https://github.com/ibhagwan/fzf-lua',
      \ 'https://github.com/obsidian-nvim/obsidian.nvim',
      \ 'https://github.com/neovim/nvim-lspconfig',
      \ 'https://github.com/jiangmiao/auto-pairs',
      \ 'https://github.com/Olical/conjure',
      \ 'https://codeberg.org/ziglang/zig.vim'
      \ ]
let s:plugins_path = stdpath('data') . '/site/pack/vendor/opt'

for s:repo in s:plugins
  let s:name = fnamemodify(s:repo, ':t')
  let s:dest = s:plugins_path . '/' . s:name
  if !isdirectory(s:dest)
    execute '!git clone --depth 1 ' . s:repo . ' ' . s:dest
    silent! execute 'helptags ' . s:dest . '/doc'
  endif
endfor

" Update plugins
command! PlugUpdate for d in split(glob(s:plugins_path . '/*'), '\n') |
                  \   execute '!git -C ' . d . ' pull -q' |
                  \   silent! execute 'helptags ' . d . '/doc' |
                  \ endfor

" Initialize plugins
packadd fzf
packadd fzf.vim
packadd auto-pairs
packadd nvim-lspconfig
packadd fzf-lua
packadd obsidian.nvim

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

" Allow undoing just a <CR>
" inoremap <CR> <C-g>u<CR>

autocmd TermOpen * startinsert
command! Trw execute '%s/\s\+$//e'

" Delete till this line twice.
" Useful to undo an accidental newline without having `u` undo all that was
" typed.
inoremap <M-BS> <C-u><C-u>

" Plugins Configurations
"""""""""""""""""""""""

lua << END
-- Fzf
require("fzf-lua").setup({
  fzf_opts = {
    ['--layout'] = 'default',
  },
})

-- Obsidian
vim.api.nvim_create_autocmd("BufEnter", {
  pattern = vim.fn.expand("~/documents/notes") .. "/**.md",
  callback = function()
    vim.opt_local.conceallevel = 2
  end,
})

local obsidian_actions = require("obsidian.actions")
require("obsidian").setup({
  legacy_commands = false,
  workspaces = {
    { name = "notes", path = "~/documents/notes" },
  },
  daily_notes = {
    enabled = true,
    folder = "daily",
    workdays_only = false,
    date_format = "daily-note-YYYY-MM-DD",
  },
  picker = {
    name = "fzf-lua",
  },
  callbacks = {
    enter_note = function ()
      -- Disable [ ] auto-pairing in Obsidian notes.
      if vim.b.AutoPairs then
        vim.b.AutoPairs["["] = nil

        pcall(vim.keymap.del, "i", "[", { buffer = true })
        pcall(vim.keymap.del, "i", "]", { buffer = true })

        vim.fn.AutoPairsInit()
      end

      -- Typing [[ opens the note picker and inserts the selected link.
      vim.keymap.set(
        "i",
        "[[",
        "<C-o>:lua require('obsidian.actions').insert_link()<CR>",
        { buffer = true, desc = "Insert Obsidian link" }
      )

      vim.keymap.set("x", "<leader>ol", obsidian_actions.link, {
        buffer = true,
        desc = "Link selected text",
      })
    end

  }
})

-- LSP
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
vim.lsp.enable('zls')

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

" Fzf Bindings
nmap <leader>b :FzfLua buffers<CR>
nmap <leader>f :FzfLua files<CR>
nmap <leader>g :FzfLua git_files<CR>
nmap <leader>h :FzfLua history<CR>
nmap <leader>m :FzfLua marks<CR>

" Look in the same directory as the current file.
nmap <leader>F :lua FzfLua.files({
      \ cwd = vim.fn.expand('%:p:h')
      \ })<CR>

" Look in the parent directory of the directory the current file is in.
nmap <leader>P :lua FzfLua.files({
      \ cwd = vim.fn.expand('%:p:h:h')
      \ })<CR>

nmap <leader>sl :FzfLua grep_project<CR>
nmap <leader>ss :FzfLua search_history<CR>

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

" Clojure
augroup clojure
  autocmd!
  autocmd FileType clojure,fennel packadd conjure
  autocmd FileType clojure let b:AutoPairs = copy(g:AutoPairs)
    \ | call remove(b:AutoPairs, "'")
    \ | call remove(b:AutoPairs, '`')
augroup END

let g:conjure#mapping#doc_word = v:false
let g:conjure#client#clojure#nrepl#connection#auto_repl#enabled = v:false

let g:conjure#mapping#doc_word = v:false
let g:conjure#client#clojure#nrepl#connection#auto_repl#enabled = v:false


" Vim
autocmd FileType vim let b:AutoPairs = copy(g:AutoPairs)  | call remove(b:AutoPairs, "\"")

" Rust
let g:rustfmt_autosave = 1
augroup rust
  autocmd FileType rust set tw=80
augroup END

" Zig
augroup zig
  autocmd!
  autocmd FileType zig packadd zig.vim
augroup END
