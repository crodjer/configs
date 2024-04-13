-- Put this at the top of 'init.lua'
local path_package = vim.fn.stdpath('data') .. '/site'
local mini_path = path_package .. '/pack/deps/start/mini.nvim'
if not vim.loop.fs_stat(mini_path) then
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    'git', 'clone', '--filter=blob:none',
    'https://github.com/echasnovski/mini.nvim', mini_path
  }
  vim.fn.system(clone_cmd)
  vim.cmd('packadd mini.nvim | helptags ALL')
end

require('mini.ai').setup({})
require('mini.basics').setup({})
require('mini.comment').setup({})
require('mini.completion').setup({})
require('mini.deps').setup({})
require('mini.files').setup({})
require('mini.pairs').setup({})
require('mini.pick').setup({})
require('mini.statusline').setup({})
require('mini.starter').setup({})
require('mini.surround').setup({})

-- Vimscript configuration.
vim.cmd [[
set expandtab tabstop=2 softtabstop=2 shiftwidth=2
set formatoptions-=t
set relativenumber number cursorline signcolumn=yes
set clipboard+=unnamedplus
set breakindent termguicolors textwidth=80 colorcolumn=+1

let mapleader = ','
]]

local plugin = MiniDeps.add
plugin('mini.nvim')

plugin({
  source = 'catppuccin/nvim',
  depends = { 'f-person/auto-dark-mode.nvim' },
})
vim.cmd.colorscheme 'catppuccin'

local function mapl(binding, mapping, desc)
  vim.keymap.set('n', '<leader>' .. binding, mapping, {
    noremap = true, silent = true, desc = desc
  })
end

mapl('e', MiniFiles.open, "Fil[e] Browser")
mapl('b', MiniPick.builtin.buffers, "Search [B]uffers")
mapl('f', MiniPick.builtin.files, "Search [F]iles")

-- [[ Configure LSP ]]
plugin('neovim/nvim-lspconfig')
require('lsp')

-- [[ Ledger ]]
plugin('ledger/vim-ledger')
require('ledger')
