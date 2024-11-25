-- Clone 'mini.nvim' manually in a way that it gets managed by 'mini.deps'
local path_package = vim.fn.stdpath('data') .. '/site/'
local mini_path = path_package .. 'pack/deps/start/mini.nvim'
if not vim.loop.fs_stat(mini_path) then
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    'git', 'clone', '--filter=blob:none',
    'https://github.com/echasnovski/mini.nvim', mini_path
  }
  vim.fn.system(clone_cmd)
  vim.cmd('packadd mini.nvim | helptags ALL')
  vim.cmd('echo "Installed `mini.nvim`" | redraw')
end

function mini(plugin, config)
  return require('mini.' .. plugin).setup(config)
end

mini('deps', { path = { package = path_package }})


local plugin = MiniDeps.add

-----------------------------
--- Colors and Theme
-----------------------------
-- This works
vim.o.termguicolors = false
vim.o.background = "light"
vim.cmd.colorscheme("vim")

-- vim.cmd([[highlight Pmenu ctermbg=None ctermfg=None]])

-- Highlight on Yank.
vim.cmd([[autocmd TextYankPost * silent! lua vim.highlight.on_yank { higroup='Visual', timeout=300 }]])

-----------------------------
--- Mini Plugins
-----------------------------

mini('statusline')
mini('pairs')
mini('surround')
-- Show trailing whitespace
mini('trailspace')
mini('pick')

--  `gc` for motion, `gcc` for line, `gc` for visual.
mini('comment')

-- Visualize and work with indent scope
mini('indentscope', {
  symbol = "|",
})

-- Highlight word under cursor!
mini('cursorword')

-- File Browser: lua MiniFiles.open()
mini('files')

-- Bracketed navigation.
-- [ + upper-suffix : go first.
-- [ + lower-suffix : go backward.
-- ] + lower-suffix : go forward.
-- ] + upper-suffix : go last.
-- b => Buffer, c => Comment,  => Diagnostic, t => Tree-sitter, q => Quickfix, u => Undo states, y => Yank
mini('bracketed')

-- A few useful functions, particularly `put` and `put_text` in global.
mini('misc', {
  make_global = { 'put', 'put_text' },
})

-- `mini.icons`
-- TODO: Maybe doesn't need setup?
-- mini('icons')

-- `mini.git`
-- Mini's Git integration.
-- TODO: For some reason, doesn't work.
-- require('mini.git').setup()

-- TODO: Checkout other `mini` plugins:
-- `mini.diff`: Diff editing?
-- `mini.extra`: Things like pickers for diagnostic, explorer, history, lsp, tree-sitter etc.
-- `mini.fuzzy`: Fuzzy Matcher - Only works with Telescope?
