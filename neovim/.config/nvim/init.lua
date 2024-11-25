-------------------------------------------------------
--- Base Config
-------------------------------------------------------


-- Options
-- See `:help vim.o`

vim.o.hlsearch = false 			            -- Set highlight on search
vim.wo.number = true			              -- Make line numbers default
vim.wo.relativenumber = true		        -- Relative line numbers
vim.o.mouse = 'a'			                  -- Enable mouse mode
vim.o.clipboard = 'unnamedplus'		      -- Sync clipboard with OS
vim.o.breakindent = true
vim.o.undofile = true			              -- Save undo history!
vim.o.ignorecase = true			            -- Case-insensitive searching unless
vim.o.smartcase = true			            -- \C or capital in search
vim.wo.signcolumn = 'yes' 		          -- Keep signcolumn on by default
vim.o.updatetime = 100 			            -- Decrease update time
vim.o.timeoutlen = 200
vim.o.textwidth = 80			              -- Color Column
vim.o.colorcolumn = "+1"
vim.o.expandtab = true                  -- Spaces for tabs
vim.o.tabstop  = 2                      -- Two spaces for tabstops
vim.o.softtabstop = 2
vim.o.shiftwidth = 2


-- Globals
vim.g.netrw_banner = 0 			-- Hide banner
vim.g.netrw_altv = 1
vim.g.netrw_liststyle = 3 		-- Tree-style view
vim.g.netrw_list_hide = ([[,\(^\|\s\s\)\zs\.\S\+]])

-- Set <comma> as the leader key
vim.g.mapleader = ','
vim.g.maplocalleader = ','

-- Functions
local function nmap(binding, mapping, desc)
  -- Helper function to map in normal mode.
  vim.keymap.set('n', binding, mapping, {
    noremap = true, silent = true, desc = desc
  })
end

local function nlmap(binding, mapping, desc)
  -- Helper function specifically for leader mappings.
  nmap('<leader>' .. binding, mapping, desc)
end

-------------------------------------------------------
--- Mini (install and deps plugin)
-------------------------------------------------------
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
  local plugin = require('mini.' .. plugin)
  plugin.setup(config)
  return plugin
end

local deps = mini('deps', { path = { package = path_package }})
local add_plugin = MiniDeps.add


-------------------------------------------------------
--- Colors and Theme
-------------------------------------------------------
vim.o.termguicolors = false
vim.cmd.colorscheme("minischeme")
vim.cmd([[highlight Normal ctermbg=None]])

-- Highlight on Yank.
vim.cmd([[autocmd TextYankPost * silent! lua vim.highlight.on_yank { higroup='Visual', timeout=100 }]])

-----------------------------
--- Mini Plugins
-----------------------------
mini('statusline')			        -- A bit nicer status line.
mini('pairs')				            -- Auto Pairs
mini('surround')			          -- Surround tricks
mini('trailspace') 			        -- Show trailing whitespace
mini('pick')				            -- Picker
mini('extra')				            -- Extra pickers and goodies
mini('comment')				          -- Comments: `gc`, `gcc`
mini('align')                   -- Align text, `ga` / `gA`
mini('completion')              -- Autocompletion
mini('notify')                  -- Show notifications
mini('move', {                  -- Move selections <M-h|j|k|l>
  -- Module mappings. Use `''` (empty string) to disable one.
  mappings = {
    -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
    left = '<',
    right = '>',

    -- Move current line in Normal mode
    line_left = '<',
    line_right = '>',
  },
})

-- Visualize and work with indent scope
mini('indentscope', {
  delay = 30,
  symbol = "│",
})

-- Highlight word under cursor!
mini('cursorword')

-- Bracketed navigation.
-- [ + upper-suffix : go first.
-- [ + lower-suffix : go backward.
-- ] + lower-suffix : go forward.
-- ] + upper-suffix : go last.
-- b => Buffer, c => Comment,  => Diagnostic, t => Tree-sitter, q => Quickfix, u => Undo states, y => Yank
mini('bracketed')

-----------------------------
--- File Picker
-----------------------------
local find_files = function()
  local git_dir = vim.fn.finddir('.git', vim.fn.getcwd() .. ";")
  if git_dir == '' then
    MiniPick.builtin.files()
  else
    MiniExtra.pickers.git_files()
  end
end

local find_in_package = function()
  local interesting_files = {
    'Cargo.toml',
    'Pipfile',
    'package.json',
    '.git',
    'shell.nix'
  }
  local parent_dir = vim.fn.expand("%:p:h")
  for _, file in pairs(interesting_files) do
    local project_dir = vim.fs.dirname(vim.fs.find(file, {
      path = parent_dir,
      upward = true
    })[1])

    if project_dir then
      MiniPick.start({ source = { items = vim.fn.readdir(project_dir) } })
      return
    end
  end

  MiniPick.start({ source = { items = vim.fn.readdir(parent_dir) } })
end

nlmap('f', find_files, "Search [F]iles")
nlmap('e', find_in_package, "Search Files in packag[e].")
nlmap('b', MiniPick.builtin.buffers, "Search [B]uffers")
nlmap('h', MiniExtra.pickers.oldfiles, "Search [H]istory")
nlmap('sl', MiniPick.builtin.grep_live, "[S]earch [L]ive Grep")
nlmap('sh', MiniExtra.pickers.history, "[S]earch Command [H]istory")
nlmap('sc', MiniExtra.pickers.commands, "[S]earch [C]ommands")

-----------------------------
--- LSP
-----------------------------
add_plugin({
  source = "neovim/nvim-lspconfig",
})
local lspconfig = require('lspconfig');
local servers = {
  ansiblels = {},
  elixirls = {},
  gopls = {},
  html = {},
  jdtls = {},
  lua_ls = {},
  nil_ls = {},
  rust_analyzer = {},
  solargraph = {},
  vtsls = {}
}

for server, config in pairs(servers) do
  config.on_attach = on_attach
  lspconfig[server].setup(config)
end

-----------------------------
--- Tree-sitter
-----------------------------
add_plugin({
  source = 'nvim-treesitter/nvim-treesitter',
  -- Use 'master' while monitoring updates in 'main'
  checkout = 'master',
  monitor = 'main',
  -- Perform action after every checkout
  hooks = { post_checkout = function() vim.cmd('TSUpdate') end },
})

require('nvim-treesitter.configs').setup {
  sync_install = false,
  auto_install = false,
  highlight = {
    enable = true,
  },
  indent = { enable = true, disable = { "ledger", "ruby" } },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "n",
      scope_incremental = "s",
      node_decremental = "p",
    },
  },
}
