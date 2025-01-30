-------------------------------------------------------
--- Base Configuration
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
vim.o.updatetime = 300 			            -- Decrease update time
vim.o.timeoutlen = 500
vim.o.textwidth = 80			              -- Color Column
vim.o.colorcolumn = "+1"
vim.o.expandtab = true                  -- Spaces for tabs
vim.o.tabstop  = 2                      -- Two spaces for tabstops
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.formatoptions = "jcroql"          -- Default format options, without the
                                        -- `t` (text)

-- Globals
vim.g.netrw_banner = 0 			            -- Hide banner
vim.g.netrw_altv = 1
vim.g.netrw_liststyle = 3 		          -- Tree-style view
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
local path_package = vim.fn.stdpath('data') .. '/site/'
local mini_path = path_package .. 'pack/deps/start/mini.nvim'
if vim.fn.isdirectory(mini_path) == 0 then
  -- Clone 'mini.nvim' manually in a way that it gets managed by 'mini.deps',
  -- if it doesn't exist.
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    'git', 'clone', '--filter=blob:none',
    'https://github.com/echasnovski/mini.nvim', mini_path
  }
  vim.fn.system(clone_cmd)
  vim.cmd('packadd mini.nvim | helptags ALL')
  vim.cmd('echo "Installed `mini.nvim`" | redraw')
end

local function mini(plugin_name, config)
  local plugin = require('mini.' .. plugin_name)
  plugin.setup(config)
  return plugin
end

local deps = mini('deps', { path = { package = path_package }})
local add_plugin = deps.add


-------------------------------------------------------
--- Colors and Theme
-------------------------------------------------------
add_plugin({ source = "catppuccin/nvim", name = "catppuccin" })
vim.cmd.colorscheme("catppuccin")

-- Highlight on Yank.
vim.cmd([[autocmd TextYankPost * silent! lua vim.highlight.on_yank { higroup='Visual', timeout=100 }]])

-----------------------------
--- Mini Plugins
-----------------------------
mini('statusline')			        -- A bit nicer status line.
mini('pairs')				            -- Auto Pairs
mini('surround')			          -- Surround tricks
mini('trailspace') 			        -- Show trailing whitespace
mini('comment')				          -- Comments: `gc`, `gcc`
mini('align')                   -- Align text, `ga` / `gA`
mini('completion')              -- Autocompletion
mini('notify', {                -- Show notifications
  window = {
    max_width_share = 0.75,
  }
})
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
--- I tried using mini.pick and mini.extra for this, but fzf is just better
add_plugin('ibhagwan/fzf-lua')
local fzf = require('fzf-lua')
if (vim.fn.executable('sk') == 1) then
  -- Prefer skim if available.
  fzf.setup({'skim'})
end

local find_files = function()
  local git_dir = vim.fn.finddir('.git', vim.fn.getcwd() .. ";")
  if git_dir == '' then fzf.files()
  else fzf.git_files()
  end
end

local package_files = function()
  local package_indicators = {
    'Cargo.toml', 'Pipfile', 'Gemfile', 'package.json', '.git', 'shell.nix'
  }
  local parent_dir = vim.fs.dirname(vim.fn.resolve(vim.fn.expand("%:p")))
  for _, file in pairs(package_indicators) do
    local project_dir = vim.fs.dirname(vim.fs.find(file, {
      path = parent_dir,
      upward = true
    })[1])

    if project_dir then
      fzf.files({ cwd= project_dir })
      return
    end
  end

  fzf.files({ cwd = parent_dir })
end

nlmap('f', find_files, "Search [F]iles")
nlmap('e', package_files, "Search Files in packag[e].")
nlmap('b', fzf.buffers, "Search [B]uffers")
nlmap('h', fzf.oldfiles, "Search [H]istory")
nlmap('k', fzf.keymaps, "Search [K]ey Maps")
nlmap('st', fzf.treesitter, "[S]earch [T]reesitter Symbols")
nlmap('sl', fzf.live_grep, "[S]earch [L]ive Grep")
nlmap('sh', fzf.command_history, "[S]earch Command [H]istory")
nlmap('sc', fzf.commands, "[S]earch [C]ommands")

nlmap('ca', fzf.lsp_code_actions, "LSP [C]ode [A]actions")
nlmap('ci', fzf.lsp_implementations, "[C]ode LSP [I]mplementations")
nlmap('cr', fzf.lsp_references, "[C]ode LSP [R]eferences")
nlmap('cf', fzf.lsp_finder, "LSP [C]ode [F]inder")
nlmap('cdf', fzf.lsp_definitions, "LSP [C]ode [D]e[F]initions")
nlmap('cdc', fzf.lsp_declarations, "LSP [C]ode [D]e[C]larations")
nlmap('csd', fzf.lsp_document_symbols, "LSP [D]ocument [S]ymbols")
nlmap('csd', fzf.lsp_workspace_symbols, "LSP [W]orkspace [S]ymbols")
nlmap('cci', fzf.lsp_incoming_calls, "[C]ode LSP [C]alls [I]ncoming")
nlmap('cco', fzf.lsp_outgoing_calls, "[C]ode LSP [C]alls [O]utgoing")


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
  lua_ls = {
    on_init = function(client)
      if client.workspace_folders then
        local path = client.workspace_folders[1].name
        if vim.fn.filereadable(path..'/.luarc.json') == 1 or
          vim.fn.filereadable(path..'/.luarc.jsonc') == 1 then
          return
        end
      end

      client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
        workspace = {
          checkThirdParty = false,
          library = vim.api.nvim_get_runtime_file("", true)
        }
      })
    end,
    settings = {
      Lua = {}
    }
  },
  jedi_language_server = {},
  ruby_lsp = {},
  ruff = {},
  rust_analyzer = {},
  denols = {
    root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
  },
  vtsls = {
    settings = {
      typescript = {
        tsserver = {
          maxTsServerMemory = 20480
        }
      }
    },
    root_dir = lspconfig.util.root_pattern("package.json"),
    single_file_support = false
  }
}

for server, config in pairs(servers) do
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
  ensure_installed = {
    -- My primary languages.
    "python", "ruby", "clojure", "rust", "lua", "bash",
    "javascript", "typescript", "tsx", "elixir", "java", "go", "vim",

    "html", "xml", "yaml", "css", "xml", "json", "terraform", "toml", "ledger",
    "gitignore", "git_config", "gitcommit", "gitattributes"
  },
  highlight = { enable = true },
  indent = { enable = true, disable = { "ledger", "typescript" } },
  ignore_install = {},
  modules = {},
  sync_install = false,
  auto_install = false,
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

-----------------------------
--- Which Key
-----------------------------
add_plugin("folke/which-key.nvim")
local wk = require("which-key")
wk.add({
  { "<leader>s", group = "Search" },
  { "<leader>c", group = "Code" },
  { "<leader>cd", group = "Def/Dec" },
  { "<leader>cs", group = "Symbols" },
  { "<leader>cc", group = "Calls" },
})


-----------------------------
--- Other plugins
-----------------------------
--- Auto-dark mode
add_plugin({
  source = "f-person/auto-dark-mode.nvim"
})
local auto_dark_mode = require('auto-dark-mode')
auto_dark_mode.setup({
	update_interval = 1000,
	set_dark_mode = function()
		vim.api.nvim_set_option_value('background', 'dark', {})
	end,
	set_light_mode = function()
		vim.api.nvim_set_option_value('background', 'light', {})
	end,
})

-- Ember handlebars
add_plugin("joukevandermaas/vim-ember-hbs")

-- If custom overrides exist, load them.
pcall(require, 'custom')
