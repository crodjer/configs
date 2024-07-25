-- Extended from nvim-lua/kickstart.nvim

-- Set <comma> as the leader key
vim.g.mapleader = ','
vim.g.maplocalleader = ','

if not package.loaded["lazy"] then
  -- [[ Install `lazy.nvim` plugin manager ]]
  --    https://github.com/folke/lazy.nvim
  --    `:help lazy.nvim.txt` for more info
  local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
      'git',
      'clone',
      '--filter=blob:none',
      'https://github.com/folke/lazy.nvim.git',
      '--branch=stable', -- latest stable release
      lazypath,
    }
  end
  vim.opt.rtp:prepend(lazypath)

  -- [[ Configure plugins ]]
  -- NOTE: Here is where you install your plugins.
  --  You can configure plugins using the `config` key.
  --
  --  You can also configure plugins after the setup call,
  --    as they will be available in your neovim runtime.
  require('lazy').setup({
    -- Git related plugins
    'tpope/vim-fugitive',
    'tpope/vim-rhubarb',

    -- Detect tabstop and shiftwidth automatically
    'tpope/vim-sleuth',

    {
      -- LSP Configuration & Plugins
      'neovim/nvim-lspconfig',
      dependencies = {
        -- Useful status updates for LSP
        { 'j-hui/fidget.nvim', opts = {} },

        -- Additional lua configuration, makes nvim stuff amazing!
        {
          'folke/neodev.nvim',
          filetype = 'lua',
          opts = {
            override = function(root_dir, library)
              -- Since I symlink my init.lua, neodev doesn't detect this right.
              if root_dir:sub(- #"nvim") == "nvim" then
                library.enabled = true
                library.plugins = true
              end
            end
          },
        },
      },
    },

    { 'nvim-treesitter', {{
      "nvim-treesitter/nvim-treesitter",
      build = ":TSUpdate",
      dependencies = { 'nvim-treesitter/nvim-treesitter-textobjects', },
      config = function ()
        local configs = require("nvim-treesitter.configs")

        vim.defer_fn(function()
          configs.setup({
            ensure_installed = {
              -- My primary languages.
              "python", "ruby", "clojure", "rust", "lua", "bash",
              -- Front-end
              "javascript", "typescript", "tsx", "html", "xml",
              "java", "go"
            },
            sync_install = false,
            highlight = { enable = true },
            indent = { enable = true, disable = { "ledger", "ruby" } },
          })
        end, 0)
      end
    }}},

    { 'folke/which-key.nvim',  opts = {} },

    {
      -- Adds git related signs to the gutter, as well as utilities for managing changes
      'lewis6991/gitsigns.nvim',
      opts = {
        -- See `:help gitsigns.txt`
        signs = {
          add = { text = '+' },
          change = { text = '~' },
          delete = { text = '_' },
          topdelete = { text = '‾' },
          changedelete = { text = '~' },
        },
      },
    },

    {
      'catppuccin/nvim',
      config = function()
        vim.cmd.colorscheme 'catppuccin'
      end,
      dependencies = {
        { 'f-person/auto-dark-mode.nvim', opts = {} }
      }
    },

    {
      -- Set lualine as statusline
      'nvim-lualine/lualine.nvim',
      -- See `:help lualine.txt`
      opts = {
        options = {
          theme = 'catppuccin',
          section_separators = { left = '', right = '' },
          component_separators = { left = '', right = '' }
        },
      },
    },

    {
      -- Add indentation guides even on blank lines
      'lukas-reineke/indent-blankline.nvim',
      -- Enable `lukas-reineke/indent-blankline.nvim`
      -- See `:help ibl`
      main = 'ibl',
      opts = {},
    },

    -- "gc" to comment visual regions/lines
    { 'numToStr/Comment.nvim', opts = {} },

    {
      "ibhagwan/fzf-lua",
      -- optional for icon support
      dependencies = { "nvim-tree/nvim-web-devicons" },
      opts = {}
    },

    {
      'windwp/nvim-autopairs',
      event = "InsertEnter",
      opts = {} -- this is equalent to setup({}) function
    },

    {
      'pearofducks/ansible-vim',
      build = 'UltiSnips/generate.sh',
      ft = 'yaml',
      config = function()
        vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
          pattern = { "*/plays/*.yaml*" },
          command = "set filetype=yaml.ansible",
        })
      end
    },

    {
      'ledger/vim-ledger',
      version = "*",
      lazy = true,
      ft = 'ledger',
      init = function()
        vim.g.ledger_default_commodity = '₹ '
      end,
      config = function()
        vim.keymap.set('i', '<Tab>', [[<C-R>=ledger#autocomplete_and_align()<CR>]], { noremap = true, silent = true, })
        vim.keymap.set('n', '<Tab>', [[:LedgerAlign<CR>]], { noremap = true, silent = true, })
        vim.keymap.set('v', '<Tab>', [[:LedgerAlign<CR>]], { noremap = true, silent = true, })
        vim.keymap.set('', '{', [[?^\d<CR>]], { noremap = true, silent = true, })
        vim.keymap.set('', '}', [[/^\d<CR>]], { noremap = true, silent = true, })
      end
    },
  }, {})
end

-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true
vim.wo.relativenumber = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 100
vim.o.timeoutlen = 200

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true

-- Show colorcolumn
vim.o.textwidth = 80
vim.o.colorcolumn = "+1"

-- [[ Basic Keymaps ]]

-- [[ FZF Keymaps ]]
local fzf = require('fzf-lua')

local function map(binding, mapping, desc)
  vim.keymap.set('n', '<leader>' .. binding, mapping, {
    noremap = true, silent = true, desc = desc
  })
end


local find_files = function()
  local git_dir = vim.fn.finddir('.git', vim.fn.getcwd() .. ";")
  if git_dir == '' then
    fzf.files()
  else
    fzf.git_files()
  end
end

local find_in_package = function()
  interesting_files = {
    'Cargo.toml',
    'Pipfile',
    'package.json',
    '.git',
    'shell.nix'
  }
  local parent_dir = vim.fn.expand("%:p:h")
  for _, file in pairs(interesting_files) do
    project_dir = vim.fs.dirname(vim.fs.find(file, {
      path = parent_dir,
      upward = true
    })[1])
    print(project_dir)

    if project_dir then
      fzf.files({ cwd=project_dir })
      return
    end
  end

  fzf.files({ cwd=parent_dir })
end

map('f', find_files, "Search [F]iles")
map('e', find_in_package, "Search Files in packag[e].")
map('b', fzf.buffers, "Search [B]uffers")
map('h', fzf.oldfiles, "Search [H]istory")
map('sl', fzf.live_grep, "[S]earch [L]ive Grep")
map('sh', fzf.command_history, "[S]earch Command [H]istory")
map('sc', fzf.commands, "[S]earch [C]ommands")

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>cr', vim.lsp.buf.rename, '[C]ode [R]ename')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

local lspconfig = require('lspconfig');
local servers = {
  ansiblels = {},
  lua_ls = {},
  pyright = {},
  -- rubocop = {},
  rust_analyzer = {},
  solargraph = {},
  tsserver = {},
}

for server, config in pairs(servers) do
  config.on_attach = on_attach
  lspconfig[server].setup(config)
end

-- document existing key chains
require('which-key').register {
  ['<leader>c'] = { name = '[C]ode', _ = 'which_key_ignore' },
  ['<leader>d'] = { name = '[D]ocument', _ = 'which_key_ignore' },
  ['<leader>g'] = { name = '[G]it', _ = 'which_key_ignore' },
  ['<leader>s'] = { name = '[S]earch', _ = 'which_key_ignore' },
  ['<leader>$'] = { name = '[$]hell', _ = 'which_key_ignore' },
  ['<leader>t'] = { name = '[T]oggle', _ = 'which_key_ignore' },
  ['<leader>w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },
}
-- register which-key VISUAL mode
-- required for visual <leader>hs (hunk stage) to work
require('which-key').register({
  ['<leader>'] = { name = 'VISUAL <leader>' },
}, { mode = 'v' })

-- If custom overrides exist, load them.
pcall(require, 'custom')
