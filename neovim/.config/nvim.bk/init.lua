-- Extended from nvim-lua/kickstart.nvim

-- Set <comma> as the leader key
vim.g.mapleader = ','
vim.g.maplocalleader = ','

-- Mapping function for easier normal mode leader mappings.
local function nmap(binding, mapping, desc)
  vim.keymap.set('n', binding, mapping, {
    noremap = true, silent = true, desc = desc
  })
end

local function nlmap(binding, mapping, desc)
  nmap('<leader>' .. binding, mapping, desc)
end

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
        { 'williamboman/mason.nvim', config = true }, -- NOTE: Must be loaded before dependants
        'williamboman/mason-lspconfig.nvim',
        'WhoIsSethDaniel/mason-tool-installer.nvim',
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

        'hrsh7th/cmp-nvim-lsp',
      },
    },

    { -- Autocompletion
      'hrsh7th/nvim-cmp',
      event = 'InsertEnter',
      dependencies = {
        -- Adds other completion capabilities.
        --  nvim-cmp does not ship with all sources by default. They are split
        --  into multiple repos for maintenance purposes.
        'hrsh7th/cmp-nvim-lsp',
        'hrsh7th/cmp-path',
      },
      config = function()
        -- See `:help cmp`
        local cmp = require 'cmp'

        cmp.setup {
          completion = { completeopt = 'menu,menuone,noinsert' },

          -- For an understanding of why these mappings were
          -- chosen, you will need to read `:help ins-completion`
          --
          -- No, but seriously. Please read `:help ins-completion`, it is really good!
          mapping = cmp.mapping.preset.insert {
            -- Select the [n]ext item
            ['<C-n>'] = cmp.mapping.select_next_item(),
            -- Select the [p]revious item
            ['<C-p>'] = cmp.mapping.select_prev_item(),

            -- Scroll the documentation window [b]ack / [f]orward
            ['<C-b>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),

            -- Accept ([y]es) the completion.
            --  This will auto-import if your LSP supports it.
            --  This will expand snippets if the LSP sent a snippet.
            ['<C-y>'] = cmp.mapping.confirm { select = true },

            -- If you prefer more traditional completion keymaps,
            -- you can uncomment the following lines
            --['<CR>'] = cmp.mapping.confirm { select = true },
            --['<Tab>'] = cmp.mapping.select_next_item(),
            --['<S-Tab>'] = cmp.mapping.select_prev_item(),

            -- Manually trigger a completion from nvim-cmp.
            --  Generally you don't need this, because nvim-cmp will display
            --  completions whenever it has completion options available.
            ['<C-Space>'] = cmp.mapping.complete {},
          },
          sources = {
            {
              name = 'lazydev',
              -- set group index to 0 to skip loading LuaLS completions as lazydev recommends it
              group_index = 0,
            },
            { name = 'nvim_lsp' },
            { name = 'path' },
          },
        }
      end,
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
            indent = { enable = true, disable = { "ledger", "ruby", "typescript" } },
            ignore_install = {},
            modules = {},
            auto_install = true,
          })
        end, 0)

        vim.opt.foldmethod = "expr"
        vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
        vim.opt.foldenable = false
        vim.opt.foldlevel = 2
      end
    }}},

    {
      'folke/which-key.nvim',
      opts = {
        preset = "modern",
      },
      dependencies = { 'echasnovski/mini.icons' },
    },

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
      ft = { 'yaml.ansible' },
    },

    {
      "pmizio/typescript-tools.nvim",
      dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
      opts = {
        settings = {
          tsserver_max_memory = 20480
        }
      },
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
        vim.keymap.set('i', '<Esc>', [[<Esc>:LedgerAlign<CR>]], { noremap = true, silent = true, })
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
vim.o.completeopt = 'menuone,preview'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true

-- Show colorcolumn
vim.o.textwidth = 80
vim.o.colorcolumn = "+1"

-- [[ Globals ]]
vim.g.netrw_banner = 0 -- Hide banner
vim.g.netrw_altv = 1
vim.g.netrw_liststyle = 3 -- Tree-style view
vim.g.netrw_list_hide = ([[,\(^\|\s\s\)\zs\.\S\+]])


-- [[ FZF Keymaps ]]
local fzf = require('fzf-lua')

local find_files = function()
  local git_dir = vim.fn.finddir('.git', vim.fn.getcwd() .. ";")
  if git_dir == '' then
    fzf.files()
  else
    fzf.git_files()
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
      fzf.files({ cwd=project_dir })
      return
    end
  end

  fzf.files({ cwd=parent_dir })
end

nlmap('f', find_files, "Search [F]iles")
nlmap('e', find_in_package, "Search Files in packag[e].")
nlmap('b', fzf.buffers, "Search [B]uffers")
nlmap('h', fzf.oldfiles, "Search [H]istory")
nlmap('sl', fzf.live_grep, "[S]earch [L]ive Grep")
nlmap('sh', fzf.command_history, "[S]earch Command [H]istory")
nlmap('sc', fzf.commands, "[S]earch [C]ommands")

--- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, { desc = 'Op en diagnostics list' })

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
  nmap('<leader>cr', vim.lsp.buf.rename, 'LSP: [C]ode [R]ename')
  nmap('<leader>ca', vim.lsp.buf.code_action, 'LSP: [C]ode [A]ction')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'LSP: Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'LSP: Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, 'LSP: [G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, 'LSP: [W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, 'LSP: [W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, 'LSP: [W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

local servers = {
  ansiblels = {},
  lua_ls = {},
  rust_analyzer = {},
}

local lspconfig = require('lspconfig')
local capabilities = vim.lsp.protocol.make_client_capabilities()
for server, config in pairs(servers) do
  config.on_attach = on_attach
  config.on_attach = on_attach
  config.capabilities = vim.tbl_deep_extend('force', {}, capabilities, config.capabilities or {})
  lspconfig[server].setup(config)
end

-- document existing key chains
local wk = require('which-key')
wk.add {
    { "<leader>s", group = "[S]earch" },
    { "<leader>s_", hidden = true },
  }

-- [[ Rust ]]
vim.g.rust_recommended_style = 0

-- [[ Ansible ]]
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = { "*/plays/*.yaml", "*/plays/*.yml" },
  command = "set filetype=yaml.ansible",
})


-- If custom overrides exist, load them.
pcall(require, 'custom')
