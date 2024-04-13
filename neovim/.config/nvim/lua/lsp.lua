local function map(binding, mapping, desc)
  vim.keymap.set('n', '<leader>' .. binding, mapping, {
    noremap = true, silent = true, desc = desc
  })
end

-- Diagnostic keymaps
map('[d', vim.diagnostic.goto_prev, 'Go to previous diagnostic message')
map(']d', vim.diagnostic.goto_next, 'Go to next diagnostic message')
map('<leader>q', vim.diagnostic.setloclist, 'Open diagnostics list')

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
