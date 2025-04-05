vim.lsp.config.solargraph = {
  cmd = { 'solargraph', 'stdio' },
  filetypes = { 'ruby' },
  root_markers = { 'Gemfile', '.git' }
}
vim.lsp.enable('solargraph')
