vim.lsp.config.solargraph = {
  cmd = { 'ruby-lsp' },
  filetypes = { 'ruby' },
  root_markers = { 'Gemfile', '.git' }
}
vim.lsp.enable('solargraph')
