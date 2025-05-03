vim.lsp.config.deno= {
  cmd = { 'deno', 'lsp' },
  filetypes = { 'ts', 'tsx' },
  root_markers = { 'deno.json', 'deno.jsonc', '.git' }
}
vim.lsp.enable('deno')

