lua << EOF

vim.api.nvim_create_autocmd({ "BufEnter" }, {
  -- ALE doesn't use the correct `selene` config as the `selene` execution
  -- happens in a temporary directory.
  pattern = { "*.lua" },
  callback = function() 
    local parent_dir = vim.fn.expand("%:p:h")
    local selene_config = vim.fs.find("selene.toml", {
      path = parent_dir,
      upward = true
    })
    if selene_config[1] then
      vim.b.ale_lua_selene_options = '--config ' .. selene_config[1]
      vim.cmd("ALEReset")
    end
  end
})
EOF
