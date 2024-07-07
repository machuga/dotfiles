require("mason").setup()

require("mason-tool-installer").setup({
  ensure_installed = {
    "prettier", -- prettier formatter
    "stylua", -- lua formatter
    "eslint_d", -- js linter
    "rubocop",
  },
})
