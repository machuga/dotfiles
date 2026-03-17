return {
  --cmd = { "bundle", "exec", "rubocop", "--lsp" },
  --root_dir = require("lspconfig.util").root_pattern("Gemfile", ".git"), -- ensures Neovim CWD is set correctly
  filetypes = { "ruby" },
  cmd = { "bundle", "exec", "ruby-lsp" },
  root_markers = { "Gemfile", ".git" },
  init_options = {
    formatter = 'rubocop',
    linters = { 'rubocop' },
    addonSettings = {
      ["Ruby LSP Rails"] = {
        enablePendingMigrationsPrompt = true,
      },
    },
  },
}
