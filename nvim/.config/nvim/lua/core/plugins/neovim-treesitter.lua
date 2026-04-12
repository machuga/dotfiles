return {
  "neovim-treesitter/nvim-treesitter",
  dependencies = { "nvim-lua/plenary.nvim" },
  lazy = false,
  build = ":TSUpdate",
  config = function()
    local ts = require("nvim-treesitter")

    local ensure = {
      "c", "lua", "ruby", "css", "html", "gitcommit", "dockerfile",
      "diff", "csv", "json", "markdown", "nix", "php", "python",
      "sql", "typescript", "toml", "tmux", "tsx", "javascript",
      "vim", "yaml",
    }

    ts.install(ensure)

    vim.api.nvim_create_autocmd("FileType", {
      pattern = ensure,
      callback = function()
        pcall(vim.treesitter.start)
      end,
    })
  end,
}
