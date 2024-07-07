require("core.lazy")

-- TODO: Extract these to files for consistency
require("lazy").setup({
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 200
    end,
    opts = {},
  },
  "tinted-theming/base16-vim",
  {
    "junegunn/fzf",
    dir = "~/.fzf",
    build = "./install --all",
  },
  "junegunn/fzf.vim",
  {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
    "mhartington/formatter.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",
  },
  {
    "stevearc/conform.nvim",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local conform = require("conform")

      --conform.setup({
      --  formatters_by_ft = {
      --    javascript = { "prettier" },
      --    typescript = { "prettier" },
      --    javascriptreact = { "prettier" },
      --    typescriptreact = { "prettier" },
      --    css = { "prettier" },
      --    html = { "prettier" },
      --    json = { "prettier" },
      --    yaml = { "prettier" },
      --    markdown = { "prettier" },
      --    graphql = { "prettier" },
      --    lua = { "stylua" },
      --    ruby = { "rubocop" },
      --    eruby = { "rubocop" },
      --  },
      --  format_on_save = {
      --    lsp_fallback = true,
      --    async = false,
      --    timeout_ms = 500,
      --  },
      --})

      vim.keymap.set({ "n", "v" }, "<leader>fm", function()
        conform.format({
          lsp_fallback = true,
          async = false,
          timeout_ms = 500,
        })
      end, { desc = "Format file or range (in visual mode)" })
    end,
  },
  "nvim-tree/nvim-web-devicons",
  { "nvim-focus/focus.nvim",           version = false },
  { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
  {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "nvim-telescope/telescope-fzf-native.nvim",
    build =
    "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
  },
  {
    "machakann/vim-sandwich",
    config = function()
      vim.cmd([[runtime macros/sandwich/keymap/surround.vim]])
    end,
  },
  {
    "Wansmer/treesj",
    keys = { "<space>m", "<space>j", "<space>s" },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("treesj").setup({
        max_join_length = 120,
      })
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },
  -- nvim v0.8.0
  {
    "kdheepak/lazygit.nvim",
    cmd = {
      "LazyGit",
      "LazyGitConfig",
      "LazyGitCurrentFile",
      "LazyGitFilter",
      "LazyGitFilterCurrentFile",
    },
    -- optional for floating window border decoration
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    lazy = false,
    config = function()
      require("telescope").load_extension("lazygit")
    end,
    -- setting the keybinding for LazyGit with 'keys' is recommended in
    -- order to load the plugin when the command is run for the first time
    --keys = {
    --  { "<leader>lg", "<cmd>LazyGit<cr>", desc = "LazyGit" }
    --}
  }
})

require("focus").setup()

require("core.plugins.treesitter")
require("core.plugins.telescope")
require("core.plugins.lualine")
require("core.plugins.mason")
