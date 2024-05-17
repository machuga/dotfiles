require('core.lazy');

require('lazy').setup({
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 200
    end,
    opts = {}
  },
  "tinted-theming/base16-vim",
  {
    "junegunn/fzf",
    dir = "~/.fzf",
    build = "./install --all"
  },
  "junegunn/fzf.vim",
  {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
  },
  "nvim-tree/nvim-web-devicons",
  { 'nvim-focus/focus.nvim',           version = false },
  { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
  {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    'nvim-telescope/telescope-fzf-native.nvim',
    build =
    'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build'
  },
  {
    "machakann/vim-sandwich",
    config = function()
      vim.cmd([[runtime macros/sandwich/keymap/surround.vim]])
    end
  },
  {
    'Wansmer/treesj',
    keys = { '<space>m', '<space>j', '<space>s' },
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('treesj').setup({
        max_join_length = 120,
      })
    end,
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' }
  }
})

require("focus").setup()

require('core.plugins.treesitter')
require('core.plugins.telescope')
require('core.plugins.lualine')
