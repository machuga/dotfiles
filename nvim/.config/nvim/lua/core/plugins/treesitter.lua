return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  config = function()
    require('nvim-treesitter.configs').setup {
      ensure_installed = {
        'c',
        'lua',
        'ruby',
        'css',
        'html',
        'gitcommit',
        'dockerfile',
        'diff',
        'csv',
        'json',
        'markdown',
        'nix',
        'php',
        'python',
        'sql',
        'typescript',
        'toml',
        'tmux',
        'tsx',
        'javascript',
        'vim',
        'yaml',
      },
      sync_install = false,
      auto_install = true,
      highlight = {
        enable = true
      },
      indent = {
        enable = true
      }
    }
  end
}
