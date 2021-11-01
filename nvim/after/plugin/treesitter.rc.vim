if !exists('g:loaded_nvim_treesitter')
  finish
end

lua << EOF

require('nvim-treesitter.configs').setup {
  highlight = {
    enable = true,
    disable = {},
  },
  indent = {
    enable = false,
    disable = {},
  },
  ensure_installed = {
    "tsx",
    "typescript",
    "bash",
    "vim",
    "json",
    "yaml",
    "html",
    "css",
    "scss",
    "lua",
    }
  }	

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.tsx.used_by = { "javascript", "typescript.tsx" }
EOF
