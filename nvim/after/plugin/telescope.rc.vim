if !exists('g:loaded_telescope')
  finish
endif

nnoremap <silent> <leader>pf <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <silent> <leader>/ <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <silent> <leader>pt <cmd>lua require('telescope.builtin').file_browser()<cr>
nnoremap <silent> <leader>tb <cmd>Telescope buffers<cr>
nnoremap <silent> <leader>tt <cmd>Telescope help_tags<cr>

lua << EOF
function telescope_buffer_dir()
  return vim.fn.expand('%:p:h')
end

local telescope = require('telescope')
local actions = require('telescope.actions')

telescope.setup{
  defaults = {
    mappings = {
      n = {
        ["q"] = actions.close
      },
    },
  }
}
EOF
