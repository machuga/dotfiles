local builtin = require('telescope.builtin')

vim.keymap.set('n', '<c-p>', builtin.find_files, {})
vim.keymap.set('n', 'ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>pf', builtin.find_files, {})
vim.keymap.set('n', 'fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>/', builtin.live_grep, {})
vim.keymap.set('n', 'fh', builtin.help_tags, {})
