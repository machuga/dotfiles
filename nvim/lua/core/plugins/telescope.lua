local builtin = require('telescope.builtin')

-- vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
-- vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
-- vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

require("which-key").register({
  f = {
    name = "File",                                -- optional group name
    f = { builtin.find_files, "Find File" },      -- create a binding with label
    p = { builtin.git_files, "Find Git File" },   -- create a binding with label
    r = { builtin.oldfiles, "Open Recent File" }, -- additional options for creating the keymap
    g = { builtin.live_grep, "Live Grep" },
    b = { builtin.buffers, "Buffers" },
    h = { builtin.help_tags, "Help Tags" },
  },
  p = {
    name = "Project",                        -- optional group name
    f = { builtin.find_files, "Find File" }, -- create a binding with label
  },
  ["/"] = { builtin.live_grep, "Live Grep" },
}, { prefix = "<leader>" })

-- Muscle memory from Doom + Spacemacs with Projectile
--vim.keymap.set('n', '<leader>pf', builtin.find_files, { desc = "Find Files" })
--vim.keymap.set('n', '<leader>/', builtin.live_grep, { desc = "Live Grep" })

-- Ctrl-p days
vim.keymap.set('n', '<c-p>', builtin.find_files, {})
