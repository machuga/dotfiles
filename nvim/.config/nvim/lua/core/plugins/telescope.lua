return {
  'nvim-telescope/telescope-fzf-native.nvim',
  dependencies = {
    "folke/which-key.nvim",
    "nvim-telescope/telescope.nvim"
  },
  build =
  'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build',
  config = function()
    local builtin = require('telescope.builtin')

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
      -- Muscle memory from Doom + Spacemacs with Projectile
      p = {
        name = "Project",                        -- optional group name
        f = { builtin.find_files, "Find File" }, -- create a binding with label
      },
      ["/"] = { builtin.live_grep, "Live Grep" },
    }, { prefix = "<leader>" })

    -- Ctrl-p days
    vim.keymap.set('n', '<c-p>', builtin.find_files, {})
  end
}
