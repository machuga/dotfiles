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

    require("which-key").add({
      { "<leader>/",  builtin.live_grep,  desc = "Live Grep" },
      { "<leader>f",  group = "File" },
      { "<leader>ff", builtin.find_files, desc = "Find File" },
      { "<leader>fp", builtin.git_files,  desc = "Find Git File" },
      { "<leader>fr", builtin.oldfiles,   desc = "Open Recent File" },
      { "<leader>fg", builtin.live_grep,  desc = "Live Grep" },
      { "<leader>fb", builtin.buffers,    desc = "Buffers" },
      { "<leader>fh", builtin.help_tags,  desc = "Help Tags" },
      { "<leader>p",  group = "Project" },
      { "<leader>pf", builtin.find_files, desc = "Find File" },
    })

    -- Ctrl-p days
    vim.keymap.set('n', '<c-p>', builtin.find_files, {})
  end
}
