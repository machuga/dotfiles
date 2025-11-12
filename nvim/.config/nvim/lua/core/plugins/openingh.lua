return {
  "almo7aya/openingh.nvim",
  config = function()
    require("which-key").add({
      { "<leader>gh",  group = "Open in GitHub" },
      { "<leader>ghr", ":OpenInGHRepo<CR>",      desc = "Open Repo on GitHub" },
      { "<leader>ghf", ":OpenInGHFile<CR>",      desc = "Open File on GitHub" },
      { "<leader>ghl", ":OpenInGHFileLines<CR>", desc = "Open Lines on GitHub" },
    })
  end
}
