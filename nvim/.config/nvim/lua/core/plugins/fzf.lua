return
    {
      "junegunn/fzf",
      dir = "~/.fzf",
      build = "./install --all",
    },
    {
      "junegunn/fzf.vim",
      dependencies = { 'junegunn/fzf' },
    }
