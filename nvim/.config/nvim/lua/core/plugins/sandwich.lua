return {
  "machakann/vim-sandwich",
  config = function()
    vim.cmd([[runtime macros/sandwich/keymap/surround.vim]])
  end
}
