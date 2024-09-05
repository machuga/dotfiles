vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Copy to system paste buffer
vim.keymap.set("n", "<leader>y", '"*y', { desc = "Copy to system paste buffer" })

-- Window movements
vim.keymap.set("n", "<leader>wh", "<C-w>h", { desc = "Focus window to the left" })
vim.keymap.set("n", "<leader>wj", "<C-w>j", { desc = "Focus window below" })
vim.keymap.set("n", "<leader>wk", "<C-w>k", { desc = "Focus window above" })
vim.keymap.set("n", "<leader>wl", "<C-w>l", { desc = "Focus window to the right" })
vim.keymap.set("n", "<leader>wH", "<C-w>H", { desc = "Move window to the left" })
vim.keymap.set("n", "<leader>wJ", "<C-w>J", { desc = "Move window up" })
vim.keymap.set("n", "<leader>wK", "<C-w>K", { desc = "Move window down" })
vim.keymap.set("n", "<leader>wL", "<C-w>L", { desc = "Move window to the right" })

-- Buffer movements
vim.keymap.set("n", "<leader>bn", ":bn<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>bp", ":bp<cr>", { desc = "Previous buffer" })
vim.keymap.set("n", "<leader>bd", ":bd<cr>", { desc = "Delete buffer" })

-- Config management
vim.keymap.set("n", "<leader>cf", ":vs ~/.config/nvim/init.lua<cr>", { desc = "Split edit config" })
vim.keymap.set("n", "<leader>cr", ":source ~/.config/nvim/init.lua<cr>", { desc = "Re-source config" })

-- Editing convenience
vim.keymap.set("n", "<leader>e", ":edit <C-R>=expand('%:p:h') . '/' <CR>", { desc = "Edit sibling file" })
vim.keymap.set("n", "<leader>v", ":view <C-R>=expand('%:p:h') . '/' <CR>", { desc = "View sibling file" })
vim.keymap.set("n", "<leader>sp", ":sp <C-R>=expand('%:p:h') . '/' <CR>",
  { desc = "Edit sibling file in horizontal split" })
vim.keymap.set("n", "<leader>vs", ":vs <C-R>=expand('%:p:h') . '/' <CR>",
  { desc = "Edit sibling file in vertical split" })

vim.keymap.set("n", "<leader>o", ':set nopaste<cr>', { desc = "Remove search highlighting" })
vim.keymap.set("i", "kj", '<esc>', { desc = "Quick exit" })

vim.api.nvim_exec([[
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rename Current File
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>rN :call RenameFile()<cr>
]], false)
