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

-- Tab movements
vim.keymap.set("n", "<leader>tn", ":tabnext<cr>", { desc = "Next tab" })
vim.keymap.set("n", "<leader>tp", ":tabprevious<cr>", { desc = "Previous tab" })
vim.keymap.set("n", "<leader>tN", ":tabnew<cr>", { desc = "New tab" })

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

-- Copy to system paste buffer
vim.keymap.set("v", "<leader>y", '"*y', { desc = "Copy to system paste buffer" })

vim.keymap.set("n", "<leader>o", ':set nopaste<cr>', { desc = "Remove search highlighting" })

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
-- Example which-key registration
--require("which-key").register({
--  w = {
--    name = "Window Movements", -- optional group name
--    f = { "<cmd>Telescope find_files<cr>", "Find File" }, -- create a binding with label
--    r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File", noremap=false, buffer = 123 }, -- additional options for creating the keymap
--    n = { "New File" }, -- just a label. don't create any mapping
--    e = "Edit File", -- same as above
--    ["1"] = "which_key_ignore",  -- special label to hide it in the popup
--    b = { function() print("bar") end, "Foobar" } -- you can also pass functions!
--  },
--}, { prefix = "<leader>" })
