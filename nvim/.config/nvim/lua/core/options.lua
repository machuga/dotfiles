--" Set up spacing and alignment
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.linespace = 2
vim.opt.expandtab = true

vim.opt.wrap = false
vim.opt.number = true
vim.opt.ruler = true
vim.opt.hidden = true

-- Keep more context when scrolling off the end of a buffer
vim.opt.scrolloff = 3

-- Show chars
--
-- vim.opt.listchars = { trail = '·', tab = ' ' }

vim.cmd([[set list listchars=tab:\ \ ,trail:·]])

-- Indentation
vim.opt.autoindent = true
vim.opt.smartindent = true

-- Set splitting
vim.opt.splitbelow = true

-- Searching
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Show (partial) command in the status line
vim.opt.showcmd = true

-- Use modeline overrides
vim.opt.modeline = true
vim.opt.modelines = 10

-- Insert only one space when joining lines with terminating punctuation
vim.opt.joinspaces = false

-- Automatically reload files changed outside of vim
vim.opt.autoread = true

-- Set cursorline
vim.cmd([[highlight Search cterm=underline]])

-- Status bar
vim.opt.laststatus = 2

-- Tab completion
vim.opt.wildmode = "list:longest,list:full"
vim.opt.wildignore:append { "*.o", "*.obj", ".git", "*.rbc", "*.class", ".svn", "vendor/gems/*", "*/node_modules/*" }

-- Load the plugin and indent settings for the detected filetype
-- Switch to lua eventually, but:
vim.cmd([[filetype plugin indent on]])


-- Directories for swp files
vim.opt.backupdir = os.getenv("HOME") .. "/.local/share/nvim/backups"
vim.opt.directory = os.getenv("HOME") .. "/.local/share/nvim/backups"

vim.opt.backupskip = "/tmp/*,/private/tmp/*"

-- Code folding
vim.opt.foldenable = true

vim.opt.termguicolors = true

-- incremental substitution (neovim)
vim.opt.inccommand = "split"

-- Suppress appending <PasteStart> and <PasteEnd> when pasting
vim.cmd([[set t_BE=]])


-- Turn off paste mode when leaving insert
vim.cmd([[autocmd InsertLeave * set nopaste]])

vim.g.markdown_fenced_languages = {
  "ts=typescript"
}

vim.api.nvim_create_autocmd('ColorScheme', {
  callback = function()
    local highlights = {
      'Normal',
      'LineNr',
      'Folded',
      'NonText',
      'SpecialKey',
      'VertSplit',
      'SignColumn',
      'EndOfBuffer',
      'TablineFill', -- this is specific to how I like my tabline to look like
    }
    --for _, name in pairs(highlights) do vim.cmd.highlight(name .. ' guibg=none ctermbg=none') end
  end,
})

--vim.cmd([[let tinted_colorspace=256]])

vim.cmd.colorscheme('base16-tomorrow-night')
