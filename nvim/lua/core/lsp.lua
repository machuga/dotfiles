require("mason").setup()
require("mason-lspconfig").setup({
  ensure_installed = {
    "lua_ls",
    "html",
    "cssls",
    "jsonls",
    "tsserver",
    --"solargraph",
    "ruby_lsp",
    "taplo",
    "yamlls",
    "vimls",
    "bashls",
    "graphql",
    "rubocop",
  },
})

require("lspconfig").lua_ls.setup({})
require("lspconfig").ruby_lsp.setup({
  cmd = { "ruby-lsp", "stdio" },
  root_dir = require("lspconfig.util").root_pattern("Gemfile", ".git", "."),
  settings = {
    solargraph = {
      autoformat = true,
      completion = true,
      diagnostic = true,
      folding = true,
      references = true,
      rename = true,
      symbols = true,
    },
  },
})
--require("lspconfig").solargraph.setup({
--  cmd = { "bin/solargraph", "stdio" },
--  root_dir = require("lspconfig.util").root_pattern("Gemfile", ".git", "."),
--  settings = {
--    solargraph = {
--      autoformat = true,
--      completion = true,
--      diagnostic = true,
--      folding = true,
--      references = true,
--      rename = true,
--      symbols = true,
--    },
--  },
--})
require("lspconfig").tsserver.setup({})
require("lspconfig").html.setup({})
require("lspconfig").cssls.setup({})
require("lspconfig").jsonls.setup({})
require("lspconfig").yamlls.setup({})

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp", { clear = true }),
  callback = function(args)
    -- 2
    vim.api.nvim_create_autocmd("BufWritePre", {
      -- 3
      buffer = args.buf,
      callback = function()
        -- 4 + 5
        vim.lsp.buf.format({ async = false, id = args.data.client_id })
      end,
    })
  end,
})

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    --local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { buffer = ev.buf, desc = "Go to declarations" })
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = ev.buf, desc = "Go to definition" })
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, { buffer = ev.buf, desc = "Go to implementation" })
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, { buffer = ev.buf, desc = "List references" })
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = ev.buf, desc = "Information" })
    vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder,
      { buffer = ev.buf, desc = "Add workspace folder" })
    vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder,
      { buffer = ev.buf, desc = "Remove workspace folder" })
    vim.keymap.set('n', '<leader>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, { buffer = ev.buf, desc = "List workspace folders" })
    vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, { buffer = ev.buf, desc = "Type definition" })
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, { buffer = ev.buf, desc = "Rename symbol" })
    vim.keymap.set({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, { buffer = ev.buf, desc = "Code Actions" })
    vim.keymap.set('n', '<leader>bf', function()
      vim.lsp.buf.format { async = true }
    end, { buffer = ev.buf, desc = "Format buffer" })
  end,
})
