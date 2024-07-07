require("mason").setup()
require("mason-lspconfig").setup({
  ensure_installed = {
    "lua_ls",
    "html",
    "cssls",
    "jsonls",
    "tsserver",
    "solargraph",
    "taplo",
    "yamlls",
    "vimls",
    "bashls",
    "graphql"
  },
})

require('lspconfig').lua_ls.setup {}

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp", { clear = true }),
  callback = function(args)
    -- 2
    vim.api.nvim_create_autocmd("BufWritePre", {
      -- 3
      buffer = args.buf,
      callback = function()
        -- 4 + 5
        vim.lsp.buf.format { async = false, id = args.data.client_id }
      end,
    })
  end
})
