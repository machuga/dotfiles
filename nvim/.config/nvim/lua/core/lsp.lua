require("mason").setup()
require("mason-lspconfig").setup({
  ensure_installed = {
    "lua_ls",
    "html",
    "cssls",
    "jsonls",
    "tsserver",
    "solargraph",
    "ruby_lsp",
    "rubocop",
    "taplo",
    "yamlls",
    "vimls",
    "bashls",
    "graphql"
  },
})

require('lspconfig').lua_ls.setup {
  Lua = {
    diagnostics = {
      -- Get the language server to recognize the `vim` global
      globals = { "vim" },
    },
    workspace = {
      -- Make the server aware of Neovim runtime files
      library = vim.api.nvim_get_runtime_file("", true),
    },
    -- Do not send telemetry data containing a randomized but unique identifier
    telemetry = {
      enable = false,
    },
  },
}

require('lspconfig').ruby_lsp.setup {
  cmd = { "./bin/bundle", "exec", "ruby-lsp" }
}

--require('lspconfig').rubocop.setup {
--  cmd = { "./bin/bundle", "exec", "rubocop", "--require", "rubocop-rails" }
--}

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
