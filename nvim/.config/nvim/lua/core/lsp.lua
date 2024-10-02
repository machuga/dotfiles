-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
--vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
--vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)



local capabilities = vim.lsp.protocol.make_client_capabilities()
-- Unused for now
--local servers = {
--  ruby_lsp = {
--    cmd = { "bundle", "exec", "ruby-lsp" }
--  },
--  lua_ls = {
--    Lua = {
--      diagnostics = {
--        -- Get the language server to recognize the `vim` global
--        globals = { "vim" },
--      },
--      workspace = {
--        -- Make the server aware of Neovim runtime files
--        library = vim.api.nvim_get_runtime_file("", true),
--      },
--      -- Do not send telemetry data containing a randomized but unique identifier
--      telemetry = {
--        enable = false,
--      }
--    }
--  }
--}

-- Consider adding the snippet from: https://shopify.github.io/ruby-lsp/EDITORS_md.html
--
require("mason").setup()
require("mason-lspconfig").setup({
  ensure_installed = {
    "lua_ls",
    "html",
    "cssls",
    "jsonls",
    "ts_ls",
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

--require("mason-lspconfig").setup_handlers {
--  function(server_name)
--    require("lspconfig")[server_name].setup {
--      capabilities = capabilities,
--      --on_attach = on_attach,
--      settings = servers[server_name],
--      filetypes = (servers[server_name] or {}).filetypes,
--    }
--  end
--}
--
---- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>ltd', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>lds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>lrn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>lca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('<leader>lwa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>lwr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>lws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
  nmap('<leader>lwl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    if vim.lsp.buf.format then
      vim.lsp.buf.format()
    elseif vim.lsp.buf.formatting then
      vim.lsp.buf.formatting()
    end
  end, { desc = 'Format current buffer with LSP' })
end

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
    capabilities = capabilities,
    on_attach = on_attach
  },
}

require('lspconfig').ruby_lsp.setup {
  cmd = { "./bin/bundle", "exec", "ruby-lsp" },
  capabilities = capabilities,
  on_attach = on_attach
}

require('lspconfig').solargraph.setup {
  cmd = { "./bin/bundle", "exec", "solargraph" },
  capabilities = capabilities,
  on_attach = on_attach
}
--require('lspconfig').rubocop.setup {
--  cmd = { "./bin/bundle", "exec", "rubocop", "--require", "rubocop-rails" }
--}

require('lspconfig').rubocop.setup {
  cmd = { "./bin/bundle", "exec", "rubocop", "--lsp" },
  capabilities = capabilities,
  on_attach = on_attach
}

require('lspconfig').ts_ls.setup {
  capabilities = capabilities,
  on_attach = on_attach
}

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
