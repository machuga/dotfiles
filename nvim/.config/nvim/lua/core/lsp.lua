vim.lsp.config("*", {
  capabilities = vim.lsp.protocol.make_client_capabilities(),
})

--require("mason").setup()
--require("mason-lspconfig").setup({
--  ensure_installed = {
--    "lua_ls",
--    "html",
--    "cssls",
--    "jsonls",
--    "ts_ls",
--    "solargraph",
--    "ruby_lsp",
--    "rubocop",
--    "taplo",
--    "yamlls",
--    "vimls",
--    "bashls",
--    "graphql",
--    "denols"
--  },
--})

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


vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("UserLspConfig", {}),
  callback = function(ev)
    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions

    local keymap = function(modes, keys, func, desc)
      if desc then
        desc = 'LSP: ' .. desc
      end

      vim.keymap.set(modes, keys, func, { buffer = ev.buf, desc = desc, silent = true })
    end

    local nmap = function(keys, func, desc)
      keymap('n', keys, func, desc)
    end

    nmap('[d', vim.diagnostic.goto_prev, 'Go to previous diagnostic')
    nmap(']d', vim.diagnostic.goto_next, 'Go to next diagnostic')

    nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
    nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
    nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
    nmap('gi', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
    nmap('<leader>ltd', vim.lsp.buf.type_definition, 'Type [D]efinition')
    nmap('<leader>lgt', "<cmd>Telescope lsp_type_definitions<CR>", 'Type [D]efinitions')
    nmap('<leader>lD', "<cmd>Telescope diagnostics bufnr=0<CR>", 'Show buffer LSP [D]iagnostics')

    nmap('<leader>lds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
    nmap('<leader>lrn', vim.lsp.buf.rename, '[R]e[n]ame')
    nmap("<leader>ldf", vim.diagnostic.open_float, "Line [d]iagnostics via Float")
    keymap({ "n", "v" }, '<leader>lca', vim.lsp.buf.code_action, '[C]ode [A]ction')
    nmap("<leader>lrs", ":LspRestart<CR>", "[R]estart LSP")

    -- See `:help K` for why this keymap
    nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
    nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(ev.buf, 'Format', function(_)
      if vim.lsp.buf.format then
        vim.lsp.buf.format()
      elseif vim.lsp.buf.formatting then
        vim.lsp.buf.formatting()
      end
    end, { desc = 'Format current buffer with LSP' })
  end,
})

-- vim.lsp.inlay_hint.enable(true)

local severity = vim.diagnostic.severity

vim.diagnostic.config({
  signs = {
    text = {
      [severity.ERROR] = " ",
      [severity.WARN] = " ",
      [severity.HINT] = "󰠠 ",
      [severity.INFO] = " ",
    },
  },
})

vim.lsp.enable("sourcekit")
