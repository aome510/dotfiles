local lspconfig = require("lspconfig")

-- nvim-cmp supports additional completion capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

local on_attach = function(client)
	if client.resolved_capabilities.document_formatting then
		vim.cmd([[
      augroup LspFormatting
      autocmd! * <buffer>
      autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
      augroup END
    ]])
	end
end

-- Configure following servers with lspconfig
local servers = {}
for _, lsp in ipairs(servers) do
	lspconfig[lsp].setup({
		on_attach = on_attach,
		capabilities = capabilities,
	})
end

local lsp_installer = require("nvim-lsp-installer")

local lua_server_opts = {
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you"re using (most likely LuaJIT in the case of Neovim)
				version = "LuaJIT",
				-- Setup your lua path
				path = vim.split(package.path, ";"),
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
				},
			},
		},
	},
}

-- Register a handler that will be called for each installed server when it"s ready (i.e. when installation is finished
-- or if the server is already installed).
lsp_installer.on_server_ready(function(server)
	local server_opts = {
		on_attach = on_attach,
		capabilities = capabilities,
		flags = {
			-- This will be the default in neovim 0.7+
			debounce_text_changes = 150,
		},
	}

	if server.name == "sumneko_lua" then
		server_opts = lua_server_opts
	end

	server:setup(server_opts)
end)

-- null-ls setup for additional lsp functions
require("null-ls").setup({
	on_attach = on_attach,
	sources = {
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.black,
		require("null-ls").builtins.formatting.isort,
		require("null-ls").builtins.diagnostics.flake8,
		require("null-ls").builtins.diagnostics.pylint,
	},
})
