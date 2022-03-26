-- local lspconfig = require("lspconfig")
local lsp_status = require("lsp-status")
local lsp_installer = require("nvim-lsp-installer")

lsp_status.register_progress()

-- nvim-cmp supports additional completion capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

-- extend the capabilities with `lsp-status` capabilities
capabilities = vim.tbl_extend("keep", capabilities or {}, lsp_status.capabilities)

local on_attach = function(client)
	-- setup formatting on save
	if client.resolved_capabilities.document_formatting then
		vim.cmd([[
      augroup LspFormatting
      autocmd! * <buffer>
      autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
      augroup END
    ]])
	end
	lsp_status.on_attach(client)
end

-- Configure following servers with lspconfig
-- local servers = {}
-- for _, lsp in ipairs(servers) do
-- 	lspconfig[lsp].setup({
-- 		on_attach = on_attach,
-- 		capabilities = capabilities,
-- 	})
-- end

local server_settings = {
	lua = {
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
	rust = {
		["rust-analyzer"] = {
			checkOnSave = {
				command = "clippy",
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
		server_opts.settings = server_settings.lua
		server:setup(server_opts)
	elseif server.name == "rust_analyzer" then
		server_opts.settings = server_settings.rust
		-- Initialize the LSP via rust-tools instead
		require("rust-tools").setup({
			-- The "server" property provided in rust-tools setup function are the
			-- settings rust-tools will provide to lspconfig during init.
			-- We merge the necessary settings from nvim-lsp-installer (server:get_default_options())
			-- with the user's own settings (opts).
			server = vim.tbl_deep_extend("force", server:get_default_options(), server_opts),
		})
		server:attach_buffers()
		-- Only if standalone support is needed
		require("rust-tools").start_standalone_if_required()
	else
		server:setup(server_opts)
	end
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
