local keymap_opts = { noremap = true, silent = true }
local keymap = vim.api.nvim_set_keymap

-- Set leader key to be <Space>
keymap("", "<Space>", "<Nop>", keymap_opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Cancel search highlighting with ESC
keymap("n", "<ESC>", ":nohlsearch<Bar>:echo<CR>", keymap_opts)

-- Redo to U
keymap("n", "U", "<cmd>redo<CR>", keymap_opts)

local wk = require("which-key")

wk.register({
	g = {
		l = { "$", "Beginning of the line" },
		h = { "^", "End of the line" },
		i = { "0", "First character of the line" },
		b = { "<cmd>lua require('Comment.api').call('toggle_blockwise_op')<CR>g@", "Toggle blockwise comment" },
		c = { "<cmd>lua require('Comment.api').call('toggle_linewise_op')<CR>g@", "Toggle linewise comment" },
		D = { "<cmd>lua require('telescope.builtin').lsp_references()<CR>", "Go to references" },
		d = { "<cmd>lua require('telescope.builtin').lsp_definition()<CR>", "Go to definition" },
		z = {
			name = "multi-cursors",
		},
	},
	K = { "<cmd>lua vim.lsp.buf.hover()<CR>", "Lsp hover" },
	["<C-k>"] = { "<cmd>lua vim.lsp.buf.signature_help()<CR>", "Lsp signature help" },
	["<leader>"] = {
		c = {
			name = "code",
			D = { "<cmd>lua require('telescope.builtin').lsp_declaration()<CR>", "Declaration" },
			d = { "<cmd>lua require('telescope.builtin').lsp_definition()<CR>", "Definition" },
			r = { "<cmd>lua require('telescope.builtin').lsp_references()<CR>", "References" },
			i = { "<cmd>lua require('telescope.builtin').lsp_implementation()<CR>", "Implementation" },
			t = { "<cmd>lua require('telescope.builtin').lsp_type_definition()<CR>", "Type definition" },
			a = { "<cmd>lua require('telescope.builtin').lsp_code_actions<CR>", "Code action" },
			s = { [[<cmd>lua require("telescope.builtin").lsp_document_symbols()<CR>]], "LSP document symbol" },
			S = { [[<cmd>lua require("telescope.builtin").lsp_workspace_symbols()<CR>]], "LSP workspace symbol" },
			R = { "<cmd>lua vim.lsp.buf.rename()<CR>", "Rename" },
			f = { "<cmd>lua vim.lsp.buf.formatting()<CR>", "Format buffer" },
			l = { [[<cmd>lua vim.cmd('e'..vim.lsp.get_log_path())<CR>]], "LSP log buffer" },
			e = { "<cmd>lua vim.diagnostic.setloclist()<CR>", "Diagnostic popup" },
		},
		s = {
			name = "search",
			s = { [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]], "Current buffer" },
			g = { [[<cmd>lua require('telescope.builtin').grep_string()<CR>]], "Grep" },
			p = { [[<cmd>lua require('telescope.builtin').live_grep()<CR>]], "Live grep" },
			h = { [[<cmd>lua require('telescope.builtin').command_history()<CR>]], "Command History" },
		},
		h = {
			name = "help",
			k = { [[<cmd>lua require('telescope.builtin').keymaps()<CR>]], "Keymaps" },
			v = { [[<cmd>lua require('telescope.builtin').vim_options()<CR>]], "Options" },
			m = { [[<cmd>lua require('telescope.builtin').man_pages()<CR>]], "Manpages" },
		},
		b = {
			name = "buffer",
			b = { [[<cmd>lua require('telescope.builtin').buffers()<CR>]], "Buffers" },
		},
		f = {
			name = "file",
			f = { [[<cmd>lua require('telescope.builtin').find_files()<CR>]], "Files" },
		},
		t = {
			name = "theme",
			t = { [[<cmd>lua require('telescope.builtin').colorscheme()<CR>]], "Themes" },
			h = { [[<cmd>lua require('telescope.builtin').highlights()<CR>]], "Highlights" },
		},
		T = { "<cmd>NvimTreeOpen<CR>", "NVim Tree" },
	},
	["<M-x>"] = { [[<cmd>lua require('telescope.builtin').commands()<CR>]], "Commands" },
	["]"] = {
		e = { "<cmd>lua vim.diagnostic.goto_next()<CR>", "Next diagnostic" },
	},
	["["] = {
		e = { "<cmd>lua vim.diagnostic.goto_prev()<CR>", "Previous diagnostic" },
	},
}, {})
