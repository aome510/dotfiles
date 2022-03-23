-- Install packer
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
	vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

require("packer").startup(function(use)
	use("wbthomason/packer.nvim") -- Package manager

	-- NVIM Tree
	use({
		"kyazdani42/nvim-tree.lua",
		requires = {
			"kyazdani42/nvim-web-devicons", -- optional, for file icon
		},
		config = function()
			require("nvim-tree").setup({})
		end,
	})

	-- Lualine
	use({
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
		config = function()
			require("lualine").setup()
		end,
	})

	-- Code Comment
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	})

	-- Telescope
	use({
		"nvim-telescope/telescope.nvim",
		requires = { { "nvim-lua/plenary.nvim" }, { "nvim-telescope/telescope-fzf-native.nvim", run = "make" } },
	})

	-- NVIM qbf
	use({ "kevinhwang91/nvim-bqf", ft = "qf" })

	-- Treesitter
	use("nvim-treesitter/nvim-treesitter")

	-- Additional textobjects for treesitter
	use("nvim-treesitter/nvim-treesitter-textobjects")

	-- LSP
	use("neovim/nvim-lspconfig") -- Collection of configurations for built-in LSP client

	-- LSP installer
	use("williamboman/nvim-lsp-installer")

	-- LSP signature help
	use({
		"ray-x/lsp_signature.nvim",
		config = function()
			require("lsp_signature").setup({})
		end,
	})

	-- Null-LS
	use("jose-elias-alvarez/null-ls.nvim")

	-- Autocompletion using nvim-cmp
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-nvim-lsp")
	use("saadparwaiz1/cmp_luasnip")

	-- Snippet
	use("L3MON4D3/LuaSnip")

	-- Which Key
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup()
		end,
	})

	-- Multiple cursors
	use({
		"mg979/vim-visual-multi",
		config = function()
			vim.g.VM_Mono_hl = "Cursor"
			vim.g.VM_leader = "gz"
			vim.g.VM_maps = {
				["Find Next"] = "<C-n>",
				["Find Prev"] = "<C-p>",
			}
		end,
	})

	-- Replacer to edit qflist buffer
	use("gabrielpoca/replacer.nvim")

	-- Github theme
	use({
		"projekt0n/github-nvim-theme",
		config = function()
			require("github-theme").setup({
				theme_style = "light_default",
			})
		end,
	})
end)
