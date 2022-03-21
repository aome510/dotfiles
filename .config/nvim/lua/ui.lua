require("telescope").setup({
	defaults = {
		layout_strategy = "vertical",
		layout_config = {
			vertical = {
				height = 0.9,
				preview_cutoff = 16,
				preview_height = 0.6,
				mirror = false,
			},
		},
		file_previewer = require("telescope.previewers").vim_buffer_cat.new,
		grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
		qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
		mappings = {
			i = {
				["<C-h>"] = "which_key",
			},
		},
	},
})
require("telescope").load_extension("fzf")
