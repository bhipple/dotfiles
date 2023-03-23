--------------------------------------------------------------------------------
-- [[ Basic Keymaps ]]
-- Keymaps for better default experience
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>fr',      require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>/', function()
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    winblend = 10,
    previewer = false,
  })
end, { desc = '[/] Fuzzily search in current buffer' })

local telescope = require('telescope.builtin')
vim.keymap.set('n', '<C-p>',      telescope.git_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>pf', telescope.git_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sf', telescope.find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', telescope.help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', telescope.grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', telescope.live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd', telescope.diagnostics, { desc = '[S]earch [D]iagnostics' })

-- Git
vim.keymap.set('n', '<leader>gs', require('neogit').open, { desc = 'Neogit Status' })
vim.keymap.set('n', '<leader>oc', ":DiffviewClose<CR>", { desc = 'Diffview Close' })
vim.keymap.set('n', '<leader>od', ":DiffviewOpen<CR>", { desc = 'Diffview Open' })
vim.keymap.set('n', '<leader>oh', ":DiffviewOpen HEAD<CR>", { desc = 'Diffview Open -- HEAD' })
vim.keymap.set('n', '<leader>oo', ":DiffviewOpen", { desc = 'Diffview Open against revision (enter prompt)' })
vim.keymap.set('n', '<leader>om', ":DiffviewOpen origin/master<CR>", { desc = 'Diffview Open -- origin/master' })

-- Harpoon
local mark = require('harpoon.mark')
local ui = require('harpoon.ui')
require("telescope").load_extension('harpoon')
vim.keymap.set('n', '<leader>m', mark.add_file)
vim.keymap.set('n', '<C-e>', ui.toggle_quick_menu)
vim.keymap.set("n", "<F1>", function() ui.nav_file(1) end)
vim.keymap.set("n", "<F2>", function() ui.nav_file(2) end)
vim.keymap.set("n", "<F3>", function() ui.nav_file(3) end)
vim.keymap.set("n", "<F4>", function() ui.nav_file(4) end)

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = "Open diagnostics list" })
