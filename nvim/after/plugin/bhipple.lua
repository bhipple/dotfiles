--------------------------------------------------------------------------------
-- Load my standard vimscript files
vim.cmd('source ~/.vim/startup/functions.vim')
vim.cmd('source ~/.vim/startup/settings.vim')
vim.cmd('source ~/.vim/startup/mappings.vim')

local mark = require('harpoon.mark')
local neogit = require('neogit')
local telescope = require('telescope.builtin')
local ui = require('harpoon.ui')

--------------------------------------------------------------------------------
-- Functions
local function cdgit_find_files()
    vim.cmd(':call Cdgit()')
    telescope.find_files()
end

--------------------------------------------------------------------------------
-- [[ Basic Keymaps ]]
-- Keymaps for better default experience
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>/', function()
    telescope.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown { winblend = 10, previewer = false })
end, { desc = '[/] Fuzzily search in current buffer' })
vim.keymap.set('n', '<leader><space>', telescope.help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<C-p>',      telescope.git_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>bb', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>fb', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>fr', require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>gg', telescope.live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>pf', telescope.git_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>pp', require('telescope').extensions.projects.projects, {})
vim.keymap.set('n', '<leader>sd', telescope.diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sf', telescope.git_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sg', telescope.live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sh', telescope.help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', telescope.grep_string, { desc = '[S]earch current [W]ord' })

-- Git
vim.keymap.set('n', '<leader>gs', neogit.open, { desc = 'Neogit Status' })
vim.keymap.set('n', '<leader>oc', ":DiffviewClose<CR>", { desc = 'Diffview Close' })
vim.keymap.set('n', '<leader>od', ":DiffviewOpen<CR>", { desc = 'Diffview Open' })
vim.keymap.set('n', '<leader>oh', ":DiffviewOpen HEAD<CR>", { desc = 'Diffview Open -- HEAD' })
vim.keymap.set('n', '<leader>om', ":DiffviewOpen origin/master<CR>", { desc = 'Diffview Open -- origin/master' })
vim.keymap.set('n', '<leader>oo', ":DiffviewOpen", { desc = 'Diffview Open against revision (enter prompt)' })

-- Harpoon
require("telescope").load_extension('harpoon')
vim.keymap.set('n', '<leader>m', mark.add_file)
vim.keymap.set('n', '<C-e>', ':Telescope harpoon marks<CR>')
vim.keymap.set("n", "<F1>", function() ui.nav_file(1) end)  -- editor prefix + l via QMK
vim.keymap.set("n", "<F2>", function() ui.nav_file(2) end)  -- editor prefix + u via QMK
vim.keymap.set("n", "<F3>", function() ui.nav_file(3) end)  -- editor prefix + y via QMK
vim.keymap.set("n", "<F4>", function() ui.nav_file(4) end)  -- editor prefix + ' via QMK

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = "Open diagnostics list" })

-- Terminal keymaps
vim.keymap.set("n", "<leader>tt", ":!", { desc = '[Terminal] command run synchronously' })
vim.keymap.set("n", "<leader>to", ":terminal\n", { desc = '[T]erminal [O]pen' })

-- Trouble keymaps
vim.keymap.set("n", "<leader>xx", "<cmd>TroubleToggle<cr>", {silent = true, noremap = true})
vim.keymap.set("n", "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", {silent = true, noremap = true})
vim.keymap.set("n", "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", {silent = true, noremap = true})
vim.keymap.set("n", "<leader>xl", "<cmd>TroubleToggle loclist<cr>", {silent = true, noremap = true})
vim.keymap.set("n", "<leader>xq", "<cmd>TroubleToggle quickfix<cr>", {silent = true, noremap = true})
vim.keymap.set("n", "gR", "<cmd>TroubleToggle lsp_references<cr>", {silent = true, noremap = true})
