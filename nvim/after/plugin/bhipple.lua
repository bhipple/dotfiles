--------------------------------------------------------------------------------
-- Load my standard vimscript files
-- vim.cmd('source ~/.vim/startup/settings.vim')

local mark = require('harpoon.mark')
local neogit = require('neogit')
local telescope = require('telescope.builtin')
local ui = require('harpoon.ui')

--------------------------------------------------------------------------------
-- [[ Basic Keymaps ]]
-- Keymaps for better default experience
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- In visual mode, <leader>p puts without overwriting the kill ring with the deleted content
vim.keymap.set("x", "<leader>p", [["_dP]])

-- If hl search is off, starting a new search or moving enables it
vim.keymap.set('n', '*', ':set hlsearch<CR>*')
vim.keymap.set('n', '#', ':set hlsearch<CR>#')
vim.keymap.set('n', 'n', ':set hlsearch<CR>n')
vim.keymap.set('n', 'N', ':set hlsearch<CR>N')
vim.keymap.set('n', '/', ':set hlsearch<CR>/')
vim.keymap.set('n', '?', ':set hlsearch<CR>?')

-- Misc keybindings
vim.keymap.set('n', '<Leader>ev', ':e $HOME/dotfiles/nvim/after/plugin/bhipple.lua<CR>')
vim.keymap.set('n', '<Leader>cx', ':!chmod a+x %<CR>')
vim.keymap.set('n', '<Leader>h',  ':set hlsearch! hlsearch?<CR>')
vim.keymap.set('n', '<Leader>rd', ':windo redraw!<CR>')
vim.keymap.set('n', '<Leader>se', ':sp<CR>:e %:h<CR>')
vim.keymap.set('n', '<Leader>ss', ':call SortSection()<CR>')
vim.keymap.set('n', '<Leader>te', ':tabe %:h<CR>')
vim.keymap.set('n', '<Leader>tm', ':Tabmerge right<CR>')
vim.keymap.set('n', '<Leader>ve', ':vsp<CR>:e %:h<CR>')
vim.keymap.set('n', '<Leader>w',  ':Trim<CR>:w<CR>')

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

-- Git operations
vim.keymap.set('n', '<leader>gs', neogit.open, { desc = 'Neogit Status' })
vim.keymap.set('n', '<leader>grf', ":!git rf master", { desc = 'git rf master' })
vim.keymap.set('n', '<leader>gfa', ":!git fetch --all", { desc = 'git fetch --all' })
vim.keymap.set('n', '<leader>gb',  ":GitBlameToggle<CR>", { })
vim.keymap.set('n', '<leader>gc',  ":GitBlameCopySHA<CR>", { desc = "Copies the SHA hash of current line's commit into the system's clipboard" })

-- Diffing
vim.keymap.set('n', '<leader>oc', ":DiffviewClose<CR>", { desc = 'Diffview Close' })
vim.keymap.set('n', '<leader>od', ":DiffviewOpen<CR>", { desc = 'Diffview Open' })
vim.keymap.set('n', '<leader>oh', ":DiffviewOpen HEAD<CR>", { desc = 'Diffview Open -- HEAD' })
vim.keymap.set('n', '<leader>om', ":DiffviewOpen origin/master<CR>", { desc = 'Diffview Open -- origin/master' })
vim.keymap.set('n', '<leader>oo', ":DiffviewOpen ", { desc = 'Diffview Open against revision (enter prompt)' })

vim.keymap.set('n', '<leader>do', ":windo diffoff<CR>")
vim.keymap.set('n', '<leader>db', ":windo diffthis<CR>")
vim.keymap.set('n', '<leader>du', ":diffupdate<CR>")
vim.keymap.set('n', '<leader>dp', ":diffput<CR>")
vim.keymap.set('n', '<leader>dg', ":diffget<CR>")

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

--------------------------------------------------------------------------------
-- [[ Settings options ]]
-- See `:help vim.o`

-- Fixing tabs
vim.o.tabstop = 4
vim.o.expandtab = true
vim.o.shiftwidth = 4

-- Autoreload when file modified on disk
vim.o.autoread = true

-- Set highlight on search
vim.o.hlsearch = true

-- Make line numbers default
vim.o.number = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- No swap files or backups
vim.o.swapfile = false
vim.o.backup = false

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeout = true
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

vim.o.termguicolors = true

-- Show tab and trailing whitespace characters
vim.o.list = true
vim.o.listchars = 'tab:>-,trail:-'

-- What to use for gq
vim.o.formatprg = 'par -w80'

-- When multiple completions are possible, show all
vim.o.wildmenu = true

-- Complete only up to point of ambiguity, like the shell does
vim.o.wildmode = 'list:longest'

-- Ignoring files (see :help wildignore)
vim.o.wildignore = '*.o,*.d,00*,nohup.out,tags,.hs-tags,*.hi,*.gcno,*.gcda,*.fasl,*.pyc,*__pycache__*'

-- Ignore whitespace on diffs
vim.o.diffopt = 'internal,filler,closeoff,iwhite'

-- Number of lines to scroll past when the cursor scrolls off the screen
vim.o.scrolloff = 0
