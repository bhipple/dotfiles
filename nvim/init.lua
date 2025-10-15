-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Set to true if you have a Nerd Font installed and selected in the terminal
vim.g.have_nerd_font = true

--------------------------------------------------------------------------------
-- Install package manager
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({ 'git', 'clone', '--filter=blob:none', 'https://github.com/folke/lazy.nvim.git', '--branch=stable', lazypath })
end
vim.opt.rtp:prepend(lazypath)

--------------------------------------------------------------------------------
-- Plugins
require('lazy').setup({
  -- NOTE: First, some plugins that don't require any configuration
  { 'ThePrimeagen/harpoon', lazy = true },              -- Workspace management
  { 'f-person/git-blame.nvim', lazy = true },           -- :GitBlameToggle
  { 'folke/trouble.nvim', lazy = true },                -- Quickfix/Loclist/LSP Info
  { 'folke/which-key.nvim', opts = {} },                -- Useful plugin to show you pending keybinds.
  { 'hrsh7th/cmp-buffer', lazy = true },                -- autocompletions from buffer
  { 'ledger/vim-ledger', lazy = true },                 -- ledger-cli mode
  { 'sindrets/diffview.nvim', lazy = true },
  { 'stevearc/dressing.nvim', lazy = true },            -- Nicer popups
  { 'tpope/vim-surround', lazy = true },
  { 'tpope/vim-vinegar' },                              -- Netrw replacement
  { 'wsdjeg/vim-fetch', lazy = true },                  -- Understand formats line file:line when opening
  { 'numToStr/Comment.nvim', lazy = true, opts = {} },   -- "gc" to comment visual regions/lines

  {'NeogitOrg/neogit',
    lazy = true,
    dependencies = {
      'nvim-lua/plenary.nvim',         -- required
      'nvim-telescope/telescope.nvim', -- optional
      'sindrets/diffview.nvim',        -- optional
      'ibhagwan/fzf-lua',              -- optional
    },
    config = true,
    diffview = true,
    icons = false,
  },

  {'DrKJeff16/project.nvim',
    lazy = false,
    version = false, -- Get the latest release
    dependencies = { -- OPTIONAL
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'ibhagwan/fzf-lua',
    },
    config = function()
      require('project_nvim').setup()
    end,
  },

  { -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    lazy = true,
    dependencies = {
      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim', tag = "legacy", opts = {} },
    },
  },

  { -- Autocompletion
    'hrsh7th/nvim-cmp',
    lazy = true,
    dependencies = { 'hrsh7th/cmp-buffer', 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip' },
  },

  { -- Adds git releated signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    opts = {
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
    },
  },

  { -- Theme inspired by Atom
    'navarasu/onedark.nvim',
    priority = 1000,
    config = function()
      vim.cmd.colorscheme 'onedark'
    end,
  },

  { -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = false,
        theme = 'onedark',
        component_separators = '|',
        section_separators = '',
      },
      sections = {
        lualine_c = {
          {
            'filename',
            file_status = true,      -- Displays file status (readonly status, modified status)
            newfile_status = false,  -- Display new file status (new file means no write after created)
            path = 3,                -- 0: Just the filename
            -- 1: Relative path
            -- 2: Absolute path
            -- 3: Absolute path, with tilde as the home directory
            -- 4: Filename and parent dir, with tilde as the home directory
            --
            shorting_target = 40,    -- Shortens path to leave 40 spaces in the window
            -- for other components. (terrible name, any suggestions?)
            symbols = {
              modified = '[+]',      -- Text to show when the file is modified.
              readonly = '[-]',      -- Text to show when the file is non-modifiable or readonly.
              unnamed = '[No Name]', -- Text to show for unnamed buffers.
              newfile = '[New]',     -- Text to show for newly created file before first write
            }
          }
        }
      },
    },
  },

  -- Fuzzy Finder (files, lsp, etc)
  { 'nvim-telescope/telescope.nvim', lazy = true, version = '*', dependencies = { 'nvim-lua/plenary.nvim', } },

  -- Fuzzy Finder Algorithm which requires local dependencies to be built.
  -- Only load if `make` is available. Make sure you have the system
  -- requirements installed.
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    -- NOTE: If you are having trouble with this installation,
    --       refer to the README for telescope-fzf-native for more instructions.
    laze = true,
    build = 'make',
    cond = function()
      return vim.fn.executable 'make' == 1
    end,
  },

  --{ -- Highlight, edit, and navigate code
  --  'nvim-treesitter/nvim-treesitter',
  --  dependencies = {
  --    'nvim-treesitter/nvim-treesitter-textobjects',
  --  },
  --  config = function()
  --    pcall(require('nvim-treesitter.install').update { with_sync = true })
  --  end,
  --},

  { -- :Trim command for whitespace cleanup
    'cappyzawa/trim.nvim',
    lazy = true,
    opts = {
      ft_blocklist = {},
      patterns = {},
      trim_on_write = false,
      trim_trailing = true,
      trim_last_line = true,
      trim_first_line = true,
    },
  },
}, {
  ui = {
    -- If you are using a Nerd Font: set icons to an empty table which will use the
    -- default lazy.nvim defined Nerd Font icons, otherwise define a unicode icons table
    icons = vim.g.have_nerd_font and {} or {
      cmd = '⌘',
      config = '🛠',
      event = '📅',
      ft = '📂',
      init = '⚙',
      keys = '🗝',
      plugin = '🔌',
      runtime = '💻',
      require = '🌙',
      source = '📄',
      start = '🚀',
      task = '📌',
      lazy = '💤 ',
    },
  },
})


-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

--------------------------------------------------------------------------------
-- Trivial Plugins
require('gitblame').setup { enabled = false }
--require('project').setup()
require('trouble').setup { }

--------------------------------------------------------------------------------
-- Telescope and Project
-- See `:help telescope` and `:help telescope.setup()`
local trouble = require("trouble.sources.telescope")
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
        ["<c-t>"] = trouble.open,
      },
      n = {
        ["<c-t>"] = trouble.open,
      },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

require('telescope').load_extension('projects')

--------------------------------------------------------------------------------
-- Treesitter
-- require('nvim-treesitter.configs').setup {
--   -- Defer to Nix instead of trying to install ourselves
--   auto_install = false,
--   sync_install = false,
--   ensure_installed = {},
--   ingore_install = { "all" },
--   modules = {},
-- 
--   highlight = {
--     enable = true,
--     disable = { "lua", "c", "query", },
--     additional_vim_regex_highlighting = false,
--   },
--   indent = { enable = true, disable = { } },
--   incremental_selection = {
--     enable = true,
--     keymaps = {
--       init_selection = '<c-space>',
--       node_incremental = '<c-space>',
--       scope_incremental = '<c-s>',
--       node_decremental = '<M-space>',
--     },
--   },
--   textobjects = {
--     select = {
--       enable = true,
--       lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
--       keymaps = {
--         -- You can use the capture groups defined in textobjects.scm
--         ['aa'] = '@parameter.outer',
--         ['ia'] = '@parameter.inner',
--         ['af'] = '@function.outer',
--         ['if'] = '@function.inner',
--         ['ac'] = '@class.outer',
--         ['ic'] = '@class.inner',
--       },
--     },
--     move = {
--       enable = true,
--       set_jumps = true, -- whether to set jumps in the jumplist
--       goto_next_start = {
--         [']m'] = '@function.outer',
--         [']]'] = '@class.outer',
--       },
--       goto_next_end = {
--         [']M'] = '@function.outer',
--         [']['] = '@class.outer',
--       },
--       goto_previous_start = {
--         ['[m'] = '@function.outer',
--         ['[['] = '@class.outer',
--       },
--       goto_previous_end = {
--         ['[M'] = '@function.outer',
--         ['[]'] = '@class.outer',
--       },
--     },
--     swap = {
--       enable = true,
--       swap_next = {
--         ['<leader>a'] = '@parameter.inner',
--       },
--       swap_previous = {
--         ['<leader>A'] = '@parameter.inner',
--       },
--     },
--   },
-- }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- nvim-cmp setup
local cmp = require 'cmp'
local luasnip = require 'luasnip'

luasnip.config.setup {}

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'buffer' },
    { name = 'luasnip' },
    { name = 'nvim_lsp' },
  },
}

-- `/` cmdline setup.
cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- After entering a python buffer, sleep to allow LSP to startup, then change its bundle
vim.api.nvim_create_autocmd("FileType", {
    pattern = "python",
    callback = function()
      vim.defer_fn(function()
        vim.cmd('PyrightSetPythonPath /n/nix/sa/links/sa-bundle/default/bin/python')
      end, 1000)  -- waits X ms before executing
    end
})

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
