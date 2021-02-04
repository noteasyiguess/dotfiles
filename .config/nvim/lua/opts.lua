local o = vim.o
local bo = vim.bo
local wo = vim.wo
local g = vim.g

vim.api.nvim_command('filetype plugin on')
vim.api.nvim_command('syntax on')

o.splitright = true
o.splitbelow = true
o.smartcase = true
o.hlsearch = true
o.termguicolors = true
o.scrolloff = 3
o.sidescrolloff = 3
o.completeopt = 'menuone,noinsert,noselect'
o.shortmess = o.shortmess .. 'c'
o.showmode = false
o.mouse = 'a'

wo.relativenumber = true
wo.signcolumn = 'number'
wo.wrap = false
wo.cursorline = true

o.tabstop, bo.tabstop = 2, 2
o.softtabstop, bo.softtabstop = 2, 2
o.shiftwidth, bo.shiftwidth = 2, 2
o.autoindent, bo.autoindent = true, true
o.expandtab, bo.expandtab = true, true

g.mapleader = ' '

-- Termdebug
g.termdebug_wide = 1 --Vertical split
g.termdebug_useFloatingHover = 0 --Show evaluations in the ex window

-- Colorscheme
--g.colors_name = 'sonokai'
g.colors_name = 'gruvbox-material'
o.background = 'dark' --Set the background too, to apply the colors

-- Completion
g.completion_confirm_key = '<C-j>'
g.completion_matching_smart_case = true
g.completion_enable_auto_hover = false
g.completion_enable_auto_signature = false
--[[
g.completion_matching_strategy_list = { 'fuzzy', 'substring', 'exact', 'all' }
]]
