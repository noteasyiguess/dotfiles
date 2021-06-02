local o = vim.o
local bo = vim.bo
local wo = vim.wo
local g = vim.g

o.splitright = true
o.splitbelow = true
o.hlsearch = true
o.background = 'dark'
o.termguicolors = true
o.scrolloff = 5
o.sidescrolloff = 5

o.tabstop, bo.tabstop = 4, 4
o.shiftwidth, bo.shiftwidth = 4, 4
o.expandtab, bo.expandtab = true, true

wo.wrap = false

g.mouse = 'a'
g.mapleader = ' '
vim.b.mapleader = ' '

vim.cmd('syntax on')
vim.cmd('filetype plugin indent on')

vim.cmd('colorscheme desert')
