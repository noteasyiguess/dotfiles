local opt = {noremap = true}
local opts = {noremap = true, silent = true}

local noremap = vim.api.nvim_set_keymap

noremap('n', '<Leader>l', ':!lua %<CR>', opt)
noremap('n', '<Leader>p', ':!python3 %<CR>', opt)
noremap('n', '<Leader>u', ':update<CR>', opt)
noremap('n', '<Leader><space>', ':nohlsearch<CR>', opts)
