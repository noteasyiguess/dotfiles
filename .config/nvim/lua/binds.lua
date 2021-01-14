local opt = {noremap = true}
local opts = {noremap = true, silent = true}

local noremap = vim.api.nvim_set_keymap

noremap('n', '<Leader>u', ':update<CR>', opts)
noremap('n', '<Leader><space>', ':nohlsearch<CR>', opts)

noremap('n', '<Leader>l', ':!lua %<CR>', opt)
noremap('n', '<Leader>p', ':!python3 %<CR>', opt)

noremap('n', '<C-m>', ':NvimTreeOpen<CR>', opts)
noremap('n', '<C-n>', ':NvimTreeToggle<CR>', opts)

vim.api.nvim_exec('autocmd TermOpen term://* startinsert', false)
