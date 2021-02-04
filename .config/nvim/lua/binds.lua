local opt = {noremap = true}
local opts = {noremap = true, silent = true}

local map = vim.api.nvim_set_keymap

map('n', '<Leader>n', ':tabnext<CR>', opts)
map('n', '<Leader>b', ':tabprev<CR>', opts)

map('n', '<Leader>u', ':update<CR>', opts)
map('n', '<Leader><space>', ':nohlsearch<CR>', opts)

map('n', '<Leader>l', ':!lua %<CR>', opt)
map('n', '<Leader>p', ':!python3 %<CR>', opt)

map('n', '<C-m>', ':NvimTreeFindFile<CR>', opts)
map('n', '<C-n>', ':NvimTreeToggle<CR>', opts)

-- Termdebug
map('n', '<F5>', ':Continue<CR>', opts)
map('n', '<F6>', ':Stop<CR>', opts)

map('n', '<F7>', ':Break<CR>', opt)
map('n', '<F8>', ':Clear<CR>', opt)

map('n', '<F10>', ':Over<CR>', opts)
map('n', '<F11>', ':Step<CR>', opts)
map('n', '<F12>', ':Finish<CR>', opts)

-- Telescope
map('n', '<Leader>f', ':Telescope find_files<CR>', opts)

vim.api.nvim_command('autocmd TermOpen term://* startinsert')
