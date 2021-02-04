lua require 'init'

nnoremap <space>l :!lua %<CR>
nnoremap <silent> <space>u :update<CR>

if exists('g:goneovim')
  call rpcnotify(1, 'Gui', 'Font', 'agave Nerd Font Mono:h18')
endif

if exists('g:gnvim')
  set guifont=agave\ Nerd\ Font\ Mono:h17
endif
