if exists('g:GtkGuiLoaded')
  call rpcnotify(1, 'Gui', 'Font', 'Cascadia Code PL 17')
  "call rpcnotify(1, 'Gui', 'Option', 'Popupmenu', 0)
  "call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0)
  "call rpcnotify(1, 'Gui', 'Command', 'SetCursorBlink', '10')
else
  set guifont=agave\ Nerd\ Font\ Mono:h19
  call rpcnotify(1, 'Gui', 'Option', 'Popupmenu', 0)
  call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0)
endif
