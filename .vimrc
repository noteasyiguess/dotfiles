call plug#begin('~/.vim/plugged')
Plug 'rust-lang/rust.vim'
Plug 'tikhomirov/vim-glsl'
Plug 'jiangmiao/auto-pairs'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'ryanoasis/vim-devicons'
Plug 'puremourning/vimspector'
Plug 'mswift42/vim-themes'
Plug 'rafi/awesome-vim-colorschemes'
call plug#end()

" Airline configs
let g:airline#extensions#tabline#enabled = 0
let g:airline_powerline_fonts = 1

let g:vimspector_enable_mappings = "HUMAN"

" Nerd commenter
" Create default mappings
let g:NERDCreateDefaultMappings = 1

" Truecolor support
if (has("nvim"))
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

if exists('+termguicolors')
   set termguicolors
   let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
   let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif 

set nocompatible

" enable syntax and plugins
syntax enable
filetype plugin indent on

set mouse=a
set incsearch
set sidescroll=1

" Tab behaviour
set tabstop=4  " show existing tabs with 4 spaces
set shiftwidth=4  " when indenting use 4 spaces
set expandtab  " expand tabs to spaces

" search every subdirectory when using any of the find commands
set path+=**

set wildmenu  " better autocomplete menu
set spr  " put the new split'ed window to the right

" change cursor shape acc. to mode
" let &t_SI = "\<esc>[5 q"  " blinking I-beam in insert mode
" let &t_SR = "\<esc>[3 q"  " blinking underline in replace mode
" let &t_EI = "\<esc>[ q"  " default cursor (usually blinking block) otherwise

" netrw tweaks
let g:netrw_banner=0  " disable the banner
let g:netrw_browse_split=4  " open in prior window
let g:netrw_liststyle=3  " tree style listing
let g:netrw_list_hide=netrw_gitignore#Hide().'.*\.swp$'

set autoindent
set cindent
set smartindent
set nowrap

set showcmd
set noshowmode
set noruler
set laststatus=0
set relativenumber
set signcolumn=number

" Coc related
" set cmdheight=2
" set shortmess+=cF
set pumheight=8
set hidden

" Custom keybindings
let mapleader=" "

nnoremap <silent> <Leader><space> :nohlsearch<CR>
nnoremap <Leader>b :ls<CR>:b
nnoremap <Leader>u :update<CR>
nnoremap <silent> <Leader>xx :FZF<CR>
nnoremap <silent> <Leader>xb :Buffers<CR>

noremap <silent> <C-n> :NERDTreeToggle<CR>
noremap <silent> <C-m> :NERDTreeFocus<CR>
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

" Compile and run shortcut
nnoremap <Leader>ee :!crun %<CR>
nnoremap <Leader>eo :!make -f logl.make TARGET_SRC=% run<CR>
nnoremap <Leader>co :!make -f logl.make TARGET_SRC=% clean<CR>
" nmap <Leader>ew :!crun_wayland %<CR>

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
" inoremap <silent><expr> <TAB>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ coc#refresh()
" inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

source ~/.vimrc_host
