" Stoic Vim
"""""""""""
" A minimalist Neovim configuration, primarily in Vimscript.
" For simple configuraiton, Vimscript is simply much more egonomic than Lua.


" Colors
"""""""""
set notermguicolors
autocmd ColorScheme default highlight LineNr ctermfg=6
autocmd ColorScheme default highlight ColorColumn ctermfg=8
colorscheme default

" Options
""""""""""
set number relativenumber
set clipboard=unnamedplus

set textwidth=80 colorcolumn=+1
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab

" File path. Use LineNr highlight group.
set statusline=%#LineNr#\ %f%m\ %=
" LSP
set statusline+=%{luaeval('vim.lsp.status()')}
" File type, percentage in file, lines/total lines:column
set statusline+=\ %Y\ \ %p%%\ \ %l/%L:%c\ 

" File Types
""""""""""""
filetype indent plugin on

" Configure file type specific config in:
" neovim/.config/nvim/after/ftplugin/<ft>.[vim|lua]

" Bindings
""""""""""
let mapleader = ","
let maplocalleader = ","

nnoremap <Leader>$ :source $MYVIMRC<CR>"

" Some nice default bindings:
" :b                => Buffers search
" Ctrl ^ or Ctrl 6  => Previous File (like :b#)

" Files and Commands
""""""""""""""""""""
" Use `Quickfix` for looking at file history and searching through files /
" file content
autocmd FileType qf nnoremap <buffer> <silent> q :cclose<CR>

func QFiles(info)
  " get information about a range of quickfix entries
  let items = getqflist({'id' : a:info.id, 'items' : 1}).items
  let l = []
  for idx in range(a:info.start_idx - 1, a:info.end_idx - 1)
    " use the simplified file name
    call add(l, fnamemodify(bufname(items[idx].bufnr), ':p:.'))
  endfor
  return l
endfunc

" Fd Find
function! FdFilesInQuickfix(args) abort
  let l:files = systemlist("fd -t f -H" . ' ' . a:args)
  call setqflist([], ' ', {'lines' : l:files, 'efm' : '%f', 'quickfixtextfunc' :'QFiles', 'title': 'Fd Find'})
  copen
endfunction
command! -nargs=* -complete=file Fd call FdFilesInQuickfix(<q-args>)
nnoremap <silent> <leader>f :Fd<Space>

" Oldfiles
function! OldfilesInQuickfix() abort
  call setqflist([], ' ', {'lines' : v:oldfiles, 'efm' : '%f', 'quickfixtextfunc' : 'QFiles', 'title': 'Oldfiles'})
  copen
endfunction
command! -bar Oldfiles call OldfilesInQuickfix()
nnoremap <silent> <leader>h :Oldfiles<CR>

" Ripgrep
set grepprg=rg\ --vimgrep\ --hidden
command! -nargs=+ Rg execute 'silent grep! <args>' | copen
nnoremap <silent> <leader>s :Rg<Space>

" Trailing Whitespaces
command! Trw execute '%s/\s\+$//e'

" Lua Configuration
"""""""""""""""""""

lua << END
-- LSP
vim.lsp.config('*', {
  root_markers = { '.git' },
})
END

augroup LspStatuslineUpdate
    autocmd!
    autocmd User LspProgressUpdate redrawstatus!
    autocmd User LspAttach redrawstatus!
    autocmd User LspDetach redrawstatus!
augroup END
