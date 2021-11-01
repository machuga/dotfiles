
" Set up spacing and alignment
set nowrap
set tabstop=2
set shiftwidth=2
set softtabstop=2
set linespace=2
set expandtab

" Imports "{{{
" ---------------------------------------------------------------------
runtime ./plug.vim
if has("unix")
  let s:uname = system("uname -s")
  " Do Mac stuff
  if s:uname == "Darwin\n"
    runtime ./macos.vim
  endif
endif
"}}}
