" The greatest leader (key) of all time
let mapleader = "\<Space>"

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

runtime ./mappings.vim

"}}}

set number
set ruler
set hidden
syntax on

" Set up spacing and alignment
set nowrap
set tabstop=2
set shiftwidth=2
set softtabstop=2
set linespace=2
set expandtab

" Keep more context when scrolling off the end of a buffer
set scrolloff=3

" Show chars
set list listchars=tab:\ \ ,trail:·

" Indentation
set autoindent
set smartindent

" Set splitting
set splitbelow

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

" Show (partial) command in the status line
set showcmd

" Use modeline overrides
set modeline
set modelines=10

" Insert only one space when joining lines with terminating punctuation
set nojoinspaces

" Automatically reload files changed outside of vim
set autoread

" Set cursorline
highlight Search cterm=underline

" Status bar
set laststatus=2

" Yank to platform clipboard
map <leader>y "*y

" Tab completion
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*

" Load the plugin and indent settings for the detected filetype
filetype plugin indent on



" Directories for swp files
set backupdir=~/.local/share/nvim/backups
set directory=~/.local/share/nvim/backups

set backupskip=/tmp/*,/private/tmp/*

" Code folding
set foldenable

set termguicolors
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif


" incremental substitution (neovim)
if has('nvim')
  set inccommand=split
endif

" Suppress appending <PasteStart> and <PasteEnd> when pasting
set t_BE=

set wildignore+=*/node_modules/*

" Turn off paste mode when leaving insert
autocmd InsertLeave * set nopaste
