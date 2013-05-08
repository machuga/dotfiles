set nocompatible "This is just to be safe - should be implicitly included
execute pathogen#infect()
set number
set ruler
syntax on

" Set encoding
set encoding=utf-8

" Whitespace stuff
set nowrap
set tabstop=4
set shiftwidth=4
set softtabstop=4
set linespace=2
set scrolloff=5
set expandtab

set list listchars=tab:\ \ ,trail:·

set autoindent
set smartindent

set cpoptions+=$
set guioptions-=T

set virtualedit=block
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

if &term != "xterm-color"
    if has("gui-running")
        let g:solarized_termcolors=256
        set t_Co=16
        set background=light
        colorscheme solarized
        "colorscheme grb256
    else
        "let g:solarized_termcolors=256
        set t_Co=16
        set background=light
        colorscheme solarized
        "colorscheme grb256
    endif
endif
" Directories for swp files
set backupdir=~/.vim/backups
set directory=~/.vim/backups

" Code folding
set foldenable
nnoremap <leader>ft Vatzf

"set cursorline
highlight Search cterm=underline

if has('mouse')
    set mouse=a
endif
" Include user's local vim config
if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif

" Tab completion
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*

" load the plugin and indent settings for the detected filetype
filetype plugin indent on

" Status bar
set laststatus=2

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Without setting this, ZoomWin restores windows in a way that causes
" equalalways behavior to be triggered the next time CommandT is used.
" This is likely a bludgeon to solve some other issue, but it works
set noequalalways


" Thorfile, Rakefile, Vagrantfile and Gemfile are Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru}    set ft=ruby

" add json syntax highlighting
au BufNewFile,BufRead *.json set ft=javascript

" ejs use html
au BufNewFile,BufRead *.ejs set filetype=html

" make uses real tabs
au FileType make set noexpandtab

" make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79 expandtab

" make Ruby use 2 spaces
au FileType ruby set softtabstop=2 tabstop=2 shiftwidth=2 textwidth=79
au FileType coffee set softtabstop=2 tabstop=2 shiftwidth=2 textwidth=79
au FileType slim set softtabstop=2 tabstop=2 shiftwidth=2 textwidth=79
au FileType haml set softtabstop=2 tabstop=2 shiftwidth=2 textwidth=79
au FileType erb set softtabstop=2 tabstop=2 shiftwidth=2 textwidth=79
au FileType ujs set softtabstop=2 tabstop=2 shiftwidth=2 textwidth=79

" make PHP conform to PSR-1 standards
au FileType php set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79 expandtab
au FileType blade set textwidth=0

" Unimpaired configuration
" Bubble single lines
nmap <C-Up> [e
nmap <C-Down> ]e
" Bubble multiple lines
vmap <C-Up> [egv
vmap <C-Down> ]egv


" Key mappings
nnoremap ,h <C-W>h
nnoremap ,j <C-W>j
nnoremap ,k <C-W>k
nnoremap ,l <C-W>l
nnoremap ,H <C-W>H
nnoremap ,J <C-W>J
nnoremap ,K <C-W>K
nnoremap ,L <C-W>L
nnoremap <Leader>] :noh <CR>
nnoremap <Leader>p :set paste<CR>
nnoremap <Leader>o :set nopaste<CR>
vmap <C-x> :!pbcopy<CR>
vmap <C-c> :w !pbcopy<CR><CR>

set shell=/bin/zsh


" Package specific bindings
if exists(":Tabularize")
    nmap <Leader>a= :Tabularize /=<CR>
    vmap <Leader>a= :Tabularize /=<CR>
    nmap <Leader>a: :Tabularize /:\zs<CR>
    vmap <Leader>a: :Tabularize /:\zs<CR>
endif

if exists(":Gstatus")
    nnoremap <leader>gs :Gstatus<CR>
    nnoremap <leader>gd :Gdiff<CR>
    nnoremap <leader>gc :Gcommit<CR>
    nnoremap <leader>gb :Gblame<CR>
    nnoremap <leader>gl :Glog<CR>
    nnoremap <leader>gp :Git push<CR>
endif

" vim-javascript html indentation
let g:html_indent_inctags = "html,body,head,tbody"
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"

" ZoomWin configuration
" ZoomWin gives me too much shit lately to be useful
" map <Leader><Leader> :ZoomWin<CR>
map <Leader>' :TagbarToggle<CR>
