set nocompatible
filetype off

function! LoadVundle()
    let vundle_installed=filereadable(expand('~/.vim/bundle/vundle/README.md'))

    if vundle_installed == 0
        echo "Creating backups directory..."
        silent !mkdir -p ~/.vim/backups
        echo "Installing Vundle.."
        echo ""
        silent !mkdir -p ~/.vim/bundle
        silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
    endif

    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()

    " Bundle definitions
    Bundle 'scrooloose/syntastic'
    Bundle 'kchmck/vim-coffee-script'
    Bundle 'slim-template/vim-slim'
    Bundle 'tpope/vim-fugitive'
    Bundle 'tpope/vim-haml'
    Bundle 'tpope/vim-rails'
    Bundle 'tpope/vim-surround'
    Bundle 'tpope/vim-repeat'
    Bundle 'vim-ruby/vim-ruby'
    Bundle 'pangloss/vim-javascript'
    Bundle 'digitaltoad/vim-jade'
    Bundle 'majutsushi/tagbar'
    Bundle 'tomtom/tcomment_vim'
    Bundle 'mileszs/ack.vim'
    Bundle 'scrooloose/nerdtree'
    Bundle 'chriskempson/base16-vim'
    Bundle 'chriskempson/vim-tomorrow-theme'
    Bundle 'altercation/vim-colors-solarized'
    Bundle 'guns/vim-clojure-static'
    Bundle 'elzr/vim-json'
    Bundle 'nono/vim-handlebars'
    Bundle 'rking/ag.vim'
    Bundle 'kien/ctrlp.vim'
    Bundle 'SuperTab'
    Bundle 'Tabular'
    Bundle 'airblade/vim-gitgutter'
    "Bundle 'jeetsukumaran/vim-buffergator'

    if vundle_installed==0
        echo vundle_installed
        echo "Vundle Installed, now Installing Bundles..."
        echo ""
        :BundleInstall
    endif

    filetype plugin indent on
endfunction

call LoadVundle()

set number
set relativenumber
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

"if &term != "xterm-color"
    "if has("gui-running")
        "let g:solarized_termcolors=256
        "set t_Co=16
        "set background=light
        "colorscheme solarized
    "else
        "let g:solarized_termcolors=256
        "colorscheme grb256
    "endif
"else
    set t_Co=16
    set background=dark
    colorscheme base16-tomorrow
"endif
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
au BufNewFile,BufRead *.json set ft=json

" ejs use html
au BufNewFile,BufRead *.ejs set filetype=html

" .html.slim are slim
au BufNewFile,BufRead *.html.slim set ft=slim

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

" Ctrlp cleanup
let g:ctrlp_custom_ignore = '\.git$\|\.o$\|\.app$\|\.beam$\|\.dSYM\|\.ipa$\|\.csv\|tags\|public\/images$\|public\/uploads$\|log\|tmp$\|source_maps\|app\/assets\/images\|test\/reports\|node_modules\|bower_components'

" ZoomWin configuration
" ZoomWin gives me too much shit lately to be useful
" map <Leader><Leader> :ZoomWin<CR>
map <Leader>' :TagbarToggle<CR>

map <Leader>n :NERDTreeToggle<CR>

map <Leader>src :source ~/.vimrc<CR>
map <Leader>esrc :tabnew ~/.vimrc<CR>

" Include user's local vim config
if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif

function! ToggleFolding()
    let curr_fold=&foldmethod
    let en='off'

    if curr_fold == 'syntax'
        :setlocal foldmethod=manual
    else
        let en='on'
        :setlocal foldmethod=syntax
    endif

    echo "Toggled folding ".en
endfunction

map <Leader>tf :call ToggleFolding()<CR>
