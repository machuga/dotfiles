set nocompatible
filetype off
let mapleader = "\<Space>"

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
    Plugin 'scrooloose/syntastic'
    Plugin 'kchmck/vim-coffee-script'
    Plugin 'slim-template/vim-slim'
    Plugin 'tpope/vim-fugitive'
    Plugin 'tpope/vim-haml'
    Plugin 'tpope/vim-rails'
    Plugin 'tpope/vim-surround'
    Plugin 'tpope/vim-repeat'
    Plugin 'vim-ruby/vim-ruby'
    Plugin 'pangloss/vim-javascript'
    Plugin 'digitaltoad/vim-jade'
    Plugin 'majutsushi/tagbar'
    Plugin 'tomtom/tcomment_vim'
    Plugin 'scrooloose/nerdtree'
    Plugin 'chriskempson/base16-vim'
    Plugin 'chriskempson/vim-tomorrow-theme'
    Plugin 'altercation/vim-colors-solarized'
    Plugin 'guns/vim-clojure-static'
    Plugin 'elzr/vim-json'
    Plugin 'nono/vim-handlebars'
    Plugin 'xsbeats/vim-blade'
    Plugin 'rking/ag.vim'
    Plugin 'kien/ctrlp.vim'
    "Plugin 'Shougo/unite.vim'
    "Plugin 'Shougo/vimproc.vim'
    Plugin 'SuperTab'
    Plugin 'Tabular'
    Plugin 'fatih/vim-go'
    "Plugin 'airblade/vim-gitgutter'
    "Plugin 'jeetsukumaran/vim-buffergator'

    if vundle_installed==0
        echo vundle_installed
        echo "Vundle Installed, now Installing Bundles..."
        echo ""
        :BundleInstall
        silent !make -C ~/.vim/bundle/vimproc.vim
    endif

    filetype plugin indent on
endfunction

call LoadVundle()

set number
set relativenumber
set ruler
set hidden
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

" Insert only one space when joining lines with terminating punctuation
set nojoinspaces

" Automatically reload files changed outside of vim
set autoread

" Display incomplete commands
set showcmd

" Keep more context when scrolling off the end of a buffer
set scrolloff=3

" Scroll optimizations
" set nocursorcolumn
" set nocursorline
" syntax sync minlines=256

" Yank to platform clipboard
map <leader>y "*y

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Open files relative to current
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%
map <leader>v :view %%

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rename Current File
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>rn :call RenameFile()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RemoveFancyCharacters COMMAND
" Remove smart quotes, etc.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RemoveFancyCharacters()
    let typo = {}
    let typo["“"] = '"'
    let typo["”"] = '"'
    let typo["‘"] = "'"
    let typo["’"] = "'"
    let typo["–"] = '--'
    let typo["—"] = '---'
    let typo["…"] = '...'
    :exe ":%s/".join(keys(typo), '\|').'/\=typo[submatch(0)]/ge'
endfunction
command! RemoveFancyCharacters :call RemoveFancyCharacters()

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
    set t_Co=256
    set background=dark
    colorscheme base16-tomorrow
"endif
" Directories for swp files
set backupdir=~/.vim/backups
set directory=~/.vim/backups

" Code folding
set foldenable
"nnoremap <leader>ft Vatzf

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

nnoremap <Leader>h <C-W>h
nnoremap <Leader>j <C-W>j
nnoremap <Leader>k <C-W>k
nnoremap <Leader>l <C-W>l
nnoremap <Leader>H <C-W>H
nnoremap <Leader>J <C-W>J
nnoremap <Leader>K <C-W>K
nnoremap <Leader>L <C-W>L
nnoremap <Leader>] :noh <CR>
nnoremap <Leader>p :set paste<CR>
nnoremap <Leader>o :set nopaste<CR>
nnoremap <Leader>bn :bn<CR>
nnoremap <Leader>bp :bp<CR>
nnoremap <Leader>bd :bd<CR>
nnoremap <Leader>q  :q<CR>
nnoremap <Leader>w  :w<CR>
nnoremap <Leader>x  :wq<CR>
vmap <C-x> :!pbcopy<CR>
vmap <C-c> :w !pbcopy<CR><CR>


set shell=zsh\ -l

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
let g:ctrlp_custom_ignore = '\.git$\|\.o$\|\.app$\|\.beam$\|\.dSYM\|\.ipa$\|\.csv\|tags\|public\/images$\|public\/uploads$\|log\|tmp$\|source_maps\|app\/assets\/images\|test\/reports\|node_modules\|bower_components\|vendor'
nnoremap <Leader>f :CtrlP<CR>

" Unite
" let g:unite_enable_start_insert = 1
" let g:unite_split_rule = "botright"
" let g:unite_force_overwrite_statusline = 0
" let g:unite_winheight = 10
" let g:unite_source_grep_command = 'ag'
" 
" call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
"       \ 'ignore_pattern', join([
"       \ '\.git/',
"       \ '\.git/',
"       \ '\.o/',
"       \ '\.app/',
"       \ 'app\/assets\/images/',
"       \ 'test\/reports/',
"       \ 'node_modules',
"       \ 'bower_components',
"       \ 'tags',
"       \ ], '\|'))
" 
" call unite#filters#matcher_default#use(['matcher_fuzzy'])
" call unite#filters#sorter_default#use(['sorter_rank'])

" replace CtrlP with Unite
"nnoremap <C-p> :Unite file_rec/async<cr>
"nnoremap <C-p> :<C-u>Unite  -buffer-name=files   -start-insert buffer file_rec/async:!<cr>
"nnoremap <Leader>f :<C-u>Unite  -buffer-name=files   -start-insert buffer file_rec/async:!<cr>

" autocmd FileType unite call s:unite_settings()

" function! s:unite_settings()
"   let b:SuperTabDisabled=1
"   imap <buffer> <C-j>   <Plug>(unite_select_next_line)
"   imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
"   imap <silent><buffer><expr> <C-x> unite#do_action('split')
"   imap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
"   imap <silent><buffer><expr> <C-t> unite#do_action('tabopen')
" 
"   nmap <buffer> <ESC> <Plug>(unite_exit)
" endfunction

" Other stuff
" nnoremap <space>/ :Unite grep:.<cr>
" let g:unite_source_history_yank_enable = 1
" nnoremap <space>y :Unite history/yank<cr>
" nnoremap <space>s :Unite -quick-match buffer<cr>

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
