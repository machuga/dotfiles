call plug#begin('~/.config/nvim/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-surround'
Plug 'vim-ruby/vim-ruby'
Plug 'chriskempson/base16-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'roman/golden-ratio'

" Group dependencies, vim-snippets depends on ultisnips
"Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

Plug 'neomake/neomake'

" Add plugins to &runtimepath
call plug#end()

if has('nvim')
	tnoremap <Esc> <C-\><C-n>
endif

let mapleader = "\<Space>"
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
set backupdir=~/.config/nvim/backups
set directory=~/.config/nvim/backups

" Set theming info
set t_Co=256
set background=dark
colorscheme base16-tomorrow

" Code folding
set foldenable

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

""""""""""""""""""""
" Setup file types
""""""""""""""""""""

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

" run neomake on everything when possible
"
autocmd! BufWritePost * Neomake

" Key mappings
nnoremap <Leader>wh <C-W>h
nnoremap <Leader>wj <C-W>j
nnoremap <Leader>wk <C-W>k
nnoremap <Leader>wl <C-W>l
nnoremap <Leader>wH <C-W>H
nnoremap <Leader>wJ <C-W>J
nnoremap <Leader>wK <C-W>K
nnoremap <Leader>wL <C-W>L
nnoremap <Leader>] :noh <CR>
nnoremap <Leader>p <C-^>

nnoremap <Leader>o :set nopaste<CR>
nnoremap <Leader>bn :bn<CR>
nnoremap <Leader>bp :bp<CR>
nnoremap <Leader>bd :bd<CR>
nnoremap <Leader>q  :q<CR>
nnoremap <Leader>w  :w<CR>
nnoremap <Leader>x  :wq<CR>
vmap <C-x> :!pbcopy<CR>
vmap <C-c> :w !pbcopy<CR><CR>
nnoremap <Leader>pt :NERDTreeToggle<CR>
nnoremap <Leader>pf :FZF<CR>
nnoremap <Leader>/ :Ag<CR>
nnoremap <Leader>fed :vsp ~/.config/nvim/init.vim<CR>
nnoremap <Leader>feR :source ~/.config/nvim/init.vim<CR>
