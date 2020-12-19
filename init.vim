call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'machakann/vim-sandwich' " vim-surround, but better
"Plug 'ervandew/supertab'
Plug 'Townk/vim-autoclose'
Plug 'w0rp/ale'
Plug 'arcticicestudio/nord-vim'

" Fuzzy Matching
"Plug '/Users/machuga/homebrew/bin/fzf' " Using Homebrew
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" Install CoC
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neoclide/coc-tsserver'

Plug 'alunny/pegjs-vim'

" Appearance
Plug 'roman/golden-ratio'
Plug 'trevordmiller/nova-vim'
Plug 'chriskempson/base16-vim'

" Language support
Plug 'sheerun/vim-polyglot'

" Group dependencies, vim-snippets depends on ultisnips
Plug 'othree/jspc.vim', { 'for': ['javascript', 'javascript.jsx'] }

" Git Integration (Magit clone)
Plug 'jreybert/vimagit'

" Plug 'neomake/neomake'
Plug 'Shougo/denite.nvim'

" Let's try ALE again
Plug 'w0rp/ale'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Rainbow Parens
Plug 'junegunn/rainbow_parentheses.vim'

" PEG Parser
Plug 'alunny/pegjs-vim'

" Add plugins to &runtimepath
call plug#end()

runtime! macros/sandwich/keymap/surround.vim

" Use deoplete.
" call deoplete#enable()
let g:deoplete#enable_at_startup = 1
let g:deoplete#omni#functions = {}
let g:deoplete#omni#functions.javascript = [
  \ 'tern#Complete',
  \ 'jspc#omni'
\]

set completeopt=longest,menuone,preview
let g:deoplete#sources = {}
let g:deoplete#sources['javascript.jsx'] = ['file', 'ultisnips', 'ternjs']
let g:tern#command = ['tern']
let g:tern#arguments = ['--persistent']

autocmd FileType javascript let g:SuperTabDefaultCompletionType = "<c-x><c-o>"
let g:UltiSnipsExpandTrigger="<C-j>"
"inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" close the preview window when you're not using it
"let g:SuperTabClosePreviewOnPopupClose = 1
" or just disable the preview entirely
"set completeopt-=preview

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
"set t_Co=256
"set background=dark
" colorscheme nova
"colorscheme base16-tomorrow
set termguicolors
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

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

"""""""""""""""""
" Elm config
"""""""""""""""""
let g:elm_format_autosave = 1
let g:elm_jump_to_error = 0
let g:elm_make_output_file = "elm.js"
let g:elm_make_show_warnings = 0
let g:elm_browser_command = ""
let g:elm_detailed_complete = 0
let g:elm_format_fail_silently = 0
let g:elm_setup_keybindings = 1

nmap <leader>ce :ElmErrorDetail<cr>
nmap <leader>cm :ElmMake<cr>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Open files relative to current
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%
map <leader>v :view %%
map <leader>sp :sp %%
map <leader>vsp :vsp %%

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


""""""""
" Linter (Ale)
""""""""

function! LinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? 'OK' : printf(
    \   '%dW %dE',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

""""""""""
" Airline
""""""""""
" Set this. Airline will handle the rest.
" let g:airline#extensions#ale#enabled = 1
let g:airline_extensions = ['ale', 'denite', 'vimagit']
let g:airline_powerline_fonts = 1
let g:airline_theme='monochrome'

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

" Load PEG.js parser grammars
au BufNewFile,BufRead *.grammar set ft=pegjs

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

" fix stylus
au FileType ruby set softtabstop=2 tabstop=2 shiftwidth=2 textwidth=79 noexpandtab

" fix stylus
au FileType markdown set wrap textwidth=79 noexpandtab

" run neomake on everything when possible
"
"autocmd! BufWritePost * Neomake
augroup on_buf_write
  autocmd!
  autocmd BufWritePre * %s/\s\+$//e
augroup END

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
nnoremap <Leader>pf :Files<CR>
nnoremap <Leader>/ :Rg<CR>
nnoremap <Leader>fed :vsp ~/.config/nvim/init.vim<CR>
nnoremap <Leader>feR :source ~/.config/nvim/init.vim<CR>
nnoremap <Leader>gs :Magit<CR>

" Easy Align
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Coc Configuration
" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
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
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
"autocmd CursorHold * silent call CocActionAsync('highlight')

function! ShowDocIfNoDiagnostic(timer_id)
  if (coc#float#has_float() == 0)
    silent call CocActionAsync('doHover')
  endif
endfunction

function! s:show_hover_doc()
  call timer_start(250, 'ShowDocIfNoDiagnostic')
endfunction

autocmd CursorHoldI * :call <SID>show_hover_doc()
autocmd CursorHold * :call <SID>show_hover_doc()

" Symbol renaming.
nmap <leader>crn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>cf  <Plug>(coc-format-selected)
nmap <leader>cf  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType ts,typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>ca  <Plug>(coc-codeaction-selected)
nmap <leader>ca  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>cal  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>cqf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

command! -nargs=0 Prettier :CocCommand prettier.formatFile

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <leader>ca  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <leader>ce  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <leader>cc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <leader>co  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <leader>cs  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <leader>cj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <leader>ck  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <leader>cp  :<C-u>CocListResume<CR>





" FZF config
"   :Ag  - Start fzf with hidden preview window that can be enabled with "?" key
"   :Ag! - Start fzf in fullscreen and display the preview window above
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

" Similarly, we can apply it to fzf#vim#grep. To use ripgrep instead of ag:
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" Likewise, Files command with preview window
command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
