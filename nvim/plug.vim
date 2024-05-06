if has("nvim")
  let g:plug_home = stdpath('data') . '/plugged'
endif

call plug#begin()

if has("nvim")
  Plug 'neovim/nvim-lspconfig'
  Plug 'glepnir/lspsaga.nvim'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': 'TSUpdate'}
  Plug 'hrsh7th/nvim-cmp'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'dcampos/nvim-snippy'
  Plug 'dcampos/cmp-snippy'
  Plug 'junegunn/vim-easy-align'
  Plug 'machakann/vim-sandwich'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  Plug 'kyazdani42/nvim-web-devicons'
  "Plug 'roman/golden-ratio'
  Plug 'beauwilliams/focus.nvim'
  Plug 'tinted-theming/base16-vim'
"You must run setup() to begin using focus
endif

call plug#end()
