vim.cmd('packadd packer.nvim')
vim.cmd('packadd termdebug')

return require('packer').startup(
  function()
    use {'wbthomason/packer.nvim', opt = true}

    -- Smooth scrolling
    use 'psliwka/vim-smoothie'

    use 'neovim/nvim-lspconfig'
    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
    --use 'nvim-lua/completion-nvim'
    use 'hrsh7th/nvim-compe'

    use 'kyazdani42/nvim-tree.lua'
    use {'nvim-telescope/telescope.nvim',
          requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    }

    use 'preservim/nerdcommenter'
    use 'jiangmiao/auto-pairs'
    use 'vimwiki/vimwiki'

    -- Coloschemes
    use 'sainnhe/sonokai'
    use 'sainnhe/edge'
    use 'sainnhe/gruvbox-material'
    use 'sainnhe/forest-night'
    use 'christianchiarulli/nvcode-color-schemes.vim'
    use 'mhartington/oceanic-next'
    use 'glepnir/zephyr-nvim'

    use 'hoob3rt/lualine.nvim'
    use 'kyazdani42/nvim-web-devicons'  -- They say it must be the last to be loaded
  end
)
