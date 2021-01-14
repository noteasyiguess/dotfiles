vim.cmd('packadd packer.nvim')

return require('packer').startup(
  function()
    use {'wbthomason/packer.nvim', opt = true}

    use 'neovim/nvim-lspconfig'
    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
    use 'nvim-lua/completion-nvim'

    use 'kyazdani42/nvim-tree.lua'
    use {'nvim-telescope/telescope.nvim',
          requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    }

    use 'preservim/nerdcommenter'
    use 'tmsvg/pear-tree'
    use 'vimwiki/vimwiki'

    -- Coloschemes
    use 'sainnhe/sonokai'
    use 'sainnhe/edge'
    use 'sainnhe/gruvbox-material'
    use 'sainnhe/forest-night'
    use 'hoob3rt/lualine.nvim'
    use 'kyazdani42/nvim-web-devicons'  -- They say it must be the last to be loaded
  end
)
