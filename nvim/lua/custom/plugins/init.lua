-- You can add your own plugins here or in other files in this directory!
--  I promise not to create any merge conflicts in this directory :)
--
-- See the kickstart.nvim README for more information

---@module 'lazy'
---@type LazySpec
return {
  {
    'tpope/vim-fugitive',
    keys = {
      { '<leader>gg', '<cmd>Git<cr>', desc = '[G]it status' },
      { '<leader>gb', '<cmd>Git blame<cr>', desc = '[G]it [B]lame' },
      { '<leader>gl', '<cmd>Git log<cr>', desc = '[G]it [L]og' },
    },
  },
}
