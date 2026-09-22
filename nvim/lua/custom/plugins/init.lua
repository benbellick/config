-- You can add your own plugins here or in other files in this directory!
--  I promise not to create any merge conflicts in this directory :)
--
-- See the kickstart.nvim README for more information

---@module 'lazy'
---@type LazySpec
return {
  {
    'dlyongemallo/diffview.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewClose', 'DiffviewFileHistory' },
  },
  {
    'tpope/vim-fugitive',
    cmd = { 'Git', 'GitReviewBranch' },
    dependencies = { 'dlyongemallo/diffview.nvim' },
    keys = {
      { '<leader>gg', '<cmd>Git<cr>', desc = '[G]it status' },
      { '<leader>gb', '<cmd>Git blame<cr>', desc = '[G]it [B]lame' },
      { '<leader>gl', '<cmd>Git log<cr>', desc = '[G]it [L]og' },
    },
    config = function()
      vim.api.nvim_create_user_command('GitReviewBranch', function()
        local result = vim.system({ 'git', 'merge-base', 'origin/HEAD', 'HEAD' }, { text = true }):wait()
        local merge_base = vim.trim(result.stdout or '')
        if result.code ~= 0 or merge_base == '' then
          vim.notify('Could not determine branch merge base', vim.log.levels.ERROR)
          return
        end

        vim.cmd('DiffviewOpen ' .. merge_base)
      end, { desc = 'Review branch changes from the merge base' })
    end,
  },
}
