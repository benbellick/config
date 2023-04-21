set encoding=UTF-8
" Add line numbers
set number
set ruler
" Colors 
syntax on
" Spacing set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
" use tabs for Makefile
autocmd FileType make set noexpandtab shiftwidth=8 softtabstop=0

set nocompatible

set path=$PWD/**
set wildmenu
set wildmode=longest:full,full

set autoindent

"Remove arrow key functionality
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>
