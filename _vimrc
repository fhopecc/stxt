cd d:\stxt
lang mes en_US

set nocompatible
set backspace=2 "let backspace can delete back a char
set encoding=utf8
set foldmethod=syntax
set tabstop=4
set shiftwidth=4
set expandtab
set textwidth=80
set number "print the line number in front of line 
set cindent "apply c indent when open a new line
set ruler   " show line and column number in the bottom
syntax on
set hlsearch "Highlighten the searched string

map <F3> :w<enter>:! lib\slides_outputter.py db_slides doc\db\db_slides.stx<enter>

map <F6> :w<enter>:!task\make_doc.py<enter>
map! <F6> <esc>:w<enter>:!%<enter>

map <F10> :w<enter>:so doc\db\term_consist.vim<enter>

map <F11> :w<enter>:bp<enter>
map! <F11> <esc>:w<enter>:bp<enter>

map <F12> :w<enter>:bn<enter>
map <F12> <esc>:w<enter>:bn<enter>

cnoremap <C-A> <Home>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>

color darkblue
set guifont=Lucida_Console:h14:cANSI
