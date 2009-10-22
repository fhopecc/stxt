cd d:\stxt
lang mes en_US

set nocompatible
set backspace=2 "let backspace can delete back a char
set encoding=utf8
set foldmethod=syntax
set expandtab
set shiftwidth=4
set textwidth=80
set number "print the line number in front of line 
set cindent "apply c indent when open a new line
set ruler   " show line and column number in the bottom
syntax on

map <F5> :w<enter>:!%<enter>
map! <F5> <esc>:w<enter>:!%<enter>

map <F11> :w<enter>:bp<enter>
map! <F11> <esc>:w<enter>:bp<enter>

map <F12> :w<enter>:bn<enter>
map <F12> <esc>:w<enter>:bn<enter>

color darkblue
set guifont=Lucida_Console:h14:cANSI
