au BufRead,BufNewFile *.stx call s:StructedText()

func! s:StructedText()
    setfiletype structed_text
    map <F5> :w<enter>:!lib\stxt\formater\web.py % <enter>
endfunc

au BufRead,BufNewFile *_test.py call s:PythonTest()

func! s:PythonTest()
    setfiletype python_test
    set syntax=python
    map <F5> :w<enter>:!%<enter>
endfunc
