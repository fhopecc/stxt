rem prerequesties
rem -------------
rem easy_install nose
rem easy_install nosegae
rem
rem add "c:\Python25\Scripts" to Path ENV
rem nosetests.exe is in thist path
nosetests -v --with-gae --gae-lib-root="c:\google_appengine"
