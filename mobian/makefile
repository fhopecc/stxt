test.exe : test.c ku.o error.o
	gcc -o test.exe ku.o error.o test.c
error.o : error.c
	gcc -c error.c
ku.o : ku.c
	gcc -c mobian.c
mobian.exe : mobian.c
	gcc -o mobian.exe mobian.c
