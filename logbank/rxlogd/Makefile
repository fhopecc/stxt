NAME=rxlogd

# ------------------------------------------

clean:
	rm -f ${NAME}.tar.gz

# ------------------------------------------

tar: clean
	tar cCpzf .. ${NAME}.tar.gz --exclude='CVS' --exclude "${NAME}.tar.gz" ${NAME}

rpm: tar
	rpmbuild -ta ${NAME}.tar.gz 
