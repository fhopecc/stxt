#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef enum {
    mobile, 
    phone, 
    birthday
}

void genmobile();
void genphone();

int 
main(int argc, char **argv) {
    
    int c;
    while ((c = getopt(argc, argv, "mp:")) != -1) {
        switch(c){
            case 'p':


    }
}

void genmobile() {
    FILE *f;
    int i, j;
    f = fopen("mobilenums", "w");
    
    if(f == NULL){
        fprintf(stderr, "can't open file:[mobilenums]");
        exit(1);
    }
    for(i=9100; i<=9142;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9150; i<=9241;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9250; i<=9344;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9350; i<=9399;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9520; i<=9541;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9550; i<=9569;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9580; i<=9589;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9600; i<=9615;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9620; i<=9620;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9630; i<=9639;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9650; i<=9658;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9650; i<=9658;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9660; i<=9673;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9680; i<=9689;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9700; i<=9718;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9720; i<=9745;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9750; i<=9777;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9780; i<=9847;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    for(i=9850; i<=9899;i++)
        for(j=0;j<=99999;j++)
            fprintf(f, "%05d%05d\n", i, j);

    fclose(f);
}

void prefixnum(const char * prefix) {
    FILE *f;
    int i;
    
    f = fopen("phonenums", "w");
    
    if(f == NULL){
        fprintf(stderr, "can't open file:[phonenums]");
        exit(1);
    }

    for(i=0;i<=999999;i++)
        fprintf(f, "038%06d\n", i);
    fclose(f);
}
