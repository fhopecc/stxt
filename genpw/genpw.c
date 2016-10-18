#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "date.h"

void prefixnums(const char * prefix, int digits);
void genmobile();
void gendates(date d1, date d2);

int 
main(int argc, char **argv) {
    char c, output[9];
    int r;
    date sdate = NULL;
    date edate = NULL; 
    char *prefix = NULL;
    int digits;
     
    while ((c = getopt(argc, argv, "mp:d:s:e:")) != -1) {
        switch(c){
            case 's':
                sdate = dateparse(optarg);
                break;
            case 'e':
                edate = dateparse(optarg);
                break;
            case 'p':
                prefix = optarg;
                break;
            case 'd':
                sscanf(optarg, "%d", &digits);
                break;
            case 'm':
                genmobile();
                break;
            case '?':
                if (optopt == 's')
                    fprintf (stderr
                            , "Option -%c requires an argument.\n"
                            , optopt);
                else if (isprint (optopt))
                    fprintf (stderr
                            , "Unknown option `-%c'.\n"
                            , optopt);
                else
                    fprintf (stderr
                            , "Unknown option character `\\x%x'.\n"
                            , optopt);
                return 1;
            default:
                abort();
            }
    }
    if(sdate!=NULL && edate!=NULL)
        gendates(sdate, edate);
    if(prefix!=NULL && digits!=0)
        prefixnums(prefix, digits);
}

void gendates(date d1, date d2) {
    FILE *f;
    int i, j;
    printf("gendates start:\n");

    f = fopen("dates", "w");
    
    if(f == NULL){
        fprintf(stderr, "can't open file:[dates]");
        exit(1);
    }
    
    do {
        fprintf(f, "%s\n", strdate(d1));
        next_day(d1);
    } while(datecmp(d1, d2)<=0);
    fclose(f);
}

void fixednums(int digits) {
    FILE *f;
    int i;
    assert(digits < 100);

}

void prefixnums(const char * prefix, int digits) {
    FILE *f;
    int i;
    assert(digits < 100);
    int fslen = strlen(prefix)+5+1;//format string length
    char *fmtstr = (char *)malloc(sizeof(char)*fslen);
    sprintf(fmtstr, "%%s%%%02dd\n", digits);
    int max = (int)pow(10, digits) - 1;
    printf("Phonenums prefix is %s and digits is %d.\n", prefix, digits);

    f = fopen("phonenums", "w");
    if(f == NULL){
        fprintf(stderr, "can't open file:[phonenums]");
        exit(1);
    }
    for(i=0;i<=max;i++)
        fprintf(f, fmtstr, prefix, i);
    fclose(f);
    puts("Create phonenums ok.\n");
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


