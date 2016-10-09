#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "date.h"

void genmobile();
void genphone();
void gendates(date d1, date d2);

int 
main(int argc, char **argv) {
    char c, output[9];
    int r;
    date sdate = NULL;
    date edate = NULL; 
     
    while ((c = getopt(argc, argv, "s:e:")) != -1) {
        switch(c){
            case 's':
                sdate = dateparse(optarg);
                printf("sdate->year %d ->mon %d ->mday %d\n"
                      , sdate->tm_year
                      , sdate->tm_mon
                      , sdate->tm_mday);
                break;
            case 'e':
                edate = dateparse(optarg);
                printf("edate->year %d ->mon %d ->mday %d\n"
                      , edate->tm_year
                      , edate->tm_mon
                      , edate->tm_mday);
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
