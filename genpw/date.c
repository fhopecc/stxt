#include <stdlib.h>
#include <stdio.h>
#include "date.h"

/* 西元年閏年規則如下： 
 * 西元年分除以400可整除，為閏年。
 * 西元年分除以4可整除但除以100不可整除，為閏年。
 * 西元年分除以4不可整除，為平年。
 * 西元年分除以100可整除但除以400不可整除，為平年
 *
 * (西元年分除以400可整除) or (西元年分除以4可整除 and 除以100不可整除)
 */
bool is_leap_year(date d) {
    return (d->tm_year % 400 * (d->tm_year % 4 + !(d->tm_year %100)) == 0);
}

char *strdate(date d) {
    char *str = (char *)malloc(sizeof(char) * 9);
    sprintf(str, "%04d%02d%02d", d->tm_year, d->tm_mon, d->tm_mday);
    return str;
}
void next_day(date d) {
    bool year_carry=false;
    bool mon_carry=false;
    int mday = d->tm_mday += 1;
    
    switch(d->tm_mon) {
    case 12:
        year_carry = mday > 31;
        break;
    case 2:
        mon_carry = is_leap_year(d)?mday>29:mday>28;
        break;
    case 1:
    case 3: 
    case 5:
    case 7:
    case 8:
    case 10:
        mon_carry = mday > 31; 
        break;
    case 4:
    case 6:
    case 9:
    case 11:
        mon_carry = mday > 30;
        break;
    }
    if(year_carry) {
        d->tm_mday=1;
        d->tm_mon=1;
        d->tm_year+=1;
    } else if(mon_carry) {
        d->tm_mday=1;
        d->tm_mon+=1;
    } 
}

date dateparse(const char * input) {
    int mday, mon, year; 
    date d = (date)malloc(sizeof(struct tm));
    sscanf(input, "%04d%02d%02d", &year, &mon, &mday);
    d->tm_mday = mday;
    d->tm_mon = mon;
    d->tm_year = year;
    return d;
}

int datecmp(date d1, date d2) {
    int r = d1->tm_year - d2->tm_year;
    r = (r==0)?d1->tm_mon - d2->tm_mon:r;
    return (r==0)?d1->tm_mday - d2->tm_mday:r;
}
