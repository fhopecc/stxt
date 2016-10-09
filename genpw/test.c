#include <assert.h>
#include <stdio.h>
#include "date.h"

void test_date(void);

void main() {
    test_date();
}

void test_date(void) {
    date d = dateparse("19790729"); 
    assert(d->tm_year==1979);
    assert(d->tm_mon==7);
    assert(d->tm_mday==29);
    assert(!is_leap_year(d));
    d->tm_year=2000;
    assert(is_leap_year(d));
    d->tm_year=2004;
    assert(is_leap_year(d));
    d->tm_year=1900;
    assert(!is_leap_year(d));
    d->tm_year=1904;
    assert(is_leap_year(d));
    next_day(d);
    assert(d->tm_mday == 30);
    next_day(d);
    assert(d->tm_mday == 31);
    next_day(d);
    assert(d->tm_mday == 1);
    assert(d->tm_mon == 8);
    d->tm_mon=2;
    d->tm_mday=28;
    next_day(d);
    assert(d->tm_mday == 29);
    next_day(d);
    assert(d->tm_mday == 1);
    assert(d->tm_mon == 3);
    d->tm_year=1979;
    d->tm_mon=2;
    d->tm_mday=28;
    next_day(d);
    assert(d->tm_mday == 1);
    assert(d->tm_mon == 3);
    d->tm_year=1979;
    d->tm_mon=12;
    d->tm_mday=31;
    next_day(d);
    assert(d->tm_mday == 1);
    assert(d->tm_mon == 1);
    assert(d->tm_year == 1980);
    d = dateparse("19770803");
    next_day(d); 
    assert(d->tm_year == 1977);
    assert(d->tm_mon == 8);
    assert(d->tm_mday == 4);
    assert(datecmp(dateparse("19770804"), d)==0);

    assert(datecmp(dateparse("19800101"), dateparse("19790729"))>0);
    assert(datecmp(dateparse("19330101"), dateparse("19790729"))<0);
    assert(datecmp(dateparse("19790729"), dateparse("19790729"))==0);
}
