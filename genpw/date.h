#include <time.h>
#include <stdbool.h>

typedef struct tm *date;

date dateparse(const char *str);
bool is_leap_year(date d);
int datecmp(date d1, date d2);
void next_day(date d);
