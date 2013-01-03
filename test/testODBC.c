#include <windows.h>
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>
#include <stdio.h>
#include <ctype.h>

void extract_error(
    char *fn,
    SQLHANDLE handle,
    SQLSMALLINT type);

char *trimwhitespace(char *str);

int main() {
      SQLHENV env;
      SQLHDBC dbc;
      SQLHSTMT stmt;
      SQLRETURN ret; /* ODBC API return status */
      SQLCHAR outstr[1024];
      SQLSMALLINT outstrlen;

      /* Allocate an environment handle */
      SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
      /* We want ODBC 3 support */
      SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, (void *) SQL_OV_ODBC3, 0);
      /* Allocate a connection handle */
      SQLAllocHandle(SQL_HANDLE_DBC, env, &dbc);
      /* Connect to the DSN mydsn */
      //char enter[80];
      //puts("Enter connect string!");
      //char* connstr =trimwhitespace(gets(enter));
      char* connstr = "Driver={SQL Server};Server=192.168.1.8;Database=yrxweb;uid=eltweb;pwd=bewt_111";
      ret = SQLDriverConnect(dbc, NULL, connstr, SQL_NTS,
                 outstr, sizeof(outstr), &outstrlen,
                 SQL_DRIVER_NOPROMPT);
      puts("END");
      if (SQL_SUCCEEDED(ret)) {
        printf("Connected\n");
        printf("Returned connection string was:\n\t%s\n", outstr);
        if (ret == SQL_SUCCESS_WITH_INFO) {
          printf("Driver reported the following diagnostics\n");
          extract_error("SQLDriverConnect", dbc, SQL_HANDLE_DBC);
        }
        SQLDisconnect(dbc);		/* disconnect from driver */
      } else {
        fprintf(stderr, "Failed to connect\n");
        extract_error("SQLDriverConnect", dbc, SQL_HANDLE_DBC);
      }
      /* free up allocated handles */
      SQLFreeHandle(SQL_HANDLE_DBC, dbc);
      SQLFreeHandle(SQL_HANDLE_ENV, env);
      puts("OK");
}

void extract_error(
    char *fn,
    SQLHANDLE handle,
    SQLSMALLINT type)
{
    SQLINTEGER	 i = 0;
    SQLINTEGER	 native;
    SQLCHAR	 state[ 7 ];
    SQLCHAR	 text[256];
    SQLSMALLINT	 len;
    SQLRETURN	 ret;

    fprintf(stderr,
            "\n"
            "The driver reported the following diagnostics whilst running "
            "%s\n\n",
            fn);

    do
    {
        ret = SQLGetDiagRec(type, handle, ++i, state, &native, text,
                            sizeof(text), &len );
        if (SQL_SUCCEEDED(ret))
            printf("%s:%ld:%ld:%s\n", state, i, native, text);
    }
    while( ret == SQL_SUCCESS );
}


char *trimwhitespace(char *str)
{
  char *end;

  // Trim leading space
  while(isspace(*str)) str++;

  if(*str == 0)  // All spaces?
    return str;

  // Trim trailing space
  end = str + strlen(str) - 1;
  while(end > str && isspace(*end)) end--;

  // Write new null terminator
  *(end+1) = 0;

  return str;
}
