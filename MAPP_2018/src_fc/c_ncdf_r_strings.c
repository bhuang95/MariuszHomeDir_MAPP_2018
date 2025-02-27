#include <stdio.h>
#include <string.h>
#include <netcdf.h>

#include "c_ncdf_r_strings.h"

#define NDIMS 4
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); puts("2");}

void c_ncdf_r_strings ( int grpid, int varid, int nlocs, int length )
{

  int i, j, number_of_arrays;

  int status;

  size_t start[] = {0,0};
  size_t count[] = {nlocs,length+1};


  /*
  size_t start[] = {0};
  size_t count[] = {nlocs-1};
  */

  char datetime[nlocs][length+1];
  char *datetime_p[nlocs];

  
  printf("grpid, varid, nlocs: %i, %i, %i \n",grpid,varid,nlocs);

  number_of_arrays = sizeof(datetime) / sizeof(datetime[0]);

  printf("number_of_arrays: %i\n", number_of_arrays);
  printf("aaa: %i\n",sizeof(datetime));
  printf("bbb: %i\n",sizeof(datetime[0]));

  for (i = 0; i < number_of_arrays; i++)
    datetime_p[i] = datetime[i];

  if ((status = nc_get_vara_string(grpid, varid, start, count, 
				   datetime_p)))
    ERR(status);

  if ((status = nc_get_vara(grpid, varid, start, count, 
				   datetime_p)))
    ERR(status);

  if ((status = nc_get_var(grpid, varid, datetime_p)))
    ERR(status);

  if ((status = nc_get_var(grpid, varid, &datetime[0][0])))
    ERR(status);

  printf("datetime: %s\n",datetime[0]);

  /*

  int ndims;
  int *ndims_p;

  if ((status = nc_inq_varndims(grpid, varid, ndims_p)))
    ERR(status);

  printf("ndims: %i\n",ndims);

  
  strcpy(datetime[0],"abc");
  printf("datetime: %s\n",datetime[0]);


  printf("datetime: %s\n",*datetime_p[0]);

  for(i = 0; i < nlocs; i++)
    {
      printf("%s\n", datetime[i]);
    }
  

  */

  
  printf("*** SUCCESS reading aeronet file!\n");
  
}
