#if defined(IBM) || defined(HP)

#define c_pause c_pause
#define iralloc iralloc
#define irfree irfree
#define c_open c_open
#define c_close c_close
#define c_read c_read
#define c_write c_write
#define findgrib findgrib
#define cv_to_ut cv_to_ut
#define cv_fr_ut cv_fr_ut
#define julian_date julian_date
#define calandar_date calander_date

#elif defined(SUN) || defined(SGI)

#define c_pause c_pause_
#define iralloc iralloc_
#define irfree irfree_
#define c_open c_open_
#define c_close c_close_
#define c_read c_read_
#define c_write c_write_
#define findgrib findgrib_
#define cv_to_ut cv_to_ut_
#define cv_fr_ut cv_fr_ut_
#define julian_date julian_date_
#define calandar_date calander_date_
#define c_swap4 c_swap4_
#define c_view4 c_view4_

#elif defined(ALPHA)

#define c_pause c_pause__
#define iralloc iralloc__
#define irfree irfree__
#define c_open c_open__
#define c_close c_close__
#define c_read c_read__
#define c_write c_write__
#define findgrib findgrib__
#define cv_to_ut cv_to_ut__
#define cv_fr_ut cv_fr_ut__
#define julian_date julian_date__
#define calandar_date calander_date__
#define c_swap4 c_swap4__
#define c_view4 c_view4__

#elif defined(STARDENT) || defined(CRAY)

#define c_pause C_PAUSE 
#define iralloc IRALLOC
#define irfree IRFREE
#define c_open C_OPEN
#define c_close C_CLOSE
#define c_read C_READ
#define c_write C_WRITE
#define findgrib FINDGRIB
#define cv_to_ut CV_TO_UT
#define cv_fr_ut CV_FR_UT
#define julian_date JULIAN_DATE
#define calandar_date CALANDER_DATE

#endif


/*#ifdef LITTLE_END*/
 #define BDS_LEN(bds)		((int) ((bds[0]<<16)+(bds[1]<<8)+bds[2]))
 #define BMS_LEN(bms)		((bms) == NULL ? 0 : (bms[0]<<16)+(bms[1]<<8)+bms[2])
 #define GDS_LEN(gds)		((int) ((gds[0]<<16)+(gds[1]<<8)+gds[2]))
 #define PDS_LEN(pds)		((int) ((pds[0]<<16)+(pds[1]<<8)+pds[2]))

 #define PDS_HAS_GDS(pds)        ((pds[7] & 128) != 0)
 #define PDS_HAS_BMS(pds)        ((pds[7] & 64) != 0)

 #define LEN_HEADER_PDS (28+42+100)
 #define END_LEN 4
/*#endif*/

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#define TRUE 1
#define FALSE 0

#define MAXFILES 256

struct file_s{
  FILE *F;
  int   f; 
};

struct file_s file[MAXFILES];
int init = 0;


struct s_gribstat{
 int         bds_start;
 long pds_len;
 long gds_len;
 long bms_len;
 long end_len;
 long bds_len;
} sg;


/*---+----------------------------------------------------------------*/
/*
int isprint(char c){
 return(c>=0 && c<126);
}
*/
/*---+----------------------------------------------------------------*/


/*---+----------------------------------------------------------------*/
FILE *findfile(int fd){

  int i;


#ifdef DEBUG
  i=0;
  printf("file[%d].f : %d\tfile[%d].F : %d\n", i, file[i].f, i, file[i].F);
  i=1;
  while(i<MAXFILES  &&   file[i].f != 0  &&  file[i].F != NULL){
    printf("file[%d].f : %d\tfile[%d].F : %d\n", i, file[i].f, i, file[i].F);
    i++;
  }
#endif



  for(i=0; i<MAXFILES; i++)
    if( file[i].f == fd)  return  file[i].F;

  perror("io_utils.c -> findfile() : file descriptor not found");
  exit(1);
}/*end fildfile()*/
/*---+----------------------------------------------------------------*/


/*---+----------------------------------------------------------------*/

void *iralloc(int *memtot, char *ia, int *ioff)

{
    char *addr;

       addr = calloc(*memtot,4);
       *ioff = (addr-ia)/4;
/*     printf("in ralloc- %i %i %i \n",*memtot,iaddr,*ioff);
*/
    return(addr);
}



/*---+----------------------------------------------------------------*/

void irfree(char **addr)

{
      free(*addr);
}


/*---+----------------------------------------------------------------*/

int c_open(char *filename, char *faccess)

{

  int i;
  int done=0;
  
  /* initialize file structure */
  if(init == 0) {
    for(i=0; i<MAXFILES; i++){
      file[i].f = 0;
      file[i].F = NULL;
    }
    init=1;
  }
  
#ifdef DEBUG
  printf("c_open called...\n\n");
  printf("filename : %s(end)\n",filename);
  printf("faccess : %s(end)\n",faccess);
  printf("press key to continue...\n");
  getchar();
#endif

  i=0;
  while(!done) {
    if (i >= MAXFILES) {
      perror("io_utils.c -> c_open(): MAXFILES exceeded");
      exit(1);
    }
    if ((file[i].f==0) && (file[i].F==NULL)) {
      file[i].F = fopen(filename,faccess);
      if (file[i].F==NULL) {
	file[i].f=0;
      }
      else {
	file[i].f = fileno(file[i].F);
      }
      done=1;
    }
    else {
      i++;
    }
  } 
  
  return(file[i].f);
}



/*---+----------------------------------------------------------------*/

int c_close(int *fd)
{
#ifdef DEBUG
  printf("c_close called...\n\n");
#endif
  
  int i,err;


  for(i=0; i<MAXFILES; i++) {
    if( file[i].f == *fd) {
      err=fclose(file[i].F);
      file[i].f=0;
      file[i].F=NULL;
      return(err);
    }
  }

  perror("io_utils.c -> c_close() : file descriptor not found");
  exit(1);
  
}



/*---+----------------------------------------------------------------*/

int c_read(int *fbyte, int *numbytes, void *a, int *fd)

{
  int retcode;

#ifdef DEBUG
  printf("c_read called...\n\n");
  printf("fbyte %d\n",(int)*fbyte);
  printf("numbytes : %d\n",(int)*numbytes);
  printf("fd : %d\n",(int)*fd);
  printf("findfile result : %d\n",findfile((int)*fd));
#endif


  retcode=fseek(findfile((int)*fd),*fbyte,0);

  if(retcode != 0)
   {
     /* JR NOTE: "file" is not a char * */
     printf("C_read error - filename %s \n",file);
     return(retcode);
   }

  retcode=fread((char*)a,1,*numbytes,findfile((int)*fd));

  if(retcode != *numbytes)
   {
     return(errno);
   }
  else
   {
     return(0);
   }
}

/*---+----------------------------------------------------------------*/

int c_write(int *fbyte, int *numbytes, void *a, int *fd)
{
  int retcode;
  int i = 0;


#ifdef DEBUG
  char *b = (char *) a; 
#endif



#ifdef DEBUG
  
  printf("c_write called...\n\n");
  printf("entered c_write...\n");
  printf("*fbyte    : %d\n",*fbyte);
  printf("*numbytes : %d\n",*numbytes);
  printf("*a        : %p\n",a);
  printf("*fd       : %d\n\n",*fd);

  while(getchar() != 'q' && (i < (*numbytes-1))){
    /*printf("a[%i] = %c\n", i, (char ) ((char *)a + i*sizeof(char *)));*/
    if((char)*(b) == 0){ printf("NULL\n"); i++; b++; continue; }

    if((char)*(b)== 1){ printf("'1'\n"); i++; b++; continue; }
  
    printf("b[%i] = %c\n", i, (char ) *(b++));
    i++;
  }
#endif

  if(*numbytes == 0 ){
     perror("asked c_write to write ZERO bytes!!");
     return -1;
  }
     
  retcode=fseek(findfile((int)*fd),*fbyte,2);

  if(retcode != 0)
   {
     /* JR NOTE "findfile" is not a char * */
     printf("C_write error - filename %s \n",findfile((int)*fd));
     return(retcode);
   }


  retcode=fwrite((char*)a,1,*numbytes,findfile((int)*fd));

  if(retcode != *numbytes)
   {

     return(errno);
   }
  else
   {
     return(0);
   }
}




/*---+----------------------------------------------------------------*/

#define WORD_SIZE sizeof(int)			/*assumed addresses are same size as INT*/



int c_swap4(int *numbytes, void *a)
{
  int retcode = 0;
  int i = 0;

  int j    = 0;                  	/*where the data starts*/
  int last = 0; 			/*index of last full word to swap*/
  int n_end = 0;			/*number of 'lose' bytes at end of odd length buffer*/

  char *b = (char *) a; 


  if(*numbytes < 4) perror("SWAPPING LESS THAN FOUR BYTES! ");

  if((unsigned long) b % WORD_SIZE != 0 ) printf("INPUT BUFFER NOT ALIGNED ON WORD BOUNDRY!\n");

  /*perror("INPUT BUFFER NOT ALIGNED ON WORD BOUNDRY! ");*/
 

  last  = (int) (((*numbytes / ((int) WORD_SIZE))-1) * ((int) WORD_SIZE));

  
  if(last < 0 || *numbytes < 0) { perror("OVERFLOW IN IO_UTILS.C - C_SWAP4"); exit(1); }

  n_end = *numbytes % WORD_SIZE;

#ifdef DEBUG
  printf("c_swap4 called...\n\n");
  printf("\n\n");
  printf("a's addr  : %ld\n",  a);
  printf("WORD_SIZE : %d\n",  WORD_SIZE);
  printf("numbytes  : %d\n",  *numbytes);
  printf("last  = (((*numbytes / WORD_SIZE)-1) * WORD_SIZE)\n");
  printf("last      : %d\n", last);
  printf("n_end     : %d\n",  n_end);

  /* get j to the right place!!!! */
j:
  printf("\nenter a byte to start swapping at!  : ");
  scanf ("%d",&j);
  printf("\n");
  if(j%100000 == 0) printf("j  : %d\n",j);
  printf("a+j's addr : %d\n\n",a+j);
  printf("SWAPPING!!\n");
#endif


/*code to align on a word boundry*/
/*while((int)a % sizeof(int)  !=  0) j++;*/

   


/* swap bytes from big/little endian to little/big endian */ 

    if(*numbytes >= 4){
      while(j <= last){

        b[j] =   b[j] ^ b[j+3]; 
        b[j+3] = b[j] ^ b[j+3]; 	/*swap j and j+3*/
        b[j] =   b[j] ^ b[j+3]; 

        b[j+1] = b[j+1] ^ b[j+2]; 
        b[j+2] = b[j+1] ^ b[j+2];		/*swap j+1 and j+2*/ 
        b[j+1] = b[j+1] ^ b[j+2]; 
     
        j += 4; 
#ifdef DEBUG
/*  printf("j : %d\n",j);     */
#endif
      }
    }

   switch(n_end){
     case 3:	b[j+1] = b[j+1] ^ b[j+2]; 
      		b[j+2] = b[j+1] ^ b[j+2];		/*swap j+1 and j+2*/ 
      		b[j+1] = b[j+1] ^ b[j+2]; 

/*#ifdef DEBUG*/
      		if(b[j] != 0) perror("CANNOT SWAP THIS WORD, UNKNOWN BYTE # 4! ");
		retcode = 1;
/*#endif*/
		break;

#ifdef DEBUG
     case 2:	if((b[j]+b[j+1]) != 0) perror("CANNOT SWAP THIS WORD, UNKNOWN BYTE # 3 AND 4! ");
		retcode = 1;
		break;

     case 1: 	if(b[j] != 0) perror("CANNOT SWAP THIS WORD, UNKNOWN BYTE # 2 AND 3 AND 4! "); 
		retcode = 1;
		break;
#endif

     default:	break;
   }

    return(retcode);
}/* end c_swap4() */


/*---+----------------------------------------------------------------*/

int c_view4(int *numbytes, void *a, int *iy)
{
  int retcode = 0;
  int i = 0;
  int old_numbytes = *numbytes;
  long j = 0;                         /*where the data starts*/
  char C[3]; 

  char  *b = (char *) a; 
  float *c = (float *) a; 
  int   *d = (int *) a; 

  C[0] = C[1] = C[2] = 0;

  printf("\n\n\n");
  printf("c_view4 called...\n");
  printf("a's addr : %p\n",a);
  printf("numbytes : %d\n",*numbytes);
  printf("iy : %i\n",*iy);
  
 if(*iy == 0){
  printf("viewing as CHAR...\n");

  i = 0;
/*
  while(i < *numbytes && ((char) *b == NULL)){
    i++;
    b++;
  }
*/
  while(i < *numbytes && getchar() != 'q'){
    C[1] = (char) *b;
    printf("b[%i] = %o\t@ %d\n", i, C[1] ,b);
      i++;
      b++; 
  }
 }

 if(*iy == 1){
  printf("viewing as FLOAT...\n");

  i = 0;
  while(i < *numbytes && ((float) *(c) == 0)){ 
    i++;
    c++;
  }

  while(i < *numbytes && getchar() != 'q'){
    printf("c[%i] = %f\t@ %p\n", i, (float ) *(c), c);
    if(i < *numbytes && ((float) *c != 0)){
      i++;
      c++;
      continue;
    }
    while(i < *numbytes && ((float) *c == 0)){ 
      i++;
      c++;
    }
  }
 }

 if(*iy == 2){
  printf("viewing as INT...\n");

  i = 0;
  while(i < *numbytes && ((int) *d == 0) ){
    i++;
    d++;
  }

  while(getchar() != 'q'){
    printf("d[%i] = %i\t@ %p\n", i, (int ) *d, d);
    if(i < *numbytes && ( (int) *d != 0)){
      i++;
      d++;
      continue;
    }
    while(i < *numbytes && ((int) *(d) == 0)){
      i++;
      d++;
    }
  }
 }

     /**numbytes = old_numbytes;*/
     return(retcode);
}/* end c_view4() */


/*---+----------------------------------------------------------------*/

int findgrib(int *fd)
{
  int found, grib_byte, buf_counter, num;
  char buf[8200], *place_ptr, *t_ptr;

  grib_byte=0;
  found=FALSE;
  buf_counter=0;

  rewind(findfile((int)*fd));

  while (!found)
  {
    fread(buf,sizeof(char),8192,findfile((int)*fd));
    buf[8192]=0;
/*         replace occurrences of null with a blank in buf
           because strstr will stop when it reaches a null
*/
    t_ptr = buf;
    num = 8192;
    while((t_ptr = memchr(t_ptr,0,num)) != NULL)
    {
      *t_ptr=' ';
      num = 8192 - (t_ptr-buf);
    }
    place_ptr = strstr(buf,"GRIB");
    if(place_ptr == NULL)
    {
      buf_counter+=8192;
    }
    else
    {
      found=TRUE;
      grib_byte=(place_ptr - buf) + buf_counter;
    }
  }
  return(grib_byte);
}










/*************************************************************************
 * TIME_DATE.C: Routines to do time/date convertions. FORTRAN CALLABLE
 *		Note. Use Full year, i.e. 1991, not '91. (Unix time routines
 *		often leave off the '19'.) Watch out!
 *		Jan is month 1.
 *
 *	F Hage	Oct, 1992   NCAR/RAP
 *   Converted names for Stupid CRAY Naming conventions - 
 */

/*************************************************************************
 *	CONVERT_TO_UNIX_TIME:  Take the separate time fields 
 * 	and calculate the unix time.  FORTRAN CALLABLE:
 *  CALL CONVERT_TO_UNIX_TIME(YEAR,MONTH,DAY,HOUR,MIN,SEC,UTIME)
 *	Returns the unix time in UTIME 
 */

void cv_to_ut(year,month,day,hour,min,sec,utime)
	int	*year,*month,*day,*hour,*min,*sec,*utime;
{
	long	u_day,jday,days;
	long	u_time;

	u_day = julian_date(1,1,1970);
	jday = julian_date((*day),(*month),(*year));

	days = jday - u_day;

	*utime = (days * 86400) + (*hour * 3600) + (*min * 60) + *sec;
}


/*************************************************************************
 *	CONVERT_FROM_UNIX_TIME:  Take the unix time field 
 * 	and calculate the seperate time fields.  FORTRAN CALLABLE:
 *  CALL CONVERT_FROM_UNIX_TIME(YEAR,MONTH,DAY,HOUR,MIN,SEC,UTIME)
 *	Returns the time in YEAR,MONTH,DAY,HOUR,MIN,SEC 
 */

void cv_fr_ut(year,month,day,hour,min,sec,utime)
	int	*year,*month,*day,*hour,*min,*sec,*utime;
{
	long	u_day,j_day,days;

	u_day = julian_date(1,1,1970);

	j_day = (*utime / 86400);

	calandar_date((u_day + j_day),day,month,year);

	j_day = (*utime % 86400);
	*hour = j_day / 3600;
	*min = (j_day / 60) - (*hour * 60);
	*sec = j_day % 60;
}
 
/*************************************************************************
 *	JULIAN_DATE: Calc the Julian calandar Day Number
 *	As Taken from Computer Language- Dec 1990, pg 58
 */

int julian_date(int *day_ptr,int *month_ptr,int *year_ptr)
{
        int     day,month,year;
	int	a,b;
	double	yr_corr;

        day=*day_ptr;
	month=*month_ptr;
	year=*year_ptr;

	/* correct for negative year */
	yr_corr = (year > 0? 0.0: 0.75);
	if(month <=2) {
		year--;
		month += 12;
	}
	b=0;

	/* Cope with Gregorian Calandar reform */
	if(year * 10000.0 + month * 100.0 + day >= 15821015.0) {
		a = year / 100;
		b = 2 - a + a / 4;
	}
	
	return (int) ((365.25 * year - yr_corr) + (long) (30.6001 * (month +1)) + day + 1720994 + b);
}

/*************************************************************************
 *	CALANDAR_DATE: Calc the calandar Day from the Julian date
 *	As Taken from Computer Language- Dec 1990, pg 58
 *	Sets day,month,year as return values.
 */

calandar_date(jdate,day,month,year)
	int	jdate;
	int	*day,*month,*year;
{
	long	a,b,c,d,e,z,alpha;

	z = jdate +1;

	/* Gregorian reform correction */
	if (z < 2299161) { 
		a = z; 
	} else {
		alpha = (long) ((z - 1867216.25) / 36524.25);
		a = z + 1 + alpha - alpha / 4;
	}

	b = a + 1524;
	c = (long) ((b - 122.1) / 365.25);
	d = (long) (365.25 * c);
	e = (long) ((b - d) / 30.6001);
	*day = (int) b - d - (long) (30.6001 * e);
	*month = (int) (e < 13.5)? e - 1 : e - 13;
	*year = (int) (*month > 2.5)? (c - 4716) : c - 4715;

	 return 0;
}


int c_pause(void){
  printf("Press any key to continue..."); getchar();
  return 0;
}
