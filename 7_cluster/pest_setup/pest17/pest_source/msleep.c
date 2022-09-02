#include <stdio.h>
#ifdef WIN32
#include <time.h>
#include <winsock2.h>
#define msleep_ MSLEEP
#else
#include <sys/time.h>
#include <sys/types.h>
#endif

/* ------------------------------------------------------------------ */
/* ------------------  Millisecond version of sleep  ---------------- */
/* ------------------------------------------------------------------ */

#ifndef WIN32
int msleep_(int *mS)
{
   struct timeval Timer;

   if ((*mS < 0) || (*mS > 3600000)) return -1;
   Timer.tv_sec  = *mS/1000;
   Timer.tv_usec = (*mS%1000)*1000;
   if (select(0,NULL,NULL,NULL,&Timer) < 0) return -1;
   return 0;
}

#else

void msleep_w(int *mS)
{
   struct timeval Timer;

   if ((*mS < 0) || (*mS > 3600000)) return;
   Timer.tv_sec  = *mS/1000;
   Timer.tv_usec = (*mS%1000)*1000;
   if (select(0,NULL,NULL,NULL,&Timer) < 0) return;
   return;
}


void msleep_(int *mS)
{
   struct timeval Timer;

   if ((*mS < 0) || (*mS > 3600000)) return;
   Timer.tv_sec  = *mS/1000;
   Timer.tv_usec = (*mS%1000)*1000;
   if (select(0,NULL,NULL,NULL,&Timer) < 0) return;
   return;
}

#endif
