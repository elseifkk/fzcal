#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "fzc.h"

int main(void)
{
  int const lenf_max=1024;
  char f[lenf_max];
  size_t i;
  size_t p = fzc_init();
  size_t pf=(size_t)&f[0];

  do{
    printf("> ");
    if(fgets(f,lenf_max,stdin)==NULL) continue;
    i=strlen(f);
    if(i<=1) continue;
    if(i!=lenf_max-1) f[i-1]='\0';
    if(strcmp(f,"q")==0) break;
    i=fzc_set_formula(pf,p);
    printf("rc1=%d\n",i);
    if(i!=0) continue;
    i=fzc_eval(p);
    printf("rc2=%d\n",i);
    if(i!=0) continue;
    fzc_get_strans(p,pf);
    printf("%s\n",f);
  } while(1);
  return 0;
}
