#include <stdlib.h>
#include <string.h>
#include <stdio.h>

extern int   __rpn_MOD_init_rpnc(void);
extern int   __rpn_MOD_rpn_set_formula(int,int);
extern int   __rpn_MOD_rpn_eval(int);
extern void  __rpn_MOD_rpn_get_str_ans(int,int);
extern void  __rpn_MOD_rpn_dump_rpnc(int);

int main(void)
{
  int const lenf_max=1024;
  char f[lenf_max];
  int i;
  int p = __rpn_MOD_init_rpnc();
  int pf=(int)&f[0];
  do{
    printf("> ");
    if(fgets(f,lenf_max,stdin)==NULL) continue;
    i=strlen(f);
    if(i<=1) continue;
    if(i!=lenf_max-1) f[i-1]='\0';
    if(strcmp(f,"q")==0) break;
    i=__rpn_MOD_rpn_set_formula(pf,p);
    if(i!=0) continue;
    i=__rpn_MOD_rpn_eval(p);
    if(i!=0) continue;
    __rpn_MOD_rpn_get_str_ans(p,pf);
    printf("%s\n",f);
  } while(1);
  return 0;
}
