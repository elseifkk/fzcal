#include <stdlib.h>
#include <string.h>
#include <stdio.h>

extern size_t   __rpn_MOD_init_rpnc(void);
extern size_t   __rpn_MOD_rpn_set_formula(const size_t, const size_t);
extern size_t   __rpn_MOD_rpn_eval(const size_t);
extern void  __rpn_MOD_rpn_get_str_ans(const size_t, const size_t);
extern void  __rpn_MOD_rpn_dump_rpnc(const size_t);

int main(void)
{
  int const lenf_max=1024;
  char f[lenf_max];
  size_t i;
  size_t p = __rpn_MOD_init_rpnc();
  size_t pf=(size_t)&f[0];

  do{
    printf("> ");
    if(fgets(f,lenf_max,stdin)==NULL) continue;
    i=strlen(f);
    if(i<=1) continue;
    if(i!=lenf_max-1) f[i-1]='\0';
    if(strcmp(f,"q")==0) break;
    i=__rpn_MOD_rpn_set_formula(pf,p);
    printf("rc1=%d\n",i);
    if(i!=0) continue;
    i=__rpn_MOD_rpn_eval(p);
    printf("rc2=%d\n",i);
    if(i!=0) continue;
    __rpn_MOD_rpn_get_str_ans(p,pf);
    printf("%s\n",f);
  } while(1);
  return 0;
}
