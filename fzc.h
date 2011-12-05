#ifndef FZCAL_H
#define FZCAL_H

#define LEN_FZCSTR_MAX 1024
#define PK_REAL 1
#define PK_DBLE 2
#define PK_COMP 0

#define FZCOPT_NOAUTO_ADDPAR 8

#ifdef __cplusplus
extern "C"
{
#endif
  size_t  __fzc_MOD_fzc_init ( void );
  size_t  __fzc_MOD_fzc_cp ( const size_t );
  void    __fzc_MOD_fzc_uinit ( const size_t );
  void    __fzc_MOD_fzc_set_opt ( const size_t, const int );
  void    __fzc_MOD_fzc_cle_opt ( const size_t, const int );
  int     __fzc_MOD_fzc_set_formula ( const size_t, const size_t );
  int     __fzc_MOD_fzc_eval ( const size_t );
  void    __fzc_MOD_fzc_get_str_ans ( const size_t, const size_t );
  int     __fzc_MOD_fzc_regist_parameter ( const size_t, const size_t, const size_t, const size_t );
  double  __fzc_MOD_fzc_get_ans(const size_t );
#ifdef __cplusplus
}
#endif

#define fzc_init        __fzc_MOD_fzc_init
#define fzc_uinit       __fzc_MOD_fzc_uinit
#define fzc_cp          __fzc_MOD_fzc_cp
#define fzc_set_opt     __fzc_MOD_fzc_set_opt
#define fzc_cle_opt     __fzc_MOD_fzc_cle_opt
#define fzc_set_formula __fzc_MOD_fzc_set_formula
#define fzc_eval        __fzc_MOD_fzc_eval
#define fzc_regpar      __fzc_MOD_fzc_regist_parameter
#define fzc_get_strans  __fzc_MOD_fzc_get_str_ans
#define fzc_get_ans     __fzc_MOD_fzc_get_ans

#endif 
