#ifndef FZCAL_H
#define FZCAL_H

#define LEN_FZCSTR_MAX 1024

#ifdef __cplusplus
extern "C"
{
#endif
	size_t  __fzc_MOD_fzc_init ( void );
	int  __fzc_MOD_fzc_set_formula ( const size_t, const size_t );
	int  __fzc_MOD_fzc_eval ( const size_t );
	void __fzc_MOD_fzc_get_str_ans ( const size_t, const size_t );
	int __fzc_MOD_fzc_regist_parameter ( const size_t, const size_t, const size_t );
#ifdef __cplusplus
}
#endif

#define fzc_init __fzc_MOD_fzc_init
#define fzc_set_formula __fzc_MOD_fzc_set_formula
#define fzc_eval __fzc_MOD_fzc_eval
#define fzc_regpar __fzc_MOD_fzc_regist_parameter
#define fzc_get_strans __fzc_MOD_fzc_get_str_ans

#endif 
