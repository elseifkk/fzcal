/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Copyright (C) 2011-2014 by Kazuaki Kumagai                            *
 *   elseifkk@users.sf.net                                                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#ifndef FZCAL_H
#define FZCAL_H

#define LEN_FZCSTR_MAX 1024

#define FZCPK_UNDEF 0
#define FZCPK_COMP  1
#define FZCPK_REAL  2
#define FZCPK_DBLE  3
#define FZCPK_INT   4

#define FZCOPT_DAT             0x0020
#define FZCOPT_STA             0x0040
#define FZCOPT_NO_AUTO_ADD_PAR 0x0800
#define FZCOPT_NO_WARN         0x1000
#define FZCOPT_NO_STDIN        0x2000
#define FZCOPT_NO_STDOUT       0x4000

#define FZCSTA_MACSET -2

#define FZCCID_NOP     0
#define FZCCID_INV  -999
#define FZCCID_DONE  999

#ifdef __cplusplus
extern "C"
{
#endif
  size_t  __fzc_MOD_fzc_init ( void );
  size_t  __fzc_MOD_fzc_cp ( const size_t );
  void    __fzc_MOD_fzc_uinit ( const size_t );
  void    __fzc_MOD_fzc_set_mode ( const size_t, const int );
  void    __fzc_MOD_fzc_cle_mode ( const size_t, const int );
  int     __fzc_MOD_fzc_set_formula ( const size_t, const size_t );
  int     __fzc_MOD_fzc_parse_formula ( const size_t, const size_t );
  int     __fzc_MOD_fzc_setparse_formula ( const size_t, const size_t );
  int     __fzc_MOD_fzc_eval ( const size_t );
  void    __fzc_MOD_fzc_get_str_ans ( const size_t, const size_t );
  void    __fzc_MOD_fzc_get_str_err ( const int, const size_t );
  int     __fzc_MOD_fzc_regist_parameter ( const size_t, const size_t, const size_t, const size_t );
  double  __fzc_MOD_fzc_get_ans(const size_t );
  void    __fzc_MOD_fzc_cle_dat ( const size_t );
#ifdef __cplusplus
}
#endif

#define fzc_init             __fzc_MOD_fzc_init
#define fzc_uinit            __fzc_MOD_fzc_uinit
#define fzc_cp               __fzc_MOD_fzc_cp
#define fzc_set_mode         __fzc_MOD_fzc_set_mode
#define fzc_cle_mode         __fzc_MOD_fzc_cle_mode
#define fzc_set_formula      __fzc_MOD_fzc_set_formula
#define fzc_parse_formula    __fzc_MOD_fzc_parse_formula
#define fzc_setparse_formula __fzc_MOD_fzc_setparse_formula
#define fzc_eval             __fzc_MOD_fzc_eval
#define fzc_regpar           __fzc_MOD_fzc_regist_parameter
#define fzc_get_strans       __fzc_MOD_fzc_get_str_ans
#define fzc_get_strerr       __fzc_MOD_fzc_get_str_err
#define fzc_get_ans          __fzc_MOD_fzc_get_ans
#define fzc_cle_dat          __fzc_MOD_fzc_cle_dat

#endif 
