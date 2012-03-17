!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2011-2012 by Kazuaki Kumagai                            *
! *   elseifkk@users.sf.net                                                 *
! *                                                                         *
! *   This program is free software; you can redistribute it and/or modify  *
! *   it under the terms of the GNU General Public License as published by  *
! *   the Free Software Foundation; either version 2 of the License, or     *
! *   (at your option) any later version.                                   *
! *                                                                         *
! *   This program is distributed in the hope that it will be useful,       *
! *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
! *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
! *   GNU General Public License for more details.                          *
! *                                                                         *
! *   You should have received a copy of the GNU General Public License     *
! *   along with this program; if not, write to the                         *
! *   Free Software Foundation, Inc.,                                       *
! *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
module fzc
  use iso_c_binding, only: C_SIZE_T, C_INT
  use rpnd
  use rpne
  use rpnp
  implicit none

#define size_t integer(C_SIZE_T)
#define int_t integer(C_INT)
#define retint int_t

contains

  size_t function fzc_init()
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    call init_rpne
    prpnc=malloc(sizeof(rpnc))
    rpnc=init_rpnc()
    fzc_init=prpnc
  end function fzc_init

  subroutine fzc_uinit(pfzc)
    size_t,intent(in),value::pfzc
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    prpnc=pfzc
    call uinit_rpnc(rpnc)
    call free(prpnc)
  end subroutine fzc_uinit

  size_t function fzc_cp(ptr_rpnc)
    size_t,intent(in),value::ptr_rpnc
    type(t_rpnc) rpnc,rpnc_in
    pointer(prpnc_in,rpnc_in)
    pointer(prpnc,rpnc)
    prpnc=malloc(sizeof(rpnc))
    prpnc_in=ptr_rpnc
    rpnc=cp_rpnc(rpnc_in)
    fzc_cp=prpnc
  end function fzc_cp

  retint function fzc_set_formula(ptr_rpnc,ptr_formula)
    size_t,intent(in),value::ptr_rpnc,ptr_formula
    character(LEN_FORMULA_MAX) f
    pointer(pf,f)
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    pf=ptr_formula
    prpnc=ptr_rpnc
    fzc_set_formula=int(set_formula(rpnc,f),kind=C_INT)
  end function fzc_set_formula

  retint function fzc_eval(ptr_rpnc)
    size_t,intent(in),value::ptr_rpnc
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    prpnc=ptr_rpnc
    fzc_eval=int(rpn_run(rpnc),kind=C_INT)
  end function fzc_eval

  real(dp) function fzc_get_ans(ptr_rpnc)
    use fpio, only: dp
    size_t,intent(in),value::ptr_rpnc
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    prpnc=ptr_rpnc
    fzc_get_ans=real(rpn_rans(rpnc),kind=dp)
  end function fzc_get_ans

  subroutine fzc_get_str_ans(ptr_rpnc,ptr_str)
    use fpio, only: LEN_STR_ANS_MAX
    size_t,intent(in),value::ptr_rpnc,ptr_str
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    character(LEN_STR_ANS_MAX) str
    pointer(ps,str)
    prpnc=ptr_rpnc
    ps=ptr_str
    str=trim(rpn_sans(rpnc))//char(0)
  end subroutine fzc_get_str_ans

  subroutine fzc_set_opt(ptr_rpnc,opt)
    size_t,intent(in),value::ptr_rpnc
    int_t,intent(in),value::opt
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    prpnc=ptr_rpnc
    rpnc%opt=ior(rpnc%opt,opt)
  end subroutine fzc_set_opt

  subroutine fzc_cle_opt(ptr_rpnc,opt)
    size_t,intent(in),value::ptr_rpnc
    int_t,intent(in),value::opt
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    prpnc=ptr_rpnc
    rpnc%opt=iand(rpnc%opt,not(opt))
  end subroutine fzc_cle_opt

  retint function fzc_regist_parameter(ptr_rpnc,ptr_var,ptr_str,pk)
    use misc, only: c2fstr
    use plist, only: add_par_by_reference
    size_t,intent(in),value::ptr_rpnc,ptr_var,ptr_str
    int_t,intent(in),value::pk
    character(LEN_STR_MAX) name
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    prpnc=ptr_rpnc
    call c2fstr(ptr_str,name)
    fzc_regist_parameter=&
         int(add_par_by_reference(rpnc%pars,trim(adjustl(name)),ptr_var,ro=.true.,pk=int(pk)),kind=C_INT)
  end function fzc_regist_parameter

  subroutine fzc_cle_dat(ptr_rpnc)
    size_t,intent(in),value::ptr_rpnc
    type(t_rpnc)::rpnc
    pointer(prpnc,rpnc)
    prpnc=ptr_rpnc
    call reset_sd(rpnc%sd)
  end subroutine fzc_cle_dat

end module fzc
