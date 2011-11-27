module fzc
  use iso_c_binding, only: C_SIZE_T, C_INT
  use rpn
  implicit none

#define size_t integer(C_SIZE_T)
#define int_t integer(C_INT)
#define retint int_t

contains

  size_t function fzc_init()
    fzc_init=init_rpnc()
  end function fzc_init

  subroutine fzc_uinit(pfzc)
    size_t,intent(in),value::pfzc
    type(t_rpnc) rpnc
    pointer(prpnc,rpnc)
    prpnc=pfzc
    call uinit_rpnc(rpnc)
  end subroutine fzc_uinit

  size_t function fzc_cp(ptr_rpnc)
    size_t,intent(in),value::ptr_rpnc
    type(t_rpnc) rpnc_in
    pointer(pr,rpnc_in)
    pr=ptr_rpnc
    fzc_cp=cp_rpnc(rpnc_in)
  end function fzc_cp

  retint function fzc_set_formula(ptr_rpnc,ptr_formula)
    size_t,intent(in),value::ptr_rpnc,ptr_formula
    character(LEN_FORMULA_MAX) f
    pointer(pf,f)
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    pf=ptr_formula
    pr=ptr_rpnc
    fzc_set_formula=int(parse_formula(rpnc,f),kind=C_INT)
  end function fzc_set_formula

  retint function fzc_eval(ptr_rpnc)
    size_t,intent(in),value::ptr_rpnc
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    p=ptr_rpnc
    fzc_eval=int(eval(rpnc),kind=C_INT)
  end function fzc_eval

  subroutine fzc_get_str_ans(ptr_rpnc,ptr_str)
    size_t,intent(in),value::ptr_rpnc,ptr_str
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    character(LEN_STR_ANS_MAX) str
    pointer(ps,str)
    pr=ptr_rpnc
    ps=ptr_str
    str=trim(ztoa(rpn_ans(rpnc)))//achar(0)
  end subroutine fzc_get_str_ans

  subroutine fzc_set_opt(ptr_rpnc,opt)
    size_t,intent(in),value::ptr_rpnc
    int_t,intent(in),value::opt
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    p=ptr_rpnc
    rpnc%opt=ior(rpnc%opt,opt)
  end subroutine fzc_set_opt

  subroutine fzc_cle_opt(ptr_rpnc,opt)
    size_t,intent(in),value::ptr_rpnc
    int_t,intent(in),value::opt
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    p=ptr_rpnc
    rpnc%opt=iand(rpnc%opt,not(opt))
  end subroutine fzc_cle_opt

  subroutine c2fstr(pstr,s)
    size_t,intent(in),value::pstr
    character*(*),intent(out)::s
    integer i
    character*1 c
    pointer(p,c)
    s=""
    p=pstr-1
    do i=1,len(s)
       p=p+1
       if(c==achar(0)) exit
       s(i:i)=c
    end do
  end subroutine c2fstr

  retint function fzc_regist_parameter(ptr_rpnc,ptr_var,ptr_str,pk)
    size_t,intent(in),value::ptr_rpnc,ptr_var,ptr_str
    int_t,intent(in),value::pk
    character(LEN_STR_MAX) name
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    pr=ptr_rpnc
    call c2fstr(ptr_str,name)
    fzc_regist_parameter=&
         int(add_par_by_reference(rpnc%pars,name,ptr_var,ro=.true.,pk=pk),kind=C_INT)
  end function fzc_regist_parameter

end module fzc
