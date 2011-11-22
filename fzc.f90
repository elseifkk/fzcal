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

  retint function fzc_set_formula(ptr_rpnc,ptr_formula)
    integer,intent(in),value::ptr_rpnc,ptr_formula
    character(LEN_FORMULA_MAX) f
    pointer(pf,f)
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    pf=ptr_formula
    pr=ptr_rpnc
    fzc_set_formula=int(parse_formula(rpnc,f),kind=C_INT)
  end function fzc_set_formula

  retint function fzc_eval(ptr_rpnc)
    integer,intent(in),value::ptr_rpnc
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    p=ptr_rpnc
    fzc_eval=int(eval(rpnc),kind=C_INT)
  end function fzc_eval

  subroutine fzc_get_str_ans(ptr_rpnc,ptr_str)
    integer,intent(in),value::ptr_rpnc,ptr_str
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    character(LEN_STR_ANS_MAX) str
    pointer(ps,str)
    pr=ptr_rpnc
    ps=ptr_str
    str=trim(ztoa(rpn_ans(rpnc)))//achar(0)
  end subroutine fzc_get_str_ans

  subroutine c2fstr(pstr,s)
    integer,intent(in)::pstr
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

  retint function fzc_regist_parameter(ptr_rpnc,ptr_var,ptr_str,dble)
    integer,intent(in),value::ptr_rpnc,ptr_var,ptr_str,dble
    character(LEN_STR_MAX) name
    logical d
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    pr=ptr_rpnc
    call c2fstr(ptr_str,name)
    d=(dble/=0)
    fzc_regist_parameter=&
         int(add_par_by_reference(rpnc%pars,name,ptr_var,ro=.true.,dble=d,real=.true.),kind=C_INT)
  end function fzc_regist_parameter

end module fzc