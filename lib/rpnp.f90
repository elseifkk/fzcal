!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2011-2014 by Kazuaki Kumagai                            *
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
module rpnp
  use rpnd
  implicit none

  private

  public set_formula
  public parse_formula
  public init_rpnp

  character*(*),parameter::LOPS_NOT ="not"
  character*(*),parameter::LOPS_AND ="and"
  character*(*),parameter::LOPS_OR  ="or"
  character*(*),parameter::LOPS_EQ  ="eq"
  character*(*),parameter::LOPS_NEQ ="neq"

  character(*),parameter::spars=&
       char(1)//"n"//&
       char(3)//"ave"//&
       char(3)//"var"//&
       char(3)//"sum"//&
       char(4)//"sum2"//&
       char(4)//"uvar"//&
       char(5)//"ave_y"//&
       char(5)//"var_y"//&
       char(5)//"sum_y"//&
       char(6)//"sum2_y"//&
       char(6)//"uvar_y"//&
       char(6)//"sum_xy"//&
       char(1)//"a"//&
       char(1)//"b"//&
       char(0)
  integer,parameter::SID_N       =  1
  integer,parameter::SID_AVE     =  2
  integer,parameter::SID_VAR     =  3
  integer,parameter::SID_SUM     =  4
  integer,parameter::SID_SUM2    =  5
  integer,parameter::SID_UVAR    =  6
  integer,parameter::SID_AVE_Y   =  7
  integer,parameter::SID_VAR_Y   =  8
  integer,parameter::SID_SUM_Y   =  9
  integer,parameter::SID_SUM2_Y  = 10
  integer,parameter::SID_UVAR_Y  = 11
  integer,parameter::SID_SUM_XY  = 12
  integer,parameter::SID_A       = 13
  integer,parameter::SID_B       = 14

  integer,parameter::int_fncs_max_len=5
  character*(*),parameter::ffncs3= & ! function of function with 3 args
       char(6)//"deint_"//&
       char(6)//"reint_"//&
       char(6)//"siint_"//&
       char(0)
  character*(*),parameter::int_fncs=&
       char(3)//"ran"//&
       char(3)//"sin"//&
       char(3)//"cos"//&
       char(3)//"tan"//&
       char(4)//"sinh"//&
       char(4)//"cosh"//&
       char(4)//"tanh"//&
       char(4)//"asin"//&
       char(4)//"acos"//&
       char(4)//"atan"//&
       char(5)//"asinh"//&
       char(5)//"acosh"//&
       char(5)//"atanh"//&
       char(3)//"exp"//&
       char(3)//"log"//&
       char(2)//"ln"//&
       char(4)//"sqrt"//&
       char(4)//"cbrt"//&
       char(3)//"abs"//&
       char(3)//"int"//&
       char(4)//"frac"//&
       char(5)//"conjg"//&
       char(4)//"nint"//&
       char(2)//"re"//&
       char(2)//"im"//&
       char(3)//"mag"//&
       char(3)//"arg"//&
       char(5)//"gamma"//&
       char(6)//"lgamma"//&
       char(3)//"psy"//&
       char(3)//"mod"//&
       char(4)//"gami"//&
       char(2)//"jn"//&
       char(2)//"yn"//&
       char(2)//"hn"//&
       char(3)//"djn"//&
       char(3)//"dyn"//&
       char(3)//"dhn"//&
       char(2)//"Fl"//&
       char(2)//"Gl"//&
       char(2)//"Hl"//&
       char(3)//"dFl"//&
       char(3)//"dGl"//&
       char(3)//"dHl"//&
       char(3)//"min"//&
       char(3)//"max"//&
       char(3)//"sum"//&
       char(3)//"ave"//&
       char(3)//"var"//&
       char(4)//"uvar"//&
       char(4)//"sum2"//&
       char(0)
  integer,parameter::FID_RAN        =  1
  integer,parameter::FID_ARG0_END   =  1 !<<<<<<<<
  integer,parameter::FID_SIN        =  2
  integer,parameter::FID_COS        =  3
  integer,parameter::FID_TAN        =  4
  integer,parameter::FID_SINH       =  5
  integer,parameter::FID_COSH       =  6
  integer,parameter::FID_TANH       =  7
  integer,parameter::FID_ASIN       =  8
  integer,parameter::FID_ACOS       =  9
  integer,parameter::FID_ATAN       = 10
  integer,parameter::FID_ASINH      = 11
  integer,parameter::FID_ACOSH      = 12
  integer,parameter::FID_ATANH      = 13
  integer,parameter::FID_EXP        = 14
  integer,parameter::FID_LOG        = 15
  integer,parameter::FID_LN         = 16
  integer,parameter::FID_SQRT       = 17
  integer,parameter::FID_CBRT       = 18
  integer,parameter::FID_ABS        = 19
  integer,parameter::FID_INT        = 20
  integer,parameter::FID_FRAC       = 21
  integer,parameter::FID_CONJG      = 22
  integer,parameter::FID_NINT       = 23
  integer,parameter::FID_RE         = 24
  integer,parameter::FID_IM         = 25
  integer,parameter::FID_MAG        = 26
  integer,parameter::FID_ARG        = 27
  integer,parameter::FID_GAMMA      = 28
  integer,parameter::FID_LGAMMA     = 29
  integer,parameter::FID_PSY        = 30
  integer,parameter::FID_ARG1_END   = 30 !<<<<<<<<
  integer,parameter::FID_MOD        = 31
  integer,parameter::FID_GAMI       = 32
  integer,parameter::FID_JN         = 33 
  integer,parameter::FID_YN         = 34
  integer,parameter::FID_HN         = 35
  integer,parameter::FID_DJN        = 36
  integer,parameter::FID_DYN        = 37
  integer,parameter::FID_DHN        = 38
  integer,parameter::FID_ARG2_END   = 38 !<<<<<<<<
  integer,parameter::FID_FL         = 39
  integer,parameter::FID_GL         = 40
  integer,parameter::FID_HL         = 41
  integer,parameter::FID_DFL        = 42
  integer,parameter::FID_DGL        = 43
  integer,parameter::FID_DHL        = 44
  integer,parameter::FID_ARG3_END   = 44 !<<<<<<<<
  integer,parameter::FID_MIN        = 45
  integer,parameter::FID_MAX        = 46
  integer,parameter::FID_SUM        = 47
  integer,parameter::FID_AVE        = 48
  integer,parameter::FID_VAR        = 49
  integer,parameter::FID_UVAR       = 50
  integer,parameter::FID_SUM2       = 51
  integer,parameter::FID_END        = 51 !<<<<<<<<
  integer,parameter::FFID_ARG1_END  = 51 !<<<<<<<<
  integer,parameter::FFID_ARG2_END  = 51 !<<<<<<<<
  integer,parameter::FFID_DEINT     = 1 +FID_END
  integer,parameter::FFID_REINT     = 2 +FID_END
  integer,parameter::FFID_SIINT     = 3 +FID_END
  integer,parameter::FFID_ARG3_END  = 3 +FID_END

  integer,parameter::AID_PAR=0
  integer,parameter::AID_MAC=1
  integer,parameter::AID_FNC=2

  integer ptr_input

  interface get_end_of_par
     module procedure get_end_of_par_b
     module procedure get_end_of_par_s
  end interface

  interface rpn_put
     module procedure rpn_put_e
     module procedure rpn_put_q
  end interface rpn_put

  interface rpn_push
     module procedure rpn_push_e
     module procedure rpn_push_q
  end interface rpn_push

  interface rpn_try_push
     module procedure rpn_try_push_e
     module procedure rpn_try_push_q
  end interface rpn_try_push

contains

  subroutine init_rpnp(pinput)
    integer,intent(in)::pinput
    ptr_input=pinput
  end subroutine init_rpnp

  integer function get_end_of_fig(rpnb,k_)
    use misc, only: is_numeric_i,is_bin_number,is_oct_number,is_hex_number,&
         is_set,is_not_set
    type(t_rpnb),intent(in)::rpnb
    integer,intent(in)::k_
    integer c,a,k
    interface
       logical function is_valid(a)
         integer,intent(in)::a
       end function is_valid
    end interface
    pointer(pf,is_valid)

    k=k_
    if(rpnb%expr(k:k)==".") then
       c=1
    else
       c=0
    end if

    if(is_not_set(rpnb%flg%mode,RCM_INM)) then
       pf=loc(is_numeric_i)
    else if(is_set(rpnb%flg%mode,RCM_IBIN)) then
       pf=loc(is_bin_number)
    else if(is_set(rpnb%flg%mode,RCM_IOCT)) then
       pf=loc(is_oct_number)
    else if(is_set(rpnb%flg%mode,RCM_IHEX)) then
       pf=loc(is_hex_number)
    end if

    do
       if(k>=rpnb%len_expr) then
          get_end_of_fig=k
          return
       end if
       k=k+1
       a=ichar(rpnb%expr(k:k))
       if(.not.is_valid(a)) then
          if(c==1.and.k-1==k_) then
             get_end_of_fig=-k_ ! only .
          else
             get_end_of_fig=k-1
          end if
          return
       else if(rpnb%expr(k:k)==".") then
          c=c+1
          if(c>1) then
             get_end_of_fig=-k ! second . found
             return
          end if
       end if
    end do

  end function get_end_of_fig

  integer function get_end_of_par_b(rpnb,k_,force_alpha)
    type(t_rpnb),intent(in)::rpnb
    integer,intent(in)::k_
    logical,intent(in),optional::force_alpha
    get_end_of_par_b=get_end_of_par_s(rpnb%len_expr,rpnb%expr,k_,force_alpha)
  end function get_end_of_par_b

  integer function get_end_of_par_s(lens,s,k_,force_alpha)
    use misc, only: is_alpha,is_number,is_symbol
    integer,intent(in)::lens
    character*(*),intent(in)::s
    integer,intent(in)::k_
    logical,intent(in),optional::force_alpha
    integer a,k
    logical alpha
    k=k_ ! it must be alpha
    alpha=present(force_alpha).and.force_alpha
    do
       if(k>=lens) then
          get_end_of_par_s=k
          return
       end if
       k=k+1
       a=ichar(s(k:k))
       if(.not.is_alpha(a)) then
          if(alpha.or.(.not.is_number(a).and..not.is_symbol(a))) then
             get_end_of_par_s=k-1
             return
          end if
       end if
    end do
  end function get_end_of_par_s

  logical function is_lop(s,t)
    character*(*),intent(in)::s
    integer,intent(inout)::t
    select case(s)
    case(LOPS_not)
       t=TID_LOP1
       is_lop=.true.
    case(LOPS_and)
       t=TID_LOP2
       is_lop=.true.
    case(LOPS_or)
       t=TID_LOP3
       is_lop=.true.
    case(LOPS_eq,LOPS_neq)
       t=TID_LOP4
       is_lop=.true.
    case default
       is_lop=.false.
    end select
  end function is_lop

  logical function is_ppar(a,ent)
    use fpio, only: si_prefix
    character,intent(in)::a
    integer,intent(out),optional::ent
    integer k
    do k=1,len(si_prefix)
       if(a==si_prefix(k:k)) then
          is_ppar=.true.
          if(present(ent)) ent=k
          return
       end if
    end do
    is_ppar=.false.
  end function is_ppar

  logical function is_spar(a,ent)
    character*(*),intent(in)::a
    integer,intent(out),optional::ent
    integer k
    k=get_index(spars,a)
    if(k/=0) then
       if(present(ent)) ent=k
       is_spar=.true.
    else
       is_spar=.false.
    end if
  end function is_spar

  logical function is_int_fnc(a,ent)
    character*(*),intent(in)::a
    integer,intent(out),optional::ent
    integer k
    k=get_index(int_fncs,a)
    if(k==0) then
       k=get_index(ffncs3,a,.true.)
       if(k/=0) k=k+FID_END
    end if
    if(k/=0) then
       if(present(ent)) ent=k
       is_int_fnc=.true.
    else
       is_int_fnc=.false.
    end if
  end function is_int_fnc

  integer function get_index(sl,a,fp)
    character*(*),intent(in)::sl,a
    logical,intent(in),optional::fp
    integer len,lenf
    integer i,k,j
    logical part
    part=(present(fp).and.fp)
    len=len_trim(a)
    k=1
    i=0
    get_index=0
    do
       i=i+1
       lenf=ichar(sl(k:k))
       if(lenf==0) exit
       if(lenf==len.or.(part.and.lenf<=len)) then
          j=k+1
          if(part.and.index(a(1:len),sl(j:j+lenf-1))==1 &
               .or.sl(j:j+len-1)==a(1:len)) then
             get_index=i
             return
          end if
       end if
       k=k+lenf+1
    end do
  end function get_index

  integer function get_tid(a)
    character*1,intent(in)::a
    get_tid=0 ! in order to avoid warning
    select case(a)
    case("+","-")
       get_tid=TID_BOP1U
    case("*","/","&")
       get_tid=TID_BOP2U
    case("!")
       get_tid=TID_UOP2U
    case("^")
       get_tid=TID_BOP3U
    case("(")
       get_tid=TID_BRA
    case(")")
       get_tid=TID_KET
    case("=")
       get_tid=TID_ASNU
    case(STID_DQ1)
       get_tid=TID_DQ1
    case(STID_DQ2)
       get_tid=TID_DQ2
    case("""")
       get_tid=TID_QTN
    case(",")
       get_tid=TID_COMA
    case("?")
       get_tid=TID_TOP1
    case(":")
       get_tid=TID_COL
    case(";")
       get_tid=TID_SCL
    case(" ","\t")
       get_tid=TID_BLK
    case("}")
       get_tid=TID_HKET
    case("~")
       get_tid=TID_LOP1U
    case("<",">")
       get_tid=TID_ROP
    case("_")
       get_tid=TID_USCR
    case("@")
       get_tid=TID_AT
    case("#")
       get_tid=TID_SHRP
    case(".")
       get_tid=TID_COM
    case default
       get_tid=TID_UNDEF
    end select
  end function get_tid

  logical function is_usr_fnc(rl,f,ent)
    use rpnlist, only: find_rpnlist
    type(t_rpnlist),intent(in)::rl
    character*(*),intent(in)::f
    integer,intent(out),optional::ent
    integer k
    k=find_rpnlist(rl,f,SC_FNC)
    if(present(ent)) ent=k
    if(k/=0) then
       is_usr_fnc=.true.
    else
       is_usr_fnc=.false.
    end if
  end function is_usr_fnc

  recursive integer function get_next(rpnb,p1,p2,rl) result(rettid)
    use slist, only: t_slist
    use misc, only: get_i32,is_alpha,is_numeric,is_number,&
         is_set
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(out)::p1,p2
    type(t_rpnlist),intent(in)::rl
    integer k,t,kf

    k=rpnb%cur_pos
    if(k>=rpnb%len_expr.or.k<0) then
       rettid=TID_FIN
       return
    end if

    k=k+1
    p1=k
    p2=k

    t=get_tid(cur_char())
    if(t==TID_UNDEF) then
       if(is_alpha(cur_char())) then
          t=TID_PARU
       else if(is_numeric(cur_char())) then
          t=TID_FIG
       end if
    end if

    select case(t)
    case(TID_UOP2U)
       select case(rpnb%old_tid)
       case(TID_UNDEF,TID_SCL,TID_COM,TID_DQ1)
          t=TID_COM
          call end_com
       case default
          if(next_char(1)=="!") p2=p2+1
          t=TID_UOP2
       end select
    case(TID_BOP1U)
       if(k<rpnb%len_expr-1.and.next_char(1)=="=") then
          t=TID_AOP
          p2=k+1
       else if(k<=rpnb%len_expr-1.and.next_char(1)==rpnb%expr(p1:p1)) then
          t=TID_UOP3
          p2=k+1
       end if
       if(t==TID_BOP1U) then
          select case(rpnb%old_tid)
          case(TID_BRA,TID_BOP3,TID_ASN,TID_AOP, & !TID_BLK, &
               TID_COMA,TID_TOP1,TID_COL,TID_SCL,TID_UNDEF,TID_DQ1) ! plus in (+, ^+ and e+ are unary
             t=TID_UOP1
          case(TID_UOP1,TID_BOP2,TID_BOP1)
             t=TID_INV
          case default
             t=TID_BOP1
          end select
       end if
    case(TID_BOP2U)
       if(k<rpnb%len_expr-1) then
          select case(next_char(1))
          case("=")
             t=TID_AOP
             p2=k+1
          case("*")
             if(cur_char()=="*") then
                t=TID_BOP3
                p2=k+1
             end if
          case("/")
             if(cur_char()=="/") then
                t=TID_BOP3
                p2=k+1
             end if
          end select
       end if
       if(t==TID_BOP2U) then
          if(cur_char()=="&") then
             select case(next_char(1))
             case("P","C")
                t=TID_BOP2
                p2=k+1
                p1=p2
             case default
                t=TID_INV
             end select
          else
             t=TID_BOP2
          end if
       end if
    case(TID_BOP3U)
       if(k<rpnb%len_expr-1.and.next_char(1)=="=") then
          t=TID_AOP
          p2=k+1
       else
          t=TID_BOP3
       end if
    case(TID_USCR)
       t=TID_INV ! no par start with _
       if(p1<rpnb%len_expr) then
          p2=get_end_of_par(rpnb,p1+1,force_alpha=.true.)
          if(p1+1==p2.and.is_ppar(rpnb%expr(p2:p2),k)) then
             t=get_i32(TID_PAR,k)
          end if
       end if
    case(TID_AT)
       if(is_alpha(next_char(1))) then
          p2=get_end_of_par(rpnb,p1+1)
          t=get_i32(TID_PAR,PID_INPUT)
       else if(next_char(1)=="?") then
          call insert_str
          t=get_next(rpnb,p1,p2,rl)
       else
          t=TID_INV
       end if
    case(TID_PARU)
       if(rpnb%old_tid==TID_FIG.and.cur_char()=="e") then
          if(k<rpnb%len_expr) then
             select case(next_char(1))
             case("+","-")
                if(k+1<rpnb%len_expr) then
                   if(is_number(ichar(next_char(2)))) t=TID_BOP3
                end if
             case default
                if(is_number(ichar(next_char(1)))) t=TID_BOP3
             end select
          end if
       end if
       if(t==TID_PARU) then
          p2=get_end_of_par(rpnb,p1)
          if(p2<rpnb%len_expr) then
             if(.not.is_lop(rpnb%expr(p1:p2),t) &
                  .and..not.(is_set(rpnb%flg%mode,RCM_STA).and.is_spar(rpnb%expr(p1:p2)))) then
                k=p2+1
                if(cur_char()=="(") then
                   if(is_usr_fnc(rl,rpnb%expr(p1:p2),kf)) then
                      t=get_i32(TID_UFNC,kf)
                   else if(is_int_fnc(rpnb%expr(p1:p2),kf)) then
                      t=get_i32(TID_IFNC,kf)
                   end if
                end if
             end if
          end if
          if(t==TID_PARU) t=TID_PAR
       end if
    case(TID_FIG)
       p2=get_end_of_fig(rpnb,k)
       if(p2<0) then
          t=TID_INV
          p2=-p2
       end if
    case(TID_ROP)
       if(k<rpnb%len_expr-1.and.next_char(1)=="=") then
          p2=k+1 ! <=, >=
       end if
    case(TID_LOP1U)
       if(k<rpnb%len_expr-1.and.next_char(1)=="=") then
          p2=k+1
          t=TID_ROP ! ~=
       else
          t=TID_LOP1
       end if
    case(TID_ASNU)
       if(k<rpnb%len_expr-1.and.next_char(1)=="=") then
          p2=k+1
          t=TID_ROP ! ==
       else
          t=TID_ASN
       end if
    case(TID_SCL)
       if(p1==1) then
          t=TID_IGNORE
       else
          call skip_tid(t)
! 1; -> 1 FIN prints 1
! 1; -> 1 SCL FIN does not print 1
!          if(p2==rpnb%len_expr) t=TID_FIN
       end if
    case(TID_COM)
       if(p1==rpnb%len_expr) then
          t=TID_INV
       else
          select case(rpnb%old_tid)
             ! prev TID_COM eats TID_SCL
          case(TID_BLK,TID_UNDEF,TID_COM,TID_DQ1,TID_SCL)
             call end_com
          case default
             t=TID_INV
          end select
       end if
    case(TID_BLK)
       call skip_tid(TID_BLK)
    end select

    rpnb%old_tid=t
    rettid=t
    rpnb%cur_pos=p2

  contains

    subroutine end_com()
      integer ii
      logical sq,dq
      sq=.false.
      dq=.false.
      do ii=p1+1,rpnb%len_expr
         select case(rpnb%expr(ii:ii))
         case(STID_SQ1)
            sq=.true.
         case(STID_SQ2)
            sq=.false.
         case(STID_DQ1)
            dq=.true.
         case(STID_DQ2)
            if(.not.dq) then
               ! COM in a macro def
               p2=ii-1
               return
            end if
            dq=.false.
         case(";")
            if(.not.dq.and..not.sq) then
               p2=ii-1
               return
            end if
         end select
      end do
      p2=rpnb%len_expr
    end subroutine end_com

    subroutine insert_str
      use misc, only: ins,mess,messp
      character(len=LEN_FORMULA_MAX) str
      integer ls,istat
      call mess("Input pending for:")
      call mess(rpnb%expr(1:rpnb%len_expr))
      call mess(repeat(" ",k-1)//"^^")
      call messp("=> ")
      call ins(str,i=istat)
      if(istat==0) then
         str=adjustl(str)
         ls=len_trim(str)
         if(k>1) then
            str=rpnb%expr(1:k-1)//str(1:ls)
            ls=ls+k-1
         end if
         if(k+2<=rpnb%len_expr) then
            str=str(1:ls)//rpnb%expr(k+2:rpnb%len_expr)
            ls=ls+rpnb%len_expr-(k+2)+1
         end if
         rpnb%expr=str(1:ls)
         rpnb%len_expr=ls
      else
         rpnb%expr(k:k+1)=""
      end if
    end subroutine insert_str

    subroutine skip_tid(tid)
      integer,intent(in)::tid
      integer i
      do i=p2+1,rpnb%len_expr
         if(get_tid(rpnb%expr(i:i))==tid) then
            p2=i
         else
            exit
         end if
      end do
    end subroutine skip_tid

    character*1 function cur_char()
      cur_char=next_char(0)
    end function cur_char

    character*1 function next_char(inc)
      integer,intent(in)::inc
      integer kk
      kk=k+inc
      if(kk<=rpnb%len_expr) then
         next_char=rpnb%expr(kk:kk)
      else
         next_char=char(0)
      end if
    end function next_char

  end function get_next

  pure function supexpr(rpnb,i)
    use misc, only: get_up32
    type(t_rpnb),intent(in)::rpnb
    integer,intent(in)::i
    character(len=get_up32(rpnb%que(i)%p2)-get_up32(rpnb%que(i)%p1)+1) supexpr
    supexpr=rpnb%expr(get_up32(rpnb%que(i)%p1):get_up32(rpnb%que(i)%p2))
  end function supexpr

  pure function subexpr(rpnb,i)
    use misc, only: get_lo32
    type(t_rpnb),intent(in)::rpnb
    integer,intent(in)::i
    character(len=get_lo32(rpnb%que(i)%p2)-get_lo32(rpnb%que(i)%p1)+1) subexpr
    subexpr=rpnb%expr(get_lo32(rpnb%que(i)%p1):get_lo32(rpnb%que(i)%p2))
  end function subexpr

  integer function set_function(rpnb,rpnc,k1)
    use rpnlist, only: t_rpnm,add_rpnm_entry
    use misc, only: set_flg
    type(t_rpnb),intent(in),target::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::k1
    type(t_rpnm),pointer::rpnm
    integer i
    integer kf,km,ka,ke,kc
    integer ac,tc
    ke=find_end()
    do i=k1,ke
       if(rpnc%que(i)%tid/=TID_AFNC) cycle
       call add_rpnm_entry(rpnc%rl,subexpr(rpnb,i),SC_FNC,kf,rpnm)
       if(allocated(rpnm%que)) deallocate(rpnm%que)
       if(allocated(rpnm%vbuf)) deallocate(rpnm%vbuf)
       if(i==k1) then
          ! | k1=i |   |   | ... | km |       | kc   | ka | ke   |
          ! | f    | x | y | arg | *  | codes | [^C] | =  | [;]  |
          km=find_implicit_mul() ! must be found
          if(km==0) STOP "*** set_function: UNEXPECTED ERROR: km=0"
          ac=km-k1+1-2 ! number of arguments
       else
          ! | k1 | ... | km=i |       | kc   | ka | ke  |
          ! | x  | arg | f    | codes | [^C] | =  | [;] |
          km=i
          ac=km-k1+1-1
       end if
       tc=(kc-1)-(km+1)+1
       rpnm%na=ac
       if(tc==0) stop "Empty function"
       allocate(rpnm%que(tc))
       rpnm%que(1:tc)=rpnc%que(km+1:kc-1)
       call alloc_vbuf()
       call init_pnames()
       call cp_vbuf()
       exit
    end do
    rpnc%que(k1:ke)%tid=TID_NOP
    set_function=0
    call set_flg(rpnc%flg%sta,RCS_FNC_SET)

  contains

    integer function find_end()
      use misc, only: get_lo32,get_up32
      integer ii
      kc=0; ka=0
      find_end=size(rpnc%que)
      do ii=k1,size(rpnc%que)
         select case(get_lo32(rpnb%que(ii)%tid))
         case(TID_END)
            find_end=ii
            return
         case(TID_ASN)
            if(get_up32(rpnb%que(ii)%tid)==AID_FNC) then
               ! it must be found
               ka=ii
               if(kc==0) kc=ii
            end if
         case(TID_DQ2)
            kc=ii
         end select
      end do
    end function find_end

    subroutine init_pnames()
      use slist, only: init_slist,uinit_slist,add_str
      use misc, only: get_up32
      call add_str(rpnm%pnames,supexpr(rpnb,i))
      call add_str(rpnm%pnames,supexpr(rpnb,ka))
    end subroutine init_pnames

    integer function find_implicit_mul()
      integer ii
      find_implicit_mul=0
      do ii=k1,ke
         if(rpnb%que(ii)%tid==TID_BOP4) then
           find_implicit_mul=ii ! start of function definition
            return
         end if
      end do
    end function find_implicit_mul

    subroutine cp_vbuf()
      use fpio, only: cp
      use misc, only: get_up32,get_lo32
      use slist, only: add_str
      integer ii,jj
      integer kp,asis
      type(t_rrpnq),pointer::qq
      complex(cp) v
      pointer(pv,v)
      rpnm%p_vbuf=0
      do ii=1,size(rpnm%que)
         select case(rpnm%que(ii)%tid)
         case(TID_VAR,TID_PAR,TID_CPAR)
         case default
            cycle
         end select
         jj=ii+km !<<<<<<<<<<<<<<<<
         qq => rpnb%que(jj)
         if(qq%tid/=TID_FIG) then ! par
            asis=get_up32(qq%tid)
            if(asis/=0) qq%p1=qq%p1+1
            call add_str(rpnm%pnames,subexpr(rpnb,jj),ent=kp)
            rpnm%que(ii)%tid=get_lo32(qq%tid)
            if(asis/=0) then
               qq%p1=qq%p1-1
               kp=-kp
            end if
            rpnm%que(ii)%cid=kp
         else
            pv=rpnm%que(ii)%cid
            rpnm%p_vbuf=rpnm%p_vbuf+1
            rpnm%vbuf(rpnm%p_vbuf)=v
            rpnm%que(ii)%tid=TID_FIG
            rpnm%que(ii)%cid=rpnm%p_vbuf
         end if
      end do
    end subroutine cp_vbuf

    subroutine alloc_vbuf()
      integer ii,vc
      vc=0
      do ii=km,ke
         if(rpnb%que(ii)%tid==TID_FIG) vc=vc+1
      end do
      if(vc>0) allocate(rpnm%vbuf(vc))
    end subroutine alloc_vbuf

  end function set_function

  integer function set_macro(rpnb,rpnc,k1)
    ! AMAC never be in AMAC
    use rpnlist, only: t_rpnm,add_rpnm_entry
    use misc, only: set_flg
    type(t_rpnb),intent(in),target::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::k1
    type(t_rpnm),pointer::rpnm
    integer k,km,ke
    integer i
    integer tc
    ke=find_end()
    do i=k1,ke
       if(rpnc%que(i)%tid/=TID_AMAC) cycle
       k=find_qend()
! | i    |      | k |   | ke |
! | AMAC | code | " | = | ;  |
       call add_rpnm_entry(rpnc%rl,subexpr(rpnb,i),SC_MAC,km,rpnm)
       rpnc%que(i)%cid=km
       if(allocated(rpnm%que)) deallocate(rpnm%que)
       if(allocated(rpnm%vbuf)) deallocate(rpnm%vbuf)
       tc=(k-1)-(i+1)+1
       if(tc==0) stop "Empty macro!"
       allocate(rpnm%que(tc))
       call alloc_vbuf()
       call init_pnames()
       rpnm%que(1:tc)=rpnc%que(i+1:i+1+tc-1)
       call cp_vbuf()
       call check_mscl()
       rpnc%que(i)%tid       = TID_MAC
       rpnc%que(i+1:k+1)%tid = TID_NOP
    end do
    set_macro=0
    call set_flg(rpnc%flg%sta,RCS_MAC_SET)
  contains

    subroutine check_mscl()
      integer ii
      do ii=1,tc
         if(rpnm%que(ii)%tid==TID_MSCL) &
              rpnm%que(ii)%tid=TID_END
      end do
    end subroutine check_mscl

    integer function find_end()
      integer ii
      find_end=size(rpnc%que)
      do ii=k1,size(rpnc%que)
         if(rpnc%que(ii)%tid==TID_END) then
            find_end=ii-1
            return
         end if
      end do
    end function find_end

    subroutine init_pnames()
      use slist, only: init_slist,uinit_slist,add_str
      use misc, only: get_up32
      call add_str(rpnm%pnames,supexpr(rpnb,i))
    end subroutine init_pnames

    subroutine cp_vbuf()
      use slist, only: add_str
      use fpio, only: cp
      use misc, only: get_up32,get_lo32
      ! Reverts TID_VAR to FIG,PAR,APAR
      ! FIG and PAR will have pointer to vbuf
      ! APAR will have pointer to pars%v
      integer ii,jj
      integer kp,asis
      type(t_rrpnq),pointer::qq
      complex(cp) v
      pointer(pv,v)
      rpnm%p_vbuf=0
      do ii=1,tc
         select case(rpnm%que(ii)%tid)
         case(TID_VAR,TID_PAR,TID_CPAR)
         case default
            cycle
         end select
         jj=ii+i
         qq => rpnb%que(jj)
         if(qq%tid/=TID_FIG) then ! par
            asis=get_up32(qq%tid)
            if(asis/=0) qq%p1=qq%p1+1
            call add_str(rpnm%pnames,subexpr(rpnb,jj),ent=kp)
            rpnm%que(ii)%tid=get_lo32(qq%tid) ! <<< TID_PAR or TID_APAR
            if(asis/=0) then
               qq%p1=qq%p1+1
               kp=-kp
            end if
            rpnm%que(ii)%cid=kp     ! <<<
         else
            pv=rpnm%que(ii)%cid
            rpnm%p_vbuf=rpnm%p_vbuf+1
            rpnm%vbuf(rpnm%p_vbuf)=v
            rpnm%que(ii)%tid=TID_FIG
            rpnm%que(ii)%cid=rpnm%p_vbuf
         end if
      end do
    end subroutine cp_vbuf

    subroutine alloc_vbuf()
      integer ii,vc
      vc=0
      do ii=i+1,k-1
         if(rpnb%que(ii)%tid==TID_FIG) vc=vc+1
      end do
      if(vc>0) allocate(rpnm%vbuf(vc))
    end subroutine alloc_vbuf

    integer function find_qend()
      integer ii
      find_qend=0
      do ii=i+1,ke
         if(rpnc%que(ii)%tid==TID_QEND) then
            find_qend=ii
            return
         end if
      end do
      STOP "*** find_qend: UNEXPECTED ERROR: TID_QEND not found"
    end function find_qend

  end function set_macro

  subroutine print_error(e,p1,p2)
    use misc, only: mess
    character*(*),intent(in)::e
    integer,intent(in)::p1,p2
    call mess("*** Syntax Error at: ")
    call mess(restoreq(trim(e)))
    if(p1<=0.or.p2<=0) return
    call mess(repeat(" ",p1-1)//repeat("^",abs(p2)-p1+1)) ! some return with negative p2
  end subroutine print_error

  integer function build_rpnc(rpnb,rpnc,nvbuf)
    use misc, only: get_lo32,get_i32,get_up32,is_not_set,mess,cle_flg,is_set,set_flg
    use plist
    use fpio, only: czero
    use memio, only: itoa
    type(t_rpnb),intent(in),target::rpnb ! only temporarly modified
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::nvbuf
    real(rp) x
    integer istat
    integer i,k
    logical amac,afnc
    integer p_q1
    type(t_rpnq),pointer::q
    type(t_rrpnq),pointer::qq
    integer inpt
    integer p1,p2
    type(t_rpnf),pointer::flg
    integer mode_save,dmode_save

    flg => rpnc%flg

    mode_save=flg%mode
    dmode_save=flg%dmode

    call cle_flg(flg%sta,RCS_FNC_SET)
    call cle_flg(flg%sta,RCS_MAC_SET)

    p_q1=1
    amac=.false.
    afnc=.false.
    inpt=0
    istat=0
    p1=1
    p2=rpnb%len_expr

    if(associated(rpnc%que).and.size(rpnc%que)>0) call uinit_rpncq(rpnc%que)
    allocate(rpnc%que(rpnb%p_que))
    rpnc%que(:)%tid=TID_UNDEF
    rpnc%que(:)%cid=0 ! dump_rpnc might refer unset cid as a pointer
    rpnc%p_vbuf=0
    if(.not.associated(rpnc%vbuf)) then
       allocate(rpnc%vbuf(nvbuf))
    else if(nvbuf>size(rpnc%vbuf)) then
       deallocate(rpnc%vbuf)
       allocate(rpnc%vbuf(nvbuf))
    end if

    do i=1,size(rpnc%que)
       istat=0
       q  => rpnc%que(i)
       qq => rpnb%que(i)
       select case(get_lo32(qq%tid))
          !
          ! operators
          !
       case(TID_UOP1,TID_UOP2,TID_UOP3)
          q%tid=get_i32(TID_OP,1)
          q%cid=get_oid1()
       case(TID_LOP1)
          q%tid=get_i32(TID_LOP,1)
          q%cid=get_loid1()
       case(TID_BOP1,TID_BOP2,TID_BOP3,TID_BOP4)
          q%tid=get_i32(TID_OP,2)
          q%cid=get_oid2()
       case(TID_LOP2,TID_LOP3,TID_LOP4)
          q%tid=get_i32(TID_LOP,2)
          q%cid=get_loid2()
       case(TID_ROP)
          q%tid=get_i32(TID_ROP,2)
          q%cid=get_oid2()
       case(TID_TOP1)
          q%tid=TID_COP
          q%cid=OID_CND ! <<<<<<<<<<<<<<<<<<<<<,
          call find_delim(qq%p1)
       case(TID_ASN,TID_AOP)
          if(get_up32(qq%TID)==AID_PAR) then
             q%tid=get_i32(TID_AOP,2)
             q%cid=get_aid()
          else
             q%tid=TID_NOP
             q%cid=TID_UNDEF
          end if
       case(TID_IFNC)
          call set_fnc_op
       case(TID_UFNC)
          q%tid=TID_UFNC
          q%cid=get_up32(qq%tid)
          if(get_up32(qq%p1)/=0) istat=FZCERR_NARG ! <<<<<<<<<
       case(TID_APAR) ! asign a parameter.
          q%tid=TID_PAR
          istat=add_par_by_entry(rpnc%pars,subexpr(rpnb,i),k)
          if(istat==0) then
             q%cid=get_par_loc(rpnc%pars,k)
          else
             if(istat==FZCERR_RDONL) then
                call mess("*** Parameter is read-only: "//subexpr(rpnb,i))
             else
                call mess("*** add_par_by_entry failed: code = "//trim(itoa(istat)))
             end if
             istat=FZCERR_ADDPAR
          end if
          !
          ! operands
          !
       case(TID_FIG)
          q%tid=TID_VAR
          if(qq%p1==qq%p2.and.subexpr(rpnb,i)=="@") then
             call put_vbuf(rpnc,i,czero)
             q%cid=-q%cid !<<<<<<<<<<<<<<<<<<<<<<<<<
          else
             istat=read_fig()
             if(istat/=0) istat=FZCERR_INVFIG
             call put_vbuf(rpnc,i,x)
          end if
       case(TID_PAR)
          k=get_up32(qq%tid)
          if(k<PID_END) then
             if(check_pop(k)) cycle
             if(check_sop()) cycle
             if(check_mac()) cycle
          end if
          call set_par(k)
          !
          ! Non-evaluatable TID
          ! function assignment
          !
       case(TID_AFNC)
          afnc=.true.
          q%tid=TID_AFNC
          q%cid=TID_UNDEF
       case(TID_DPAR)
          q%tid=TID_DPAR
          q%cid=get_up32(qq%p1)
       case(TID_IVAR1,TID_IVAR1L,TID_IVAR1U)
          q%tid=qq%tid
          q%cid=1
          !
          ! macro assignment
          !
       case(TID_AMAC)
          amac=.true.
          q%tid=TID_AMAC
          q%cid=TID_UNDEF
       case(TID_QEND)
          q%tid=TID_QEND
          q%cid=0
          istat=set_macro(rpnb,rpnc,p_q1)
          amac=.false.
          !
          ! special TIDs
          !
       case(TID_ISTA,TID_IEND)
          q%tid=get_lo32(qq%tid)
          q%cid=get_up32(qq%tid) ! len code
       case(TID_DLM1)
          q%tid=TID_DLM1
          q%cid=0 ! will be set later in find_delim
       case(TID_DLM2)
          q%tid=TID_DLM2
          q%cid=qq%p2 ! bc-kc
       case(TID_SCL)
          q%tid=TID_END
          q%cid=qq%p2-qq%p1 ! count of ;
          p2=qq%p1
          if(amac) istat=set_macro(rpnb,rpnc,p_q1)
          if(afnc) istat=set_function(rpnb,rpnc,p_q1)
          amac=.false.
          afnc=.false.
          if(inpt>0) istat=proc_input(i)
          inpt=0
          p_q1=i+1
          p1=qq%p2+1
          p2=rpnb%len_expr
       case(TID_MSCL)
          q%tid=TID_MSCL
          q%cid=qq%p2-qq%p1 ! count of ;
       case(TID_COL)
          q%tid=TID_COL
          q%cid=0
       case(TID_TCOL)
          q%tid=TID_TCOL
          q%cid=0
       case(TID_COM,TID_MCOM)
          call set_com()
       case default
          call mess("que="//trim(itoa(i))//", tid="//trim(itoa(qq%tid)))
          STOP "*** build_rpnc: UNEXPECTED ERROR: unexpected tid in rpnb"
       end select
       if(istat/=0) then
          if(is_not_set(rpnc%flg%mode,RCPM_NO_WARN)) &
               call print_error(rpnb%expr(1:rpnb%len_expr),get_lo32(qq%p1),get_lo32(qq%p2))
          exit
       end if
    end do

    flg%mode=mode_save
    flg%dmode=dmode_save

    if(istat==0) then
       if(amac) istat=set_macro(rpnb,rpnc,p_q1)
       if(afnc) istat=set_function(rpnb,rpnc,p_q1)
       if(inpt>0) istat=proc_input(size(rpnc%que))
    end if

    if(istat==0) then
       if(is_set(rpnc%flg%sta,RCS_FNC_SET)) then
          istat=RPNSTA_FNCSET
       else if(is_set(rpnc%flg%sta,RCS_MAC_SET).and.count_que()==1) then
          istat=RPNSTA_MACSET
       end if
    end if
    if(istat==0) call set_flg(rpnc%flg%sta,RCS_READY)

    build_rpnc=istat

  contains

    integer function count_que()
      integer ii
      count_que=0
      do ii=1,size(rpnc%que)
         if(rpnc%que(ii)%tid/=TID_NOP) count_que=count_que+1
      end do
    end function count_que

    subroutine set_com()
      use com, only: exe_mode_com
      use memio, only: mcp
      integer ll,pp1,pa1,pa2
      pa1=get_up32(qq%p1)
      pa2=get_up32(qq%p2)
      if(pa1>0.and.pa2>0) then
         ll=pa2-pa1+1
      else
         ll=0
      end if
      q%tid=get_i32(get_up32(qq%tid),ll)
      if(ll>0) then
         pp1=get_lo32(qq%p1)
         pa1=pa1+pp1-1
         pa2=pa2+pp1-1
         q%cid=malloc(ll)
         call mcp(q%cid,loc(rpnb%expr(pa1:pa1)),ll)
      else
         q%cid=0
      end if
      if((amac.or.afnc).and.get_lo32(qq%tid)==TID_MCOM) &
           call exe_mode_com(get_up32(qq%tid),rpnc%flg,dec=.true.)
    end subroutine set_com

    integer function proc_input(ii2)
      use fpio, only: cp
      integer,intent(in)::ii2
      integer ii,kk,ke
      type(t_rrpnq),pointer::qqq
      complex(cp) zz
      pointer(pzz,zz)
      interface
         recursive integer function fnc_input(rpnc,p,s,z) result(istat)
           use rpnd, only: t_rpnc
           use fpio, only: cp
           type(t_rpnc),intent(in)::rpnc
           character*(*),intent(in)::p,s
           complex(cp),intent(inout)::z
         end function fnc_input
      end interface
      pointer(pfnc_input,fnc_input)
      proc_input=0
      if(ii2==size(rpnc%que)) then
         ke=rpnb%len_expr
      else
         ke=rpnb%que(ii2)%p1-1
      end if
      do ii=p_q1,ii2
         qqq => rpnb%que(ii)
         if(get_lo32(qqq%tid)==TID_PAR &
              .and.get_up32(qqq%tid)==PID_INPUT) then
            qqq%p1=qqq%p1+1
            istat=find_par(rpnc%pars,subexpr(rpnb,ii),ent=kk)
            if(istat/=0) stop "proc_input: UNEXPECTED ERROR: find_par failed"
            pzz=get_par_loc(rpnc%pars,kk)
            if(pzz==0) stop "proc_input: UNEXPECTED ERROR: get_par_loc failed"
            qqq%p1=qqq%p1-1
            pfnc_input=ptr_input
            istat=fnc_input(rpnc,&
                 rpnb%expr(p1:p2),&
                 rpnb%expr(get_lo32(qqq%p1):get_lo32(qqq%p2)),&
                 zz)
            if(istat/=0) then
               proc_input=FZCERR_READ
               exit
            end if
         end if
      end do
    end function proc_input

    logical function check_pop(kk)
      integer,intent(in)::kk
      check_pop=.false.
      if(kk/=0.and.kk<=PID_END) then
         q%tid=TID_POP
         q%cid=kk
         check_pop=.true.
      end if
    end function check_pop

    logical function check_sop()
      use misc, only: is_set
      integer kk
      check_sop=.false.
      if(is_set(rpnc%flg%mode,RCM_STA).and.is_spar(subexpr(rpnb,i),kk)) then
         q%tid=TID_SOP
         q%cid=get_sid(kk)
         check_sop=.true.
      end if
    end function check_sop

    logical function check_mac()
      use rpnlist, only: rpnlist_count,find_rpnlist
      integer kk
      check_mac=.false.
      if(rpnlist_count(rpnc%rl)>0) then
         kk=find_rpnlist(rpnc%rl,subexpr(rpnb,i),SC_MAC)
         if(kk>0) then
            q%tid=TID_MAC
            q%cid=kk
            check_mac=.true.
         end if
      end if
    end function check_mac

    subroutine set_par(asis)
      integer,intent(in)::asis
      integer kk
      logical dup
      if(asis/=0) then
         qq%p1=qq%p1+1 ! skip @ (modifying intent in var)
         if(.not.amac.and..not.afnc) inpt=inpt+1
      end if
      istat=find_par(rpnc%pars,subexpr(rpnb,i),ent=kk)
      if((asis/=0.or.amac.or.afnc) &
           .and.istat/=0.and.is_not_set(rpnc%flg%mode,RCPM_NO_AUTO_ADD_PAR)) then
         ! par may not already exist
         istat=add_par_by_entry(rpnc%pars,subexpr(rpnb,i),kk)
         if(istat/=0) then
            if(istat==FZCERR_RDONL) then
               call mess("*** Parameter is read-only: "//subexpr(rpnb,i))
            else
               call mess("*** add_par_by_entry failed: code = "//trim(itoa(istat)))
            end if
            istat=FZCERR_ADDPAR
         end if
      else if(istat/=0) then
         call mess("*** No such parameter: "//subexpr(rpnb,i))
         istat=FZCERR_NOPAR
      end if
      if(istat==0) then
         q%tid=TID_PAR
         q%cid=get_par_loc(rpnc%pars,kk,dup)
         if(q%cid==0) then
            call mess("*** get_par failed: code = "//trim(itoa(istat)))
            istat=FZCERR_GETPAR
         else
            if(dup) q%tid=TID_CPAR
         end if
      end if
      if(asis/=0) qq%p1=qq%p1-1
    end subroutine set_par

    subroutine set_fnc_op()
      integer m,op
      m=get_up32(qq%p2)
      if(m/=narg_max) then
         if(get_up32(qq%p1)/=0) then
            istat=FZCERR_NARG
            return
         end if
         if(get_up32(qq%tid)<=FID_END) then
            op=TID_OP
         else
            op=TID_IOP
         end if
         q%tid=get_i32(op,m)
      else
         q%tid=get_i32(TID_OPN,narg_max-get_up32(qq%p1))
      end if
      q%cid=get_fid(get_up32(qq%tid))
    end subroutine set_fnc_op

    integer function read_fig()
      use memio
      use fpio, only: atox
      use misc, only: is_set,is_not_set
      integer b
      if(is_not_set(rpnc%flg%mode,RCM_INM)) then
         x=atox(subexpr(rpnb,i),read_fig)
         return
      else if(is_set(rpnc%flg%mode,RCM_IBIN)) then
         b=IBASE_BIN
      else if(is_set(rpnc%flg%mode,RCM_IOCT)) then
         b=IBASE_OCT
      else if(is_set(rpnc%flg%mode,RCM_IHEX)) then
         b=IBASE_HEX
      else
         STOP "*** read_fig: ERROR: UNEXPECTED mode"
      end if
      x=real(atoi(subexpr(rpnb,i),b,b,read_fig),kind=rp)
    end function read_fig

    subroutine find_delim(pos)
      integer,intent(in)::pos
      integer ii
      ! i => TOP1
      do ii=i,p_q1,-1
         select case(rpnb%que(ii)%tid)
         case(TID_DLM1)
            if(rpnb%que(ii)%p1==pos) then
               rpnc%que(ii)=q
               rpnc%que(ii)%cid=get_i32(rpnc%que(ii)%cid,i-ii)
               q%tid=TID_DLM1
               q%cid=rpnb%que(ii)%p2 ! = bc-kc
               return
            end if
         case(TID_SCL)
            exit
         end select
      end do
      istat=FZCERR_PARSER
    end subroutine find_delim

    integer function get_sid(sid)
      use zmath
      integer,intent(in)::sid
      select case(sid)
      case(SID_N)
         get_sid=loc(zms_n)
      case(SID_AVE)
         get_sid=loc(zms_ave_x)
      case(SID_VAR)
         get_sid=loc(zms_var_x)
      case(SID_SUM)
         get_sid=loc(zms_sum_x)
      case(SID_SUM2)
         get_sid=loc(zms_sum2_x)
      case(SID_UVAR)
         get_sid=loc(zms_uvar_x)
      case(SID_AVE_Y)
         get_sid=loc(zms_ave_y)
      case(SID_VAR_Y)
         get_sid=loc(zms_var_y)
      case(SID_SUM_Y)
         get_sid=loc(zms_sum_y)
      case(SID_SUM2_Y)
         get_sid=loc(zms_sum2_y)
      case(SID_UVAR_Y)
         get_sid=loc(zms_uvar_y)
      case(SID_SUM_XY)
         get_sid=loc(zms_sum_xy)
      case(SID_A)
         get_sid=loc(zms_a)
      case(SID_B)
         get_sid=loc(zms_b)
      case default
         STOP "*** get_sid: UNEXPECTED ERROR: unknown sid"
      end select
    end function get_sid

    integer function get_fid(fid)
      use zmath
      use misc, only: is_set
      integer,intent(in)::fid
      select case(fid)
      case(FID_RAN)
         get_fid=loc(zm_ran)
      case(FID_SIN)
         if(is_set(rpnc%flg%mode,RCM_DEG)) then
            get_fid=loc(zm_sind)
         else
            get_fid=loc(zm_sin)
         end if
      case(FID_COS)
         if(is_set(rpnc%flg%mode,RCM_DEG)) then
            get_fid=loc(zm_cosd)
         else
            get_fid=loc(zm_cos)
         end if
      case(FID_TAN)
         if(is_set(rpnc%flg%mode,RCM_DEG)) then
            get_fid=loc(zm_tand)
         else
            get_fid=loc(zm_tan)
         end if
      case(FID_ASIN)
         if(is_set(rpnc%flg%mode,RCM_DEG)) then
            get_fid=loc(zm_asind)
         else
            get_fid=loc(zm_asin)
         end if
      case(FID_ACOS)
         if(is_set(rpnc%flg%mode,RCM_DEG)) then
            get_fid=loc(zm_acosd)
         else
            get_fid=loc(zm_acos)
         end if
      case(FID_ATAN)
         if(is_set(rpnc%flg%mode,RCM_DEG)) then
            get_fid=loc(zm_atand)
         else
            get_fid=loc(zm_atan)
         end if
      case(FID_EXP)
         get_fid=loc(zm_exp)
      case(FID_SQRT)
         get_fid=loc(zm_sqrt)
      case(FID_CBRT)
         get_fid=loc(zm_cbrt)
      case(FID_LN)
         get_fid=loc(zm_log)
      case(FID_LOG)
         get_fid=loc(zm_log10)
      case(FID_SINH)
         get_fid=loc(zm_sinh)
      case(FID_COSH)
         get_fid=loc(zm_cosh)
      case(FID_TANH)
         get_fid=loc(zm_tanh)
      case(FID_ASINH)
         get_fid=loc(zm_asinh)
      case(FID_ACOSH)
         get_fid=loc(zm_acosh)
      case(FID_ATANH)
         get_fid=loc(zm_atanh)
      case(FID_ABS)
         get_fid=loc(zm_abs)
      case(FID_INT)
         get_fid=loc(zm_int)
      case(FID_FRAC)
         get_fid=loc(zm_frac)
      case(FID_CONJG)
         get_fid=loc(zm_conjg)
      case(FID_NINT)
         get_fid=loc(zm_nint)
      case(FID_RE)
         get_fid=loc(zm_re)
      case(FID_IM)
         get_fid=loc(zm_im)
      case(FID_MAG)
         get_fid=loc(zm_mag)
      case(FID_ARG)
         get_fid=loc(zm_arg)
      case(FID_GAMMA)
         get_fid=loc(zm_gamma)
      case(FID_LGAMMA)
         get_fid=loc(zm_lgamma)
      case(FID_MOD)
         get_fid=loc(zm_mod)
      case(FID_MIN)
         get_fid=loc(zm_min)
      case(FID_MAX)
         get_fid=loc(zm_max)
      case(FID_GAMI)
         get_fid=loc(zm_gami)
      case(FID_PSY)
         get_fid=loc(zm_psy)
      case(FID_JN)
         get_fid=loc(zm_jn)
      case(FID_YN)
         get_fid=loc(zm_yn)
      case(FID_HN)
         get_fid=loc(zm_hn)
      case(FID_DJN)
         get_fid=loc(zm_djn)
      case(FID_DYN)
         get_fid=loc(zm_dyn)
      case(FID_DHN)
         get_fid=loc(zm_dhn)
      case(FID_FL)
         get_fid=loc(zm_Fl)
      case(FID_GL)
         get_fid=loc(zm_Gl)
      case(FID_HL)
         get_fid=loc(zm_Hl)
      case(FID_DFL)
         get_fid=loc(zm_dFl)
      case(FID_DGL)
         get_fid=loc(zm_dGl)
      case(FID_DHL)
         get_fid=loc(zm_dHl)
      case(FID_SUM)
         get_fid=loc(zm_sum)
      case(FID_AVE)
         get_fid=loc(zm_ave)
      case(FID_VAR)
         get_fid=loc(zm_var)
      case(FID_UVAR)
         get_fid=loc(zm_uvar)
      case(FID_SUM2)
         get_fid=loc(zm_sum2)
      case(FFID_DEINT)
         get_fid=loc(zm_deint)
      case default
         STOP "*** get_fid: UNEXPECTED ERROR: unknown fid"
      end select
    end function get_fid

    integer function get_aid()
      use zmath
      get_aid=AID_NOP ! to avoid warning
      select case(subexpr(rpnb,i))
      case("=")
         get_aid=loc(zm_mov)
      case("+=")
         get_aid=loc(zm_add)
      case("-=")
         get_aid=loc(zm_sub)
      case("*=")
         get_aid=loc(zm_mul)
      case("/=")
         get_aid=loc(zm_div)
      case("^=")
         get_aid=loc(zm_pow)
      case default
         STOP "*** get_aid: UNEXPECTED ERROR unknown aid"
      end select
    end function get_aid

    integer function get_oid1()
      use zmath
      use misc, only: is_not_set
      get_oid1=OID_NOP
      select case(subexpr(rpnb,i))
      case("+")
         get_oid1=loc(zm_nop)
      case("-")
         if(is_not_set(rpnc%flg%mode,RCM_RATIO)) then
            get_oid1=loc(zm_neg)
         else
            get_oid1=loc(zm_neg_f)
         end if
      case("!")
         get_oid1=loc(zm_fac)
      case("!!")
         get_oid1=loc(zm_dfac)
      case("++")
         if(is_not_set(rpnc%flg%mode,RCM_RATIO)) then
            get_oid1=loc(zm_inc)
         else
            get_oid1=loc(zm_inc_f)
         end if
      case("--")
         if(is_not_set(rpnc%flg%mode,RCM_RATIO)) then
            get_oid1=loc(zm_dec)
         else
            get_oid1=loc(zm_dec_f)
         end if
      case default
         STOP "*** get_oid1: UNEXPECTED ERROR: unknown oid"
      end select
    end function get_oid1

    integer function get_loid1()
      use zmath
      select case(subexpr(rpnb,i))
      case("~",LOPS_NOT)
         get_loid1=LOID_NOT
      case default
         STOP "*** get_loid1: UNEXPECTED ERROR: unknown loid"
      end select
    end function get_loid1

    integer function get_oid2()
      use zmath
      use misc, only: is_not_set
      if(qq%tid==TID_BOP4) then
         get_oid2=loc(zm_mul)
         return
      end if
      get_oid2=OID_NOP
      select case(subexpr(rpnb,i))
      case("+")
         if(is_not_set(rpnc%flg%mode,RCM_RATIO)) then
            get_oid2=loc(zm_add)
         else
            get_oid2=loc(zm_add_f)
         end if
      case("-")
         if(is_not_set(rpnc%flg%mode,RCM_RATIO)) then
            get_oid2=loc(zm_sub)
         else
            get_oid2=loc(zm_sub_f)
         end if
      case("*")
         if(is_not_set(rpnc%flg%mode,RCM_RATIO)) then
            get_oid2=loc(zm_mul)
         else
            get_oid2=loc(zm_mul_f)
         end if
      case("/")
         if(is_not_set(rpnc%flg%mode,RCM_RATIO)) then
            get_oid2=loc(zm_div)
         else
            get_oid2=loc(zm_div_f)
         end if
      case("C")
         get_oid2=loc(zm_comb)
      case("P")
         get_oid2=loc(zm_perm)
      case("**","^")
         get_oid2=loc(zm_pow)
      case("//")
         get_oid2=loc(zm_invpow)
      case("e")
         get_oid2=loc(zm_exp10)
      case("==")
         get_oid2=loc(zmr_eq)
      case("~=")
         get_oid2=loc(zmr_neq)
      case(">")
         get_oid2=loc(zmr_gt)
      case("<")
         get_oid2=loc(zmr_lt)
      case("<=")
         get_oid2=loc(zmr_le)
      case(">=")
         get_oid2=loc(zmr_ge)
      case default
         STOP "*** get_oid2: UNEXPECTED ERROR: unknown oid"
      end select
    end function get_oid2

    integer function get_loid2()
      use zmath
      select case(subexpr(rpnb,i))
      case(LOPS_AND)
         get_loid2=LOID_AND
      case(LOPS_OR)
         get_loid2=LOID_OR
      case(LOPS_EQ)
         get_loid2=LOID_EQ
      case(LOPS_NEQ)
         get_loid2=LOID_NEQ
      case default
         STOP "*** get_loid2: UNEXPECTED ERROR: unknown loid"
      end select
    end function get_loid2

  end function build_rpnc

  pure logical function is_left_op(tid)
    integer,intent(in)::tid
    select case(tid)
    case(TID_ASN,TID_AOP,TID_TOP1,TID_LOP1, &
         TID_UOP1,TID_BOP3)
       is_left_op=.false.
    case default
       is_left_op=.true.
    end select
  end function is_left_op

  pure subroutine rpn_put_q(rpnb,q)
    type(t_rpnb),intent(inout)::rpnb
    type(t_rrpnq),intent(in)::q
    rpnb%p_que=rpnb%p_que+1
    if(rpnb%p_que>NUM_RRPNQ_MAX) then
       rpnb%err=FZCERR_MEMOV
       return
    end if
    rpnb%que(rpnb%p_que)=q
  end subroutine rpn_put_q

  pure subroutine rpn_put_e(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid,p1,p2
    type(t_rrpnq) q
    q%tid=tid; q%p1=p1; q%p2=p2
    call rpn_put_q(rpnb,q)
  end subroutine rpn_put_e

  pure subroutine rpn_push_q(rpnb,q)
    type(t_rpnb),intent(inout)::rpnb
    type(t_rrpnq),intent(in)::q
    rpnb%p_buf=rpnb%p_buf+1
    if(rpnb%p_buf>NUM_RRPNQ_MAX) then
       rpnb%err=FZCERR_MEMOV
       return
    end if
    rpnb%buf(rpnb%p_buf)=q
  end subroutine rpn_push_q

  pure subroutine rpn_push_e(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid,p1,p2
    type(t_rrpnq) q
    q%tid=tid; q%p1=p1; q%p2=p2
    call rpn_push_q(rpnb,q)
  end subroutine rpn_push_e

  pure subroutine rpn_pop(rpnb)
    type(t_rpnb),intent(inout)::rpnb
    if(rpnb%p_buf<=0) return
    call rpn_put(rpnb,rpnb%buf(rpnb%p_buf))
    rpnb%p_buf=rpnb%p_buf-1
  end subroutine rpn_pop

  pure subroutine rpn_spop(rpnb)
    type(t_rpnb),intent(inout)::rpnb
    if(rpnb%p_buf<=0) return
    select case(rpnb%buf(rpnb%p_buf)%tid) ! get_lo32 ?
    case(TID_BRA,TID_QSTA,TID_IBRA,TID_COL,TID_TCOL)
       rpnb%p_buf=rpnb%p_buf-1
    case default
       call rpn_pop(rpnb)
    end select
  end subroutine rpn_spop

  pure subroutine rpn_pop_to(rpnb,tid)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid
    do while(rpnb%p_buf>0)
       if(rpnb%buf(rpnb%p_buf)%tid==tid) exit
       call rpn_spop(rpnb)
    end do
  end subroutine rpn_pop_to

  pure subroutine rpn_pop_until(rpnb,tid,fnc)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid
    logical,intent(in),optional::fnc
    logical exit
    exit=.false.
    do while(rpnb%p_buf>0)
        if(present(fnc)) then
           if(fnc.and.rpnb%buf(rpnb%p_buf)%tid==tid) exit
        end if
        if(rpnb%buf(rpnb%p_buf)%tid==tid) exit=.true.
        call rpn_spop(rpnb)
        if(exit) exit
    end do
  end subroutine rpn_pop_until

  subroutine rpn_pop_all_bra(rpnb)
    type(t_rpnb),intent(inout)::rpnb
    integer i,k
    k=1
    do i=rpnb%p_buf,1,-1
       select case(rpnb%buf(i)%tid)
       case(TID_BRA,TID_IBRA)
          k=i
       end select
    end do
    if(k==0) return
    do while(rpnb%p_buf>=k)
       call rpn_spop(rpnb)
    end do
  end subroutine rpn_pop_all_bra

  subroutine rpn_pop_all(rpnb)
    type(t_rpnb),intent(inout)::rpnb
    do while(rpnb%p_buf>0)
       call rpn_spop(rpnb)
    end do
  end subroutine rpn_pop_all

  subroutine rpn_try_pop(rpnb,tend,dlm2c,p1)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tend
    integer,intent(in),optional::dlm2c
    integer,intent(in),optional::p1
    integer tid,told
    told=TID_UNDEF
    do
       if(rpnb%p_buf==0) exit
       tid=rpnb%buf(rpnb%p_buf)%tid
       select case(tid)
       case(TID_BRA,TID_QSTA,TID_COL,TID_TCOL)
          rpnb%p_buf=rpnb%p_buf-1
          if(tid==tend) then
             if(present(dlm2c).and.told==TID_TCOL)&
                  ! (: sequence in the buf
                  ! <<<  KET marks end of TOP1 <<<<
                  call rpn_put(rpnb,TID_DLM2,p1,dlm2c)
             exit
          end if
       case(TID_IBRA)
          rpnb%p_buf=rpnb%p_buf-1
       case default
          call rpn_pop(rpnb)
       end select
       told=tid
    end do
  end subroutine rpn_try_pop

  subroutine rpn_try_push_e(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid,p1,p2
    type(t_rrpnq) q
    q%tid=tid; q%p1=p1; q%p2=p2
    call rpn_try_push_q(rpnb,q)
  end subroutine rpn_try_push_e

  subroutine rpn_try_push_q(rpnb,q)
    use misc, only: get_lo32
    type(t_rpnb),intent(inout)::rpnb
    type(t_rrpnq),intent(in)::q
    integer tst,t
    do while(rpnb%p_buf>0)
       tst=get_lo32(rpnb%buf(rpnb%p_buf)%tid)
       if(tst<=TID_PRI_MAX) then
          t=get_lo32(q%tid)
          if(tst>t.or.(tst==t.and.is_left_op(t))) then
             call rpn_pop(rpnb)
             cycle
          end if
       end if
       exit
    end do
    call rpn_push_q(rpnb,q)
  end subroutine rpn_try_push_q

  integer function set_tid_par(rpnb,tid,p1,p2)
    use misc, only: get_i32
    type(t_rpnb),intent(inout),target::rpnb
    integer,intent(in)::tid
    integer,intent(in),optional::p1,p2
    type(t_rrpnq),pointer::q
    q => rpnb%que(rpnb%p_que)
    select case(q%tid)
    case(TID_PAR,TID_APAR)
       q%tid=tid
       if(present(p1)) q%p1=get_i32(q%p1,p1)
       if(present(p2)) q%p2=get_i32(q%p2,p2)
       set_tid_par=0
    case default
       set_tid_par=FZCERR_PARSER
    end select
  end function set_tid_par

  integer function parse_formula(rpnc,pnext)
    ! return value
    ! 0<: error code: FZCERR_*
    !  0: success and ready to eval
    ! 0>: success and no need eval: RPNSTA_*
    use misc, only: get_i32,get_lo32,get_up32,is_set,is_not_set,cle_flg,set_flg
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(inout)::pnext
    type(t_rpnb),target::rpnb
    type(t_rrpnq),target::qcur
    integer istat
    integer t,told,btold
    integer tidold,btidold
    integer,pointer::tid,p1,p2
    integer bc,kc,pc,ac,fc,oc,fnc,qc,cc,amc,clc,tc,sc,otc,tclc
    logical amac,com
    integer pfasn
    integer p_q1
    integer pfnc_opened
    integer terr,p1err,p2err
    integer nvbuf
    integer pfsta
    integer mode_save,dmode_save

    mode_save=rpnc%flg%mode
    dmode_save=rpnc%flg%dmode

    call init_rpnb()

    call cle_flg(rpnc%flg%sta,RCS_PRINT_ANS_REQ)

    rpnc%ip=1 ! <<<

    nvbuf=0
    call init_stat()
    amac=.false.
    pfasn=0
    com=.false.

    istat=0
    pfsta=pnext
    tid => qcur%tid
    p1 => qcur%p1
    p2 => qcur%p2

    do
       tid=get_next(rpnb,p1,p2,rpnc%rl)
       pnext=p2
       t=get_lo32(tid)
       select case(t)
       case(TID_BLK,TID_IGNORE)
          cycle
       case(TID_COM)
          if(.not.set_com()) istat=FZCERR_PARSER
       case(TID_FIN,TID_SCL)
          if(.not.check_end()) exit
          if(.not.check_narg_all()) exit
          if(amac.or.pfasn/=0) then
             call rpn_pop_to(rpnb,TID_QSTA)
          else
             call rpn_pop_all(rpnb)
          end if
          if(t==TID_FIN) then
             pnext=0
             if(amac.or.pfasn/=0) then
                call rpn_try_pop(rpnb,TID_QSTA)
                if(amac) call rpn_put(rpnb,TID_QEND,0,0)
                amac=.false.
             end if
             if(is_set(rpnc%flg%mode,RCM_ECHO)) &
                  call echo(p2)
             exit
          else ! TID_SCL
             if(amac.or.pfasn/=0) then
                call rpn_put(rpnb,TID_MSCL,p1,p2) ! <<<<<<<<<<<<
                call init_stat()
             else
                if(is_set(rpnc%flg%mode,RCM_ECHO)) &
                     call echo(p1-1)
                if(p1/=p2) call set_flg(rpnc%flg%sta,RCS_PRINT_ANS_REQ)
                exit
             end if
          end if
       case(TID_INV,TID_UNDEF)
          istat=FZCERR_PARSER
       case(TID_PAR)
          pc=pc+1
          call set_arg_read()
          select case(told)
          case(TID_FIG,TID_KET,TID_PAR,TID_UOP2,TID_UOP3)
             call push_implicit_mul()
          end select
          call rpn_put(rpnb,qcur)
       case(TID_FIG)
          fc=fc+1
          call set_arg_read()
          select case(told)
          case(TID_KET,TID_PAR,TID_FIG,TID_UOP2,TID_UOP3)
             call push_implicit_mul()
          end select
          call rpn_put(rpnb,qcur)
       case(TID_BOP1,TID_BOP2,TID_BOP3,TID_TOP1,& !<<<< TOP1
            TID_LOP2,TID_LOP3,TID_LOP4,&
            TID_ROP)
          oc=oc+1
          if(.not.was_operand()) then
             istat=FZCERR_PARSER
          else if(t==TID_TOP1) then
             tc=tc+1
             otc=otc+1
             call rpn_try_push(rpnb,t,p1,bc-kc)
             call rpn_put(rpnb,TID_DLM1,p1,bc-kc)
          else
             call rpn_try_push(rpnb,qcur)
          end if
       case(TID_UOP2)
          oc=oc+1
          select case(told)
          case(TID_FIG,TID_PAR,TID_KET,TID_UOP2)
             call rpn_try_push(rpnb,qcur)
          case default
             istat=FZCERR_PARSER
          end select
       case(TID_UOP1,TID_LOP1)
          oc=oc+1
          call rpn_try_push(rpnb,qcur)
       case(TID_UOP3)
          oc=oc+1
          select case(told)
          case(TID_FIG,TID_PAR,TID_KET,TID_UOP2)
          case(TID_SCL,TID_BOP1,TID_BOP2,TID_BOP3,TID_BRA,TID_UNDEF)
             t=TID_UOP1
          case default
             istat=FZCERR_PARSER
          end select
          if(istat==0) call rpn_try_push(rpnb,qcur)
       case(TID_BRA)
          bc=bc+1
          select case(told)
          case(TID_FIG,TID_PAR,TID_KET,TID_UOP2,TID_UOP3)
             call push_implicit_mul()
          end select
          call rpn_push(rpnb,qcur)
      case(TID_HKET)
          if(.not.was_operand()) then
             istat=FZCERR_PARSER
          else
             if(.not.check_narg_all()) exit ! not checked
             call rpn_pop_all_bra(rpnb)
          end if
          kc=bc
       case(TID_KET)
          if(.not.was_operand()) then
             istat=FZCERR_PARSER
          else
             call rpn_try_pop(rpnb,TID_BRA,bc-kc,p1)
             if(.not.check_narg(0,.true.,terr)) then
                if(get_lo32(terr)==TID_UFNC) then ! err delayed
                   terr=istat
                   p1err=p1
                   p2err=p2
                   istat=0
                else
                   exit
                end if
             end if
          end if
          kc=kc+1
       case(TID_AOP)
          if(check_assignable()) then
             istat=set_tid_par(rpnb,TID_APAR)
             call rpn_try_push(rpnb,qcur)
          else
             istat=FZCERR_PARSER
          end if
       case(TID_ASN)
          ac=ac+1
          if(is_fnc_asn()) then
             tid=get_i32(TID_ASN,AID_FNC)
             if(terr/=0) terr=0
          else
             if(check_assignable()) then
                istat=set_tid_par(rpnb,TID_APAR)
             else
                istat=FZCERR_PARSER
             end if
          end if
          if(istat==0) call rpn_try_push(rpnb,qcur)
       case(TID_DQ1)
          qc=qc+1
          ! the first "
          if(told/=TID_ASN) then
             istat=FZCERR_PARSER
          else
             ! m="
             ! q    s      q     s
             ! APAR =  ->  AMAC  =
             !                   QSTA
             if(pfasn==0) then
                istat=set_tid_par(rpnb,TID_AMAC,p1+1,find_char(p1+1,STID_DQ2)-1)
                rpnb%buf(rpnb%p_buf)%tid=get_i32(TID_ASN,AID_MAC)
                amac=.true.
             end if
             call rpn_push(rpnb,TID_QSTA,p1,p2)
          end if
       case(TID_DQ2)
          qc=qc+1
          if(.not.was_operand()) then
             istat=FZCERR_PARSER ! Error for ;"
          else if(amac.or.pfasn/=0) then
             if(.not.check_narg_all(TID_QSTA)) exit ! check unclosed ket of fnc
             call rpn_try_pop(rpnb,TID_QSTA)
             call rpn_put(rpnb,TID_QEND,p1,p2)
             call rpn_pop(rpnb) ! pop TID_ASN
             if(pfasn/=0)  call set_par_dummy()
             amac=.false.
             pfasn=0
          end if
       case(TID_COMA)
          cc=cc+1
          if(.not.was_operand()) then
             istat=FZCERR_PARSER
          else
             call rpn_pop_until(rpnb,TID_BRA,.true.)
             if(.not.check_narg(1,.false.,terr)) then
                if(get_lo32(terr)==TID_UFNC) then
                   ! it might be assignment. err delayed.
                   terr=istat
                   p1err=p1
                   p2err=p2
                   istat=0
                else
                   exit
                end if
             end if
          end if
          if(sc/=0.and.rpnb%p_buf>1) then
             if(get_up32(rpnb%buf(rpnb%p_buf-1)%tid)==FFID_DEINT) then
                call rpn_put(rpnb,TID_IEND,0,0)
                call set_idmy(rpnb%p_que)
                sc=sc-1
             end if
          end if
       case(TID_COL)
          if(otc>0) then
             tclc=tclc+1
             otc=otc-1 ! open TOP count
             call rpn_pop_until(rpnb,TID_TOP1)
             call rpn_push(rpnb,TID_TCOL,p1,p2)
          else if(clc<=1) then
             clc=clc+1
             call rpn_pop_to(rpnb,TID_QSTA)
             call rpn_put(rpnb,qcur)
          else
             istat=FZCERR_PARSER
          end if
        case(TID_IFNC,TID_UFNC)
          fnc=fnc+1
          call set_arg_read()
          select case(told)
          case(TID_KET,TID_FIG,TID_PAR,TID_UOP2,TID_UOP3)
             call push_implicit_mul()
          end select
          call set_narg
          call rpn_try_push(rpnb,qcur)
          pfnc_opened=rpnb%p_buf
          if(get_up32(tid)==FFID_DEINT) then
             if(sc/=0) then
                istat=FZCERR_PARSER
                exit
             end if
             sc=sc+1
             call rpn_put(rpnb,TID_ISTA,0,0)
          end if
       case default
          STOP "*** parse_formula: UNEXPECTED ERROR: unrecognized tid"
       end select
       if(rpnb%err/=0) istat=rpnb%err
       if(istat/=0) exit
       btidold=tidold
       tidold=tid
       btold=told
       told=t
    end do

    if(is_set(rpnc%flg%mode,RCM_DEBUG)) call dump_rpnb(rpnb)

    if(istat==0.and.terr/=0) then
       istat=terr
       p1=p1err
       p2=p1err
    end if

    rpnc%flg%mode=mode_save
    rpnc%flg%dmode=dmode_save

    if(istat==0) then
       istat=build_rpnc(rpnb,rpnc,nvbuf)
    else if(is_not_set(rpnc%flg%mode,RCPM_NO_WARN).and.istat>0) then
       call print_error(rpnb%expr(1:rpnb%len_expr),get_lo32(p1),get_lo32(p2))
    end if
    if(is_set(rpnc%flg%mode,RCM_DEBUG)) call dump_rpnc(rpnc)

    parse_formula=istat

  contains

    logical function set_com()
      use com, only: exe_mode_com,parse_command,is_mode_com
      integer tt,pp2,ii,cid
      integer pa1,pa2,tcom
      pp2=p2
      do ii=p2,p1,-1
         if(rpnb%expr(ii:ii)/=";") then
            pp2=ii
            exit
         end if
      end do
      tt=parse_command(rpnb%expr(p1:pp2),pa1,pa2)
      if(tt>0) then
         if(is_mode_com(tt,cid)) then
            tcom=TID_MCOM
         else
            tcom=TID_COM
         end if
         call rpn_put(rpnb, &
              get_i32(tcom,tt), &
              get_i32(p1,pa1), &
              get_i32(pp2,pa2))
         set_com=.true.
         com=.true.
         if(tcom==TID_MCOM) call exe_mode_com(cid,rpnc%flg)
      else
         set_com=.false.
     end if
   end function set_com

    subroutine echo(pp2)
      use misc, only: mess,is_not_set
      integer,intent(in)::pp2
      character*(*),parameter::echo_prompt="===> "
      if(is_not_set(rpnc%flg%mode,RCPM_NO_STDOUT)) &
           call mess(echo_prompt//restoreq(rpnb%expr(pfsta+1:pp2)))
    end subroutine echo

    subroutine init_stat()
      btold=TID_UNDEF
      told=TID_UNDEF
      tidold=TID_UNDEF
      btidold=TID_UNDEF
      pfnc_opened=0
      bc=0; kc=0; pc=0; ac=0; fc=0; oc=0; fnc=0; qc=0; cc=0
      amc=0; clc=0; tc=0; sc=0; otc=0; tclc=0
      p_q1=rpnb%p_que+1
      terr=0
      p1err=0
      p2err=0
    end subroutine init_stat

    subroutine set_idmy(iend)
      integer,intent(in)::iend
      integer ii
      character*32 dmy,dmy_lo,dmy_up
      integer ld,p1,p2,lc,ld_b
      ! FID_DEINT at p_buf-1
      p1=get_lo32(rpnb%buf(rpnb%p_buf-1)%p1)
      p2=get_lo32(rpnb%buf(rpnb%p_buf-1)%p2)
      ii=index(rpnb%expr(p1:p2),"_")+p1-1
      ld=p2-(ii+1)+1
      if(ld==0) then
         dmy="X" ! <<< default dmy argument
         ld=1
      else
         dmy=rpnb%expr(ii+1:get_lo32(rpnb%buf(rpnb%p_buf-1)%p2))
      end if
      dmy_lo=trim(dmy)//"_lo"
      dmy_up=trim(dmy)//"_up"
      ld_b=ld+3
      do ii=rpnb%p_que-1,p_q1,-1
         select case(rpnb%que(ii)%TID)
         case(TID_PAR)
            if(subexpr(rpnb,ii)==dmy(1:ld)) then
               rpnb%que(ii)%tid=TID_IVAR1
            else if(subexpr(rpnb,ii)==dmy_lo(1:ld_b)) then
               rpnb%que(ii)%tid=TID_IVAR1L
            else if(subexpr(rpnb,ii)==dmy_up(1:ld_b)) then
               rpnb%que(ii)%tid=TID_IVAR1U
            end if
         case(TID_ISTA)
            lc=(iend+1)-(ii+1)-1
            rpnb%que(ii)%tid  =get_i32(TID_ISTA,lc)
            rpnb%que(iend)%tid=get_i32(TID_IEND,lc)
            exit
         end select
      end do
    end subroutine set_idmy

    logical function check_narg_all(tend)
      integer,intent(in),optional::tend
      integer i
      check_narg_all=.true.
      do i=0,rpnb%p_buf-1
         if(present(tend).and.rpnb%buf(rpnb%p_buf-i)%tid==tend) return
         check_narg_all=check_narg(i,.true.)
         if(.not.check_narg_all) return
      end do
    end function check_narg_all

    logical function check_narg(pfnc_off,close,who)
      integer,intent(in)::pfnc_off
      logical,intent(in)::close
      integer,intent(inout),optional::who
      integer na,namax
      integer*2 w1,w2
      type(t_rrpnq),pointer::b
      if(rpnb%p_buf<=pfnc_off) then
         check_narg=.true.
         return
      end if
      b => rpnb%buf(rpnb%p_buf-pfnc_off)
      w1=int(get_up32(b%p1),kind=2) ! negative if none read
      w2=int(get_up32(b%p2),kind=2) ! positive if closed
      na=abs(w1)
      namax=abs(w2)
      select case(get_lo32(b%tid))
      case(TID_UFNC,TID_IFNC)
         if(w2<=0) then
            if(w1>=0) then
               if(w1>0) then
                  b%p1=get_i32(get_lo32(b%p1),na-1)
                  if(close) then
                     if(na/=1.and.namax/=narg_max) then
                        istat=FZCERR_TOO_FEW_ARG
                     end if
                     b%p2=get_i32(get_lo32(b%p2),namax)
                  else if(na==1) then
                     istat=FZCERR_TOO_MANY_ARG
                  end if
               else if(namax/=0) then
                  istat=FZCERR_TOO_MANY_ARG
               end if
            else
               istat=FZCERR_NOARG
            end if
         end if
      case(TID_BOP4)
         ! comma in function definition
         ! simply ignore this case
      case default
         ! return <<<<<<<
      end select
      if(istat==0) then
         check_narg=.true.
      else
         if(present(who)) who=b%tid
         check_narg=.false.
      end if
    end function check_narg

    subroutine set_arg_read()
      integer*2 w
      integer na
      if(pfnc_opened==0) return
      w=int(get_up32(rpnb%buf(pfnc_opened)%p1),kind=2)
      na=abs(w)
      rpnb%buf(pfnc_opened)%p1=get_i32(get_lo32(rpnb%buf(pfnc_opened)%p1),na)
      pfnc_opened=0
    end subroutine set_arg_read

    subroutine set_narg()
      use rpnlist, only: kth_na
      integer na
      integer fid
      fid=get_up32(tid)
      select case(t)
      case(TID_IFNC)
         if(fid<=FID_ARG0_END) then
            na=0
         else if(fid<=FID_ARG1_END) then
            na=1
         else if(fid<=FID_ARG2_END) then
            na=2
         else if(fid<=FID_ARG3_END) then
            na=3
         else if(fid<=FID_END) then
            na=narg_max ! arbitrary
         else if(fid<=FFID_ARG1_END) then
            na=1
         else if(fid<=FFID_ARG2_END) then
            na=2
         else if(fid<=FFID_ARG3_END) then
            na=3
         end if
      case(TID_UFNC)
         na=kth_na(rpnc%rl,get_up32(tid))
      end select
      p1=get_i32(p1,-na) ! negative for no arg read
      p2=get_i32(p2,-na)
    end subroutine set_narg

    logical function check_end()
      nvbuf=nvbuf+fc+oc+fnc+pc ! pc includes macro which needs vbuf
      if(nvbuf==0) then
         check_end=.true.
         if(.not.com) istat=RPNSTA_EMPTY
      else
         check_end=(kc-bc<=0).and.was_operand().and.(tc==tclc)
         if(.not.check_end) then
            istat=FZCERR_PARSER
         else if(pfasn/=0) then
            call set_par_dummy()
         end if
      end if
    end function check_end

    character*1 function next_char()
      if(p2>=rpnb%len_expr) then
         next_char=char(0)
      else
         next_char=rpnb%expr(p2+1:p2+1)
      end if
    end function next_char

    logical function is_fnc_asn()
      integer ii2,ii1
      type(t_rrpnq),pointer::q
      logical first
      is_fnc_asn=.false.
      if(amac) return
      if(ac==1.and.bc==1.and.kc==1.and.pc>=0.and.fc==0.and.p2<rpnb%len_expr) then
         if(next_char()==STID_DQ1) then
            ii1=p2+2
            ii2=index(rpnb%expr(p2+1:rpnb%len_expr),STID_DQ2)
         else
            ii1=p2+1
            ii2=index(rpnb%expr(p2+1:rpnb%len_expr),";")
         end if
         if(ii2==0) then
            ii2=rpnb%len_expr ! never come here
         else
            ii2=p2+1+ii2-1-1
         end if
         select case(get_lo32(rpnb%buf(rpnb%p_buf)%tid))
         case(TID_UFNC,TID_IFNC) ! overwrites intrinsic
            q => rpnb%buf(rpnb%p_buf)
            q%tid=TID_AFNC
            q%p1=get_i32(get_lo32(q%p1),ii1)
            q%p2=get_i32(get_lo32(q%p2),ii2)
            call rpn_pop(rpnb)
            is_fnc_asn=.true.
            first=.false.
         case default
            if(rpnb%que(p_q1)%tid==TID_PAR) then ! exclude TID_POP
               q => rpnb%que(p_q1)
               q%tid=TID_AFNC
               q%p1=get_i32(q%p1,ii1) ! function body in upper
               q%p2=get_i32(q%p2,ii2)
               pc=pc-1
               is_fnc_asn=.true.
               first=.true.
            end if
         end select
      end if
      if(.not.is_fnc_asn) return
      pfasn=rpnb%p_que ! end of dummy arg and func
      if(pc-1/=cc.or.set_dummy_par(first)<0) then
         is_fnc_asn=.false.
         istat=FZCERR_PARSER
      else
         p1=get_i32(p1,get_lo32(q%p1)) ! function definition in upper
         p2=get_i32(p2,p2-1)
      end if
    end function is_fnc_asn

    logical function check_assignable()
      check_assignable=.false.
      if(tidold/=TID_PAR) return ! exclude TID_POP
      select case(btold)
      case(TID_UNDEF,TID_BRA,TID_ASN)
      case(TID_SCL)
         ! it only happens in macro def
      case(TID_COM)
         ! prev SCL was eaten by COM
      case(TID_DQ1)
      case default
         return
      end select
      check_assignable=.true.
    end function check_assignable

    subroutine set_par_dummy()
      use misc, only: mess
      integer ii,jj
      integer did
      logical found
      did=0
      do ii=p_q1,pfasn
         if(rpnb%que(ii)%tid==TID_DPAR) then
            did=did+1
            found=.false.
            do jj=pfasn+1,rpnb%p_que ! <<<<
               if(rpnb%que(jj)%tid==TID_PAR) then
                  if(subexpr(rpnb,ii)&
                       ==subexpr(rpnb,jj)) then
                     rpnb%que(jj)%tid=TID_DPAR
                     rpnb%que(jj)%p1=get_i32(rpnb%que(jj)%p1,did) !<<<
                     found=.true.
                  end if
               end if
            end do
            if(.not.found) then
              call mess("*** Warning: Unused parameter: "//subexpr(rpnb,ii))
            end if
         end if
      end do
    end subroutine set_par_dummy

    integer function set_dummy_par(first)
      logical,intent(in)::first
      integer ii,ii1,ii2
      integer dummy_count
      dummy_count=0
      if(first) then
         ii1=p_q1+1
         ii2=rpnb%p_que
      else
         ii1=p_q1
         ii2=rpnb%p_que-1
      end if
      do ii=ii1,ii2
         if(rpnb%que(ii)%tid==TID_PAR) then
            rpnb%que(ii)%tid=TID_DPAR
            dummy_count=dummy_count+1
         else
            set_dummy_par=-1
            return
         end if
      end do
      set_dummy_par=dummy_count
    end function set_dummy_par

    integer function find_char(pos,chr)
      integer,intent(in)::pos
      character*1,intent(in)::chr
      find_char=index(rpnb%expr(pos:),chr)
      if(find_char==0) then
         find_char=rpnb%len_expr
      else
         find_char=find_char+pos-1
       end if
     end function find_char

    logical function was_operand()
      select case(told)
      case(TID_UNDEF,TID_BOP1,TID_BOP2,TID_BOP3,TID_UOP1,TID_AOP,&
           TID_ASN,TID_COMA,TID_TOP1,TID_COL,TID_SCL)
         was_operand=.false.
      case(TID_BRA)
         ! empty () comes here
         select case(btold)
         case(TID_UFNC,TID_IFNC)
            was_operand=.true.
         case default
            was_operand=.false.
         end select
      case default
         was_operand=.true.
      end select
    end function was_operand

    subroutine push_implicit_mul()
      call rpn_try_push(rpnb,TID_BOP4,0,0)
      oc=oc+1
    end subroutine push_implicit_mul

    subroutine push_implicit_bra()
      call rpn_push(rpnb,TID_IBRA,0,0)
    end subroutine push_implicit_bra

    subroutine init_rpnb()
      rpnb%expr => rpnc%expr
      rpnb%len_expr => rpnc%len_expr
      rpnb%cur_pos=pnext
      rpnb%old_tid=TID_UNDEF
      rpnb%p_buf=0
      rpnb%p_que=0
      rpnb%flg=rpnc%flg
      rpnb%err=0
    end subroutine init_rpnb

  end function parse_formula

  integer function set_formula(rpnc,f)
    use misc, only: cle_flg, set_flg
    type(t_rpnc),intent(inout)::rpnc
    character*(*),intent(in)::f
    integer i,k
    logical wc,dq,sq,dup,esc
    rpnc%rc=0
    call cle_flg(rpnc%flg%sta,RCS_SRC_SET)
    call cle_flg(rpnc%flg%sta,RCS_READY)
    if(.not.associated(rpnc%expr)) allocate(rpnc%expr)
    if(.not.associated(rpnc%len_expr)) allocate(rpnc%len_expr)
    dup=.false.
    wc=.true.
    dq=.false.
    sq=.false.
    esc=.false.
    k=0
    set_formula=0
    i=0
    do
       i=i+1
       if(i>len(f)) exit
       select case(f(i:i))
       case("\\")
          esc=.not.esc
          if(esc) cycle
       case("#")
          if(.not.sq.and..not.esc) exit
       case("$")
          wc=.false.
          if(.not.sq.and..not.esc) then
             select case(exp_mac())
             case(1)
                cycle
             case(-1)
                return
             end select
          end if
       case("'")
          wc=.false.
          if(.not.dq.and..not.esc) then
             select case(proc_q("'",sq))
             case(1)
                if(.not.putc(STID_SQ1)) return
                cycle
             case(2)
                if(.not.putc(STID_SQ2)) return
                cycle
             case(-1)
                cycle
             end select
          end if
       case("""")
          wc=.false.
          if(.not.sq.and..not.esc) then
             select case(proc_q("""",dq))
             case(1)
                if(.not.putc(STID_DQ1)) return
                cycle
             case(2)
                if(.not.putc(STID_DQ2)) return
                cycle
             case(-1)
                cycle
             end select
          end if
       case(" ","\t")
          if(.not.sq.and.wc.and..not.esc) cycle
          wc=.true.
          if(.not.putc(" ")) return
          cycle
       case(char(0))
          exit
       case default
          wc=.false.
       end select
       if(.not.putc(f(i:i))) return
    end do
    if(wc) k=k-1
    if(sq) then
       if(.not.putc(STID_SQ2)) return
    else if(dq) then
       if(.not.putc(STID_DQ2)) return
    end if
    rpnc%len_expr=k
    if(k==0) then
       set_formula=FZCERR_EMPTY_INPUT
    else
       call set_flg(rpnc%flg%sta,RCS_SRC_SET)
    end if

  contains

    integer function proc_q(c,q)
      character*1,intent(in)::c
      logical,intent(inout)::q
      proc_q=0
      if(.not.dup) then
         if(q) then
            call check_dup(c)
            if(.not.dup) then
               if(i<len(f)) then
                  if(is_quote(f(i+1:i+1))) then
                     proc_q=2
                  end if
               else
                  proc_q=2
               end if
            end if
         else
            if(i>1) then
               if(is_quote(f(i-1:i-1))) then
                  proc_q=1
               end if
            else
               proc_q=1
            end if
         end if
         if(.not.dup.and.proc_q/=0) q=.not.q
      else
         dup=.false.
         proc_q=-1
      end if
    end function proc_q

    logical function is_quote(c)
      use misc, only: is_alpha,is_numeric
      character*1,intent(in)::c
      is_quote=(.not.is_alpha(c) &
           .and..not.is_numeric(c) &
           .and.c/="""" &
           .and.c/="'")
    end function is_quote

    logical function putc(c)
      character*1,intent(in)::c
      k=k+1
      if(k>LEN_FORMULA_MAX) then
         set_formula=FZCERR_TOO_LONG_STR
         putc=.false.
         return
      end if
      rpnc%expr(k:k)=c
      putc=.true.
      esc=.false.
    end function putc

    subroutine check_dup(c)
      character*1,intent(in)::C
      if(i<len(f)) then
         if(f(i+1:i+1)==c) then
            dup=.true.
         end if
      end if
    end subroutine check_dup

    integer function exp_mac()
      use rpnlist, only: rpnlist_count,find_rpnlist,t_rpnm
      use misc, only: is_alpha
      use slist, only: get_str_ptr
      use memio, only: cpstr
      integer pp1,pp2,jj,p,l
      type(t_rpnm),pointer::rpnm
      exp_mac=0
      if(rpnlist_count(rpnc%rl)==0) return
      pp1=i+1
      if(pp1>len(f)) return
      if(.not.is_alpha(ichar(f(pp1:pp1)))) return
      pp2=get_end_of_par(len(f),f,pp1)
      jj=find_rpnlist(rpnc%rl,f(pp1:pp2),SC_MAC,rpnm)
      if(jj<=0) return
      if(get_str_ptr(rpnm%pnames,ent=1,ptr=p,len=l)/=0) return
      if(k+l+1>LEN_FORMULA_MAX) then
         set_formula=FZCERR_TOO_LONG_STR
         exp_mac=-1
         return
      end if
      k=k+1
      rpnc%expr(k:)=cpstr(p,l)
      k=k+l-1
      i=i+(pp2-pp1+1)
      exp_mac=1
    end function exp_mac

  end function set_formula

end module rpnp
