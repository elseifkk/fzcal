module rpnp
  use slist
  use plist
  use fpio
  use rpnd
  implicit none

#define is_set(x) (iand(rpnc%opt,(x))/=0)
#define is_uset(x) (iand(rpnc%opt,(x))==0)
#define is_setb(x) (iand(rpnb%opt,(x))/=0)
#define is_usetb(x) (iand(rpnb%opt,(x))==0)
#define set_opt(x) rpnc%opt=ior(rpnc%opt,(x))
#define cle_opt(x) rpnc%opt=iand(rpnc%opt,not(x))

  character*(*),parameter::LOPS_NOT ="not"
  character*(*),parameter::LOPS_AND ="and"
  character*(*),parameter::LOPS_OR  ="or"
  character*(*),parameter::LOPS_EQ  ="eq"
  character*(*),parameter::LOPS_NEQ ="neq" 

  character(*),parameter::spars=&
       achar(1)//"n"//&
       achar(3)//"ave"//&
       achar(3)//"var"//&
       achar(3)//"sum"//&
       achar(4)//"sum2"//&
       achar(4)//"uvar"//&
       achar(5)//"ave_y"//&
       achar(5)//"var_y"//&
       achar(5)//"sum_y"//&
       achar(6)//"sum2_y"//&
       achar(6)//"uvar_y"//&
       achar(6)//"sum_xy"//&
       achar(1)//"a"//&
       achar(1)//"b"//&
       achar(0)
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
  character*(*),parameter::int_fncs=&
       achar(3)//"ran"//&
       achar(3)//"sin"//&
       achar(3)//"cos"//&
       achar(3)//"tan"//&
       achar(4)//"sinh"//&
       achar(4)//"cosh"//&
       achar(4)//"tanh"//&
       achar(4)//"asin"//&
       achar(4)//"acos"//&
       achar(4)//"atan"//&
       achar(5)//"asinh"//&
       achar(5)//"acosh"//&
       achar(5)//"atanh"//&
       achar(3)//"exp"//&
       achar(3)//"log"//&
       achar(2)//"ln"//&
       achar(4)//"sqrt"//&
       achar(4)//"cbrt"//&
       achar(3)//"abs"//&
       achar(3)//"int"//&
       achar(4)//"frac"//&
       achar(5)//"conjg"//&
       achar(4)//"nint"//&
       achar(2)//"re"//&
       achar(2)//"im"//&
       achar(3)//"mag"//&
       achar(3)//"arg"//&
       achar(5)//"gamma"//&
       achar(6)//"lgamma"//&
       achar(3)//"psy"//&
       achar(3)//"mod"//&
       achar(4)//"gami"//&
       achar(5)//"deint"//&
       achar(3)//"min"//&
       achar(3)//"max"//&
       achar(3)//"sum"//&
       achar(3)//"ave"//&
       achar(3)//"var"//&
       achar(4)//"uvar"//&
       achar(4)//"sum2"//&
       achar(0)
  integer,parameter::FID_RAN       =  1
  integer,parameter::FID_ARG0_END  =  1 !<<<<<<<<
  integer,parameter::FID_SIN       =  2
  integer,parameter::FID_COS       =  3
  integer,parameter::FID_TAN       =  4
  integer,parameter::FID_SINH      =  5
  integer,parameter::FID_COSH      =  6
  integer,parameter::FID_TANH      =  7
  integer,parameter::FID_ASIN      =  8
  integer,parameter::FID_ACOS      =  9
  integer,parameter::FID_ATAN      = 10
  integer,parameter::FID_ASINH     = 11
  integer,parameter::FID_ACOSH     = 12
  integer,parameter::FID_ATANH     = 13
  integer,parameter::FID_EXP       = 14
  integer,parameter::FID_LOG       = 15
  integer,parameter::FID_LN        = 16
  integer,parameter::FID_SQRT      = 17
  integer,parameter::FID_CBRT      = 18
  integer,parameter::FID_ABS       = 19
  integer,parameter::FID_INT       = 20
  integer,parameter::FID_FRAC      = 21
  integer,parameter::FID_CONJG     = 22
  integer,parameter::FID_NINT      = 23
  integer,parameter::FID_RE        = 24
  integer,parameter::FID_IM        = 25
  integer,parameter::FID_MAG       = 26
  integer,parameter::FID_ARG       = 27
  integer,parameter::FID_GAMMA     = 28
  integer,parameter::FID_LGAMMA    = 29
  integer,parameter::FID_PSY       = 30
  integer,parameter::FID_ARG1_END  = 30 !<<<<<<<<
  integer,parameter::FID_MOD       = 31
  integer,parameter::FID_GAMI      = 32
  integer,parameter::FID_ARG2_END  = 32 !<<<<<<<<
  integer,parameter::FID_DEINT     = 33
  integer,parameter::FID_ARG3_END  = 33 !<<<<<<<<
  integer,parameter::FID_MIN       = 34
  integer,parameter::FID_MAX       = 35
  integer,parameter::FID_SUM       = 36
  integer,parameter::FID_AVE       = 37
  integer,parameter::FID_VAR       = 38
  integer,parameter::FID_UVAR      = 39
  integer,parameter::FID_SUM2      = 40

contains
  
  integer function strip(s)
    character*(*),intent(inout)::s
    integer i,k,wc
    k=0
    wc=0
    do i=1,len(s)
       select case(s(i:i))
       case(" ","\t")
          wc=wc+1
          if(wc>1) cycle
       case(achar(0))
          exit
       case default
          wc=0
       end select
       k=k+1
       if(k/=i) s(k:k)=s(i:i)
    end do
    if(wc/=0) k=k-1
    strip=k
  end function strip

  integer function get_end_of_fig(rpnb,k_)
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
    
    if(is_usetb(RPNCOPT_INM)) then
       pf=loc(is_numeric)
    else if(is_setb(RPNCOPT_IBIN)) then
       pf=loc(is_bin_number)
    else if(is_setb(RPNCOPT_IOCT)) then
       pf=loc(is_oct_number)
    else if(is_setb(RPNCOPT_IHEX)) then
       pf=loc(is_hex_number)
    end if

    do
       if(k>=rpnb%len_expr) then
          get_end_of_fig=k
          return
       end if
       k=k+1
       a=ichar(rpnb%expr(k:k))
       if(.not.is_valid(a)) then !numeric(a)) then
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
  
  integer function get_end_of_par(rpnb,k_)
    type(t_rpnb),intent(in)::rpnb
    integer,intent(in)::k_
    integer a,k
    k=k_
    do
       if(k>=rpnb%len_expr) then
          get_end_of_par=k
          return
       end if
       k=k+1
       a=ichar(rpnb%expr(k:k))
       if(.not.is_alpha(a).and..not.is_number(a).and..not.is_symbol(a)) then
          get_end_of_par=k-1
          return
       end if
    end do
  end function get_end_of_par
  
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

  logical function is_symbol(a)
    integer,intent(in)::a
    select case(a)
    case(95,36) ! _, $
       is_symbol=.true.
    case default
       is_symbol=.false.
    end select
  end function is_symbol

  logical function is_alpha(a)
    integer,intent(in)::a
    integer b
    b=ior(a,32)
    is_alpha=(b>=97.and.b<=122)
  end function is_alpha
  
  logical function is_hex_number(a)
    integer,intent(in)::a
    is_hex_number=(a>=65.and.a<=70).or.(a>=97.and.a<=102).or.is_number(a)
  end function is_hex_number

  logical function is_oct_number(a)
    integer,intent(in)::a
    is_oct_number=(a>=48.and.a<=55)
  end function is_oct_number

  logical function is_bin_number(a)
    integer,intent(in)::a
    is_bin_number=(a>=48.and.a<=49)
  end function is_bin_number

  logical function is_number(a)
    integer,intent(in)::a
    is_number=(a>=48.and.a<=57)
  end function is_number
  
  logical function is_numeric(a)
    integer,intent(in)::a
    is_numeric=(is_number(a).or.a==46)
  end function is_numeric
  
  logical function is_ppar(a,ent)
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
    if(k/=0) then
       if(present(ent)) ent=k
       is_int_fnc=.true.
    else
       is_int_fnc=.false.
    end if
  end function is_int_fnc

  integer function get_index(sl,a)
    character*(*),intent(in)::sl,a
    integer len,lenf
    integer i,k,j
    len=len_trim(a)
    k=1
    i=0
    get_index=0
    do
       i=i+1
       lenf=ichar(sl(k:k))
       if(lenf==0) exit
       if(lenf==len) then
          j=k+1
          if(sl(j:j+len-1)==a(1:len)) then
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
    case("*","/")
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
    case default
       get_tid=TID_UNDEF
    end select
  end function get_tid

  logical function is_usr_fnc(sl,f,ent)
    type(t_slist),intent(in)::sl
    character*(*),intent(in)::f
    integer,intent(out),optional::ent
    integer k
    k=find_str(sl,f,target_code=SC_FNC)
    if(present(ent)) ent=k
    if(k/=0) then
       is_usr_fnc=.true.
    else
       is_usr_fnc=.false.
    end if
  end function is_usr_fnc
  
  integer function get_next(rpnb,p1,p2,sl)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(out)::p1,p2
    type(t_slist),intent(in)::sl
    integer k,t,kf
    
    k=rpnb%cur_pos
    if(k>=rpnb%len_expr.or.k<0) then
       get_next=TID_FIN
       return
    end if
    
    k=k+1
    p1=k
    p2=k
    t=get_tid(rpnb%expr(k:k))
    if(t==TID_UNDEF) then
       if(is_alpha(ichar(rpnb%expr(k:k)))) then
          t=TID_PARU
       else if(is_numeric(ichar(rpnb%expr(k:k)))) then
          t=TID_FIG
       end if
    end if
    
    select case(t)
    case(TID_UOP2U)
       if(next_char(1)=="!") p2=p2+1
       t=TID_UOP2
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
          case(TID_BRA,TID_BOP3,TID_ASN,TID_AOP,&
               TID_COMA,TID_TOP1,TID_COL,TID_SCL,TID_UNDEF) ! plus in (+, ^+ and e+ are unary
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
             if(rpnb%expr(k:k)=="*") then
                t=TID_BOP3
                p2=k+1
             end if
          case("/")
             if(rpnb%expr(k:k)=="/") then
                t=TID_BOP3
                p2=k+1
             end if
          end select
       end if
       if(t==TID_BOP2U) t=TID_BOP2
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
          p2=get_end_of_par(rpnb,p1+1)
          if(p1+1==p2.and.is_ppar(rpnb%expr(p2:p2),k)) then
             t=get_i32(TID_PAR,k)
          end if
       end if
    case(TID_PARU)
       if(rpnb%old_tid==TID_FIG.and.rpnb%expr(k:k)=="e") then
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
             if(.not.is_lop(rpnb%expr(p1:p2),t)&
                  .and..not.(is_setb(RPNCOPT_STA).and.is_spar(rpnb%expr(p1:p2)))) then
                k=p2+1
                if(rpnb%expr(k:k)=="(") then
                   if(is_usr_fnc(sl,rpnb%expr(p1:p2),kf)) then
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
       call skip_tid(t)
       if(p2==rpnb%len_expr) t=TID_FIN
    end select
    
    rpnb%old_tid=t
    get_next=t
    rpnb%cur_pos=p2
    
  contains
    
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

    character*1 function next_char(inc)
      integer,intent(in)::inc
      integer kk
      kk=k+inc
      next_char=rpnb%expr(kk:kk)
    end function next_char

  end function get_next
  
#define _EXPR_(i) rpnb%expr(get_lo32(rpnb%que(i)%p1):get_lo32(rpnb%que(i)%p2))
#define _UEXPR_(i) rpnb%expr(get_up32(rpnb%que(i)%p1):get_up32(rpnb%que(i)%p2))

  integer function add_rpnm_entry(rpnc,rpnb,i,code,k)
    type(t_rpnc),intent(inout)::rpnc
    type(t_rpnb),intent(in)::rpnb
    integer i
    integer,intent(in)::code
    integer,intent(out)::k
    integer istat
    istat=try_add_str(rpnc%rl%s,_EXPR_(i),code,k)
    if(istat==0) then
       if(k>size(rpnc%rl%rpnm)) then
          write(*,*) "add_rpnm_entry faild: buffer overflow"
          istat=RPNERR_MEMOV
       else
          rpnc%que(i)%cid=k
       end if
    else
       write(*,*) "*** try_add_str failed: code = ",istat
       istat=RPNERR_ADDSTR
    end if
    add_rpnm_entry=istat
  end function add_rpnm_entry
    
  integer function set_function(rpnb,rpnc,k1)
    type(t_rpnb),intent(in)::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::k1
    type(t_rpnm),pointer::rpnm
    integer istat
    integer i
    integer kf,km,ka,ke
    integer ac,vc,pc,plen
    integer tc

    ke=find_end()
    do i=k1,ke
       if(rpnc%que(i)%tid/=TID_AFNC) cycle
       istat=add_rpnm_entry(rpnc,rpnb,i,SC_FNC,kf)
       if(istat/=0) then
          set_function=istat
          exit
       end if
       rpnc%que(i)%cid=kf
       rpnm=>rpnc%rl%rpnm(kf)
       if(allocated(rpnm%que)) deallocate(rpnm%que)
       if(allocated(rpnm%vbuf)) deallocate(rpnm%vbuf)       
       if(.not.allocated(rpnm%na)) allocate(rpnm%na)
       if(i==k1) then
          ! | k1=i |   |   | ... | km |       | ke |
          ! | f    | x | y | arg | *  | codes | =  |
          km=find_implicit_mul() ! must be found
          if(km==0) stop "*** UNEXPECTED ERROR in set_function"
          ac=km-k1+1-2 ! number of arguments
       else
          ! | k1 | ... | km=i |       | ke |
          ! | x  | arg | f    | codes | =  |
          km=i
          ac=km-k1+1-1
       end if
       ka=ke ! must be asn
       tc=(ke-1)-(km+1)+1 ! que must end with =
       rpnm%na=ac
       if(tc==0) return !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       rpnm%pars=>rpnc%pars
       rpnm%answer=>rpnc%answer
       rpnm%tmpans=>rpnc%tmpans
       allocate(rpnm%que(tc))
       rpnm%que(1:tc)=rpnc%que(km+1:ke-1)
       call count_var()
       call init_pnames()
       if(vc>0) allocate(rpnm%vbuf(vc))
       call cp_vbuf()
       call trim_slist(rpnm%pnames)
       exit
    end do
    rpnc%que(k1:ke)%tid=TID_NOP
    set_function=istat

  contains
    
    integer function find_end()
      integer ii
      find_end=size(rpnc%que)
      do ii=k1,size(rpnc%que)
         if(rpnc%que(ii)%tid==TID_END) then
            find_end=ii-1 ! <<<<<<<<<<<<<<<
            return
         end if
      end do
    end function find_end

    subroutine init_pnames()
      integer istat
      if(allocated(rpnm%pnames)) deallocate(rpnm%pnames)
      allocate(rpnm%pnames)
      plen=plen+get_up32(rpnb%que(i)%p2)-get_up32(rpnb%que(i)%p1)+1&
           +get_up32(rpnb%que(ka)%p2)-get_up32(rpnb%que(ka)%p1)+1
      rpnm%pnames=init_slist(plen+(pc+1)*LEN_SLIST_HDR)
      istat=add_str(rpnm%pnames,_UEXPR_(i),SC_RO)
      if(istat/=0) stop "hoge"
      istat=add_str(rpnm%pnames,_UEXPR_(ka),SC_RO)
      if(istat/=0) stop "hogehoge"
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
      integer ii,jj
      integer kp
      integer istat
      complex(cp) v
      pointer(pv,v)
      if(.not.allocated(rpnm%p_vbuf)) allocate(rpnm%p_vbuf)
      rpnm%p_vbuf=0
      do ii=1,size(rpnm%que)
         select case(rpnm%que(ii)%tid)
         case(TID_VAR,TID_PAR,TID_CPAR)
         case default
            cycle
         end select
         jj=ii+km !<<<<<<<<<<<<<<<<
         if(rpnb%que(jj)%tid/=TID_FIG) then ! par
            istat=try_add_str(rpnm%pnames,_EXPR_(jj),SC_RO,ent=kp)            
            if(istat/=0) stop "hego"
            rpnm%que(ii)%tid=rpnb%que(jj)%tid
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

    subroutine count_var()
      integer ii
      vc=0
      pc=0
      plen=0
      do ii=km,ke
         select case(rpnc%que(ii)%tid)
         case(TID_VAR,TID_PAR,TID_CPAR) 
            if(rpnb%que(ii)%tid/=TID_FIG) then ! par
               plen=plen+rpnb%que(ii)%p2-rpnb%que(ii)%p1+1
               pc=pc+1
            end if
            vc=vc+1
         case(TID_DPAR)
            vc=vc+1
         case(TID_MAC)
            vc=vc+1
         end select
      end do
    end subroutine count_var

  end function set_function

  integer function set_macro(rpnb,rpnc,k1)
    type(t_rpnb),intent(in)::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::k1
    type(t_rpnm),pointer::rpnm
    integer k,km,ke
    integer i
    integer tc,vc,plen,pc
    integer istat
    ke=find_end()
    do i=k1,ke
       if(rpnc%que(i)%tid/=TID_AMAC) cycle
       k=find_qend() 
       istat=add_rpnm_entry(rpnc,rpnb,i,SC_MAC,km)
       if(istat/=0) then
          set_macro=istat
          exit
       end if
       rpnc%que(i)%cid=km
       rpnm=>rpnc%rl%rpnm(km)
       if(allocated(rpnm%que)) deallocate(rpnm%que)
       if(allocated(rpnm%vbuf)) deallocate(rpnm%vbuf)
       tc=(k-1)-(i+1)+1
       if(tc==0) exit ! empty macro
       rpnm%pars=>rpnc%pars
       rpnm%answer=>rpnc%answer
       rpnm%tmpans=>rpnc%tmpans
       allocate(rpnm%que(tc))
       call count_var()
       call init_pnames()
       rpnm%que(1:tc)=rpnc%que(i+1:i+1+tc-1)
       if(vc>0) allocate(rpnm%vbuf(vc))
       call cp_vbuf()
       rpnc%que(i+1:k)%tid=TID_NOP
       rpnc%que(i)%tid=TID_MAC
       call trim_slist(rpnm%pnames)
    end do

    set_macro=0

  contains
    
    integer function find_end()
      integer ii
      find_end=size(rpnc%que)
      do ii=k1,size(rpnc%que)
         if(rpnc%que(ii)%tid==TID_END) then
            find_end=ii-1
         end if
      end do
    end function find_end

    subroutine init_pnames()
      if(allocated(rpnm%pnames)) deallocate(rpnm%pnames)
      allocate(rpnm%pnames)
      plen=plen+get_up32(rpnb%que(i)%p2)-get_up32(rpnb%que(i)%p1)+1
      rpnm%pnames=init_slist(plen+(pc+1)*LEN_SLIST_HDR)
      istat=add_str(rpnm%pnames,_UEXPR_(i),ior(SC_RO,SC_MAC))
      if(istat/=0) stop "hogemac" ! <<<<<<<<<<<<<<<<<<
    end subroutine init_pnames

    subroutine cp_vbuf()
      ! Reverts TID_VAR to FIG,PAR,APAR
      ! FIG and PAR will have pointer to vbuf
      ! APAR will have pointer to pars%v
      integer ii,jj
      integer kp
      complex(cp) v
      pointer(pv,v)
      if(.not.allocated(rpnm%p_vbuf)) allocate(rpnm%p_vbuf)
      rpnm%p_vbuf=0
      do ii=1,tc
         select case(rpnm%que(ii)%tid)
         case(TID_VAR,TID_PAR,TID_CPAR)
         case default
            cycle
         end select
         jj=ii+i
         if(rpnb%que(jj)%tid/=TID_FIG) then ! par
            istat=try_add_str(rpnm%pnames,_EXPR_(jj),SC_RO,ent=kp)            
            if(istat/=0) stop "hego" ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            rpnm%que(ii)%tid=rpnb%que(jj)%tid ! <<< TID_PAR or TID_APAR
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
    
    subroutine count_var()
      integer ii
      vc=0
      plen=0
      pc=0
      do ii=i+1,k-1
         select case(rpnc%que(ii)%tid)
         case(TID_VAR,TID_PAR,TID_CPAR) 
            if(rpnb%que(ii)%tid/=TID_FIG) then ! par
               plen=plen+rpnb%que(ii)%p2-rpnb%que(ii)%p1+1
               pc=pc+1
               if(rpnb%que(ii)%tid/=TID_APAR) vc=vc+1
            else
               vc=vc+1
            end if
         case(TID_MAC)
            vc=vc+1 !<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         end select
      end do
    end subroutine count_var
    
    integer function find_qend()
      integer ii
      find_qend=0
      do ii=i+1,ke
         if(rpnc%que(ii)%tid==TID_QEND) then
            find_qend=ii
            return
         end if
      end do
    end function find_qend
    
  end function set_macro
      
  subroutine print_error(e,p1,p2)
    character*(*),intent(in)::e
    integer,intent(in)::p1,p2
    write(*,*) "*** Syntacs Error at: "
    write(*,*) trim(e)
    if(p1<=0.or.p2<=0) return
    write(*,*) repeat(" ",p1-1)//repeat("^",abs(p2)-p1+1) ! some return with negative p2
  end subroutine print_error
  
  integer function build_rpnc(rpnb,rpnc)
    type(t_rpnb),intent(in),target::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    real(rp) x
    integer istat
    integer i,k
    logical amac,afnc
    integer p_q1
    type(t_rpnq),pointer::q
    type(t_rrpnq),pointer::qq
    logical dup

    p_q1=1
    amac=.false.
    afnc=.false.
    istat=0

    if(associated(rpnc%que).and.size(rpnc%que)>0) deallocate(rpnc%que)
    allocate(rpnc%que(rpnb%p_que))
    rpnc%rc=0
    rpnc%que(:)%tid=TID_UNDEF
    rpnc%que(:)%cid=0 ! dump_rpnc might refer unset cid as a pointer 
    rpnc%p_vbuf=0
    do i=1,size(rpnc%que)
       istat=0
       q=>rpnc%que(i)
       qq=>rpnb%que(i)
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
          q%tid=get_i32(TID_AOP,2)
          q%cid=get_aid()
       case(TID_IFNC)
          q%cid=get_fid(get_up32(qq%tid))
          select case(get_up32(qq%p2))
          case(0)
             q%tid=TID_OP
             if(get_up32(qq%p1)/=0) istat=RPNERR_NARG
             cycle
          case(1) 
             q%tid=get_i32(TID_OP,1)
          case(2)
             q%tid=get_i32(TID_OP,2)
          case(3)
             q%tid=get_i32(TID_OP,3)
          case(narg_max)
             q%tid=get_i32(TID_OPN,narg_max-get_up32(qq%p1))
             cycle
          end select
          if(get_up32(qq%p1)/=0) istat=RPNERR_NARG ! <<<<<<<<<
       case(TID_UFNC)
          q%tid=TID_UFNC
          q%cid=get_up32(qq%tid)
          if(get_up32(qq%p1)/=0) istat=RPNERR_NARG ! <<<<<<<<<
       case(TID_APAR) ! asign a parameter.
          q%tid=TID_PAR
          istat=add_par_by_entry(rpnc%pars,_EXPR_(i),k)
          if(istat==0) then
             q%cid=get_par_loc(rpnc%pars,k)
          else
             if(istat==PLERR_RDONL) then
                write(*,*) "*** Parameter is read-only: "//_EXPR_(i)
             else
                write(*,*) "*** add_par_by_entry failed: code = ",istat
             end if
             istat=RPNERR_ADDPAR
          end if
          !
          ! operands
          !
       case(TID_FIG)
          q%tid=TID_VAR
          istat=read_fig()
          if(istat/=0) istat=RPNCERR_INVFIG
          call put_vbuf(rpnc,i,x)
       case(TID_PAR)
          k=get_up32(qq%tid)
          if(k/=0) then
             q%tid=TID_POP
             q%cid=k
             cycle
          end if
          if(is_set(RPNCOPT_STA).and.is_spar(_EXPR_(i),k)) then
             q%tid=TID_SOP
             q%cid=get_sid(k)
             cycle
          end if
          if(rpnc%rl%s%n>0) then
             ! find the macro first
             k=find_str(rpnc%rl%s,_EXPR_(i))
             if(k>0) then
                q%tid=TID_MAC
                q%cid=k
                cycle
             end if
          end if
          istat=find_par(rpnc%pars,_EXPR_(i),ent=k)
          if(amac.and.istat/=0.and.is_uset(RPNCOPT_NO_AUTO_ADD_PAR)) then
             ! par may not already exist
             istat=add_par_by_entry(rpnc%pars,_EXPR_(i),k)
             if(istat/=0) then
                write(*,*) "*** add_par_by_entry failed: code = ",istat
                istat=RPNERR_ADDPAR
             end if
          end if
          if(istat==0) then
             q%tid=TID_PAR
             q%cid=get_par_loc(rpnc%pars,k,dup)
             if(q%cid==0) then
                write(*,*) "*** get_par failed: code = ",istat
                istat=RPNERR_GETPAR
             else
                if(dup) q%tid=TID_CPAR
             end if
          else
             write(*,*) "*** No such parameter: "//_EXPR_(i)
             istat=RPNERR_NOPAR
          end if
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
          !
          ! macro assignment
          !
       case(TID_AMAC)
          amac=.true.
          q%tid=TID_AMAC
          q%cid=TID_UNDEF
       case(TID_MASN)
          q%tid=TID_NOP
          q%cid=TID_UNDEF
       case(TID_QEND)
          q%tid=TID_QEND
          q%cid=0
          istat=set_macro(rpnb,rpnc,p_q1)
          amac=.false.
          !
          ! special TIDs
          !
       case(TID_DLM1)
          q%tid=TID_DLM1
          q%cid=0 ! will be set later in find_delim
       case(TID_DLM2)
          q%tid=TID_DLM2
          q%cid=qq%p2 ! bc-kc 
       case(TID_SCL)
          q%tid=TID_END
          q%cid=0
          if(amac) istat=set_macro(rpnb,rpnc,p_q1)
          if(afnc) istat=set_function(rpnb,rpnc,p_q1)
          ! istat=RPNSTA_FNCSET
          amac=.false.
          afnc=.false.
          p_q1=i+1
       case(TID_COL) ! only for dat mode
          q%tid=TID_COL
          q%cid=0
       case default
          CALL DUMP_RPNB(RPNB)
          WRITE(*,*) "que=",i,"tid=",qq%tid
          STOP "*** UNEXPECTED ERROR in build_rpnc"
       end select
       if(istat/=0) then
          if(is_set(RPNCOPT_NO_WARN)) &
               call print_error(rpnb%expr(1:rpnb%len_expr),get_lo32(qq%p1),get_lo32(qq%p2)) 
          exit
       end if
    end do

    if(istat==0) then
       if(amac) istat=set_macro(rpnb,rpnc,p_q1)
       if(afnc) istat=set_function(rpnb,rpnc,p_q1)
    end if

    if(afnc.and.istat==0) istat=RPNSTA_FNCSET
    if(istat==0) rpnc%opt=ior(rpnc%opt,RPNCOPT_READY)
    
    build_rpnc=istat

  contains

    integer function read_fig()
      integer f
      if(is_uset(RPNCOPT_INM)) then
         read(_EXPR_(i),*,iostat=read_fig) x
         return
      else if(is_set(RPNCOPT_IBIN)) then
         f=DISP_FMT_BIN
      else if(is_set(RPNCOPT_IOCT)) then
         f=DISP_FMT_OCT
      else if(is_set(RPNCOPT_IHEX)) then
         f=DISP_FMT_HEX
      end if
      x=real(atoi(_EXPR_(i),f,read_fig),kind=rp)
    end function read_fig

    subroutine find_delim(pos)
      integer,intent(in)::pos
      integer ii
      do ii=1,size(rpnb%que)
         if(rpnb%que(ii)%tid==TID_DLM1&
              .and.rpnb%que(ii)%p1==pos) then
            rpnc%que(ii)=q
            rpnc%que(ii)%cid=get_i32(rpnc%que(ii)%cid,i)
            q%tid=TID_DLM1
            q%cid=rpnb%que(ii)%p2 ! = bc-kc
            return
         end if
      end do
      istat=RPNERR_PARSER
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
         STOP "*** UNEXPECTED ERROR in get_sid" 
      end select
    end function get_sid

#define isdeg is_set(RPNCOPT_DEG)

    integer function get_fid(fid)
      use zmath
      integer,intent(in)::fid
      select case(fid)
      case(FID_RAN)
         get_fid=loc(zm_ran)
      case(FID_SIN)
         if(isdeg) then
            get_fid=loc(zm_sind)
         else
            get_fid=loc(zm_sin)
         end if
      case(FID_COS)
         if(isdeg) then
            get_fid=loc(zm_cosd)
         else
            get_fid=loc(zm_cos)
         end if
      case(FID_TAN)
         if(isdeg) then
            get_fid=loc(zm_tand)
         else
            get_fid=loc(zm_tan)
         end if
      case(FID_ASIN)
         if(isdeg) then 
            get_fid=loc(zm_asind)
         else
            get_fid=loc(zm_asin)
         end if
      case(FID_ACOS)
         if(isdeg) then
            get_fid=loc(zm_acosd)
         else
            get_fid=loc(zm_acos)
         end if
      case(FID_ATAN)
         if(isdeg) then
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
      case(FID_DEINT)
         get_fid=loc(zm_deint)
      case default
         STOP "*** UNEXPECTED ERROR in get_fid" 
      end select
    end function get_fid

    integer function get_aid()
      use zmath
      get_aid=AID_NOP ! to avoid warning
      select case(_EXPR_(i))
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
         STOP "*** UNEXPECTED ERROR in get_aid"
      end select
    end function get_aid

    integer function get_oid1()
      use zmath
      get_oid1=OID_NOP
      select case(_EXPR_(i))
      case("+")
         get_oid1=loc(zm_nop) 
      case("-")
         if(is_uset(RPNCOPT_RATIO)) then
            get_oid1=loc(zm_neg) 
         else
            get_oid1=loc(zm_neg_f)
         end if
      case("!")
         get_oid1=loc(zm_fac) 
      case("!!")
         get_oid1=loc(zm_dfac)
      case("++")
         if(is_uset(RPNCOPT_RATIO)) then
            get_oid1=loc(zm_inc)
         else
            get_oid1=loc(zm_inc_f)
         end if
      case("--")
         if(is_uset(RPNCOPT_RATIO)) then
            get_oid1=loc(zm_dec)
         else
            get_oid1=loc(zm_dec_f)
         end if
      case default
         STOP "*** UNEXPECTED ERROR in get_oid1"
      end select
    end function get_oid1

    integer function get_loid1()
      use zmath
      select case(_EXPR_(i)) 
      case("~",LOPS_NOT)
         get_loid1=LOID_NOT
      case default
         STOP "*** UNEXPECTED ERROR in get_loid1"
      end select      
    end function get_loid1

    integer function get_oid2()
      use zmath
      if(qq%tid==TID_BOP4) then
         get_oid2=loc(zm_mul)
         return
      end if
      get_oid2=OID_NOP
      select case(_EXPR_(i))
      case("+")
         if(is_uset(RPNCOPT_RATIO)) then
            get_oid2=loc(zm_add)   
         else
            get_oid2=loc(zm_add_f)
         end if
      case("-")
         if(is_uset(RPNCOPT_RATIO)) then
            get_oid2=loc(zm_sub)   
         else
            get_oid2=loc(zm_sub_f)   
         end if
      case("*")
         if(is_uset(RPNCOPT_RATIO)) then
            get_oid2=loc(zm_mul)
         else
            get_oid2=loc(zm_mul_f)
         end if
      case("/")
         if(is_uset(RPNCOPT_RATIO)) then
            get_oid2=loc(zm_div)
         else
            get_oid2=loc(zm_div_f)
         end if
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
         STOP "*** UNEXPECTED ERROR in get_oid2"
      end select
    end function get_oid2

    integer function get_loid2()
      use zmath
      select case(_EXPR_(i))
      case(LOPS_AND)
         get_loid2=LOID_AND
      case(LOPS_OR)
         get_loid2=LOID_OR
      case(LOPS_EQ)
         get_loid2=LOID_EQ
      case(LOPS_NEQ)
         get_loid2=LOID_NEQ
      case default
         STOP "*** UNEXPECTED ERROR in get_loid2"
      end select
    end function get_loid2

  end function build_rpnc

  subroutine rpn_put(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid,p1,p2
    rpnb%p_que=rpnb%p_que+1
    rpnb%que(rpnb%p_que)%tid=tid
    rpnb%que(rpnb%p_que)%p1=p1
    rpnb%que(rpnb%p_que)%p2=p2
  end subroutine rpn_put

  subroutine rpn_push(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid,p1,p2
    rpnb%p_buf=rpnb%p_buf+1
    rpnb%buf(rpnb%p_buf)%tid=tid
    rpnb%buf(rpnb%p_buf)%p1=p1
    rpnb%buf(rpnb%p_buf)%p2=p2
  end subroutine rpn_push

  subroutine rpn_pop(rpnb)
    type(t_rpnb),intent(inout)::rpnb
    if(rpnb%p_buf<0) return
    rpnb%p_que=rpnb%p_que+1
    rpnb%que(rpnb%p_que)=rpnb%buf(rpnb%p_buf)
    rpnb%p_buf=rpnb%p_buf-1
  end subroutine rpn_pop

  subroutine rpn_pop_until(rpnb,tid,fnc)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid
    logical,intent(in),optional::fnc
    integer i
    i=rpnb%p_buf+1
    do 
       i=i-1
       if(i==0) exit
       if(present(fnc).and.fnc.and.rpnb%buf(i)%tid==tid) exit
       select case(rpnb%buf(i)%tid)
       case(TID_BRA,TID_QSTA,TID_IBRA,TID_COL)
          cycle ! skip unclosed bra
       end select
       rpnb%p_que=rpnb%p_que+1
       rpnb%que(rpnb%p_que)=rpnb%buf(i)
       if(rpnb%buf(i)%tid==tid) then
          i=i-1
          exit
       end if
    end do
    rpnb%p_buf=i
  end subroutine rpn_pop_until

  subroutine rpn_pop_all_bra(rpnb)
    type(t_rpnb),intent(inout)::rpnb
    integer i,k
    k=0
    do i=rpnb%p_buf,1,-1
       select case(rpnb%buf(i)%tid)
       case(TID_BRA,TID_IBRA)
          k=i
       end select
    end do
    if(k==0) return
    do i=rpnb%p_buf,k,-1
       select case(rpnb%buf(i)%tid)
       case(TID_BRA,TID_QSTA,TID_IBRA,TID_COL)
          cycle ! skip unclosed bra
       end select
       rpnb%p_que=rpnb%p_que+1
       rpnb%que(rpnb%p_que)=rpnb%buf(i)
    end do
    rpnb%p_buf=k-1
  end subroutine rpn_pop_all_bra

  subroutine rpn_pop_all(rpnb)
    type(t_rpnb),intent(inout)::rpnb
    integer i
    do i=rpnb%p_buf,1,-1
       select case(rpnb%buf(i)%tid)
       case(TID_BRA,TID_QSTA,TID_IBRA,TID_COL)
          cycle ! skip unclosed bra
       end select
       rpnb%p_que=rpnb%p_que+1
       rpnb%que(rpnb%p_que)=rpnb%buf(i)
    end do
    rpnb%p_buf=0
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
       case(TID_BRA,TID_QSTA,TID_COL)
          rpnb%p_buf=rpnb%p_buf-1
          if(tid==tend) then
             if(present(dlm2c).and.told==TID_COL)&
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

  subroutine rpn_try_push(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid,p1,p2
    integer tst
    if(rpnb%p_buf==0) then
       call rpn_push(rpnb,tid,p1,p2)
    else
       do 
          if(rpnb%p_buf<=0) exit
          tst=get_lo32(rpnb%buf(rpnb%p_buf)%tid)
          if(tst<=TID_PRI_MAX &
               .and.tst>=tid &
               .and..not.(tid==TID_TOP1.and.tst==TID_TOP1)) then
             call rpn_pop(rpnb) ! TOP1 must not popout TOP1
          else
             exit
          end if
       end do
       call rpn_push(rpnb,tid,p1,p2)
    end if
  end subroutine rpn_try_push

  subroutine revert_tid(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout),target::rpnb
    integer,intent(in)::tid
    integer,intent(in),optional::p1,p2
    type(t_rrpnq),pointer::q
    q=>rpnb%que(rpnb%p_que)
    q%tid=tid
    if(present(p1)) q%p1=ior(q%p1,ishft(p1,16))
    if(present(p2)) q%p2=ior(q%p2,ishft(p2,16))
  end subroutine revert_tid

  integer function parse_formula(rpnc,formula)
    type(t_rpnc),intent(inout)::rpnc
    character*(*),intent(in)::formula
    type(t_rpnb),target::rpnb
    integer istat
    integer t,told,btold
    integer tid,tidold,btidold
    integer p1,p2
    integer bc,kc,pc,ac,fc,oc,fnc,qc,cc,amc,clc,tc
    logical amac
    integer pfasn
    integer p_q1
    integer pfnc_opened
    integer terr,p1err,p2err

    call init_rpnb(formula)

    cle_opt(RPNCOPT_READY)

    call init_stat()
    istat=0

    do 
       tid=get_next(rpnb,p1,p2,rpnc%rl%s)
       t=get_lo32(tid)
       select case(t)
       case(TID_BLK)
          cycle
       case(TID_FIN,TID_SCL)
          if(.not.was_operand().or.(tc/=clc.and.is_uset(RPNCOPT_DAT))) then
             istat=RPNERR_PARSER
             exit
          else
             if(.not.check_narg_all()) exit
             if(iand(qc,1)==1) then
                ! close "
                call rpn_try_pop(rpnb,TID_QSTA)
                call rpn_put(rpnb,TID_QEND,0,0)
             end if
             call rpn_pop_all(rpnb)
          end if
          if(.not.check_end()) exit
          if(t==TID_FIN) then
             exit
          else
             call rpn_put(rpnb,TID_SCL,p1,p2) ! <<<<<<<<<<<<
             call init_stat
             cycle
          end if
       case(TID_INV,TID_UNDEF)
          istat=RPNERR_PARSER
       case(TID_PAR)
          pc=pc+1
          call set_arg_read()
          select case(told)
          case(TID_FIG,TID_KET,TID_PAR,TID_UOP2,TID_UOP3)
             call push_implicit_mul()
          end select
          call rpn_put(rpnb,tid,p1,p2)
       case(TID_FIG)
          fc=fc+1
          call set_arg_read()
          select case(told)
          case(TID_KET,TID_PAR,TID_FIG,TID_UOP2,TID_UOP3) 
             call push_implicit_mul()
          end select
          call rpn_put(rpnb,t,p1,p2)
       case(TID_BOP1,TID_BOP2,TID_BOP3,TID_TOP1,& !<<<< TOP1
            TID_LOP2,TID_LOP3,TID_LOP4,&
            TID_ROP) 
          oc=oc+1
          if(.not.was_operand()) then
             istat=RPNERR_PARSER        
          else if(t==TID_TOP1) then
             tc=tc+1
             call rpn_try_push(rpnb,t,p1,bc-kc)
             call rpn_put(rpnb,TID_DLM1,p1,bc-kc)
          else
             call rpn_try_push(rpnb,t,p1,p2)
          end if
       case(TID_UOP2)
          oc=oc+1
          select case(told)
          case(TID_FIG,TID_PAR,TID_KET)
             call rpn_try_push(rpnb,t,p1,p2)
          case default
             istat=RPNERR_PARSER
          end select
       case(TID_UOP1,TID_LOP1)
          oc=oc+1
          call rpn_try_push(rpnb,t,p1,p2)
       case(TID_UOP3)
          oc=oc+1
          select case(told)
          case(TID_FIG,TID_PAR,TID_KET,TID_UOP2)
          case(TID_SCL,TID_BOP1,TID_BOP2,TID_BOP3,TID_BRA,TID_UNDEF)
             t=TID_UOP1
          case default
             istat=RPNERR_PARSER
          end select
          if(istat==0) call rpn_try_push(rpnb,t,p1,p2)
       case(TID_BRA)
          bc=bc+1
          select case(told)
          case(TID_FIG,TID_PAR,TID_KET,TID_UOP2,TID_UOP3)
             call push_implicit_mul()
          end select
          call rpn_push(rpnb,t,p1,p2)
      case(TID_HKET)
          if(.not.was_operand()) then
             istat=RPNERR_PARSER
          else
             if(.not.check_narg_all()) exit ! not checked 
             call rpn_pop_all_bra(rpnb)
          end if
          kc=bc
       case(TID_KET)
          if(.not.was_operand()) then
             istat=RPNERR_PARSER
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
             call revert_tid(rpnb,TID_APAR)
             call rpn_try_push(rpnb,t,p1,p2)
          else
             istat=RPNERR_PARSER
          end if
       case(TID_ASN)
          ac=ac+1
          if(is_fnc_asn()) then
             if(terr/=0) terr=0
          else
             if(check_assignable()) then
                call revert_tid(rpnb,TID_APAR)
             else
                istat=RPNERR_PARSER
             end if
          end if
          if(istat==0) then
             call rpn_try_push(rpnb,t,p1,p2)
             if(next_chr()/="""") call push_implicit_bra() ! a=b=c=1 is a=(b=(c=(1
          end if
       case(TID_QTN)
          qc=qc+1
          if(and(qc,1)/=0) then
             ! the first "
             if(told/=TID_ASN) then
                istat=RPNERR_PARSER
             else
                ! m="
                ! q   s      q     s
                ! PAR =  ->  AMAC  MASN
                !                  QSTA
                call revert_tid(rpnb,TID_AMAC,p1,find_chr(rpnb%expr(p1+1:),"""")+p1)
                rpnb%buf(rpnb%p_buf)%tid=TID_MASN ! it must be TID_ASN
                call rpn_push(rpnb,TID_QSTA,p1,p2)
                amac=.true.
             end if
          else
             if(.not.was_operand()) then
                istat=RPNERR_PARSER
             else
                if(.not.check_narg_all(TID_QSTA)) exit ! check unclosed ket of fnc 
                call rpn_try_pop(rpnb,TID_QSTA)
                call rpn_put(rpnb,TID_QEND,p1,p2)
                call rpn_pop(rpnb) ! pop MASN
             end if
          end if
       case(TID_COMA)
          cc=cc+1
          if(.not.was_operand()) then
             istat=RPNERR_PARSER
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
       case(TID_COL)
          clc=clc+1
          if(is_uset(RPNCOPT_DAT)) then
             call rpn_pop_until(rpnb,TID_TOP1)
             call rpn_push(rpnb,t,p1,p2)
          else
             if(clc<=1) then
                call rpn_put(rpnb,t,p1,p2)
             else
                istat=RPNERR_PARSER
             end if
          end if
       case(TID_IFNC,TID_UFNC)
          fnc=fnc+1
          call set_arg_read()
          select case(told)
          case(TID_KET,TID_FIG,TID_PAR,TID_UOP2,TID_UOP3)
             call push_implicit_mul()
          end select
          call set_narg
          call rpn_try_push(rpnb,tid,p1,p2)
          pfnc_opened=rpnb%p_buf
       case default
          stop "*** UNEXPECTED ERROR in parse_formula"
       end select
       if(istat/=0) exit
       btidold=tidold
       tidold=tid
       btold=told
       told=t
    end do

    if(is_set(RPNCOPT_DEBUG)) call dump_rpnb(rpnb)

    if(istat==0.and.terr/=0) then
       istat=terr
       p1=p1err
       p2=p1err
    end if

    if(istat==0) then
       istat=build_rpnc(rpnb,rpnc)       
    else if(.not.is_set(RPNCOPT_NO_WARN)) then
       call print_error(rpnb%expr(1:rpnb%len_expr),get_lo32(p1),get_lo32(p2))
    end if

    parse_formula=istat
    
  contains
    
    subroutine init_stat()
      btold=TID_UNDEF
      told=TID_UNDEF
      tidold=TID_UNDEF
      btidold=TID_UNDEF
      amac=.false.
      pfnc_opened=0
      pfasn=0
      bc=0; kc=0; pc=0; ac=0; fc=0; oc=0; fnc=0; qc=0; cc=0
      amc=0; clc=0; tc=0
      p_q1=rpnb%p_que+1
      terr=0
      p1err=0
      p2err=0
    end subroutine init_stat

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
                        istat=RPNERR_TOO_FEW_ARG
                     end if
                     b%p2=get_i32(get_lo32(b%p2),namax)
                  else if(na==1) then
                     istat=RPNERR_TOO_MANY_ARG
                  end if
               else if(namax/=0) then
                  istat=RPNERR_TOO_MANY_ARG
               end if
            else
               istat=RPNERR_NO_ARG
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
      integer na
      select case(t)
      case(TID_IFNC)
         if(get_up32(tid)<=FID_ARG0_END) then
            na=0
         else if(get_up32(tid)<=FID_ARG1_END) then
            na=1
         else if(get_up32(tid)<=FID_ARG2_END) then
            na=2
         else
            na=narg_max ! arbitrary
         end if
      case(TID_UFNC)
         na=rpnc%rl%rpnm(get_up32(tid))%na
      end select
      p1=get_i32(p1,-na) ! negative for no arg read
      p2=get_i32(p2,-na)
    end subroutine set_narg

    logical function check_end()
      check_end=(kc-bc<=0)
      if(.not.check_end) then
         istat=RPNERR_PARSER
      else if(pfasn/=0) then
         call set_par_dummy()
      end if
    end function check_end

    logical function is_fnc_asn()
      integer ii
      type(t_rrpnq),pointer::q
      is_fnc_asn=.false.
      if(ac==1.and.bc==1.and.kc==1.and.pc>=1.and.fc==0) then
         ii=index(rpnb%expr(p1+1:),";")
         if(ii==0) then
            ii=rpnb%len_expr
         else
            ii=p1+1+ii
         end if
         if(get_lo32(rpnb%buf(rpnb%p_buf)%tid)==TID_UFNC) then
            q => rpnb%buf(rpnb%p_buf)
            q%tid=TID_AFNC
            q%p1=get_i32(get_lo32(q%p1),p1+1)
            q%p2=get_i32(get_lo32(q%p2),ii)
            call rpn_pop(rpnb)
            is_fnc_asn=.true.
         else if(rpnb%que(p_q1)%tid==TID_PAR) then ! exclude TID_POP
            q => rpnb%que(p_q1)
            q%tid=TID_AFNC
            q%p1=get_i32(q%p1,p1+1) ! function body in upper
            q%p2=get_i32(q%p2,ii)
            pc=pc-1
            is_fnc_asn=.true.
         end if
      end if
      if(.not.is_fnc_asn) return
      pfasn=rpnb%p_que
      if(pc-1/=cc.or.set_dummy_par()==0) then
         is_fnc_asn=.false.
         istat=RPNERR_PARSER
      else
         if(p_q1==1) then
            ii=1
         else
            ii=rpnb%que(p_q1-1)%p1+1 ! TID_SCL + 1
         end if
         p1=get_i32(p1,ii) ! function definition in upper
         p2=get_i32(p2,p2-1)          
      end if
    end function is_fnc_asn

    logical function check_assignable()
      check_assignable=.false.
      if(tidold/=TID_PAR) return ! exclude TID_POP
      select case(btold) 
      case(TID_UNDEF,TID_BRA,TID_ASN)
      case(TID_QTN)
         if(iand(qc,1)==0) return ! second "
      case default
         return
      end select
      check_assignable=.true.
    end function check_assignable

    subroutine set_par_dummy()
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
                  if(_EXPR_(ii)&
                       ==_EXPR_(jj)) then
                     rpnb%que(jj)%tid=TID_DPAR
                     rpnb%que(jj)%p1=get_i32(rpnb%que(jj)%p1,did) !<<<
                     found=.true.
                  end if
               end if
            end do
            if(.not.found) then
               write(*,*) "*** Warning: Unused parameter: "//_EXPR_(ii)
            end if
         end if
      end do
    end subroutine set_par_dummy
    
    integer function set_dummy_par()
      integer ii
      integer dummy_count
      dummy_count=0
      do ii=p_q1,rpnb%p_que
         if(rpnb%que(ii)%tid==TID_PAR) then
            rpnb%que(ii)%tid=TID_DPAR
            dummy_count=dummy_count+1
         end if
      end do
      set_dummy_par=dummy_count
    end function set_dummy_par

    integer function find_chr(str,chr)
      character*(*),intent(in)::str
      character*1,intent(in)::chr
      find_chr=index(str,chr)
      if(find_chr==0) find_chr=len_trim(str)
    end function find_chr

    logical function was_operand()
      select case(told)
      case(TID_UNDEF,TID_BOP1,TID_BOP2,TID_BOP3,TID_UOP1,TID_AOP,&
           TID_ASN,TID_COMA,TID_TOP1,TID_COL,TID_SCL)
         was_operand=.false.
      case(TID_BRA)
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

    subroutine init_rpnb(s)
      character*(*),intent(in)::s
      rpnb%expr=s(1:LEN_FORMULA_MAX)
      rpnb%len_expr=strip(rpnb%expr)
      rpnb%cur_pos=0
      rpnb%old_pos=0
      rpnb%old_tid=TID_UNDEF ! <<<<<<<<<<<<<<
      rpnb%p_buf=0
      rpnb%p_que=0
      allocate(rpnb%que(rpnb%len_expr*2)) ! << at most
      allocate(rpnb%buf(rpnb%len_expr*2)) ! << at most
      rpnb%opt=rpnc%opt
    end subroutine init_rpnb
        
    character*1 function next_chr()
      if(p2<rpnb%len_expr) then
         next_chr=rpnb%expr(p2+1:p2+1)
      else
         next_chr=""
      end if
    end function next_chr
    
  end function parse_formula

end module rpnp
