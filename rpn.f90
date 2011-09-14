module rpn
  use slist
  use plist
  use fpio
  implicit none

!  private

  integer,parameter::RPNSTA_FNCSET = -1
  integer,parameter::RPNSTA_OK     =  0

  integer,parameter::RPNERR_NOENT  =  1
  integer,parameter::RPNERR_NOOP   =  2
  integer,parameter::RPNERR_NOPAR  =  3
  integer,parameter::RPNERR_NOFNC  =  4
  integer,parameter::RPNERR_ADDPAR =  5
  integer,parameter::RPNERR_INVASN =  6
  integer,parameter::RPNERR_INVOP  =  7
  integer,parameter::RPNERR_INVFNC =  8
  integer,parameter::RPNERR_PARSER =  9
  integer,parameter::RPNERR_ADDSTR = 10
  integer,parameter::RPNERR_MEMOV  = 11
  integer,parameter::RPNERR_RECOV  = 12

  integer,parameter::RPN_REC_MAX   =  256
  integer,parameter::NUM_VBUF_MIN  =   32
  integer,parameter::NUM_PBUF_MIN  =   32
  integer,parameter::NUM_RPNM_MIN  =    8
  integer,parameter::LEN_PLIST_MIN = 1024
  integer,parameter::LEN_RLIST_MIN = 1024

  ! meta tid
  integer,parameter::TID_FIN   = -999
  integer,parameter::TID_UNDEF =  0
  integer,parameter::TID_INV   =  -666
  ! operators
  integer,parameter::TID_ASN   =  1  ! =
  integer,parameter::TID_AOP   =  2 
  integer,parameter::TID_TOP1  =  3  ! ?
  integer,parameter::TID_BOP1  =  4  ! +,-
  integer,parameter::TID_BOP1U = -4  !
  integer,parameter::TID_BOP2  =  5  ! *,/
  integer,parameter::TID_BOP2U = -5  !
  integer,parameter::TID_BOP4  =  6  ! implicit * <<<<<<<<<<< 
  integer,parameter::TID_BOP3  =  7  ! ^,**,e
  integer,parameter::TID_BOP3U = -7  !
  integer,parameter::TID_UOP1  =  8  ! +,-
  integer,parameter::TID_UOP2  =  9  ! !
  integer,parameter::TID_IFNC  = 10  ! sin, cos,...
  integer,parameter::TID_UFNC  = 11  !
  ! braket
  integer,parameter::TID_SCL   = -64  ! ;
  integer,parameter::TID_COL   = -65  ! :
  integer,parameter::TID_IBRA  = -66   ! implicit (
  integer,parameter::TID_BRA   = -67   ! (
  integer,parameter::TID_KET   = -68   ! )
  integer,parameter::TID_QTN   = -69   ! "
  integer,parameter::TID_QEND  = -70
  integer,parameter::TID_QSTA  = -71
  integer,parameter::TID_COMA  = -72  ! ,
  integer,parameter::TID_MASN  = -73  ! = for macro
  integer,parameter::TID_DLM1  = -74
  integer,parameter::TID_DLM2  = -75
  ! 
  integer,parameter::TID_PAR   =  32  ! a,b,c,...
  integer,parameter::TID_PARU  = -32  ! a,b,c,...
  integer,parameter::TID_FIG   =  33  ! 1,2,3,...
  integer,parameter::TID_VAR   =  34  ! fig in rbuf
  integer,parameter::TID_MAC   =  35
  integer,parameter::TID_OP1   =  36  ! operators
  integer,parameter::TID_OP2   =  37
  integer,parameter::TID_OP3   =  38
  integer,parameter::TID_APAR  =  39  ! par assign
  integer,parameter::TID_AMAC  =  40
  integer,parameter::TID_AFNC  =  41
  integer,parameter::TID_DPAR  =  42  ! dummy par

  ! Do nothing or undef
  integer,parameter::TID_NOP   =  999

  integer,parameter::LEN_STR_MAX=1024
  integer,parameter::LEN_FORMULA_MAX=LEN_STR_MAX

  type t_rrpnq
     integer tid
     integer p1,p2
  end type t_rrpnq

  type t_rpnq
     integer tid
     integer cid ! oid or fid or pointer to value
  end type t_rpnq
  
  type t_rpnb
     character(LEN_FORMULA_MAX) expr
     integer len_expr
     integer cur_pos
     integer old_pos
     integer old_tid
     type(t_rrpnq),allocatable::que(:)
     type(t_rrpnq),allocatable::buf(:)
     integer p_buf,p_que
  end type t_rpnb

  type t_rpnm
     type(t_rpnq),pointer::que(:)   ! allocated
     complex(cp),pointer::vbuf(:)   ! allocated
     integer,pointer::p_vbuf        ! allocated
     type(t_plist),pointer::pars    ! => rpnc%pars
     complex(cp),pointer::answer    ! => rpnc%answer
     complex(cp),pointer::tmpans    ! => rpnc%tmpans
     type(t_slist),pointer::pnames  ! allocated
     integer,pointer::na            ! num arg
  end type t_rpnm

  type t_rpnlist
     type(t_rpnm),allocatable::rpnm(:)
     type(t_slist) s
  end type t_rpnlist

  type t_rpnc
     type(t_rpnq),pointer::que(:)
     complex(cp),pointer::vbuf(:)
     integer,pointer::p_vbuf
     type(t_plist),pointer::pars
     complex(cp),pointer::answer
     complex(cp),pointer::tmpans
     type(t_rpnlist),pointer::rl
     integer,pointer::rc ! recursion count
     integer,pointer::opt
  end type t_rpnc

  integer,parameter::RPNCOPT_NOP  =  0
  integer,parameter::RPNCOPT_DMSK =  Z"FF"
  integer,parameter::RPNCOPT_DBG  =  Z"08000000"

  integer,parameter::AID_NOP = 0
  integer,parameter::AID_MOV = 1
  integer,parameter::AID_INC = 2
  integer,parameter::AID_DEC = 3
  integer,parameter::AID_MUL = 4
  integer,parameter::AID_DIV = 5
  integer,parameter::AID_POW = 6
  
  integer,parameter::OID_NOP = 0
  integer,parameter::OID_NEG = 1
  integer,parameter::OID_ADD = 2 
  integer,parameter::OID_SUB = 3
  integer,parameter::OID_MUL = 4
  integer,parameter::OID_DIV = 5
  integer,parameter::OID_POW = 6
  integer,parameter::OID_EXP = 7
  integer,parameter::OID_FAC = 8
  integer,parameter::OID_CND = 9

  integer,parameter::int_fnc_max_len=5
  character*(*),parameter::int_fnc=&
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
       achar(3)//"abs"//&
       achar(3)//"int"//&
       achar(4)//"frac"//&
       achar(5)//"conjg"//&
       achar(4)//"nint"//&
       achar(4)//"real"//&
       achar(4)//"imag"//&
       achar(0)
  integer,parameter::FID_SIN   =  1
  integer,parameter::FID_COS   =  2 
  integer,parameter::FID_TAN   =  3
  integer,parameter::FID_SINH  =  4
  integer,parameter::FID_COSH  =  5
  integer,parameter::FID_TANH  =  6
  integer,parameter::FID_ASIN  =  7
  integer,parameter::FID_ACOS  =  8
  integer,parameter::FID_ATAN  =  9
  integer,parameter::FID_ASINH = 10
  integer,parameter::FID_ACOSH = 11
  integer,parameter::FID_ATANH = 12
  integer,parameter::FID_EXP   = 13
  integer,parameter::FID_LOG   = 14
  integer,parameter::FID_LN    = 15
  integer,parameter::FID_SQRT  = 16
  integer,parameter::FID_ABS   = 17
  integer,parameter::FID_INT   = 18
  integer,parameter::FID_FRAC  = 19
  integer,parameter::FID_CONJG = 20
  integer,parameter::FID_NINT  = 21
  integer,parameter::FID_REAL  = 22
  integer,parameter::FID_IMAG  = 23

contains

  complex(cp) function rpn_ans(rpnc)
    type(t_rpnc),intent(in)::rpnc
    rpn_ans=rpnc%answer
  end function rpn_ans
  
  integer function strip(s)
    character*(*),intent(inout)::s
    integer i,k
    k=0
    do i=1,len_trim(s)
       select case(s(i:i))
       case(" ","\t")
       case(achar(0))
          exit
       case default
          k=k+1
          if(k/=i) s(k:k)=s(i:i)
       end select
    end do
    strip=k
  end function strip

  subroutine init_rpnlist(rl)
    type(t_rpnlist),intent(inout)::rl
    integer i
    rl%s=init_slist(LEN_RLIST_MIN)
    allocate(rl%rpnm(NUM_RPNM_MIN))
    do i=1,NUM_RPNM_MIN
       nullify(rl%rpnm(i)%que)
       nullify(rl%rpnm(i)%vbuf)
       nullify(rl%rpnm(i)%p_vbuf)
       nullify(rl%rpnm(i)%pnames)
       nullify(rl%rpnm(i)%na)
    end do
  end subroutine init_rpnlist

  subroutine uinit_rpnlist(rl)
    type(t_rpnlist),intent(inout),target::rl
    integer i
    type(t_rpnm),pointer::rpnm
    call uinit_slist(rl%s)
    if(.not.allocated(rl%rpnm)) return
    do i=1,size(rl%rpnm)
       rpnm=>rl%rpnm(i)
       if(associated(rpnm%que).and.size(rpnm%que)>0) deallocate(rpnm%que)
       if(associated(rpnm%vbuf).and.size(rpnm%vbuf)>0) deallocate(rpnm%vbuf)
       if(associated(rpnm%na)) deallocate(rpnm%na)
       if(associated(rpnm%pars)) nullify(rpnm%pars)
       if(associated(rpnm%tmpans)) nullify(rpnm%tmpans)
       if(associated(rpnm%answer)) nullify(rpnm%answer)
       if(associated(rpnm%p_vbuf)) deallocate(rpnm%p_vbuf)
    end do
  end subroutine uinit_rpnlist

  subroutine inc_vbuf(rpnc,n)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::n
    complex(cp),allocatable::vb(:)
    if(n<=0) return
    if(associated(rpnc%vbuf).and.size(rpnc%vbuf)>0) then
       allocate(vb(size(rpnc%vbuf)))
       vb=rpnc%vbuf
       deallocate(rpnc%vbuf)
       allocate(rpnc%vbuf(size(vb)+n))
       rpnc%vbuf(1:size(vb))=vb
       deallocate(vb)
    else
       allocate(rpnc%vbuf(n))
    end if
  end subroutine inc_vbuf
  
  integer function init_rpnc()
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    p=malloc(sizeof(rpnc))
    nullify(rpnc%que)
    nullify(rpnc%vbuf)
    allocate(rpnc%vbuf(NUM_VBUF_MIN))
    allocate(rpnc%rl)
    allocate(rpnc%tmpans)
    allocate(rpnc%answer)
    allocate(rpnc%pars)
    allocate(rpnc%p_vbuf)
    allocate(rpnc%rc)
    allocate(rpnc%opt)
    call init_par(rpnc)
    call init_rpnlist(rpnc%rl)
    rpnc%opt=RPNCOPT_NOP
    init_rpnc=p
  end function init_rpnc

  subroutine uinit_rpnc(rpnc)
    type(t_rpnc),intent(inout)::rpnc
    if(associated(rpnc%que).and.size(rpnc%que)>0) deallocate(rpnc%que)
    if(associated(rpnc%vbuf).and.size(rpnc%vbuf)>0) deallocate(rpnc%vbuf)
    if(associated(rpnc%tmpans)) deallocate(rpnc%tmpans)
    if(associated(rpnc%answer)) deallocate(rpnc%answer)
    if(associated(rpnc%pars)) call uinit_par(rpnc)
    if(associated(rpnc%p_vbuf)) deallocate(rpnc%p_vbuf)
    if(associated(rpnc%rc)) deallocate(rpnc%rc)
    if(associated(rpnc%rl)) call uinit_rpnlist(rpnc%rl)
  end subroutine uinit_rpnc

  subroutine init_par(rpnc)
    type(t_rpnc),intent(inout)::rpnc
    integer istat
    rpnc%pars=init_plist(LEN_PLIST_MIN,NUM_PBUF_MIN)
    istat=add_par_by_reference(rpnc%pars,"tmp",loc(rpnc%tmpans),.true.)
    istat=add_par_by_reference(rpnc%pars,"ans",loc(rpnc%answer),.true.)
    istat=add_par_by_value(rpnc%pars,"eps",epsilon(0.0_rp),.true.)    
    istat=add_par_by_value(rpnc%pars,"huge",huge(0.0_rp),.true.)    
    istat=add_par_by_value(rpnc%pars,"i",complex(0.0_rp,1.0_rp),.true.)
    istat=add_par_by_value(rpnc%pars,"pi",atan(1.0_rp)*4.0_rp,.true.)
    istat=add_par_by_value(rpnc%pars,"c",2.99792458e8_rp,.true.)
  end subroutine init_par

  subroutine uinit_par(rpnc)
    type(t_rpnc),intent(inout)::rpnc
    call uinit_plist(rpnc%pars)
  end subroutine uinit_par

  subroutine delete_par(rpnc,s)
    type(t_rpnc),intent(inout)::rpnc
    character*(*),intent(in)::s
    integer istat
    istat=rm_par(rpnc%pars,trim(adjustl(s)))
    if(istat/=0) write(*,*) "*** Error delete_par: "//trim(s)//": code = ",istat
  end subroutine delete_par

  integer function get_operand(rpnc,i)
    type(t_rpnc),intent(in)::rpnc
    integer,intent(in)::i
    integer j
    get_operand=0
    do j=i,1,-1
       if(rpnc%que(j)%tid==TID_VAR) then
          get_operand=j
          return
       end if
    end do
  end function get_operand

  complex(cp) function fac(v)
    complex(cp),intent(in)::v
    integer i,n
    n=int(v)
    fac=1
    if(n<=1) return
    do i=2,n
       fac=fac*i
    end do
  end function fac

  integer function eval_1(rpnc,i)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer od
    complex(cp) v1,v2
    pointer(pv1,v1)

    od=get_operand(rpnc,i-1)
    if(od==0) then
       eval_1=RPNERR_NOOP
       return
    end if
    pv1=rpnc%que(od)%cid
    select case(rpnc%que(i)%cid)
    case(OID_NOP)
    case(OID_NEG)
       v2=-v1
       if(abs(imagpart(v2))==rzero) then ! gfortran46 gives -0 for negation of 0
          v2=complex(realpart(v2),rzero)
       end if
       if(abs(realpart(v2))==rzero) then 
          v2=complex(rzero,imagpart(v2))
       end if
    case(OID_FAC)
       v2=fac(v1)
    end select
    rpnc%tmpans=v2
    call put_vbuf(rpnc,i,v2)
    rpnc%que(od)%tid=TID_NOP
    eval_1=0
    rpnc%tmpans=v2
  end function eval_1

  integer function eval_2a(rpnc,i)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer od1,od2    
    complex(cp) v1,v2
    pointer(pv1,v1)
    pointer(pv2,v2)
    od2=get_operand(rpnc,i-1)
    if(od2<=1) then
       eval_2a=RPNERR_NOOP
       return
    end if
    od1=get_operand(rpnc,od2-1)
    if(od1==0) then
       eval_2a=RPNERR_NOOP
       return
    end if
    pv2=rpnc%que(od2)%cid
    pv1=rpnc%que(od1)%cid 

    ! pv1 => pars
    select case(rpnc%que(i)%cid)
    case(AID_MOV)
       v1=v2
    case(AID_INC)
       v1=v1+v2
    case(AID_DEC)
       v1=v1-v2
    case(AID_MUL)
       v1=v1*v2
    case(AID_DIV)
       v1=v1/v2
    case(AID_POW)
       v1=v1**v2
    end select
    rpnc%tmpans=v1
    call put_vbuf(rpnc,i,v1)
    rpnc%que(od2)%tid=TID_NOP
    rpnc%que(od1)%tid=TID_NOP
    eval_2a=0
  end function eval_2a

  logical function is_integer(z,n)
    complex(cp),intent(in)::z
    integer,intent(out),optional::n
    integer m
    real(rp) x
    is_integer=.false.
    if(imagpart(z)/=rzero) return
    x=realpart(z)
    m=int(x)
    x=x-m
    if(x==0) then 
       is_integer=.true.
       if(present(n)) n=m
    end if
  end function is_integer

  integer function eval_3(rpnc,i)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer cid,kd
    integer od1,od2,od3
    complex(cp) v1,v2,v3
    pointer(pv1,v1)
    cid=get_lo32(rpnc%que(i)%cid) ! <<<<<<<<<<<<<,

    select case(cid)
    case(OID_CND)
       kd=get_up32(rpnc%que(i)%cid)
       od1=get_operand(rpnc,i-1)
       if(od1==0) then
          eval_3=RPNERR_NOOP
          return
       end if
       pv1=rpnc%que(od1)%cid
       if(realpart(v1)/=0.0_rp) then
          rpnc%que(kd:size(rpnc%que))%tid=TID_NOP
       else
          rpnc%que(i:kd)%tid=TID_NOP
       end if

    end select
    
    eval_3=0
    
  end function eval_3


  integer function eval_2(rpnc,i)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer od1,od2
    complex(cp) v1,v2,v3
    integer n1,n2
    pointer(pv1,v1)
    pointer(pv2,v2)

    od2=get_operand(rpnc,i-1)
    if(od2<=1) then
       eval_2=RPNERR_NOOP
       return
    end if
    od1=get_operand(rpnc,od2-1)
    if(od1==0) then
       eval_2=RPNERR_NOOP
       return
    end if

    pv1=rpnc%que(od1)%cid
    pv2=rpnc%que(od2)%cid

    select case(rpnc%que(i)%cid)
    case(OID_ADD)
       v3=v1+v2
    case(OID_SUB)
       v3=v1-v2
    case(OID_MUL)
       v3=v1*v2
    case(OID_DIV)
       v3=v1/v2
    case(OID_POW)
       if(is_integer(v2,n2)) then
          if(is_integer(v1,n1)) then
             v3=real(n1,kind=rp)**real(n2,kind=rp)
          else
             v3=v1**real(n2,kind=rp)
          end if
       else
          v3=v1**v2
       end if
    case(OID_EXP)
       if(is_integer(v2,n2)) then
          if(is_integer(v1,n1)) then
             v3=real(n1,kind=rp)*10.0_rp**real(n2,kind=rp)
          else
             v3=v1*10.0_rp**real(n2,kind=rp)
          end if
       else
          v3=v1*10.0_rp**v2
       end if
    end select
    rpnc%tmpans=v3
    call put_vbuf(rpnc,i,v3)
    rpnc%que(od1)%tid=TID_NOP
    rpnc%que(od2)%tid=TID_NOP
    eval_2=0
  end function eval_2

  recursive function eval_nf(rpnc,i) result(istat)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer istat
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) fnc
    integer j
    integer kp,ko
    integer,allocatable::ods(:)
    complex(cp) v
    pointer(pv,v)

    rpnm=>rpnc%rl%rpnm(rpnc%que(i)%cid)

    if(i-1<rpnm%na) then
       istat=RPNERR_NOOP
       return
    end if

    ko=rpnm%na+1
    allocate(ods(rpnm%na))
    do j=i-1,1,-1
       if(rpnc%que(j)%tid==TID_VAR) then
          ko=ko-1
          ods(ko)=j
          if(ko==rpnm%na) exit
       end if
    end do
    if(ko/=1) then
       deallocate(ods)
       istat=RPNERR_NOOP
       return
    end if

    allocate(fnc%que(size(rpnm%que)))
    allocate(fnc%vbuf(NUM_VBUF_MIN))
    allocate(fnc%p_vbuf)
    fnc%que=rpnm%que
    fnc%p_vbuf=0

    fnc%pars=>rpnc%pars
    fnc%answer=>rpnc%answer
    fnc%tmpans=>rpnc%tmpans
    fnc%rl=>rpnc%rl
    fnc%rc=>rpnc%rc
    
    istat=0
    do j=1,size(fnc%que)
       select case(fnc%que(j)%tid)
       case(TID_FIG)
          fnc%que(j)%cid=loc(rpnm%vbuf(fnc%que(j)%cid))
          fnc%que(j)%tid=TID_VAR
       case(TID_PAR,TID_APAR)
          call set_par_ptr(kp)
          if(istat/=0) exit 
          fnc%que(j)%cid=loc(fnc%pars%v(kp))
          fnc%que(j)%tid=TID_VAR
       case(TID_DPAR)
          pv=rpnc%que(ods(fnc%que(j)%cid))%cid
          call put_vbuf(fnc,j,v)
       end select
    end do

    if(istat==0) istat=eval(fnc)
    
    if(istat==0) then
       rpnc%que(i)%tid=TID_VAR
       call put_vbuf(rpnc,i,fnc%answer)
       rpnc%tmpans=fnc%answer
      do j=1,rpnm%na
          rpnc%que(ods(j))%tid=TID_NOP
       end do
    end if

    deallocate(ods)
    deallocate(fnc%que)
    deallocate(fnc%vbuf)
    deallocate(fnc%p_vbuf)

  contains
    
    subroutine set_par_ptr(ent) 
      integer,intent(out)::ent
      integer ptr,len      
      istat=get_str_ptr(rpnm%pnames,fnc%que(j)%cid,ptr,len)
      if(istat/=0) stop "*** UNEXPECTED ERROR in set_par_ptr"
      istat=find_par(fnc%pars,trim(cpstr(ptr,len)),ent=ent)
      if(istat/=0) then
         write(*,*) "*** No such parameter: "//trim(cpstr(ptr,len))
      end if
    end subroutine set_par_ptr

  end function eval_nf
  
  integer function eval_1f(rpnc,i)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer od
    complex(cp) v1,v2
    pointer(pv1,v1)

    od=get_operand(rpnc,i-1)
    if(od==0) then
       eval_1f=RPNERR_NOOP
       return
    end if
    pv1=rpnc%que(od)%cid
    select case(rpnc%que(i)%cid)
    case(FID_SIN)
       v2=sin(v1)
    case(FID_COS)
       v2=cos(v1)
    case(FID_TAN)
       v2=tan(v1)
    case(FID_ASIN)
       v2=asin(v1)
    case(FID_ACOS)
       v2=acos(v1)
    case(FID_ATAN)
       v2=atan(v1)
    case(FID_EXP)
       v2=exp(v1)
    case(FID_SQRT)
       v2=sqrt(v1)
    case(FID_LN)
       v2=log(v1)
    case(FID_LOG)
       v2=log(v1)/log(10.0_rp)
    case(FID_SINH)
       v2=sinh(v1)
    case(FID_COSH)
       v2=cosh(v1)
    case(FID_TANH)
       v2=tanh(v1)
    case(FID_ASINH)
       v2=log(v1+sqrt(v1*v1+1.0_rp))
    case(FID_ACOSH)
       v2=log(v1+sqrt(v1*v1-1.0_rp))
    case(FID_ATANH)
       v2=log((1.0_rp+v1)/(1.0_rp-v1))/2.0_rp
    case(FID_ABS)
       v2=abs(v1)
    case(FID_INT)
       v2=int(v1)
    case(FID_FRAC)
       v2=v1-int(v1)
    case(FID_CONJG)
       v2=conjg(v1)
       if(abs(imagpart(v2))==rzero)  then
          v2=complex(realpart(v2),rzero)
       end if
    case(FID_NINT)
       v2=int(v1+0.5_rp)
    case(FID_REAL)
       v2=realpart(v1)
    case(FID_IMAG)
       v2=imagpart(v1)
    end select
    rpnc%tmpans=v2
    call put_vbuf(rpnc,i,v2)
    rpnc%que(od)%tid=TID_NOP
    eval_1f=0
  end function eval_1f

  subroutine put_vbuf(rpnc,i,v)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    complex(cp),intent(in)::v
    if(rpnc%p_vbuf>=size(rpnc%vbuf)) call inc_vbuf(rpnc,NUM_VBUF_MIN)
    rpnc%p_vbuf=rpnc%p_vbuf+1
    rpnc%vbuf(rpnc%p_vbuf)=v
    rpnc%que(i)%cid=loc(rpnc%vbuf(rpnc%p_vbuf))
    rpnc%que(i)%tid=TID_VAR
  end subroutine put_vbuf

  recursive function eval_m(rpnc,i) result(istat)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer istat
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) mac
    integer j
    integer kp

    rpnm=>rpnc%rl%rpnm(rpnc%que(i)%cid)
    
    allocate(mac%que(size(rpnm%que)))
    allocate(mac%vbuf(NUM_VBUF_MIN))
    allocate(mac%p_vbuf)
    mac%que    = rpnm%que
    mac%p_vbuf = 0

    mac%pars   => rpnc%pars
    mac%answer => rpnc%answer
    mac%tmpans => rpnc%tmpans
    mac%rl     => rpnc%rl
    mac%rc     => rpnc%rc

    istat=0
    do j=1,size(mac%que)
       select case(mac%que(j)%tid) 
       case(TID_PAR,TID_APAR)
          call set_par_ptr(kp)
          if(istat/=0) exit
          mac%que(j)%cid=loc(mac%pars%v(kp))
          mac%que(j)%tid=TID_VAR
       case(TID_FIG)
          mac%que(j)%cid=loc(rpnm%vbuf(mac%que(j)%cid))
          mac%que(j)%tid=TID_VAR
       end select
    end do

    if(istat==0) istat=eval(mac)

    if(istat==0) then
       rpnc%tmpans=mac%answer
       call put_vbuf(rpnc,i,mac%answer)
    end if

    deallocate(mac%que)
    deallocate(mac%vbuf)
    deallocate(mac%p_vbuf)
 
  contains

    subroutine set_par_ptr(ent)
      integer,intent(out)::ent
      integer ptr,len
      istat=get_str_ptr(rpnm%pnames,mac%que(j)%cid,ptr,len)
      if(istat/=0) stop "*** UNEXPECTED ERROR in load_par: get_str_ptr"
      istat=find_par(mac%pars,trim(cpstr(ptr,len)),ent=ent)
      if(istat/=0) then
         write(*,*) "*** No such parameter: "//trim(cpstr(ptr,len))
         istat=RPNERR_NOPAR
      end if
    end subroutine set_par_ptr

  end function eval_m

  recursive function eval(rpnc) result(istat)
    type(t_rpnc),intent(inout),target::rpnc
    integer i,istat,ec
    complex(cp) v
    pointer(pv,v)
    istat=0
    ec=0
    if(rpnc%rc>RPN_REC_MAX) then
       istat=RPNERR_RECOV
       return
    end if
    rpnc%rc=rpnc%rc+1
    do i=1,size(rpnc%que)
       ec=ec+1
       select case(rpnc%que(i)%tid)
       case(TID_OP1)
          istat=eval_1(rpnc,i)
       case(TID_OP2)
          istat=eval_2(rpnc,i)
       case(TID_OP3)
          istat=eval_3(rpnc,i)
       case(TID_IFNC)
          istat=eval_1f(rpnc,i)
       case(TID_ASN)
          istat=eval_2a(rpnc,i)
       case(TID_MAC)
          istat=eval_m(rpnc,i)
       case(TID_UFNC)
          istat=eval_nf(rpnc,i)
       case default
          ec=ec-1
       end select
       if(istat/=0) then
          write(*,*) "*** Error in eval at que = ", i
          exit
       end if
    end do
    
    if(istat/=0) return

    if(ec==0.and.size(rpnc%que)==1) then ! only fig or par
       pv=rpnc%que(1)%cid
       rpnc%answer=v
    else
       rpnc%answer=rpnc%tmpans
    end if
    
    rpnc%rc=rpnc%rc-1

  end function eval

  integer function get_lo32(cid)
    integer,intent(in)::cid
    get_lo32=iand(cid,int(Z"FFFF",kind=4))
  end function get_lo32

  integer function get_up32(cid)
    integer,intent(in)::cid
    get_up32=iand(ishft(cid,-16),int(Z"FFFF",kind=4))
  end function get_up32

  integer function get_i32(lo,up)
    integer,intent(in)::lo,up
    get_i32=ior(lo,ishft(up,16))
  end function get_i32

  integer function get_end_of_fig(rpnb,k_)
    type(t_rpnb),intent(in)::rpnb
    integer,intent(in)::k_
    integer c,a,k

    k=k_
    if(rpnb%expr(k:k)==".") then
       c=1
    else
       c=0
    end if
    
    do
       if(k>=rpnb%len_expr) then
          get_end_of_fig=k
          return
       end if
       k=k+1
       a=ichar(rpnb%expr(k:k))
       if(.not.is_numeric(a)) then
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
       if(.not.is_alpha(a).and..not.is_number(a)) then
          get_end_of_par=k-1
          return
       end if
    end do
  end function get_end_of_par
  
  logical function is_alpha(a)
    integer,intent(in)::a
    integer b
    b=ior(a,32)
    is_alpha=(b>=97.and.b<=122)
  end function is_alpha
  
  logical function is_number(a)
    integer,intent(in)::a
    is_number=(a>=48.and.a<=57)
  end function is_number
  
  logical function is_numeric(a)
    integer,intent(in)::a
    is_numeric=(is_number(a).or.a==46)
  end function is_numeric
  
  logical function is_int_fnc(a,ent)
    character*(*),intent(in)::a
    integer,intent(out),optional::ent
    integer len,lenf
    integer i,k,j
    len=len_trim(a)
    is_int_fnc=.false.
    k=1
    i=0
    do
       i=i+1
       lenf=ichar(int_fnc(k:k))
       if(lenf==0) exit
       if(lenf==len) then
          j=k+1
          if(int_fnc(j:j+len-1)==a(1:len)) then
             is_int_fnc=.true.
             if(present(ent)) ent=i
             return
          end if
       end if
       k=k+lenf+1
    end do
  end function is_int_fnc

  integer function get_tid(a)
    character*1,intent(in)::a
    get_tid=0 ! in order to avoid warning
    select case(a)
    case("+","-")
       get_tid=TID_BOP1U
    case("*","/")
       get_tid=TID_BOP2U
    case("!")
       get_tid=TID_UOP2
    case("^")
       get_tid=TID_BOP3U
    case("(")
       get_tid=TID_BRA
    case(")")
       get_tid=TID_KET
    case("=")
       get_tid=TID_ASN
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
    case(TID_BOP1U)
       if(k<rpnb%len_expr-1) then
          if(next_char(1)=="=") then             
             t=TID_AOP
             p2=k+1
          end if
       end if
       if(t==TID_BOP1U) then
          select case(rpnb%old_tid)
          case(TID_BRA,TID_BOP3,TID_ASN,TID_AOP,&
               TID_COMA,TID_TOP1,TID_COL) ! plus in (+, ^+ and e+ are unary
             t=TID_UOP1
          case(TID_UOP1,TID_BOP2,TID_BOP1)
             t=TID_INV
          case default
             if(k==1) then
                t=TID_UOP1
             else
                t=TID_BOP1
             end if
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
             k=p2+1
             if(rpnb%expr(k:k)=="(") then
                if(is_usr_fnc(sl,rpnb%expr(p1:p2),kf)) then
                   t=get_i32(TID_UFNC,kf) ! intrinsic func
                else if(is_int_fnc(rpnb%expr(p1:p2),kf)) then
                   t=get_i32(TID_IFNC,kf)
                end if
             end if
          end if
          if(t==TID_PARU) t=TID_PAR
       end if
    case(TID_FIG)
       p2=get_end_of_fig(rpnb,k)
       if(p2<0) t=TID_INV
    end select
    
    rpnb%old_tid=t
    get_next=t
    rpnb%cur_pos=p2
    
  contains
    
    character*1 function next_char(inc)
      integer,intent(in)::inc
      integer kk
      kk=k+inc
      next_char=rpnb%expr(kk:kk)
    end function next_char

  end function get_next
  
  subroutine dump_rpnm(rpnc,ent)
    type(t_rpnc),intent(in),target::rpnc
    integer,intent(in)::ent
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) tmprpnc
    integer ptr,len
    integer*1 code
    if(ent>rpnc%rl%s%n.or.ent<=0.or.ent>size(rpnc%rl%rpnm)&
         .or.get_str_ptr(rpnc%rl%s,ent,ptr,len,code)/=0) then
       write(*,*) "*** dump_rpnm: no such entry: ",ent
       return
    end if
    rpnm=>rpnc%rl%rpnm(ent)
    if(iand(code,SC_MAC)/=0) then
       write(*,*) "MACRO entry: ",ent
       write(*,*) trim(cpstr(ptr,len))
    else
       write(*,*) "FUNCTION entry:",ent
       if(get_str_ptr(rpnm%pnames,2,ptr,len)/=0) then
          write(*,*) "???"
       else
          write(*,*) trim(cpstr(ptr,len))
       end if
    end if

    if(associated(rpnm%pnames).and.get_str_ptr(rpnm%pnames,1,ptr,len)==0) then
       write(*,*) trim(cpstr(ptr,len))
       tmprpnc%que=>rpnm%que
       tmprpnc%vbuf=>rpnm%vbuf
       tmprpnc%p_vbuf=>rpnm%p_vbuf
       tmprpnc%pars=>rpnm%pars
       tmprpnc%answer=>rpnm%answer
       tmprpnc%tmpans=>rpnm%tmpans
       if(associated(rpnm%na)) write(*,*) "number of arguments = ",rpnm%na
       call dump_rpnc(tmprpnc)
    else
       write(*,*) "(empty)"
    end if
  end subroutine dump_rpnm

  subroutine dump_rpnc(rpnc)
    type(t_rpnc),intent(in)::rpnc
    integer i
    complex(cp) v
    pointer(pv,v)
    write(*,*) "rpnc dump:\n# tid cid value"
    if(.not.associated(rpnc%que).or.size(rpnc%que)<1) then
       write(*,*) "(empty)"
       return
    end if
    do i=1,size(rpnc%que)
       write(*,10) i,rpnc%que(i)%tid,rpnc%que(i)%cid
       select case(rpnc%que(i)%tid)
       case(TID_VAR)
          pv=rpnc%que(i)%cid
          if(pv/=0) then
             write(*,*) trim(ztoa(v,fmt=DISP_FMT_RAW))
          else
             write(*,*) "(undef)"
          end if
       case default
          write(*,*)
       end select
    end do
10  format(x,i4,x,i4,x,i9,$)    
    write(*,*) "# vbuf size = ",rpnc%p_vbuf
    if(rpnc%p_vbuf>0) then
       do i=1,rpnc%p_vbuf
          write(*,*) i,loc(rpnc%vbuf(i)),trim(ztoa(rpnc%vbuf(i),fmt=DISP_FMT_RAW))
       end do
    end if
  end subroutine dump_rpnc

#define _EXPR_(i) rpnb%expr(get_lo32(rpnb%que(i)%p1):get_lo32(rpnb%que(i)%p2))
#define _UEXPR_(i) rpnb%expr(get_up32(rpnb%que(i)%p1):get_up32(rpnb%que(i)%p2))

  subroutine set_function(rpnb,rpnc)
    type(t_rpnb),intent(in)::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    type(t_rpnm),pointer::rpnm
    integer i
    integer kf,km,ka
    integer ac,vc,pc,plen
    integer tc
    do i=1,size(rpnc%que)
       if(rpnc%que(i)%tid/=TID_AFNC) cycle
       kf=rpnc%que(i)%cid
       rpnm=>rpnc%rl%rpnm(kf)
       if(associated(rpnm%que).and.size(rpnm%que)>0) deallocate(rpnm%que)
       if(associated(rpnm%vbuf).and.size(rpnm%vbuf)>0) deallocate(rpnm%vbuf)       
       if(.not.associated(rpnm%na)) allocate(rpnm%na)
       if(i==1) then
          km=find_implicit_mul() ! must be found
          if(km==0) stop "*** UNEXPECTED ERROR in set_function"
          ac=km-2 ! number of arguments
       else
          km=i
          ac=i-1
       end if
       ka=size(rpnc%que) ! must be asn
       tc=size(rpnc%que)-1-(km+1)+1 ! que must end with =
       rpnm%na=ac
       if(tc==0) return !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       rpnm%pars=>rpnc%pars
       rpnm%answer=>rpnc%answer
       rpnm%tmpans=>rpnc%tmpans
       allocate(rpnm%que(tc))
       rpnm%que(1:tc)=rpnc%que(km+1:size(rpnc%que)-1)
       call count_var()
       call init_pnames()
       if(vc>0) allocate(rpnm%vbuf(vc))
       call cp_vbuf()
       exit
    end do
    rpnc%que(:)%tid=TID_NOP
  contains
    
    subroutine init_pnames()
      integer istat
      if(associated(rpnm%pnames)) deallocate(rpnm%pnames)
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
      do ii=1,size(rpnb%que)
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
      if(.not.associated(rpnm%p_vbuf)) allocate(rpnm%p_vbuf)
      rpnm%p_vbuf=0
      do ii=1,tc
         if(rpnm%que(ii)%tid/=TID_VAR) cycle
         jj=ii+km !<<<<<<<<<<<<<<<<
         if(rpnb%que(jj)%tid/=TID_FIG) then ! par
            istat=add_str(rpnm%pnames,_EXPR_(jj),SC_RO,ent=kp)            
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
      do ii=km,size(rpnc%que)
         select case(rpnc%que(ii)%tid)
         case(TID_VAR) 
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

  end subroutine set_function

  subroutine set_macro(rpnb,rpnc)
    type(t_rpnb),intent(in)::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    type(t_rpnm),pointer::rpnm
    integer k,km
    integer i
    integer tc,vc,plen,pc
    integer istat
    do i=1,size(rpnc%que)
       if(rpnc%que(i)%tid/=TID_AMAC) cycle
       k=find_qend() 
       if(k==0) stop "*** UNEXPECTED ERROR in set_macro"
       km=rpnc%que(i)%cid
       rpnm=>rpnc%rl%rpnm(km)
       if(associated(rpnm%que).and.size(rpnm%que)>0) deallocate(rpnm%que)
       if(associated(rpnm%vbuf).and.size(rpnm%vbuf)>0) deallocate(rpnm%vbuf)
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
    end do
    
  contains

    subroutine init_pnames()
      if(associated(rpnm%pnames)) deallocate(rpnm%pnames)
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
      if(.not.associated(rpnm%p_vbuf)) allocate(rpnm%p_vbuf)
      rpnm%p_vbuf=0
      do ii=1,tc
         if(rpnm%que(ii)%tid/=TID_VAR) cycle
         jj=ii+i
         if(rpnb%que(jj)%tid/=TID_FIG) then ! par
            istat=add_str(rpnm%pnames,_EXPR_(jj),SC_RO,ent=kp)            
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
         case(TID_VAR) 
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
      do ii=i+1,size(rpnc%que)
         if(rpnc%que(ii)%tid==TID_QEND) then
            find_qend=ii
            return
         end if
      end do
    end function find_qend
    
  end subroutine set_macro
      
  integer function build_rpnc(rpnb,rpnc,tc)
    type(t_rpnb),intent(in)::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::tc
    complex(cp) v
    real(rp) x
    integer istat
    integer i,k,t
    logical amac,afnc
    amac=.false.
    afnc=.false.
    rpnc%rc=0

    if(associated(rpnc%que).and.size(rpnc%que)>0) deallocate(rpnc%que)
    allocate(rpnc%que(tc))
    rpnc%que(:)%tid=TID_UNDEF
    rpnc%que(:)%cid=0 ! dump_rpnc might refer unset cid as a pointer 
    rpnc%p_vbuf=0
    do i=1,tc
       istat=0
       t=rpnb%que(i)%tid
       if(t>0) t=get_lo32(t)
       select case(t)
       case(TID_BOP4)
          rpnc%que(i)%tid=TID_OP2
          rpnc%que(i)%cid=OID_MUL
       case(TID_UOP1,TID_UOP2)
          rpnc%que(i)%tid=TID_OP1
          rpnc%que(i)%cid=get_oid1(_EXPR_(i))
       case(TID_BOP1,TID_BOP2,TID_BOP3)
          rpnc%que(i)%tid=TID_OP2
          rpnc%que(i)%cid=get_oid2(_EXPR_(i))
       case(TID_ASN,TID_AOP)
          rpnc%que(i)%tid=TID_ASN
          rpnc%que(i)%cid=get_aid(_EXPR_(i))
       case(TID_IFNC)
          rpnc%que(i)%tid=TID_IFNC
          rpnc%que(i)%cid=get_up32(rpnb%que(i)%tid)
       case(TID_UFNC)
          rpnc%que(i)%tid=TID_UFNC
          rpnc%que(i)%cid=get_up32(rpnb%que(i)%tid)
       case(TID_FIG)
          rpnc%que(i)%tid=TID_VAR
          read(_EXPR_(i),*,iostat=istat) x          
          if(istat/=0) then
             WRITE(*,*) "READ iostat=",istat
             STOP "*** UNEXPECTED ERROR in build_rpnc"
          end if
          call put_vbuf(complex(x,rzero))
       case(TID_PAR) 
          if(rpnc%rl%s%n>0) then
             ! find the macro first
             k=find_str(rpnc%rl%s,_EXPR_(i))
             if(k>0) then
                rpnc%que(i)%tid=TID_MAC
                rpnc%que(i)%cid=k
                cycle
             end if
          end if
          rpnc%que(i)%tid=TID_VAR
          istat=find_par(rpnc%pars,_EXPR_(i),v,k)          
          if(amac.and.istat/=0) then
             ! par may not already exist
             istat=add_par_by_entry(rpnc%pars,_EXPR_(i),k)
             if(istat/=0) then
                write(*,*) "*** add_par_by_entry failed: code = ",istat
                istat=RPNERR_ADDPAR
             end if
          end if
          if(istat==0) then
             call put_vbuf(v)
          else
             write(*,*) "*** No such parameter: "//_EXPR_(i)
             istat=RPNERR_NOPAR
          end if
       case(TID_APAR) ! asign a parameter.
          rpnc%que(i)%tid=TID_VAR
          istat=add_par_by_entry(rpnc%pars,_EXPR_(i),k)
          if(istat==0) then
             rpnc%que(i)%cid=loc(rpnc%pars%v(k)) !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
          else
             write(*,*) "*** add_par_by_entry failed: code = ",istat
             istat=RPNERR_ADDPAR
          end if
       case(TID_AFNC)
          afnc=.true.
          rpnc%que(i)%tid=TID_AFNC
          call add_rpnm_entry(SC_FNC)
       case(TID_DPAR)
          rpnc%que(i)%tid=TID_DPAR
          rpnc%que(i)%cid=get_up32(rpnb%que(i)%p1)
       case(TID_AMAC)
          amac=.true.
          rpnc%que(i)%tid=TID_AMAC
          call add_rpnm_entry(SC_MAC)
       case(TID_TOP1)
          rpnc%que(i)%tid=TID_OP3
          rpnc%que(i)%cid=OID_CND ! <<<<<<<<<<<<<<<<<<<<<,
          call find_delim(rpnb%que(i)%p1)
       case(TID_DLM1,TID_DLM2)
          rpnc%que(i)%tid=t
          rpnc%que(i)%cid=0 !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       case(TID_MASN)
          rpnc%que(i)%tid=TID_NOP
          rpnc%que(i)%cid=TID_UNDEF
       case(TID_QEND)
          amac=.false.
          rpnc%que(i)%tid=TID_QEND
          rpnc%que(i)%cid=0
       case default
          CALL DUMP_RPNB(RPNB)
          WRITE(*,*) "que=",i,"tid=",rpnb%que(i)%tid
          STOP "*** UNEXPECTED ERROR in build_rpnc"
       end select
       if(istat/=0) exit
    end do

    build_rpnc=istat

  contains

    subroutine find_delim(pos)
      integer,intent(in)::pos
      integer ii
      do ii=1,size(rpnb%que)
         if(rpnb%que(ii)%tid==TID_DLM1) then
            if(rpnb%que(ii)%p1==pos) then
               rpnc%que(ii)=rpnc%que(i)
               rpnc%que(ii)%cid=get_i32(rpnc%que(ii)%cid,i)
               rpnc%que(i)%tid=TID_DLM1
               rpnc%que(i)%cid=0
               return
            end if
         end if
      end do
    end subroutine find_delim

    subroutine add_rpnm_entry(code)
      integer*1,intent(in)::code
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
    end subroutine add_rpnm_entry
    
    subroutine put_vbuf(val)
      complex(cp),intent(in)::val
      if(rpnc%p_vbuf>=size(rpnc%vbuf)) call inc_vbuf(rpnc,NUM_VBUF_MIN)
      rpnc%p_vbuf=rpnc%p_vbuf+1
      rpnc%vbuf(rpnc%p_vbuf)=val
      rpnc%que(i)%cid=loc(rpnc%vbuf(rpnc%p_vbuf))
    end subroutine put_vbuf

    integer function get_aid(a)
      character*(*),intent(in)::a
      get_aid=AID_NOP ! to avoid warning
      select case(a)
      case("=")
         get_aid=AID_MOV
      case("+=")
         get_aid=AID_INC
      case("-=")
         get_aid=AID_DEC
      case("*=")
         get_aid=AID_MUL
      case("/=")
         get_aid=AID_DIV
      case("^=")
         get_aid=AID_POW
      case default
         STOP "*** UNEXPECTED ERROR in get_aid"
      end select
    end function get_aid

    integer function get_oid1(a)
      character*(*),intent(in)::a
      get_oid1=OID_NOP
      select case(a)
      case("+")
         get_oid1=OID_NOP
      case("-")
         get_oid1=OID_NEG
      case("!")
         get_oid1=OID_FAC
      case default
         STOP "*** UNEXPECTED ERROR in get_oid1"
      end select
    end function get_oid1

    integer function get_oid2(a)
      character*(*),intent(in)::a
      get_oid2=OID_NOP
      select case(a)
      case("+")
         get_oid2=OID_ADD
      case("-")
         get_oid2=OID_SUB
      case("*")
         get_oid2=OID_MUL
      case("/")
         get_oid2=OID_DIV
      case("**","^")
         get_oid2=OID_POW
      case("e")
         get_oid2=OID_EXP
      case default
         STOP "*** UNEXPECTED ERROR in get_oid2"
      end select
    end function get_oid2

  end function build_rpnc

  subroutine dump_rpnb(rpnb)
    type(t_rpnb),intent(in)::rpnb
    integer i,p1lo,p2lo,p1up,p2up,tid
    write(*,*) "rpnb dump:\n# tid p1 p2 expr"
    if(.not.allocated(rpnb%que).or.rpnb%p_que<1) then
       write(*,*) "(empty)"
       return
    end if
    do i=1,rpnb%p_que
       p1lo=get_lo32(rpnb%que(i)%p1)
       p2lo=get_lo32(rpnb%que(i)%p2)
       tid=rpnb%que(i)%tid
       if(tid>0) tid=get_lo32(tid)
       write(*,10) i,tid,p1lo,p2lo
10     format(4(x,i4),x,$)
       if(rpnb%que(i)%tid==TID_VAR) then
          write(*,*) rpnb%expr(p1lo:p2lo)
       else if(rpnb%que(i)%tid==TID_AMAC) then
          p1up=get_up32(rpnb%que(i)%p1)
          p2up=get_up32(rpnb%que(i)%p2)
          write(*,*) rpnb%expr(p1lo:p2lo)//" "//rpnb%expr(p1up:p2up)
       else if(p1lo==0) then
          write(*,*) "(no ref)"
       else if(rpnb%que(i)%tid/=TID_NOP) then
          write(*,*) rpnb%expr(p1lo:p2lo)
       else
          write(*,*)
       end if
    end do
  end subroutine dump_rpnb

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

  subroutine rpn_pop_until(rpnb,tid)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid
    integer i
    i=rpnb%p_buf+1
    do 
       i=i-1
       if(i==0) exit
       select case(rpnb%buf(i)%tid)
       case(TID_BRA,TID_QSTA,TID_IBRA)
          cycle ! skip unclosed bra
       end select
       rpnb%p_que=rpnb%p_que+1
       rpnb%que(rpnb%p_que)=rpnb%buf(i)
       if(rpnb%buf(i)%tid==tid) then
          if(tid<0) rpnb%p_que=rpnb%p_que-1
          i=i-1
          exit
       end if
    end do
    rpnb%p_buf=i
  end subroutine rpn_pop_until

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

  subroutine rpn_try_pop(rpnb,tend)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tend
    integer tid
    do
       if(rpnb%p_buf<=0) exit
       tid=rpnb%buf(rpnb%p_buf)%tid
       select case(tid)
       case(TID_BRA,TID_QSTA,TID_COL)
          rpnb%p_buf=rpnb%p_buf-1
          if(tid==tend) exit
       case(TID_IBRA)
          rpnb%p_buf=rpnb%p_buf-1
       case default
          call rpn_pop(rpnb)
       end select
    end do
  end subroutine rpn_try_pop

  subroutine rpn_try_push(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid,p1,p2
    if(rpnb%p_buf==0) then
       call rpn_push(rpnb,tid,p1,p2)
    else
       do 
          if(rpnb%p_buf<=0) exit
          if(rpnb%buf(rpnb%p_buf)%tid>=tid) then
             call rpn_pop(rpnb)
          else
             exit
          end if
       end do
       call rpn_push(rpnb,tid,p1,p2)
    end if
  end subroutine rpn_try_push

  subroutine revert_tid(rpnb,tid,p1,p2)
    type(t_rpnb),intent(inout)::rpnb
    integer,intent(in)::tid
    integer,intent(in),optional::p1,p2
    rpnb%que(rpnb%p_que)%tid=tid
    if(present(p1)) rpnb%que(rpnb%p_que)%p1=ior(rpnb%que(rpnb%p_que)%p1,ishft(p1,16))
    if(present(p2)) rpnb%que(rpnb%p_que)%p2=ior(rpnb%que(rpnb%p_que)%p2,ishft(p2,16))
  end subroutine revert_tid

  integer function parse_formula(formula,rpnc)
    character*(*),intent(in)::formula
    type(t_rpnc),intent(inout)::rpnc
    type(t_rpnb) rpnb
    integer t,told,btold,istat
    integer p1,p2
    integer bc,kc,pc,ac,fc,oc,fnc,qc,cc,apc,amc
    integer tc
    logical amac
    integer pfasn
    integer p_q1

    call init_rpnb(formula)
    
    call init_stat()
    istat=0

    do 
       t=get_next(rpnb,p1,p2,rpnc%rl%s)
       select case(t)
       case(TID_FIN,TID_SCL)
          if(.not.was_operand()) then
             call print_error()
             istat=RPNERR_PARSER
          else
             if(iand(qc,1)==1) then
                ! close "
                call rpn_try_pop(rpnb,TID_QSTA)
                call rpn_put(rpnb,TID_QEND,0,0)
             end if
             call rpn_pop_all(rpnb)
          end if
          if(t==TID_FIN) then
             exit
          else
             call init_stat
             cycle
          end if
       case(TID_INV,TID_UNDEF)
          call print_error()
          istat=RPNERR_PARSER
       case(TID_PAR)
          pc=pc+1
          select case(told)
          case(TID_FIG,TID_KET)
             call push_implicit_mul()
          end select
          call rpn_put(rpnb,t,p1,p2)
       case(TID_FIG)
          fc=fc+1
          if(told==TID_KET) call push_implicit_mul()
          call rpn_put(rpnb,t,p1,p2)
       case(TID_BOP1,TID_BOP2,TID_BOP3,TID_TOP1) !<<<< TOP1
          oc=oc+1
          if(.not.was_operand()) then
             call print_error()
             istat=RPNERR_PARSER        
          else
             call rpn_try_push(rpnb,t,p1,p2)
          end if
          if(t==TID_TOP1) then !<<<<<<<<<<<<<<<<<<<<<<<<<<,
             call rpn_put(rpnb,TID_DLM1,p1,bc-kc) ! <<<<<<<<<<<<<<<<<<<
          end if
       case(TID_UOP2)
          oc=oc+1
          select case(told)
          case(TID_FIG,TID_PAR,TID_KET)
             call rpn_try_push(rpnb,t,p1,p2)
          case default
             call print_error()
             istat=RPNERR_PARSER
          end select
       case(TID_UOP1)
          oc=oc+1
          call rpn_try_push(rpnb,t,p1,p2)
       case(TID_BRA)
          bc=bc+1
          select case(told)
          case(TID_FIG,TID_PAR,TID_KET)
             call push_implicit_mul()
          end select
          call rpn_push(rpnb,t,p1,p2)
       case(TID_KET)
          if(.not.was_operand()) then
             call print_error()
             istat=RPNERR_PARSER
          else
             call rpn_try_pop(rpnb,TID_BRA)
          end if
          call rpn_put(rpnb,TID_DLM2,p1,bc-kc)
          kc=kc+1
       case(TID_AOP)
          if(check_assignable()) then
             call revert_tid(rpnb,TID_APAR)
             call rpn_try_push(rpnb,t,p1,p2)
          else
             call print_error()
             istat=RPNERR_PARSER
          end if
       case(TID_ASN)
          ac=ac+1
          if(.not.is_fnc_asn()) then
             if(check_assignable()) then
                apc=apc+1
                call revert_tid(rpnb,TID_APAR)
             else
                call print_error()
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
                call print_error()
                istat=RPNERR_PARSER
             else
                call revert_tid(rpnb,TID_AMAC,p1,find_chr(rpnb%expr(p1+1:),"""")+p1)
                rpnb%buf(rpnb%p_buf)%tid=TID_MASN ! it must be TID_ASN
                call rpn_push(rpnb,TID_QSTA,p1,p2)
                amac=.true.
             end if
          else
             if(.not.was_operand()) then
                call print_error()
                istat=RPNERR_PARSER
             else
                call rpn_try_pop(rpnb,TID_QSTA)
                call rpn_put(rpnb,TID_QEND,p1,p2)
             end if
          end if
       case(TID_COMA)
          cc=cc+1
          if(.not.was_operand()) then
             call print_error()
             istat=RPNERR_PARSER
          end if
       case(TID_COL)
          call rpn_pop_until(rpnb,TID_TOP1)
          call rpn_push(rpnb,t,p1,p2)
       case default
          select case(get_lo32(t))
          case(TID_IFNC,TID_UFNC)
             fnc=fnc+1
             select case(told)
             case(TID_KET,TID_FIG)
                call push_implicit_mul()
             end select
             call rpn_try_push(rpnb,t,p1,p2)
             t=get_lo32(t)
          case default
             stop "*** UNEXPECTED ERROR in parse_formula"
          end select
       end select
       btold=told
       told=t
       if(istat/=0) exit
    end do

    tc=rpnb%p_que

    if(kc-bc>0) then
       write(*,*) "*** Too many kets" !<<<<<<<<<<<<<<<<<<<<
       istat=RPNERR_PARSER
    end if

    if(istat==0.and.pfasn/=0) call set_par_dummy()

    if(iand(rpnc%opt,RPNCOPT_DBG)/=0) call dump_rpnb(rpnb)

    if(istat==0) istat=build_rpnc(rpnb,rpnc,tc)

    if(istat==0.and.amac) call set_macro(rpnb,rpnc)

    if(istat==0.and.pfasn/=0) then
       call set_function(rpnb,rpnc)
       istat=RPNSTA_FNCSET
    end if

    parse_formula=istat
    
  contains
    
    subroutine init_stat()
      btold=TID_NOP
      told=TID_NOP
      amac=.false.
      pfasn=0
      bc=0; kc=0; pc=0; ac=0; fc=0; oc=0; fnc=0; qc=0; cc=0
      apc=0; amc=0
      p_q1=rpnb%p_que+1
    end subroutine init_stat

    logical function is_fnc_asn()
      is_fnc_asn=.false.
      if(ac==1.and.bc==1.and.kc==1.and.pc>=1.and.fc==0) then
         if(get_lo32(rpnb%buf(rpnb%p_buf)%tid)==TID_UFNC) then
            rpnb%buf(rpnb%p_buf)%tid=TID_AFNC
            rpnb%buf(rpnb%p_buf)%p1=get_i32(rpnb%buf(rpnb%p_buf)%p1,p1+1)
            rpnb%buf(rpnb%p_buf)%p2=get_i32(rpnb%buf(rpnb%p_buf)%p2,rpnb%len_expr)
            is_fnc_asn=.true.
         else if(rpnb%que(p_q1)%tid==TID_PAR) then
            rpnb%que(1)%tid=TID_AFNC
            pc=pc-1 ! <<<<<<<<<<<<<<<<<<<<<
            rpnb%que(p_q1)%p1=get_i32(rpnb%que(p_q1)%p1,p1+1)
            rpnb%que(p_q1)%p2=get_i32(rpnb%que(p_q1)%p2,rpnb%len_expr)
            is_fnc_asn=.true.
         end if
      end if
      if(.not.is_fnc_asn) return
      pfasn=rpnb%p_que
      if(pc-1/=cc.or.set_dummy_par()==0) then
         is_fnc_asn=.false.
         istat=RPNERR_PARSER
      else
         p1=get_i32(p1,1)
         p2=get_i32(p2,p2-1)          
      end if
    end function is_fnc_asn

    logical function check_assignable()
      check_assignable=.false.
      if(told/=TID_PAR) return
      select case(btold) 
      case(TID_NOP,TID_BRA)
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
      did=0
      do ii=p_q1,pfasn
         if(rpnb%que(ii)%tid==TID_DPAR) then
            did=did+1
            do jj=pfasn+1,rpnb%p_que ! <<<<
               if(rpnb%que(jj)%tid==TID_PAR) then
                  if(_EXPR_(ii)&
                       ==_EXPR_(jj)) then
                     rpnb%que(jj)%tid=TID_DPAR
                     rpnb%que(jj)%p1=get_i32(rpnb%que(jj)%p1,did) !<<<
                  end if
               end if
            end do
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
      case(TID_BOP1,TID_BOP2,TID_BOP3,TID_UOP1,TID_AOP,&
           TID_ASN,TID_BRA,TID_COMA,TID_TOP1,TID_COL,TID_SCL)
         was_operand=.false.
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
      rpnb%expr=trim(adjustl(s))
      rpnb%len_expr=strip(rpnb%expr)
      rpnb%cur_pos=0
      rpnb%old_pos=0
      rpnb%old_tid=0
      rpnb%p_buf=0
      rpnb%p_que=0
      allocate(rpnb%que(rpnb%len_expr*2)) ! << at most
      allocate(rpnb%buf(rpnb%len_expr*2)) ! << at most
    end subroutine init_rpnb
        
    subroutine print_error()
      write(*,*) "*** Syntacs Error at: "
      write(*,*) trim(rpnb%expr)
      write(*,*) repeat(" ",p1-1)//repeat("^",abs(p2)-p1+1) ! some return with negative p2
    end subroutine print_error

    character*1 function next_chr()
      if(p2<rpnb%len_expr) then
         next_chr=rpnb%expr(p2+1:p2+1)
      else
         next_chr=""
      end if
    end function next_chr
    
  end function parse_formula

  integer function rpn_set_formula(ptr_formula,ptr_rpnc)
    integer,intent(in),value::ptr_formula,ptr_rpnc
    character(LEN_FORMULA_MAX) f
    pointer(pf,f)
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    pf=ptr_formula
    pr=ptr_rpnc
    rpn_set_formula=parse_formula(f,rpnc)
  end function rpn_set_formula

  integer function rpn_eval(ptr_rpnc)
    integer,intent(in),value::ptr_rpnc
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    p=ptr_rpnc
    rpn_eval=eval(rpnc)
  end function rpn_eval

  subroutine rpn_get_str_ans(ptr_rpnc,ptr_str)
    integer,intent(in),value::ptr_rpnc,ptr_str
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    character(LEN_STR_ANS_MAX) str
    pointer(ps,str)
    pr=ptr_rpnc
    ps=ptr_str
    str=trim(ztoa(rpn_ans(rpnc)))//achar(0)
  end subroutine rpn_get_str_ans

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

  integer function rpn_regist_parameter(ptr_rpnc,ptr_var,ptr_str)
    integer,intent(in),value::ptr_rpnc,ptr_var,ptr_str
    character(LEN_STR_MAX) name
    type(t_rpnc) rpnc
    pointer(pr,rpnc)
    pr=ptr_rpnc
    call c2fstr(ptr_str,name)
    rpn_regist_parameter=&
         add_par_by_reference(rpnc%pars,name,ptr_var,ro=.true.,dble=.true.)
  end function rpn_regist_parameter

end module rpn
