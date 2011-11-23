module rpn
  use slist
  use plist
  use fpio
  implicit none

  integer,parameter::narg_max=32

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
  integer,parameter::RPNERR_NARG   = 13
  integer,parameter::RPNERR_GETPAR = 14

  integer,parameter::RPN_REC_MAX   =  256
  integer,parameter::NUM_VBUF_MIN  =   32
  integer,parameter::NUM_PBUF_MIN  =   32
  integer,parameter::NUM_RPNM_MIN  =    8
  integer,parameter::LEN_PLIST_MIN = 1024
  integer,parameter::LEN_RLIST_MIN = 1024

  ! meta tid
  integer,parameter::TID_FIN   =   -999
  integer,parameter::TID_UNDEF =  -1000
  integer,parameter::TID_INV   =   -666
  integer,parameter::TID_NOP   =      0
  ! operators
  integer,parameter::TID_ASN   =   1  ! =
  integer,parameter::TID_AOP   =   2 
  integer,parameter::TID_TOP1  =   3  ! ?
  integer,parameter::TID_BOP1  =   4  ! +,-
  integer,parameter::TID_BOP1U =  -4  !
  integer,parameter::TID_BOP2  =   5  ! *,/
  integer,parameter::TID_BOP2U =  -5  !
  integer,parameter::TID_BOP4  =   6  ! implicit * <<<<<<<<<<< 
  integer,parameter::TID_BOP3  =   7  ! ^,**,e
  integer,parameter::TID_BOP3U =  -7  !
  integer,parameter::TID_UOP3  =   8  ! a++
  integer,parameter::TID_UOP1  =   9  ! +a,-a,++a
  integer,parameter::TID_UOP2  =  10  ! !,!!
  integer,parameter::TID_UOP2U = -10  ! !,!!
  integer,parameter::TID_IFNC  =  11  ! sin, cos,...
  integer,parameter::TID_UFNC  =  12  !
  ! braket
  integer,parameter::TID_SCL   = -64  ! ;
  integer,parameter::TID_COL   = -65  ! :
  integer,parameter::TID_IBRA  = -66   ! implicit (
  integer,parameter::TID_BRA   = -67   ! (
  integer,parameter::TID_KET   = -69   ! )
  integer,parameter::TID_QTN   = -70   ! "
  integer,parameter::TID_QEND  = -71
  integer,parameter::TID_QSTA  = -72
  integer,parameter::TID_COMA  = -73  ! ,
  integer,parameter::TID_MASN  = -74  ! = for macro
  integer,parameter::TID_DLM1  = -75
  integer,parameter::TID_DLM2  = -76  ! ket
  integer,parameter::TID_BLK   = -77  ! space and tab
  integer,parameter::TID_HKET  = -78  ! }
  ! 
  integer,parameter::TID_PAR   =  32  ! a,b,c,...
  integer,parameter::TID_PARU  = -32  ! a,b,c,...
  integer,parameter::TID_FIG   =  33  ! 1,2,3,...
  integer,parameter::TID_VAR   =  34  ! fig in rbuf
  integer,parameter::TID_MAC   =  35
  integer,parameter::TID_OP    =  36  ! operators
  integer,parameter::TID_COP   =  37
  integer,parameter::TID_OPN   =  39
  integer,parameter::TID_APAR  =  40  ! par assign
  integer,parameter::TID_AMAC  =  41
  integer,parameter::TID_AFNC  =  42
  integer,parameter::TID_DPAR  =  43  ! dummy par
  integer,parameter::TID_END   =  44
  integer,parameter::TID_ROVAR =  45 

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
     integer,pointer::pfs(:) 
  end type t_rpnc

  integer,parameter::RPNCOPT_NOP    =  0
  integer,parameter::RPNCOPT_DEBUG  =  Z"08000000"
  integer,parameter::RPNCOPT_READY  =  Z"00000001"
  integer,parameter::RPNCOPT_DEG    =  Z"00000002"
  integer,parameter::RPNCOPT_NEW    =  Z"00000004"

  integer,parameter::AID_NOP = 0
  integer,parameter::OID_NOP = 0
  integer,parameter::OID_CND = 1

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
       achar(3)//"mag"//&
       achar(3)//"arg"//&
       achar(5)//"gamma"//&
       achar(6)//"lgamma"//&
       achar(3)//"psy"//&
       achar(3)//"min"//&
       achar(3)//"max"//&
       achar(4)//"gami"//&
       achar(5)//"deint"//&
       achar(3)//"sum"//&
       achar(3)//"ave"//&
       achar(3)//"var"//&
       achar(4)//"uvar"//&
       achar(4)//"sum2"//&
       achar(0)
  integer,parameter::FID_SIN       =  1
  integer,parameter::FID_COS       =  2 
  integer,parameter::FID_TAN       =  3
  integer,parameter::FID_SINH      =  4
  integer,parameter::FID_COSH      =  5
  integer,parameter::FID_TANH      =  6
  integer,parameter::FID_ASIN      =  7
  integer,parameter::FID_ACOS      =  8
  integer,parameter::FID_ATAN      =  9
  integer,parameter::FID_ASINH     = 10
  integer,parameter::FID_ACOSH     = 11
  integer,parameter::FID_ATANH     = 12
  integer,parameter::FID_EXP       = 13
  integer,parameter::FID_LOG       = 14
  integer,parameter::FID_LN        = 15
  integer,parameter::FID_SQRT      = 16
  integer,parameter::FID_ABS       = 17
  integer,parameter::FID_INT       = 18
  integer,parameter::FID_FRAC      = 19
  integer,parameter::FID_CONJG     = 20
  integer,parameter::FID_NINT      = 21
  integer,parameter::FID_REAL      = 22
  integer,parameter::FID_IMAG      = 23
  integer,parameter::FID_MAG       = 24
  integer,parameter::FID_ARG       = 25
  integer,parameter::FID_GAMMA     = 26
  integer,parameter::FID_LGAMMA    = 27
  integer,parameter::FID_PSY       = 28
  integer,parameter::FID_ARG1_END  = 28 !<<<<<<<<
  integer,parameter::FID_MIN       = 29
  integer,parameter::FID_MAX       = 30
  integer,parameter::FID_GAMI      = 31
  integer,parameter::FID_ARG2_END  = 31 !<<<<<<<<
  integer,parameter::FID_DEINT     = 32
  integer,parameter::FID_ARG3_END  = 32 !<<<<<<<<
  integer,parameter::FID_SUM       = 33
  integer,parameter::FID_AVE       = 34
  integer,parameter::FID_VAR       = 35
  integer,parameter::FID_UVAR      = 36
  integer,parameter::FID_SUM2      = 37

  interface put_vbuf
     module procedure put_vbuf_r
     module procedure put_vbuf_z
  end interface put_vbuf

contains

  complex(cp) function rpn_ans(rpnc)
    type(t_rpnc),intent(in)::rpnc
    rpn_ans=rpnc%answer
  end function rpn_ans
  
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
    use zmath
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
    allocate(rpnc%pfs(3))
    rpnc%pfs(1)=loc(zm_f1)
    rpnc%pfs(2)=loc(zm_f2)
    rpnc%pfs(3)=loc(zm_f3)
    call init_par(rpnc)
    call init_rpnlist(rpnc%rl)
    rpnc%opt=RPNCOPT_NOP
    init_rpnc=p
  end function init_rpnc

  integer function cp_rpnc(rpnc_in)
    type(t_rpnc),intent(in)::rpnc_in
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    p=init_rpnc()
!!$    call cp_par(rpnc%pars)
!!$    call cp_rpnlist(rpnc%rl)
  end function cp_rpnc

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
    if(associated(rpnc%pfs)) deallocate(rpnc%pfs)
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
       select case(rpnc%que(j)%tid)
       case(TID_VAR,TID_PAR,TID_ROVAR)
          get_operand=j
          return
       end select
    end do
  end function get_operand

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

  integer function eval_c(rpnc,i)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer kd,ke,n,kz
    integer od1,j
    complex(cp) v1
    pointer(pv1,v1)

    kd=get_up32(rpnc%que(i)%cid)
    od1=get_operand(rpnc,i-1)
    if(od1==0) then
       eval_c=RPNERR_NOOP
       return
    end if
    pv1=rpnc%que(od1)%cid
    ke=find_end()
    if(ke==0) then
       kz=size(rpnc%que)
    else
       kz=ke
    end if
    kz=trim_end(kz)
    if(realpart(v1)/=0.0_rp) then
       ! true case
       rpnc%que(kd:kz)%tid=TID_NOP
       n=(kd-1)-(i+1)+1
       j=i+1
    else
       ! false case
       rpnc%que(i:kd)%tid=TID_NOP
       n=(kz)-(kd+1)+1
       if(ke/=0) n=n-1
       j=kd+1
    end if
    if(n==1) then
       select case(rpnc%que(j)%tid)
       case(TID_VAR,TID_PAR) 
          pv1=rpnc%que(j)%cid
          rpnc%tmpans=v1
       end select
    end if
    rpnc%que(od1)%tid=TID_NOP
     
    eval_c=0

  contains
 
    integer function trim_end(kend)
      integer,intent(in)::kend
      integer ii
      do ii=kend,1,-1
         if(rpnc%que(ii)%tid/=TID_NOP) then
            trim_end=ii
            return
         end if
      end do
      trim_end=0
    end function trim_end

    integer function find_end()
      integer ii
      integer c
      find_end=0
      c=rpnc%que(kd)%cid ! bra-ket count
      do ii=i,size(rpnc%que)
         select case(rpnc%que(ii)%tid)
         case(TID_END)
            find_end=ii
            return
         case(TID_DLM2)
            if(rpnc%que(ii)%cid==c) then
               find_end=ii
               return
            end if
         end select
      end do
    end function find_end
    
  end function eval_c

  subroutine set_result(rpnc,i,n,ks,v)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i,n
    integer,intent(in)::ks(n)
    complex(cp),intent(in)::v
    integer j
    logical set
    complex(cp) z
    pointer(pz,z)
    set=.false.
    do j=n,1,-1
       if(rpnc%que(ks(j))%tid==TID_VAR.and..not.set) then
          pz=rpnc%que(ks(j))%cid
          z=v
          set=.true.
       else
          rpnc%que(ks(j))%tid=TID_NOP
       end if
    end do
    if(.not.set) then
       call put_vbuf(rpnc,i,v)
    else
       rpnc%que(i)%tid=TID_NOP
    end if
    rpnc%tmpans=v
  end subroutine set_result
  
  integer function get_operands(rpnc,i,n,ps,ks)
    type(t_rpnc),intent(in)::rpnc
    integer,intent(in)::i,n
    integer,intent(out),optional::ps(0:n) 
    integer,intent(out),optional::ks(n)
    integer k,j
    k=i-1
    do j=n,1,-1
       k=get_operand(rpnc,k)
       if(k<=j-1) then
          get_operands=RPNERR_NOOP
          return
       end if
       if(present(ps)) ps(j)=rpnc%que(k)%cid
       if(present(ks)) ks(j)=k
       k=k-1
    end do
    get_operands=0
  end function get_operands

  recursive function eval_n(rpnc,i) result(istat)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    interface
       function fn(n,ps)
         use fpio, only: cp
         complex(cp) fn
         integer,intent(in)::n
         integer,intent(in)::ps(0:n)
       end function fn
    end interface
    pointer(pfn,fn)
    integer istat
    integer na
    integer pvs(0:narg_max)
    integer ods(1:narg_max)
    complex(cp) v
    integer tid

    na=get_up32(rpnc%que(i)%tid)
    tid=get_lo32(rpnc%que(i)%tid)

    if(i-1<na) then
       istat=RPNERR_NOOP
       return
    end if
    
    istat=get_operands(rpnc,i,na,ks=ods,ps=pvs)
    if(istat/=0) return

    pvs(0)=rpnc%que(i)%cid
    if(tid/=TID_OPN) then
       pfn=rpnc%pfs(na) 
    else
       pfn=rpnc%que(i)%cid
    end if
    v=fn(na,pvs)
    if(tid==TID_AOP) call set_assign()

    call set_result(rpnc,i,na,ods,v)
    
  contains
    
    subroutine set_assign()
      complex(cp) z
      pointer(pz,z)
      pz=pvs(1)
      z=v
      rpnc%opt=ior(rpnc%opt,RPNCOPT_NEW)
    end subroutine set_assign

  end function eval_n

  recursive function eval_uf(rpnc,i) result(istat)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer istat
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) fnc
    integer j
    integer kp
    integer,allocatable::ods(:)
    complex(cp) v
    pointer(pv,v)

    rpnm=>rpnc%rl%rpnm(rpnc%que(i)%cid)

    if(i-1<rpnm%na) then
       istat=RPNERR_NOOP
       return
    end if

    allocate(ods(rpnm%na))
    istat=get_operands(rpnc,i,rpnm%na,ks=ods)
    if(istat/=0) then
       deallocate(ods)
       return
    end if

    allocate(fnc%que(size(rpnm%que)))
    allocate(fnc%vbuf(NUM_VBUF_MIN))
    allocate(fnc%p_vbuf)
    fnc%que=rpnm%que
    fnc%p_vbuf=0

    fnc%pars   => rpnc%pars
    fnc%answer => rpnc%answer
    fnc%tmpans => rpnc%tmpans
    fnc%rl     => rpnc%rl
    fnc%rc     => rpnc%rc
    fnc%pfs    => rpnc%pfs
    fnc%opt    => rpnc%opt
    
    istat=0
    do j=1,size(fnc%que)
       select case(fnc%que(j)%tid)
       case(TID_FIG)
          fnc%que(j)%cid=loc(rpnm%vbuf(fnc%que(j)%cid))
          fnc%que(j)%tid=TID_ROVAR
       case(TID_PAR,TID_APAR)
          call set_par_ptr(kp)
          if(istat/=0) exit 
          fnc%que(j)%cid=get_par_loc(fnc%pars,kp)
          fnc%que(j)%tid=TID_PAR
       case(TID_DPAR)
          pv=rpnc%que(ods(fnc%que(j)%cid))%cid
          call put_vbuf(fnc,j,v)
       end select
    end do

    if(istat==0) istat=eval(fnc)
    
    if(istat==0)&
       call set_result(rpnc,i,rpnm%na,ods,fnc%answer)

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

  end function eval_uf
  
  subroutine put_vbuf_r(rpnc,i,v)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(inout)::i
    real(rp),intent(in)::v
    call put_vbuf_z(rpnc,i,complex(v,rzero))
  end subroutine put_vbuf_r

  subroutine put_vbuf_z(rpnc,i,v)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    complex(cp),intent(in)::v
    if(rpnc%p_vbuf>=size(rpnc%vbuf)) call inc_vbuf(rpnc,NUM_VBUF_MIN)
    rpnc%p_vbuf=rpnc%p_vbuf+1
    rpnc%vbuf(rpnc%p_vbuf)=v
    rpnc%que(i)%cid=loc(rpnc%vbuf(rpnc%p_vbuf))
    rpnc%que(i)%tid=TID_VAR
  end subroutine put_vbuf_z

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
    mac%pfs    => rpnc%pfs
    mac%opt    => rpnc%opt

    istat=0
    do j=1,size(mac%que)
       select case(mac%que(j)%tid) 
       case(TID_PAR,TID_APAR)
          call set_par_ptr(kp)
          if(istat/=0) exit
          mac%que(j)%cid=get_par_loc(mac%pars,kp)
          mac%que(j)%tid=TID_PAR
       case(TID_FIG)
          mac%que(j)%cid=loc(rpnm%vbuf(mac%que(j)%cid))
          mac%que(j)%tid=TID_ROVAR
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
       select case(get_lo32(rpnc%que(i)%tid))
       case(TID_OP,TID_OPN,TID_AOP)
          istat=eval_n(rpnc,i)
       case(TID_COP)
          istat=eval_c(rpnc,i)
       case(TID_MAC)
          istat=eval_m(rpnc,i)
       case(TID_UFNC)
          istat=eval_uf(rpnc,i)
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

    if(iand(rpnc%opt,RPNCOPT_NEW)/=0) then
       istat=realloc_new(rpnc%pars)
    end if
    istat=remove_dup(rpnc%pars)

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
       get_tid=TID_UOP2U
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
    case(" ","\t")
       get_tid=TID_BLK
    case("}")
       get_tid=TID_HKET
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
                   t=get_i32(TID_UFNC,kf)
                else if(is_int_fnc(rpnb%expr(p1:p2),kf)) then
                   t=get_i32(TID_IFNC,kf)
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
    integer i1,i2,i

    if(ent==0) then
       i1=1
       i2=rpnc%rl%s%n
    else
       i1=ent
       i2=ent
    end if
    
    do i=i1,i2
       if(i>rpnc%rl%s%n.or.i<=0.or.i>size(rpnc%rl%rpnm)&
            .or.get_str_ptr(rpnc%rl%s,i,ptr,len,code)/=0) then
          write(*,*) "*** dump_rpnm: no such entry: ",i
          cycle
       end if
       rpnm=>rpnc%rl%rpnm(i)
       if(iand(code,SC_MAC)/=0) then
          write(*,*) "MACRO entry: ",i
       else
          write(*,*) "FUNCTION entry:",i
          if(get_str_ptr(rpnm%pnames,2,ptr,len)/=0) then
             write(*,*) "???"
             cycle !<<<<<<<<<
          end if
       end if
       write(*,*) "name: "//trim(cpstr(ptr,len))

       if(associated(rpnm%pnames).and.get_str_ptr(rpnm%pnames,1,ptr,len)==0) then
          write(*,*) "definition: "//trim(cpstr(ptr,len))
          tmprpnc%que=>rpnm%que
          tmprpnc%vbuf=>rpnm%vbuf
          tmprpnc%p_vbuf=>rpnm%p_vbuf
          tmprpnc%pars=>rpnm%pars
          tmprpnc%answer=>rpnm%answer
          tmprpnc%tmpans=>rpnm%tmpans
          tmprpnc%rl=>rpnc%rl
          tmprpnc%rc=>rpnc%rc
          tmprpnc%pfs=>rpnc%pfs
          if(associated(rpnm%na)) write(*,*) "number of arguments = ",rpnm%na
          call dump_rpnc(tmprpnc,i)
          call dump_slist(rpnm%pnames)
       else
          write(*,*) "(empty)"
       end if
       write(*,*)
    end do
  end subroutine dump_rpnm
  
  subroutine dump_rpnc(rpnc,mid)
    use slist
    type(t_rpnc),intent(in)::rpnc
    integer,intent(in),optional::mid
    type(t_rpnm),pointer::rpnm
    integer i,t,istat
    integer ptr,len
    complex(cp) z
    complex(cp) v
    pointer(pv,v)
    write(*,*) "rpnc dump:"
    if(.not.associated(rpnc%que).or.size(rpnc%que)<1) then
       write(*,*) "(empty)"
       return
    end if
    if(.not.present(mid).and.iand(rpnc%opt,RPNCOPT_READY)==0) then
       write(*,*) "(not set)"
       return
    end if
    write(*,*) "# tid cid value"
    if(present(mid)) rpnm=>rpnc%rl%rpnm(mid)
    do i=1,size(rpnc%que)
       t=rpnc%que(i)%tid
       write(*,10) i,t
       select case(t)
       case(TID_VAR,TID_PAR,TID_FIG)
          write(*,11) rpnc%que(i)%cid
          if(present(mid)) then
             if(t/=TID_FIG) then
                istat=get_str_ptr(rpnm%pnames,rpnc%que(i)%cid,ptr,len)
                write(*,*) trim(cpstr(ptr,len))
                cycle
             else
                z=rpnm%vbuf(rpnc%que(i)%cid)
             end if
          else
             pv=rpnc%que(i)%cid
             z=v
             if(pv==0) then
                write(*,*) "(undef)"
                cycle
             end if
          end if
          write(*,*) trim(ztoa(z,fmt=DISP_FMT_RAW))
       case(TID_OP,TID_OPN)
          write(*,14) rpnc%que(i)%cid
       case(TID_DPAR)
          write(*,16) rpnc%que(i)%cid,"(dummy par)"
       case default
          write(*,15) rpnc%que(i)%cid
       end select
    end do
10  format(2(x,i4),$)
11  format(x,z8,$)
13  format(x,i4,x,z8,x,a)
14  format(x,z8)
15  format(x,i8)
16  format(x,i8,x,a)
    write(*,*) "vbuf dump:"
    write(*,*) "size= ",rpnc%p_vbuf
    if(rpnc%p_vbuf>0) then
       do i=1,rpnc%p_vbuf
          write(*,13) i,loc(rpnc%vbuf(i)),trim(ztoa(rpnc%vbuf(i),fmt=DISP_FMT_RAW))
       end do
    end if
    if(.not.present(mid)) write(*,*)
  end subroutine dump_rpnc

#define _EXPR_(i) rpnb%expr(get_lo32(rpnb%que(i)%p1):get_lo32(rpnb%que(i)%p2))
#define _UEXPR_(i) rpnb%expr(get_up32(rpnb%que(i)%p1):get_up32(rpnb%que(i)%p2))

  integer function add_rpnm_entry(rpnc,rpnb,i,code,k)
    type(t_rpnc),intent(inout)::rpnc
    type(t_rpnb),intent(in)::rpnb
    integer i
    integer*1,intent(in)::code
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
       if(associated(rpnm%que).and.size(rpnm%que)>0) deallocate(rpnm%que)
       if(associated(rpnm%vbuf).and.size(rpnm%vbuf)>0) deallocate(rpnm%vbuf)       
       if(.not.associated(rpnm%na)) allocate(rpnm%na)
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
      if(.not.associated(rpnm%p_vbuf)) allocate(rpnm%p_vbuf)
      rpnm%p_vbuf=0
      do ii=1,size(rpnm%que)
         select case(rpnm%que(ii)%tid)
         case(TID_VAR,TID_PAR)
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
         case(TID_VAR,TID_PAR) 
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
         select case(rpnm%que(ii)%tid)
         case(TID_VAR,TID_PAR)
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
         case(TID_VAR,TID_PAR) 
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
    type(t_rpnb),intent(in)::rpnb
    type(t_rpnc),intent(inout),target::rpnc
    real(rp) x
    complex(rp) z
    integer istat
    integer i,k,t
    logical amac,afnc
    integer p_q1
    integer*1 c
    type(t_rpnq),pointer::q

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
       t=rpnb%que(i)%tid
       if(t>0) t=get_lo32(t)
       select case(t)
          !
          ! operators 
          !
       case(TID_UOP1,TID_UOP2,TID_UOP3)
          q%tid=get_i32(TID_OP,1)
          q%cid=get_oid1()
       case(TID_BOP1,TID_BOP2,TID_BOP3,TID_BOP4)
          q%tid=get_i32(TID_OP,2)
          q%cid=get_oid2()
       case(TID_TOP1)
          q%tid=TID_COP
          q%cid=OID_CND ! <<<<<<<<<<<<<<<<<<<<<,
          call find_delim(rpnb%que(i)%p1)
       case(TID_ASN,TID_AOP)
          q%tid=get_i32(TID_AOP,2)
          q%cid=get_aid()
       case(TID_IFNC)
          q%cid=get_fid(get_up32(rpnb%que(i)%tid))
          select case(get_up32(rpnb%que(i)%p2))
          case(1) 
             q%tid=get_i32(TID_OP,1)
          case(2)
             q%tid=get_i32(TID_OP,2)
          case(3)
             q%tid=get_i32(TID_OP,3)
          case(narg_max)
             q%tid=get_i32(TID_OPN,narg_max-get_up32(rpnb%que(i)%p1)+1)
             cycle
          end select
          if(get_up32(rpnb%que(i)%p1)/=1) istat=RPNERR_NARG ! <<<<<<<<<
       case(TID_UFNC)
          q%tid=TID_UFNC
          q%cid=get_up32(rpnb%que(i)%tid)
          if(get_up32(rpnb%que(i)%p1)/=1) istat=RPNERR_NARG ! <<<<<<<<<
       case(TID_APAR) ! asign a parameter.
          q%tid=TID_PAR
          istat=add_par_by_entry(rpnc%pars,_EXPR_(i),k)
          if(istat==0) then
             istat=alloc_par(rpnc%pars,k,PK_COMP)
             q%cid=get_par_loc(rpnc%pars,k)
          else
             write(*,*) "*** add_par_by_entry failed: code = ",istat
             istat=RPNERR_ADDPAR
          end if
          !
          ! operands
          !
       case(TID_FIG)
          q%tid=TID_VAR
          read(_EXPR_(i),*,iostat=istat) x          
          if(istat/=0) then
             WRITE(*,*) "READ iostat=",istat
             STOP "*** UNEXPECTED ERROR in build_rpnc"
          end if
          call put_vbuf(rpnc,i,x)
       case(TID_PAR) 
          if(rpnc%rl%s%n>0) then
             ! find the macro first
             k=find_str(rpnc%rl%s,_EXPR_(i))
             if(k>0) then
                q%tid=TID_MAC
                q%cid=k
                cycle
             end if
          end if
          c=0
          istat=find_par(rpnc%pars,_EXPR_(i),ent=k,code=c)          
          if(amac.and.istat/=0) then
             ! par may not already exist
             istat=add_par_by_entry(rpnc%pars,_EXPR_(i),k)
             if(istat/=0) then
                write(*,*) "*** add_par_by_entry failed: code = ",istat
                istat=RPNERR_ADDPAR
             else
                istat=alloc_par(rpnc%pars,k,PK_COMP,.true.)
             end if
          end if
          if(istat==0) then
             q%tid=TID_PAR
             q%cid=get_par_loc(rpnc%pars,k)
             if(q%cid==0) then
                write(*,*) "*** get_par failed: code = ",istat
                istat=RPNERR_GETPAR
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
          q%cid=get_up32(rpnb%que(i)%p1)
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
          q%cid=rpnb%que(i)%p2 ! bc-kc 
       case(TID_SCL)
          q%tid=TID_END
          q%cid=0
          if(amac) istat=set_macro(rpnb,rpnc,p_q1)
          if(afnc) istat=set_function(rpnb,rpnc,p_q1)
          ! istat=RPNSTA_FNCSET
          amac=.false.
          afnc=.false.
          p_q1=i+1
       case default
          CALL DUMP_RPNB(RPNB)
          WRITE(*,*) "que=",i,"tid=",rpnb%que(i)%tid
          STOP "*** UNEXPECTED ERROR in build_rpnc"
       end select
       if(istat/=0) then
          call print_error(rpnb%expr(1:rpnb%len_expr),get_lo32(rpnb%que(i)%p1),get_lo32(rpnb%que(i)%p2)) 
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

#define isdeg iand(rpnc%opt,RPNCOPT_DEG)/=0

    integer function get_fid(fid)
      use zmath
      integer,intent(in)::fid
      select case(fid)
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
      case(FID_REAL)
         get_fid=loc(zm_real)
      case(FID_IMAG)
         get_fid=loc(zm_imag)
      case(FID_MAG)
         get_fid=loc(zm_mag)
      case(FID_ARG)
         get_fid=loc(zm_arg)
      case(FID_GAMMA)
         get_fid=loc(zm_gamma)
      case(FID_LGAMMA)
         get_fid=loc(zm_lgamma)
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
         get_oid1=loc(zm_neg) 
      case("!")
         get_oid1=loc(zm_fac) 
      case("!!")
         get_oid1=loc(zm_dfac)
      case("++")
         get_oid1=loc(zm_inc)
      case("--")
         get_oid1=loc(zm_dec)
      case default
         STOP "*** UNEXPECTED ERROR in get_oid1"
      end select
    end function get_oid1

    integer function get_oid2()
      use zmath
      if(t==TID_BOP4) then
         get_oid2=loc(zm_mul)
         return
      end if
      get_oid2=OID_NOP
      select case(_EXPR_(i))
      case("+")
         get_oid2=loc(zm_add)   
      case("-")
         get_oid2=loc(zm_sub)   
      case("*")
         get_oid2=loc(zm_mul)   
      case("/")
         get_oid2=loc(zm_div)
      case("**","^")
         get_oid2=loc(zm_pow)   
      case("e")
         get_oid2=loc(zm_exp10) 
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
    if(rpnb%p_buf==0) then
       call rpn_push(rpnb,tid,p1,p2)
    else
       do 
          if(rpnb%p_buf<=0) exit
          if(rpnb%buf(rpnb%p_buf)%tid>=tid&
               .and..not.(tid==TID_TOP1.and.rpnb%buf(rpnb%p_buf)%tid==TID_TOP1)) then
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
    integer t,told,btold,istat
    integer p1,p2
    integer bc,kc,pc,ac,fc,oc,fnc,qc,cc,apc,amc,clc,tc
    logical amac
    integer pfasn
    integer p_q1

    call init_rpnb(formula)

    rpnc%opt=iand(rpnc%opt,not(RPNCOPT_READY))

    call init_stat()
    istat=0

    do 
       t=get_next(rpnb,p1,p2,rpnc%rl%s)
       select case(t)
       case(TID_BLK)
          cycle
       case(TID_FIN,TID_SCL)
          if(.not.was_operand().or.tc/=clc) then
             istat=RPNERR_PARSER
             exit
          else
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
          select case(told)
          case(TID_FIG,TID_KET,TID_PAR,TID_UOP2,TID_UOP3)
             call push_implicit_mul()
          end select
          call rpn_put(rpnb,t,p1,p2)
       case(TID_FIG)
          fc=fc+1
          select case(told)
          case(TID_KET,TID_PAR,TID_FIG,TID_UOP2,TID_UOP3) 
             call push_implicit_mul()
          end select
          call rpn_put(rpnb,t,p1,p2)
       case(TID_BOP1,TID_BOP2,TID_BOP3,TID_TOP1) !<<<< TOP1
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
       case(TID_UOP1)
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
             call rpn_pop_all_bra(rpnb)
          end if
          kc=bc
       case(TID_KET)
          if(.not.was_operand()) then
             istat=RPNERR_PARSER
          else
             call rpn_try_pop(rpnb,TID_BRA,bc-kc,p1)
          end if
          kc=kc+1
       case(TID_AOP)
          if(check_assignable()) then
             call revert_tid(rpnb,TID_PAR)
             call rpn_try_push(rpnb,t,p1,p2)
          else
             istat=RPNERR_PARSER
          end if
       case(TID_ASN)
          ac=ac+1
          if(.not.is_fnc_asn()) then
             if(check_assignable()) then
                apc=apc+1
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
                call revert_tid(rpnb,TID_AMAC,p1,find_chr(rpnb%expr(p1+1:),"""")+p1)
                rpnb%buf(rpnb%p_buf)%tid=TID_MASN ! it must be TID_ASN
                call rpn_push(rpnb,TID_QSTA,p1,p2)
                amac=.true.
             end if
          else
             if(.not.was_operand()) then
                istat=RPNERR_PARSER
             else
                call rpn_try_pop(rpnb,TID_QSTA)
                call rpn_put(rpnb,TID_QEND,p1,p2)
             end if
          end if
       case(TID_COMA)
          cc=cc+1
          if(.not.was_operand()) then
             istat=RPNERR_PARSER
          else
             call rpn_pop_until(rpnb,TID_BRA,.true.)
             if(.not.check_narg()) exit
          end if
       case(TID_COL)
          clc=clc+1
          call rpn_pop_until(rpnb,TID_TOP1)
          call rpn_push(rpnb,t,p1,p2)
       case default
          select case(get_lo32(t))
          case(TID_IFNC,TID_UFNC)
             fnc=fnc+1
             select case(told)
             case(TID_KET,TID_FIG,TID_PAR,TID_UOP2,TID_UOP3)
                call push_implicit_mul()
             end select
             call set_narg
             call rpn_try_push(rpnb,t,p1,p2)
          case default
             stop "*** UNEXPECTED ERROR in parse_formula"
          end select
          t=get_lo32(t)
       end select
       if(istat/=0) exit
       btold=told
       told=t
    end do

    if(iand(rpnc%opt,RPNCOPT_DEBUG)/=0) call dump_rpnb(rpnb)

    if(istat==0) then
       istat=build_rpnc(rpnb,rpnc)       
    else
       call print_error(rpnb%expr,get_lo32(p1),get_lo32(p2))
    end if

    parse_formula=istat
    
  contains
    
    subroutine init_stat()
      btold=TID_UNDEF
      told=TID_UNDEF
      amac=.false.
      pfasn=0
      bc=0; kc=0; pc=0; ac=0; fc=0; oc=0; fnc=0; qc=0; cc=0
      apc=0; amc=0; clc=0; tc=0
      p_q1=rpnb%p_que+1
    end subroutine init_stat

    logical function check_narg()
      integer na
      type(t_rrpnq),pointer::b
      check_narg=.false.
      istat=RPNERR_PARSER
      if(rpnb%p_buf<=1) return
      b=>rpnb%buf(rpnb%p_buf-1)
      na=get_up32(b%p1)
      select case(get_lo32(b%tid))
      case(TID_UFNC,TID_IFNC)
         b%p1=get_i32(get_lo32(b%p1),na-1)
      case(TID_BOP4) 
         ! comma in function definition
         ! simply ignore this case
      case default
         return
      end select
      istat=0
      check_narg=.true.
    end function check_narg

    subroutine set_narg()
      integer na
      select case(get_lo32(t))
      case(TID_IFNC)
         if(get_up32(t)<=FID_ARG1_END) then
            na=1
         else if(get_up32(t)<=FID_ARG2_END) then
            na=2
         else
            na=narg_max ! arbitrary
         end if
      case(TID_UFNC)
         na=rpnc%rl%rpnm(get_up32(t))%na
      end select
      p1=get_i32(p1,na)
      p2=get_i32(p2,na)
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
            q=>rpnb%buf(rpnb%p_buf)
            q%tid=TID_AFNC
            q%p1=get_i32(get_lo32(q%p1),p1+1)
            q%p2=get_i32(get_lo32(q%p2),ii)
            is_fnc_asn=.true.
         else if(rpnb%que(p_q1)%tid==TID_PAR) then
            q=>rpnb%que(p_q1)
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
      if(told/=TID_PAR) return
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
      rpnb%expr=s(1:LEN_FORMULA_MAX)
      rpnb%len_expr=strip(rpnb%expr)
      rpnb%cur_pos=0
      rpnb%old_pos=0
      rpnb%old_tid=TID_UNDEF ! <<<<<<<<<<<<<<
      rpnb%p_buf=0
      rpnb%p_que=0
      allocate(rpnb%que(rpnb%len_expr*2)) ! << at most
      allocate(rpnb%buf(rpnb%len_expr*2)) ! << at most
    end subroutine init_rpnb
        
    character*1 function next_chr()
      if(p2<rpnb%len_expr) then
         next_chr=rpnb%expr(p2+1:p2+1)
      else
         next_chr=""
      end if
    end function next_chr
    
  end function parse_formula

end module rpn
