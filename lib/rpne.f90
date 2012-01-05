module rpne
  use slist
  use plist
  use fpio
  use rpnd
  implicit none

#define is_set(x) (iand(rpnc%opt,(x))/=0)
#define is_uset(x) (iand(rpnc%opt,(x))==0)
#define set_opt(x) rpnc%opt=ior(rpnc%opt,(x))
#define cle_opt(x) rpnc%opt=iand(rpnc%opt,not(x))

contains

  character(LEN_STR_ANS_MAX) function rpn_sans(rpnc)
    use memio
    type(t_rpnc),intent(in)::rpnc
    integer f
    if(is_uset(RPNCOPT_OUTM)) then
       f=ishft(rpnc%opt,-32) !DISP_FMT_NORM
    else if(is_set(RPNCOPT_OBIN)) then
       f=DISP_FMT_BIN
    else if(is_set(RPNCOPT_OOCT)) then
       f=DISP_FMT_OCT
    else if(is_set(RPNCOPT_OHEX)) then
       f=DISP_FMT_HEX
    end if
    if(is_uset(RPNCOPT_RATIO)) then
          rpn_sans=trim(ztoa(rpnc%answer,fmt=f))
    else
       rpn_sans=trim(itoa(int(realpart(rpnc%answer),kind=8),f))
       if(int(imagpart(rpnc%answer),kind=8)>1) then
          rpn_sans=trim(rpn_sans)//"/"//trim(itoa(int(imagpart(rpnc%answer),kind=8),f))
       end if
    end if
  end function rpn_sans

  complex(cp) function rpn_ans(rpnc)
    type(t_rpnc),intent(in)::rpnc
    rpn_ans=rpnc%answer
  end function rpn_ans

  real(rp) function rpn_rans(rpnc)
    type(t_rpnc),intent(in)::rpnc
    rpn_rans=realpart(rpnc%answer)
  end function rpn_rans

  real(dp) function rpn_dans(rpnc)
    type(t_rpnc),intent(in)::rpnc
    rpn_dans=real(realpart(rpnc%answer),kind=dp)
  end function rpn_dans

  integer function get_operand(rpnc,i)
    type(t_rpnc),intent(in)::rpnc
    integer,intent(in)::i
    integer j
    get_operand=0
    do j=i,1,-1
       select case(rpnc%que(j)%tid)
       case(TID_VAR,TID_PAR,TID_ROVAR,TID_LVAR_T,TID_LVAR_F,TID_CPAR)
          get_operand=j
          return
       end select
    end do
  end function get_operand

  integer function eval_c(rpnc,i)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer kd,ke,n,kz
    integer od1,j
    logical ok
    complex(cp) v
    pointer(pv,v)

    kd=get_up32(rpnc%que(i)%cid)
    od1=get_operand(rpnc,i-1)

    if(od1==0) then
       eval_c=RPNERR_NOOP
       return
    end if
    select case(rpnc%que(od1)%tid)
    case(TID_LVAR_T)
       ok=.true.
    case(TID_LVAR_F)
       ok=.false.
    case default
       eval_c=RPNCERR_INVARG
       return
    end select

    ke=find_end()
    if(ke==0) then
       kz=size(rpnc%que)
    else
       kz=ke
    end if
    kz=trim_end(kz)
    if(ok) then
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
       case(TID_VAR,TID_PAR,TID_CPAR,TID_ROVAR) ! ROVAR?
          pv=rpnc%que(j)%cid
          rpnc%tmpans=v
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

  subroutine set_result(rpnc,i,v,n,ks,logical)
    use zmath, only: zfalse
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    complex(cp),intent(in)::v
    integer,intent(in),optional::n
    integer,intent(in),optional::ks(:)
    logical,intent(in),optional::logical
    integer j,tid,k1
    logical var
    complex(cp) z
    pointer(pz,z)
    var=.true.
    tid=TID_VAR
    if(present(logical)) then
       if(logical) then
          var=.false.
          if(v==zfalse) then
             tid=TID_LVAR_F
          else
             tid=TID_LVAR_T
          end if
       end if
    end if
    k1=0
    if(present(ks)) then
       if(var) then
          ! find the first writable var
          do j=n,1,-1
             select case(rpnc%que(ks(j))%tid)
             case(TID_VAR,TID_LVAR_T,TID_LVAR_F)
                k1=j
                exit
             end select
          end do
       else
          select case(rpnc%que(ks(n))%tid)
          case(TID_VAR,TID_LVAR_T,TID_LVAR_F)
             k1=n ! writable var
          case(TID_ROVAR,TID_PAR,TID_CPAR)
             pz=rpnc%que(ks(n))%cid ! unwratable; k1=0
          end select
       end if
       do j=n,1,-1
          rpnc%que(ks(j))%tid=TID_NOP
       end do
       if(k1/=0) then
          ! writable var found
          if(var) then
             pz=rpnc%que(ks(k1))%cid
             z=v
             ! else if logical the first operand left unchenged
             ! and given TID_LVAR
          end if
          rpnc%que(ks(k1))%tid=tid
       end if
    end if
    rpnc%que(i)%tid=TID_NOP
    if(k1==0) then
       ! no wratable vars
       if(var) then
          ! allocate buffer for i
          call put_vbuf(rpnc,i,v,tid)
       else
          ! change the last operands to z with TID_LVAR
          call put_vbuf(rpnc,ks(n),z,tid)          
       end if
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

  recursive function eval_0(rpnc,i) result(istat)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    interface
       function f0()
         use fpio, only: cp
         complex(cp) f0
       end function f0
    end interface
    integer istat
    pointer(pf0,f0)
    complex(cp) v
    pf0=rpnc%que(i)%cid
    v=f0()
    call set_result(rpnc,i,v,0)
    istat=0
  end function eval_0

  recursive function eval_r(rpnc,i) result(istat)
    use zmath, only: zfalse
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    interface
       function f2(z1,z2)
         use fpio, only: cp
         complex(cp) f2
         complex(cp),intent(in)::z1,z2
       end function f2
    end interface
    pointer(pfn,f2)
    integer istat
    integer pvs(0:narg_max)
    integer ods(1:narg_max)
    complex(cp) v,z1,z2
    pointer(pz1,z1)
    pointer(pz2,z2)
    logical ok
    
    istat=get_operands(rpnc,i,2,ks=ods,ps=pvs)
    if(istat/=0) return

    ! ods(1) may be logical
    ! ods(2) must be value but no ckeck here
    select case(rpnc%que(ods(1))%tid)
    case(TID_LVAR_F)
       ok=.false.
       v=zfalse
    case default
       ok=.true.
    end select

    if(ok) then
       pfn=rpnc%que(i)%cid
       pz1=pvs(1)
       pz2=pvs(2)
       v=f2(z1,z2)
    end if
    call set_result(rpnc,i,v,2,ods,logical=.true.)

  end function eval_r

  recursive function eval_p(rpnc,i) result(istat)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer istat
    real(rp) p
    complex(cp) z
    select case(rpnc%que(i)%cid)
    case(PID_yoc)
       p=1.0e-24_rp
    case(PID_zep)
       p=1.0e-21_rp
    case(PID_a)
       p=1.0e-18_rp
    case(PID_f)
       p=1.0e-15_rp
    case(PID_pi)
       p=1.0e-12_rp
    case(PID_n)
       p=1.0e-9_rp
    case(PID_u)
       p=1.0e-6_rp
    case(PID_mi)
       p=1.0e-3_rp
    case(PID_k)
       p=1.0e+3_rp
    case(PID_M)
       p=1.0e+6_rp
    case(PID_G)
       p=1.0e+9_rp
    case(PID_T)
       p=1.0e+12_rp
    case(PID_P)
       p=1.0e+15_rp
    case(PID_E)
       p=1.0e+18_rp
    case(PID_Z)
       p=1.0e+21_rp
    case(PID_Y)
       p=1.0e+24_rp
    case default
       stop "unexpected error in eval_p"
    end select
    z=complex(p,rzero)
    call put_vbuf(rpnc,i,z)
    rpnc%tmpans=z
    istat=0
  end function eval_p

  recursive function eval_l(rpnc,i) result(istat)
    use zmath, only: ztrue, zfalse
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer istat
    integer na
    integer ks(1:narg_max)
    logical ods(1:narg_max)
    logical v
    integer j,tid
    complex(cp) z

    na=get_up32(rpnc%que(i)%tid)
    if(i-1<na) then
       istat=RPNERR_NOOP
       return
    end if
    
    istat=get_operands(rpnc,i,na,ks=ks)
    if(istat/=0) return

    do j=1,na
       select case(rpnc%que(ks(j))%tid)
       case(TID_LVAR_T)
          ods(j)=.true.
       case(TID_LVAR_F)
          ods(j)=.false.
       case default
          istat=RPNCERR_INVARG
          return
       end select
    end do

    select case(rpnc%que(i)%cid)
    case(LOID_NOT)
       v=.not.ods(1)
    case(LOID_AND)
       v=ods(1).and.ods(2)
    case(LOID_OR)
       v=ods(1).or.ods(2)
    case(LOID_EQ)
       v=ods(1).eqv.ods(2)
    case(LOID_NEQ)
       v=ods(1).neqv.ods(2)
    case default
       stop "internal error"
    end select

    do j=1,na
       rpnc%que(ks(j))%tid=TID_NOP
    end do

    if(v) then
       tid=TID_LVAR_T
       z=ztrue
    else
       tid=TID_LVAR_F
       z=zfalse
    end if

    call put_vbuf(rpnc,i,z,tid)
    rpnc%tmpans=z

  end function eval_l

  recursive function eval_s(rpnc,i) result(istat)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer istat
    interface
       function fn(n,vs,ws)
         use fpio, only: cp, rp
         complex(cp) fn
         integer,intent(in)::n
         complex(cp),intent(in)::vs(n,2)
         real(rp),intent(in)::ws(n)
       end function fn
    end interface
    pointer(pfn,fn)
    complex(cp) v
    if(rpnc%sd%p_vs==0) then
       v=czero
    else
       pfn=rpnc%que(i)%cid
       v=fn(rpnc%sd%p_vs,rpnc%sd%vs(1:rpnc%sd%p_vs,:),rpnc%sd%ws(1:rpnc%sd%p_vs))
    end if
    call set_result(rpnc,i,v)
    istat=0
  end function eval_s

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
    if(na==0) then
       istat=eval_0(rpnc,i)
       return
    end if

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

    call set_result(rpnc,i,v,na,ods)
    
    if(tid==TID_AOP) call set_assign()

  contains
    
    subroutine set_assign()
      complex(cp) z
      pointer(pz,z)
      pz=pvs(1)
      z=v
      rpnc%que(ods(1))%tid=TID_NPAR
      set_opt(RPNCOPT_NEW)
    end subroutine set_assign

  end function eval_n

  recursive function eval_uf(rpnc,i) result(istat)
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::i
    integer istat
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) fnc
    integer j
    integer kp
    integer,allocatable::ods(:)
    complex(cp) v
    pointer(pv,v)
    logical dup
    type(t_rpnq),pointer::q

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
    allocate(fnc%ip)
    fnc%ip     = 1
    fnc%que    = rpnm%que
    fnc%p_vbuf = 0

    fnc%pars   => rpnc%pars
    fnc%answer => rpnc%answer
    fnc%tmpans => rpnc%tmpans
    fnc%rl     => rpnc%rl
    fnc%rc     => rpnc%rc
    fnc%pfs    => rpnc%pfs
    fnc%opt    => rpnc%opt
    fnc%sd     => rpnc%sd

    istat=0
    do j=1,size(fnc%que)
       q => fnc%que(j)
       select case(q%tid)
       case(TID_FIG)
          q%cid=loc(rpnm%vbuf(q%cid))
          q%tid=TID_ROVAR
       case(TID_PAR,TID_APAR)
          call set_par_ptr(kp)
          if(istat/=0) exit 
          q%cid=get_par_loc(fnc%pars,kp,dup)
          if(.not.dup) then
             q%tid=TID_PAR
          else
             q%tid=TID_CPAR
          end if
       case(TID_DPAR)
          pv=rpnc%que(ods(q%cid))%cid
          call put_vbuf(fnc,j,v)
       end select
    end do

    if(istat==0) istat=eval(fnc)
    
    if(istat==0)&
       call set_result(rpnc,i,fnc%answer,rpnm%na,ods)

    deallocate(ods)
    deallocate(fnc%que)
    deallocate(fnc%vbuf)
    deallocate(fnc%p_vbuf)
    deallocate(fnc%ip)

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

  recursive function eval_m(rpnc,i) result(istat)
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::i
    integer istat
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) mac
    integer j
    integer kp
    logical dup
    type(t_rpnq),pointer::q

    rpnm=>rpnc%rl%rpnm(rpnc%que(i)%cid)
    
    allocate(mac%que(size(rpnm%que)))
    allocate(mac%vbuf(NUM_VBUF_MIN))
    allocate(mac%p_vbuf)
    allocate(mac%ip)
    mac%ip     = 1
    mac%que    = rpnm%que
    mac%p_vbuf = 0

    mac%pars   => rpnc%pars
    mac%answer => rpnc%answer
    mac%tmpans => rpnc%tmpans
    mac%rl     => rpnc%rl
    mac%rc     => rpnc%rc
    mac%pfs    => rpnc%pfs
    mac%opt    => rpnc%opt
    mac%sd     => rpnc%sd

    istat=0
    do j=1,size(mac%que)
       q => mac%que(j)
       select case(q%tid) 
       case(TID_PAR,TID_APAR)
          call set_par_ptr(kp)
          if(istat/=0) exit
          q%cid=get_par_loc(mac%pars,kp,dup)
          if(.not.dup) then
             q%tid=TID_PAR
          else
             q%tid=TID_CPAR
          end if
       case(TID_FIG)
          q%cid=loc(rpnm%vbuf(q%cid))
          q%tid=TID_ROVAR
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
    deallocate(mac%ip)

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
    logical ansset
    complex(cp) v
    pointer(pv,v)
    istat=0
    ec=0
    if(rpnc%rc>RPN_REC_MAX) then
       istat=RPNERR_RECOV
       return
    end if
    ansset=.false.
    rpnc%rc=rpnc%rc+1
    do i=rpnc%ip,size(rpnc%que)
       ec=ec+1
       select case(get_lo32(rpnc%que(i)%tid))
       case(TID_OP,TID_OPN,TID_AOP)
          istat=eval_n(rpnc,i)
       case(TID_LOP)
          istat=eval_l(rpnc,i)          
       case(TID_ROP)
          istat=eval_r(rpnc,i)
       case(TID_COP)
          istat=eval_c(rpnc,i)
       case(TID_MAC)
          istat=eval_m(rpnc,i)
       case(TID_UFNC)
          istat=eval_uf(rpnc,i)
       case(TID_SOP)
          istat=eval_s(rpnc,i)
       case(TID_POP)
          istat=eval_p(rpnc,i)
       case(TID_END)
          ec=ec-1
          rpnc%rc=rpnc%rc-1
          call set_ans(.true.)
          if(rpnc%rc==0.and.rpnc%que(i)%cid/=0) then
             rpnc%que(i)%tid=TID_NOP
             rpnc%ip=i+1 ! the next code
             exit
          end if
          rpnc%rc=rpnc%rc+1
          ec=0
       case default
          ec=ec-1
       end select

       if(i==size(rpnc%que)) then
          rpnc%ip=0
          call set_ans(.false.)
          rpnc%rc=rpnc%rc-1       
       end if

       if(istat/=0) then
          write(*,*) "*** Error in eval at que = ", i
          exit
       end if

    end do

    if(istat/=0) return
    if(rpnc%rc==0.and.is_set(RPNCOPT_DAT)) &
         call set_sd(rpnc%ip,rpnc)

    ! order is important
    call remove_dup(rpnc%pars)
    if(is_set(RPNCOPT_NEW)) call set_newpar

  contains
    
    subroutine set_newpar
      integer ii
      do ii=1,size(rpnc%que)
         if(rpnc%que(ii)%tid==TID_NPAR) call sort_par(rpnc%pars,rpnc%que(ii)%cid)
      end do
      cle_opt(RPNCOPT_NEW)
    end subroutine set_newpar

    subroutine set_ans(end)
      logical,intent(in)::end
      integer k
      if(ec==0) then ! only fig or par
         if(end) then
            k=i-1
         else
            k=size(rpnc%que)
         end if
         pv=rpnc%que(k)%cid
         rpnc%answer=v
      else
         rpnc%answer=rpnc%tmpans
      end if
   end subroutine set_ans
    
  end function eval

end module rpne
