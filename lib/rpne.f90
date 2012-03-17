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
module rpne
  use rpnd
  implicit none

  private 

  public init_rpne
  public eval
  public rpn_sans
  public rpn_ans
  public rpn_rans
  public rpn_dans
  public rpn_run
  public get_formula
  public open_hist

  integer::hist_unit=0

contains

  subroutine init_rpne()
    use rpnp, only: init_rpnp
    call init_rpnp(loc(input))
  end subroutine init_rpne

  subroutine open_hist(on)
    use misc, only: open_file
    logical,intent(in)::on
    character*(*),parameter::histfile=".fzcalc-hist"
    character(len=LEN_STR_MAX) f
    if(on.and.hist_unit==0) then
       call getenv("HOME",f)
       if(f/="") then
          f=trim(adjustl(f))//"/"//histfile
       else
          f=histfile
       end if
       hist_unit=open_file(f,.true.,.false.,acce="append")
    else if(hist_unit/=0) then
       close(hist_unit)
    end if
  end subroutine open_hist
  
  integer function get_formula(rpnc)
    use memio, only: cpstr
    use misc, only: messp,ins
    use rpnp, only: set_formula
    type(t_rpnc),intent(inout)::rpnc
    character(len=LEN_FORMULA_MAX) s
    integer istat
    if(associated(rpnc%prompt).and.size(rpnc%prompt)>0) &
         call messp(cpstr(loc(rpnc%prompt(1)),size(rpnc%prompt)))
    call ins(s,i=istat)
    if(istat/=0) then
       get_formula=FZCERR_READ
       return
    end if
    get_formula=set_formula(rpnc,s)
  end function get_formula

  character(LEN_STR_ANS_MAX) function rpn_sans(rpnc)
    use memio, only: DISP_FMT_BIN,DISP_FMT_OCT,DISP_FMT_HEX,itoa
    use fpio, only: ztoa,LEN_STR_ANS_MAX
    use misc, only: is_set, is_not_set
    type(t_rpnc),intent(in)::rpnc
    integer f
    if(is_not_set(rpnc%opt,RPNCOPT_OUTM)) then
       f=ishft(rpnc%opt,-32) !DISP_FMT_NORM
    else if(is_set(rpnc%opt,RPNCOPT_OBIN)) then
       f=DISP_FMT_BIN
    else if(is_set(rpnc%opt,RPNCOPT_OOCT)) then
       f=DISP_FMT_OCT
    else if(is_set(rpnc%opt,RPNCOPT_OHEX)) then
       f=DISP_FMT_HEX
    end if
    if(is_not_set(rpnc%opt,RPNCOPT_RATIO)) then
          rpn_sans=trim(ztoa(rpnc%answer,fmt=f))
    else
       rpn_sans=trim(itoa(int(realpart(rpnc%answer),kind=8),f))
       if(int(imagpart(rpnc%answer),kind=8)>1) then
          rpn_sans=trim(rpn_sans)//"/"//trim(itoa(int(imagpart(rpnc%answer),kind=8),f))
       end if
    end if
  end function rpn_sans

  complex(cp) function rpn_ans(rpnc)
    use fpio, only: cp
    type(t_rpnc),intent(in)::rpnc
    rpn_ans=rpnc%answer
  end function rpn_ans

  real(rp) function rpn_rans(rpnc)
    use fpio, only: rp
    type(t_rpnc),intent(in)::rpnc
    rpn_rans=realpart(rpnc%answer)
  end function rpn_rans

  real(dp) function rpn_dans(rpnc)
    use fpio, only: dp
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
          exit
       end select
    end do
  end function get_operand

  integer function eval_c(rpnc,i)
    use fpio, only: cp
    use misc, only: get_up32
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer kd,ke,n,kz
    integer od1,j
    logical ok
    complex(cp) v
    pointer(pv,v)

    kd=get_up32(rpnc%que(i)%cid)+i
    od1=get_operand(rpnc,i-1)

    if(od1==0) then
       eval_c=FZCERR_NOOP
       return
    end if
    select case(rpnc%que(od1)%tid)
    case(TID_LVAR_T)
       ok=.true.
    case(TID_LVAR_F)
       ok=.false.
    case default
       eval_c=FZCERR_INVARG
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
    use fpio, only: cp,zfalse
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
    integer,intent(out),optional::ps(n) 
    integer,intent(out),optional::ks(n)
    integer k,j
    k=i-1
    do j=n,1,-1
       k=get_operand(rpnc,k)
       if(k<=j-1) then
          get_operands=FZCERR_NOOP
          return
       end if
       if(present(ps)) ps(j)=rpnc%que(k)%cid
       if(present(ks)) ks(j)=k
       k=k-1
    end do
    get_operands=0
  end function get_operands

  recursive function eval_0(rpnc,i) result(istat)
    use fpio, only: cp
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
    use fpio, only: cp,zfalse
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
    
    istat=get_operands(rpnc,i,2,ks=ods,ps=pvs(1:narg_max))
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
    use fpio, only: cp,rp,rzero
    use misc, only: is_not_set
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer istat
    real(rp) p
    complex(cp) z
    real(rp),parameter::kB=1024.0_rp

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
       if(is_not_set(rpnc%opt,RPNCOPT_BYTE)) then
          p=1.0e+3_rp
       else
          p=kB
       end if
    case(PID_M)
       if(is_not_set(rpnc%opt,RPNCOPT_BYTE)) then
          p=1.0e+6_rp
       else
          p=kB**2.0_rp
       end if
    case(PID_G)
       if(is_not_set(rpnc%opt,RPNCOPT_BYTE)) then
          p=1.0e+9_rp
       else
          p=kB**3.0_rp
       end if
    case(PID_T)
       if(is_not_set(rpnc%opt,RPNCOPT_BYTE)) then
          p=1.0e+12_rp
       else
          p=kB**4.0_rp
       end if
    case(PID_P)
       if(is_not_set(rpnc%opt,RPNCOPT_BYTE)) then
          p=1.0e+15_rp
       else
          p=kB**5.0_rp
       end if
    case(PID_E)
       if(is_not_set(rpnc%opt,RPNCOPT_BYTE)) then
          p=1.0e+18_rp
       else
          p=kB**6.0_rp
       end if
    case(PID_Z)
       if(is_not_set(rpnc%opt,RPNCOPT_BYTE)) then
          p=1.0e+21_rp
       else
          p=kB**7.0_rp
       end if
    case(PID_Y)
       if(is_not_set(rpnc%opt,RPNCOPT_BYTE)) then
          p=1.0e+24_rp
       else
          p=kB**8.0_rp
       end if
    case default
       stop "unexpected error in eval_p"
    end select
    z=complex(p,rzero)
    call put_vbuf(rpnc,i,z)
    rpnc%tmpans=z
    istat=0
  end function eval_p

  recursive function eval_l(rpnc,i) result(istat)
    use fpio, only: cp,zfalse,ztrue
    use misc, only: get_up32
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    integer istat
    integer na
    integer ks(narg_max)
    logical ods(narg_max)
    logical v
    integer j,tid
    complex(cp) z

    na=get_up32(rpnc%que(i)%tid)
    if(i-1<na) then
       istat=FZCERR_NOOP
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
          istat=FZCERR_INVARG
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
    use fpio, only: cp,czero
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
    use fpio, only: cp
    use misc, only: get_up32,get_lo32
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
       istat=FZCERR_NOOP
       return
    end if
    
    istat=get_operands(rpnc,i,na,ks=ods,ps=pvs(1:narg_max))
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
      use misc, only: set_opt
      complex(cp) z
      pointer(pz,z)
      pz=pvs(1)
      z=v
      rpnc%que(ods(1))%tid=TID_NPAR
      call set_opt(rpnc%opt,RPNCOPT_NEW)
    end subroutine set_assign

  end function eval_n

  recursive function integrand1(pc,n,x) result(s)
    use fpio, only: rp,rzero
    integer,intent(in)::pc
    integer,intent(in)::n
    real(rp),intent(in)::x(n)
    complex(cp) s
    type(t_rpnc) rpnc
    type(t_rpnc),pointer::ifnc
    pointer(prpnc,rpnc)
    integer istat
    integer p_vbuf_in

    prpnc=pc
    ifnc => rpnc%ifnc

    p_vbuf_in=ifnc%p_vbuf
    ifnc%que=rpnc%ique
    ifnc%ip = 1
    call set_dmy_par

    istat=eval(ifnc)
    s=ifnc%answer
    ifnc%p_vbuf=p_vbuf_in

  contains

    subroutine set_dmy_par()
      integer i
      do i=1,size(ifnc%que)
         select case(ifnc%que(i)%tid)
         case(TID_IVAR1)  ! X
            call put_dmy_par(i,1)
         case(TID_IVAR1L) ! b-X
            call put_dmy_par(i,2)
         case(TID_IVAR1U) ! X-a
            call put_dmy_par(i,3)
         end select
      end do
    end subroutine set_dmy_par

    subroutine put_dmy_par(i,j)
      integer,intent(in)::i,j
      complex(cp) z
      pointer(pz,z)
      pz=ifnc%que(i)%cid
      z=complex(x(j),rzero)
      ifnc%que(i)%tid=TID_VAR      
    end subroutine put_dmy_par

  end function integrand1

  recursive function eval_i(rpnc,i) result(istat)
    use fpio, only: cp,rzero
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::i
    interface
       complex(cp) function f1(ptr_rpnc,ptr_integrand,a,b)
         use fpio, only: cp, rp
         integer,intent(in)::ptr_rpnc
         integer,intent(in)::ptr_integrand
         real(rp),intent(in)::a,b
       end function f1
    end interface
    pointer(pf1,f1)
    integer istat
    integer pvs(0:2)
    integer ods(2)
    type(t_rpnc),pointer::ifnc
    integer i1,i2,nc
    complex(cp) a,b,ans
    pointer(pa,a)
    pointer(pb,b)

    istat=get_operands(rpnc,i,2,ks=ods,ps=pvs(1:2))
    if(istat/=0) return

    call find_code
    if(i1==0.or.i2==0) then
       istat=FZCERR_NOOP
       return
    end if

    nc=i2-i1+1
    allocate(rpnc%ifnc)
    ifnc => rpnc%ifnc
    ifnc=cp_rpnc(rpnc,deep=.false.)

    allocate(ifnc%que(nc),rpnc%ique(nc))
    ifnc%ip     = 1
    ifnc%rc     = rpnc%rc
    ifnc%que    = rpnc%que(i1:i2)
    ifnc%p_vbuf = 0

    call alloc_vbuf
    call set_var
    call set_idmy
    rpnc%ique=ifnc%que

    pf1=rpnc%que(i)%cid
    pa=pvs(1)
    pb=pvs(2)

    ans=f1(loc(rpnc),loc(integrand1),realpart(a),realpart(b))

    call set_result(rpnc,i,ans,2,ods)
    rpnc%que(i1:i2)%tid=TID_NOP

    call uinit_rpncq(rpnc%ique)
    call uinit_rpnc(ifnc,deep=.false.)
    deallocate(rpnc%ique)

    istat=0

  contains
    
    subroutine alloc_vbuf()
      integer oc,vc
      oc=count_op(ifnc%que)
      vc=count_tid(ifnc%que,TID_IVAR1) &
           +count_tid(ifnc%que,TID_IVAR1L) &
           +count_tid(ifnc%que,TID_IVAR1U) &
           +count_tid(ifnc%que,TID_VAR)
      if(vc+oc>0) allocate(ifnc%vbuf(vc+oc))
    end subroutine alloc_vbuf

    subroutine set_idmy()
      use fpio, only: czero
      integer ii
      do ii=1,nc
         select case(ifnc%que(ii)%tid)
         case(TID_IVAR1,TID_IVAR1L,TID_IVAR1U)
            call put_vbuf(ifnc,ii,czero,ifnc%que(ii)%tid)
         end select
      end do
    end subroutine set_idmy

    subroutine set_var()
      integer ii
      complex(rp) z
      pointer(pz,z)
      do ii=1,nc
         select case(ifnc%que(ii)%tid)
         case(TID_VAR)
            pz=ifnc%que(ii)%cid
            call put_vbuf(ifnc,ii,z,TID_ROVAR)
         end select
      end do
    end subroutine set_var

    subroutine find_code()
      integer ii
      i1=0
      i2=0
      do ii=i-1,1,-1
         select case(rpnc%que(ii)%tid)
         case(TID_IEND)
            rpnc%que(ii)%tid=TID_NOP
            i2=ii-1
         case(TID_ISTA)
            rpnc%que(ii)%tid=TID_NOP
            i1=ii+1
            exit
         end select
      end do
    end subroutine find_code

  end function eval_i

  recursive function eval_uf(rpnc,i) result(istat)
    use rpnlist, only: kth_rpnm, t_rpnm
    use fpio, only: cp
    use plist, only: get_par_loc
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::i
    integer istat
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) fnc
    Integer j
    integer kp
    integer ods(narg_max)
    complex(cp) v
    pointer(pv,v)
    logical dup
    type(t_rpnq),pointer::q

    rpnm => kth_rpnm(rpnc%rl,rpnc%que(i)%cid)

    if(i-1<rpnm%na) then
       istat=FZCERR_NOOP
       return
    end if

    istat=get_operands(rpnc,i,rpnm%na,ks=ods)
    if(istat/=0) return

    fnc=cp_rpnc(rpnc,deep=.false.)
    allocate(fnc%que(size(rpnm%que)))
    fnc%ip     = 1
    fnc%rc     = rpnc%rc
    fnc%que    = rpnm%que
    fnc%p_vbuf = 0
    call alloc_vbuf

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

    call uinit_rpnc(fnc,deep=.false.)

  contains
    
    subroutine alloc_vbuf()
      integer oc,vc
      oc=count_op(fnc%que)
      vc=count_tid(fnc%que,TID_DPAR)
      if(oc+vc>0) allocate(fnc%vbuf(oc+vc))
    end subroutine alloc_vbuf

    subroutine set_par_ptr(ent) 
      use slist, only: get_str_ptr
      use plist, only: find_par
      use memio, only: cpstr
      use misc, only: mess
      integer,intent(out)::ent
      integer ptr,len,cid
      integer ptr0,len0
      complex(cp) z
      pointer(pz,z)

      cid=fnc%que(j)%cid
      if(cid<0) cid=-cid
      istat=get_str_ptr(rpnm%pnames,cid,ptr,len)
      if(istat/=0) stop "*** set_par_ptr: UNEXPECTED ERROR: get_str_ptr failed"
      istat=find_par(fnc%pars,cpstr(ptr,len),ent=ent)
      if(istat/=0) then
         call mess("*** No such parameter: "//cpstr(ptr,len))
      end if
      if(fnc%que(j)%cid<0) then
         pz=get_par_loc(fnc%pars,ent)
         if(pz/=0) then
            istat=get_str_ptr(rpnm%pnames,1,ptr0,len0)
            istat=input(rpnc,cpstr(ptr0,len0),cpstr(ptr,len),z)
         else
            stop "*** set_par_ptr: UNEXPETED ERROR: get_par_loc failed"   
         end if
      end if
    end subroutine set_par_ptr

  end function eval_uf
  
  recursive function eval_m(rpnc,i) result(istat)
    use rpnlist, only: kth_rpnm, t_rpnm
    use plist, only: get_par_loc
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::i
    integer istat
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) mac
    integer j
    integer kp
    logical dup
    type(t_rpnq),pointer::q

    rpnm => kth_rpnm(rpnc%rl,rpnc%que(i)%cid)

    mac=cp_rpnc(rpnc,deep=.false.)    
    allocate(mac%que(size(rpnm%que)))
    mac%ip     = 1
    mac%rc     = rpnc%rc
    mac%que    = rpnm%que
    mac%p_vbuf = 0
    call alloc_vbuf

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

    if(istat==0) then
       istat=eval(mac)
       if(istat==0) then
          rpnc%tmpans=mac%answer
          call put_vbuf(rpnc,i,mac%answer)
       end if
    end if

    call uinit_rpnc(mac,deep=.false.)

  contains

    subroutine alloc_vbuf()
      integer oc
      oc=count_op(mac%que)
      if(oc>0) allocate(mac%vbuf(oc))
    end subroutine alloc_vbuf

    subroutine set_par_ptr(ent)
      use fpio, only: cp
      use slist, only: get_str_ptr
      use plist, only: find_par
      use memio, only: cpstr
      use misc, only: mess
      integer,intent(out)::ent
      integer ptr,len,cid
      integer ptr0,len0
      complex(cp) z
      pointer(pz,z)

      cid=mac%que(j)%cid
      if(cid<0) cid=-cid
      istat=get_str_ptr(rpnm%pnames,cid,ptr,len)
      if(istat/=0) stop "*** set_par_ptr: UNEXPECTED ERROR: get_str_ptr failed"
      istat=find_par(mac%pars,cpstr(ptr,len),ent=ent)
      if(istat/=0) then
         call mess("*** No such parameter: "//cpstr(ptr,len))
         istat=FZCERR_NOPAR
         return
      end if
      if(mac%que(j)%cid<0) then
         pz=get_par_loc(mac%pars,ent)
         if(pz/=0) then
            istat=get_str_ptr(rpnm%pnames,1,ptr0,len0)
            istat=input(rpnc,cpstr(ptr0,len0),cpstr(ptr,len),z)
         else
            stop "*** set_par_ptr: UNEXPETED ERROR: get_par_loc failed"
         end if
      end if
    end subroutine set_par_ptr

  end function eval_m

  recursive function input(rpnc,p,s,z) result(istat)
    use fpio, only: cp
    use rpnp, only: set_formula
    use misc, only: mess,messp,ins,is_set,is_not_set
    type(t_rpnc),intent(in),target::rpnc
    character*(*),intent(in)::p
    character*(*),intent(in)::s
    complex(cp),intent(inout)::z
    character(LEN_FORMULA_MAX) expr
    integer istat
    type(t_rpnc) tmpc
    tmpc=cp_rpnc(rpnc,deep=.false.)
    call messp("Input pending for: "//trim(p)//"\n"//trim(s(2:))//"? > ")
    call ins(expr)
    istat=set_formula(tmpc,expr)
    if(istat==0) then
       istat=rpn_run(tmpc)
       if(istat==0) then
          tmpc%rc=rpnc%rc+1 ! <<<
          istat=eval(tmpc) 
          if(istat==0) then
             z=tmpc%answer
          end if
       end if
    end if
    call uinit_rpnc(tmpc,deep=.false.)
  end function input

  recursive function eval(rpnc) result(istat)
    use fpio, only: cp
    use misc, only: get_lo32,get_up32,is_set,mess
    use plist, only: remove_dup,sort_par
    use memio, only: itoa,CPSTR
    use com
    type(t_rpnc),intent(inout)::rpnc
    type(t_rpnq),pointer::q
    integer i,istat,ec,ip1,t
    complex(cp) v
    pointer(pv,v)

    if(rpnc%rc>RPN_REC_MAX) then
       istat=FZCERR_RECOV
       return
    end if

    rpnc%rc=rpnc%rc+1
    istat=0
    ec=0

    ip1=rpnc%ip
    i=rpnc%ip-1
    do 
       i=i+1
       if(i>size(rpnc%que)) exit
       t=rpnc%que(i)%tid
       if(t==TID_NOP) cycle
       q => rpnc%que(i)
       ec=ec+1
       select case(get_lo32(t))
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
       case(TID_IOP)
          istat=eval_i(rpnc,i)
       case(TID_ISTA)
          i=i+q%cid+1
       case(TID_COM)
          call mess(cpstr(q%cid,get_up32(q%tid)))
          ec=ec-1
       case(TID_END)
          if(rpnc%rc==1.and.q%cid/=0) then
             ! multiple ";"  will exit the loop
             ec=ec-1
             exit
          else
             ec=0
          end if
       case default
          if(is_command(t)) then
             istat=exe_com(rpnc,i)
             if(istat==0) then
                istat=RPNSTA_COMSET
             else
                select case(istat)
                case(CID_EXIT)
                   istat=RPNSTA_EXIT
                case(CID_LOAD)
                   istat=load()
                case default
                   istat=FZCERR_INVCOM
                end select
             end if
          end if
          ec=ec-1
       end select

       if(istat/=0) then
          if(istat>0) &
               call mess("*** Error in eval at que = " &
               //trim(itoa(i))//", rc= "//trim(itoa(rpnc%rc)) &
               //", code = "//trim(itoa(istat)))
          exit
       end if

    end do

    rpnc%rc=rpnc%rc-1

    if(istat/=0) return

    if(rpnc%rc==0.and.is_set(rpnc%opt,RPNCOPT_DAT)) &
         call set_sd(ip1,i,rpnc)

    call set_ans

    if(i>=size(rpnc%que)) then
       rpnc%ip=0
       if(rpnc%rc==0) then
          ! this is the last time
          ! order is important
          call remove_dup(rpnc%pars)
          if(is_set(rpnc%opt,RPNCOPT_NEW)) call set_newpar
       end if
    else
       rpnc%ip=i+1 ! the next code
    end if

  contains
    
    integer function load()
      use misc, only: open_file,ins
      use rpnp, only: set_formula
      type(t_rpnc) tmpc
      integer u,is
      character(LEN_FORMULA_MAX) s
      load=RPNSTA_OK
      u=open_file(cpstr(q%cid,get_up32(q%tid)),.true.,.false.,"old")
      if(u==0) then
         load=FZCERR_NOFILE
         return
      end if
      tmpc=cp_rpnc(rpnc,deep=.false.)
      do
         call ins(s,u,is)
         if(is/=0) exit
         load=set_formula(tmpc,s)
         if(load/=0) exit
         load=rpn_run(tmpc)
         if(load/=0) exit
      end do
      call uinit_rpnc(tmpc,deep=.false.)
      close(u)
    end function load

    subroutine set_newpar
      use misc, only: cle_opt
      integer ii
      do ii=1,size(rpnc%que)
         if(rpnc%que(ii)%tid==TID_NPAR) call sort_par(rpnc%pars,rpnc%que(ii)%cid)
      end do
      call cle_opt(rpnc%opt,RPNCOPT_NEW)
    end subroutine set_newpar

    subroutine set_ans
      integer k1,kk
      if(ec==0) then ! only fig or par
         if(i>size(rpnc%que)) then
            k1=size(rpnc%que)
         else
            k1=i-1 ! i => TID_END
         end if
         do kk=k1,ip1,-1
            select case(rpnc%que(kk)%tid)
            case(TID_VAR,TID_PAR,TID_CPAR,TID_ROVAR)
               pv=rpnc%que(kk)%cid             
               rpnc%answer=v                   
               rpnc%que(kk)%tid=TID_NOP
               exit                            
               !case(TID_END)
               !  should never come here 
               !exit
            end select
         end do                                
      else
         rpnc%answer=rpnc%tmpans
      end if
   end subroutine set_ans
    
  end function eval

  recursive function rpn_run(rpnc) result(istat)
    use rpnp, only: parse_formula
    type(t_rpnc),intent(inout)::rpnc
    integer istat
    integer p2
    p2=0
    do
       istat=parse_formula(rpnc,p2)
       if(istat==0) then
          istat=eval(rpnc)
          if(istat==0) then
             call print_ans
             call write_hist
          else if(istat>0) then
             exit
          else
             select case(istat)
             case(RPNSTA_EXIT)
                exit
             end select
          end if
       else if(istat>0) then
          exit
       else if(istat<0) then
          select case(istat)
          case(RPNSTA_EMPTY)
          case(RPNSTA_FNCSET)
          end select
       end if
       if(p2==0) exit
    end do

  contains

    subroutine print_ans()
      use misc, only: is_not_set,is_set,mess
      if(is_not_set(rpnc%opt,RPNCOPT_NO_STDOUT) &
           .and.(p2==0.or.is_set(rpnc%opt,RPNCOPT_PRINT_REQ))) &
           call mess(trim(rpn_sans(rpnc)))
    end subroutine print_ans

    subroutine write_hist()
      use misc, only: mess,is_not_set
      if(hist_unit==0.or.is_not_set(rpnc%opt,RPNCOPT_HIST)) return
      call mess(rpnc%expr(1:rpnc%len_expr),hist_unit)
      flush(hist_unit)
    end subroutine write_hist
    
  end function rpn_run
  
end module rpne
