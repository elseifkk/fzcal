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
module rpnd
  use slist, only: t_slist
  use plist, only: t_plist
  use fpio, only: rp,cp
  implicit none

  integer,parameter::RPNSTA_EMPTY  = -2
  integer,parameter::RPNSTA_FNCSET = -1
  integer,parameter::RPNSTA_OK     =  0

  integer,parameter::RPNCERR_NOENT        =  1
  integer,parameter::RPNCERR_NOOP         =  2
  integer,parameter::RPNCERR_NOPAR        =  3
  integer,parameter::RPNCERR_NOFNC        =  4
  integer,parameter::RPNCERR_ADDPAR       =  5
  integer,parameter::RPNCERR_INVASN       =  6
  integer,parameter::RPNCERR_INVOP        =  7
  integer,parameter::RPNCERR_INVFNC       =  8
  integer,parameter::RPNCERR_PARSER       =  9
  integer,parameter::RPNCERR_ADDSTR       = 10
  integer,parameter::RPNCERR_MEMOV        = 11
  integer,parameter::RPNCERR_RECOV        = 12
  integer,parameter::RPNCERR_NARG         = 13
  integer,parameter::RPNCERR_GETPAR       = 14
  integer,parameter::RPNCERR_TOO_MANY_ARG = 15
  integer,parameter::RPNCERR_TOO_FEW_ARG  = 16
  integer,parameter::RPNCERR_NO_ARG       = 17
  integer,parameter::RPNCERR_INVARG       = 18
  integer,parameter::RPNCERR_INVFIG       = 19
  integer,parameter::RPNCERR_READ         = 20

  integer,parameter::RPN_REC_MAX     =  256
  integer,parameter::NUM_VBUF_MIN    =   32
  integer,parameter::NUM_PBUF_MIN    =   32
  integer,parameter::NUM_RPNM_MIN    =    8
  integer,parameter::LEN_RLIST_MIN   = 1024
  integer,parameter::NUM_VS_MIN      =   32
  integer,parameter::LEN_STR_MAX     = 1024
  integer,parameter::LEN_FORMULA_MAX = LEN_STR_MAX

  ! meta tid
  integer,parameter::TID_FIN   =   999
  integer,parameter::TID_UNDEF =  1000
  integer,parameter::TID_INV   =   666
  integer,parameter::TID_NOP   =     0

  !! priority table begin
  ! asign and conditional
  integer,parameter::TID_ASN    =   1  ! =
  integer,parameter::TID_ASNU   =  -1
  integer,parameter::TID_AOP    =   2 
  integer,parameter::TID_TOP1   =   3  ! ?
  ! logical                     
  integer,parameter::TID_LOP4    =  4  ! eq,neq
  integer,parameter::TID_LOP3    =  5  ! or
  integer,parameter::TID_LOP2    =  6  ! and
  integer,parameter::TID_LOP1    =  7  ! not, ~
  integer,parameter::TID_LOP1U   = -7  ! ~
  integer,parameter::TID_ROP     =  8  ! ==, ~=, <=, >=,...
  ! unary, binary and functions  
  integer,parameter::TID_BOP1    =   9  ! +,-
  integer,parameter::TID_BOP1U   =  -9  !
  integer,parameter::TID_BOP2    =  10  ! *,/,&P,&C
  integer,parameter::TID_BOP2U   = -10  !
  integer,parameter::TID_UOP1    =  11  ! +a,-a,++a
  integer,parameter::TID_BOP4    =  12  ! implicit * <<<<<<<<<<< 
  integer,parameter::TID_BOP3    =  13  ! ^,**,e
  integer,parameter::TID_BOP3U   = -13  !
  integer,parameter::TID_UOP2    =  14  ! !,!!  
  integer,parameter::TID_UOP3    =  15  ! a++  
  integer,parameter::TID_UOP2U   = -15  ! 
  integer,parameter::TID_IFNC    =  16  ! sin, cos,...
  integer,parameter::TID_UFNC    =  17  !
  integer,parameter::TID_PRI_MAX =  18  ! 
  !! priority tabel end

  ! braket and delimiters
  integer,parameter::TID_SCL   =  64   ! ;
  integer,parameter::TID_COL   =  65   ! : PUSHED!
  integer,parameter::TID_IBRA  =  66   ! implicit (
  integer,parameter::TID_BRA   =  67   ! ( PUSHED!
  integer,parameter::TID_KET   =  69   ! )
  integer,parameter::TID_QTN   =  70   ! "
  integer,parameter::TID_QEND  =  71
  integer,parameter::TID_QSTA  =  72   ! PUSHED!
  integer,parameter::TID_COMA  =  73   ! ,
  integer,parameter::TID_MASN  =  74   ! = for macro PUSHED!
  integer,parameter::TID_DLM1  =  75   
  integer,parameter::TID_DLM2  =  76   ! ket
  integer,parameter::TID_BLK   =  77   ! space and tab
  integer,parameter::TID_HKET  =  78   ! }
  integer,parameter::TID_USCR  =  79   ! _
  integer,parameter::TID_ISTA  =  80
  integer,parameter::TID_IEND  =  81
  !                                    
  integer,parameter::TID_PAR   =  32   ! a,b,c,...
  integer,parameter::TID_PARU  = -32   ! a,b,c,...
  integer,parameter::TID_FIG   =  33   ! 1,2,3,...
  integer,parameter::TID_VAR   =  34   ! fig in rbuf
  integer,parameter::TID_MAC   =  36   
  integer,parameter::TID_OP    =  37   ! operators
  integer,parameter::TID_COP   =  38   
  integer,parameter::TID_OPN   =  39   
  integer,parameter::TID_APAR  =  40   ! par assign
  integer,parameter::TID_AMAC  =  41   
  integer,parameter::TID_AFNC  =  42   
  integer,parameter::TID_DPAR  =  43   ! dummy par
  integer,parameter::TID_END   =  44
  integer,parameter::TID_ROVAR =  45 
  integer,parameter::TID_LVAR_T = 46
  integer,parameter::TID_LVAR_F = 47
  integer,parameter::TID_LOP    = 48
  integer,parameter::TID_SOP    = 49
  integer,parameter::TID_POP    = 50
  integer,parameter::TID_CPAR   = 51 ! copied par
  integer,parameter::TID_NPAR   = 52 ! just assigned par
  integer,parameter::TID_IOP    = 53 ! integral
  integer,parameter::TID_IVAR1  = 54 ! dummy par in integrand x
  integer,parameter::TID_IVAR1L = 55 ! dummy par in integrand b-x
  integer,parameter::TID_IVAR1U = 56 ! dummy par in integrand x-a
  integer,parameter::TID_AT     = 57 ! @
  integer,parameter::TID_MSCL   = 58 ! ; in macro definition
  integer,parameter::TID_SHRP   = 59 ! #
  integer,parameter::TID_IGNORE = 60 
  integer,parameter::TID_EMAC   = 61 ! $mac to be expanded

  integer,parameter::LOID_NOT = 1
  integer,parameter::LOID_AND = 2
  integer,parameter::LOID_OR  = 3
  integer,parameter::LOID_EQ  = 4
  integer,parameter::LOID_NEQ = 5

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
     integer*8 opt
  end type t_rpnb

  type t_rpnm
     type(t_rpnq),allocatable::que(:)  ! allocated
     complex(cp),allocatable::vbuf(:)  ! allocated
     integer,allocatable::p_vbuf       ! allocated
     type(t_plist),pointer::pars       ! => rpnc%pars
     complex(cp),pointer::answer       ! => rpnc%answer
     complex(cp),pointer::tmpans       ! => rpnc%tmpans
     type(t_slist),allocatable::pnames ! allocated
     integer,allocatable::na               ! num arg
  end type t_rpnm

  type t_rpnlist
     type(t_rpnm),allocatable::rpnm(:)
     type(t_slist) s
  end type t_rpnlist

  type t_sd
     real(rp),allocatable::ws(:)
     complex(cp),allocatable::vs(:,:) ! n:2
     integer p_vs
  end type t_sd

  type t_rpnc
     type(t_rpnq),pointer::que(:)
     complex(cp),pointer::vbuf(:)
     integer,pointer::p_vbuf
     type(t_plist),pointer::pars
     complex(cp),pointer::answer
     complex(cp),pointer::tmpans
     type(t_rpnlist),pointer::rl
     integer,pointer::rc ! recursion count
     integer*8,pointer::opt
     integer,pointer::pfs(:)
     type(t_sd),pointer::sd 
     integer,pointer::ip
     type(t_rpnc),pointer::ifnc      ! integrand
     type(t_rpnq),pointer::ique(:)   ! backup of ifnc%que
  end type t_rpnc

  integer*8,parameter::RPNCOPT_NOP             =  0
  integer*8,parameter::RPNCOPT_DEBUG           =  Z"08000000"
  integer*8,parameter::RPNCOPT_READY           =  Z"00000001"
  integer*8,parameter::RPNCOPT_DEG             =  Z"00000002"
  integer*8,parameter::RPNCOPT_NEW             =  Z"00000004"
  integer*8,parameter::RPNCOPT_NO_AUTO_ADD_PAR =  Z"00000008"
  integer*8,parameter::RPNCOPT_RATIO           =  Z"00000010"
  integer*8,parameter::RPNCOPT_NO_WARN         =  Z"00000020"
  integer*8,parameter::RPNCOPT_DAT             =  Z"00000040"
  integer*8,parameter::RPNCOPT_STA             =  Z"00000080"
  integer*8,parameter::RPNCOPT_OBIN            =  Z"00000100"
  integer*8,parameter::RPNCOPT_OOCT            =  Z"00000200"
  integer*8,parameter::RPNCOPT_OHEX            =  Z"00000400"
  integer*8,parameter::RPNCOPT_OUTM = ior(RPNCOPT_OHEX,ior(RPNCOPT_OOCT,RPNCOPT_OBIN))     
  integer*8,parameter::RPNCOPT_IBIN            =  Z"00000800"
  integer*8,parameter::RPNCOPT_IOCT            =  Z"00001000"
  integer*8,parameter::RPNCOPT_IHEX            =  Z"00002000"
  integer*8,parameter::RPNCOPT_INM = ior(RPNCOPT_IHEX,ior(RPNCOPT_IOCT,RPNCOPT_IBIN))     
  integer*8,parameter::RPNCOPT_BYTE            =  Z"00004000" ! for SI prefix k to be 1024
  integer*8,parameter::RPNCOPT_NO_STDIN        =  Z"00008000"
  integer*8,parameter::RPNCOPT_NO_STDOUT       =  Z"00010000"

  integer,parameter::AID_NOP = 0
  integer,parameter::OID_NOP = 0
  integer,parameter::OID_CND = 1

  integer,parameter::PID_yoc =  1
  integer,parameter::PID_zep =  2
  integer,parameter::PID_a   =  3
  integer,parameter::PID_f   =  4
  integer,parameter::PID_pi  =  5
  integer,parameter::PID_n   =  6
  integer,parameter::PID_u   =  7
  integer,parameter::PID_mi  =  8
  integer,parameter::PID_    =  9
  integer,parameter::PID_k   = 10
  integer,parameter::PID_M   = 11
  integer,parameter::PID_G   = 12
  integer,parameter::PID_T   = 13
  integer,parameter::PID_P   = 14
  integer,parameter::PID_E   = 15
  integer,parameter::PID_Z   = 16
  integer,parameter::PID_Y   = 17
  integer,parameter::PID_END = 17
  integer,parameter::PID_INPUT  = 18
  integer,parameter::PID_EMAC   = 19

  integer,parameter::SC_RO  = 1
  integer,parameter::SC_MAC = 2
  integer,parameter::SC_FNC = 4

  interface put_vbuf
     module procedure put_vbuf_r
     module procedure put_vbuf_z
  end interface put_vbuf

  integer,parameter::narg_max=32

contains

  integer function rpnlist_count(rl)
    use slist, only: slist_count
    type(t_rpnlist),intent(in)::rl
    rpnlist_count=slist_count(rl%s)
  end function rpnlist_count

  function init_rpnlist(nmax)
    use slist, only: init_slist
    type(t_rpnlist) init_rpnlist
    integer,intent(in)::nmax
    init_rpnlist%s=init_slist()
    if(nmax>0) allocate(init_rpnlist%rpnm(nmax))
  end function init_rpnlist

  subroutine uinit_rpnms(n,rpnm)
    use slist, only: uinit_slist
    integer,intent(in)::n
    type(t_rpnm),intent(inout)::rpnm(n)
    integer i
    do i=1,n
       if(allocated (rpnm(i)%que))    deallocate(rpnm(i)%que)
       if(allocated (rpnm(i)%vbuf))   deallocate(rpnm(i)%vbuf)
       if(allocated (rpnm(i)%na))     deallocate(rpnm(i)%na)
       if(associated(rpnm(i)%pars))   nullify(   rpnm(i)%pars)
       if(associated(rpnm(i)%tmpans)) nullify(   rpnm(i)%tmpans)
       if(associated(rpnm(i)%answer)) nullify(   rpnm(i)%answer)
       if(allocated (rpnm(i)%p_vbuf)) deallocate(rpnm(i)%p_vbuf)
       if(allocated (rpnm(i)%na))     deallocate(rpnm(i)%na)
       call uinit_slist(rpnm(i)%pnames)
    end do
  end subroutine uinit_rpnms

  subroutine uinit_rpnlist(rl)
    use slist, only: uinit_slist
    type(t_rpnlist),intent(inout),target::rl
    call uinit_slist(rl%s)
    if(.not.allocated(rl%rpnm)) return
    call uinit_rpnms(size(rl%rpnm),rl%rpnm)
  end subroutine uinit_rpnlist
  
  integer function init_rpnc(npbuf_,szrlist_,nrpnm_)
     ! type(t_rpnc) function init_rpnc causes segmentation fault
    use zmath, only: zm_f1,zm_f2,zm_f3
    use fpio, only: czero,X2A_DEFAULT
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    integer,intent(in),optional::npbuf_,szrlist_,nrpnm_
    integer npbuf,szrlist,nrpnm
    p=malloc(sizeof(rpnc))
    if(present(npbuf_)) then
       npbuf=npbuf_
    else
       npbuf=NUM_PBUF_MIN
    end if
    if(present(szrlist_)) then
       szrlist=szrlist_
    else
       szrlist=LEN_RLIST_MIN
    end if
    if(present(nrpnm_)) then
       nrpnm=nrpnm_
    else
       nrpnm=NUM_RPNM_MIN
    end if
    nullify(rpnc%que)
    nullify(rpnc%vbuf)
    allocate(rpnc%rl)
    allocate(rpnc%tmpans)
    allocate(rpnc%answer)
    allocate(rpnc%pars)
    allocate(rpnc%p_vbuf)
    allocate(rpnc%rc)
    allocate(rpnc%opt)
    allocate(rpnc%sd)
    allocate(rpnc%ip)
    allocate(rpnc%pfs(3))
    nullify(rpnc%ifnc)
    nullify(rpnc%ique)
    rpnc%answer=czero
    rpnc%pfs(1)=loc(zm_f1)
    rpnc%pfs(2)=loc(zm_f2)
    rpnc%pfs(3)=loc(zm_f3)
    rpnc%pars=init_par(rpnc,npbuf)
    rpnc%rl=init_rpnlist(nrpnm)
    rpnc%opt=ior(RPNCOPT_NOP,ishft(X2A_DEFAULT,32))
    init_rpnc=p
  end function init_rpnc

  subroutine cp_rpnm(rpnm1,rpnm2)
    use slist, only: cp_slist
    type(t_rpnm),intent(in)::rpnm1
    type(t_rpnm),intent(inout)::rpnm2
    if(.not.allocated(rpnm2%que)) then
       allocate(rpnm2%que(size(rpnm1%que)))
       rpnm2%que=rpnm1%que
    end if
    if(.not.allocated(rpnm2%vbuf)) then
       allocate(rpnm2%vbuf(size(rpnm1%vbuf)))
       rpnm2%vbuf=rpnm1%vbuf
    end if
    if(.not.allocated(rpnm2%p_vbuf)) then 
       allocate(rpnm2%p_vbuf)
       rpnm2%p_vbuf=rpnm1%p_vbuf
    end if
    if(.not.allocated(rpnm2%pnames)) then
       allocate(rpnm2%pnames)
       rpnm2%pnames=cp_slist(rpnm1%pnames)
    end if
  end subroutine cp_rpnm

  subroutine min_cp_rpnlist(rl1,rl2)
    use slist, only: cp_slist
    type(t_rpnlist),intent(in)::rl1
    type(t_rpnlist),intent(inout)::rl2
    integer i,nrl2
    rl2%s=cp_slist(rl1%s)
    nrl2=rpnlist_count(rl2)
    if(nrl2<=0) return
    allocate(rl2%rpnm(nrl2))
    do i=1,nrl2
       call cp_rpnm(rl1%rpnm(i),rl2%rpnm(i))
    end do
  end subroutine min_cp_rpnlist

  integer function cp_rpnc(rpnc_in)
    use plist, only: add_par_by_reference,cp_plist
    type(t_rpnc),intent(in)::rpnc_in
    type(t_rpnc) rpnc
    integer istat
    pointer(p,rpnc)
    p=init_rpnc(0,0,0)
    call min_cp_rpnlist(rpnc_in%rl,rpnc%rl)
    rpnc%pars=cp_plist(rpnc_in%pars)
    istat=add_par_by_reference(rpnc%pars,"tmp",loc(rpnc%tmpans),.true.)
    istat=add_par_by_reference(rpnc%pars,"ans",loc(rpnc%answer),.true.)
    cp_rpnc=p
  end function cp_rpnc

  subroutine uinit_rpnc(rpnc)
    type(t_rpnc),intent(inout)::rpnc
    if(associated(rpnc%que).and.size(rpnc%que)>0) deallocate(rpnc%que)
    if(associated(rpnc%vbuf).and.size(rpnc%vbuf)>0) deallocate(rpnc%vbuf)
    if(associated(rpnc%tmpans)) deallocate(rpnc%tmpans)
    if(associated(rpnc%answer)) deallocate(rpnc%answer)
    if(associated(rpnc%pars)) then
       call uinit_par(rpnc)
       deallocate(rpnc%pars)
    end if
    if(associated(rpnc%p_vbuf)) deallocate(rpnc%p_vbuf)
    if(associated(rpnc%rc)) deallocate(rpnc%rc)
    if(associated(rpnc%rl)) then
       call uinit_rpnlist(rpnc%rl)
       deallocate(rpnc%rl)
    end if
    if(associated(rpnc%pfs)) deallocate(rpnc%pfs)
    if(associated(rpnc%sd)) then
       call uinit_sd(rpnc%sd)
       deallocate(rpnc%sd)
    end if
  end subroutine uinit_rpnc

  function init_par(rpnc,nmax)
    use plist, only: init_plist,add_par_by_reference,add_par_by_value
    type(t_plist) init_par
    type(t_rpnc),intent(in)::rpnc
    integer,intent(in)::nmax
    integer istat
    init_par=init_plist()
    if(nmax==0) return
    istat=add_par_by_reference(init_par,"tmp",loc(rpnc%tmpans),.true.)
    istat=add_par_by_reference(init_par,"ans",loc(rpnc%answer),.true.)
    istat=add_par_by_value(init_par,"eps",epsilon(0.0_rp),.true.)    
    istat=add_par_by_value(init_par,"huge",huge(0.0_rp),.true.)    
    istat=add_par_by_value(init_par,"i",complex(0.0_rp,1.0_rp),.true.)
    istat=add_par_by_value(init_par,"pi",atan(1.0_rp)*4.0_rp,.true.)
    istat=add_par_by_value(init_par,"c",2.99792458e8_rp,.true.)
  end function init_par

  subroutine uinit_par(rpnc)
    use plist, only: uinit_plist
    type(t_rpnc),intent(inout)::rpnc
    call uinit_plist(rpnc%pars)
  end subroutine uinit_par

  subroutine delete_par_all(rpnc)
    use plist, only: rm_par_all
    type(t_rpnc),intent(inout)::rpnc
    call rm_par_all(rpnc%pars)
  end subroutine delete_par_all

  subroutine delete_par(rpnc,s)
    use plist, only: rm_par
    use misc, only: mess
    use memio, only: itoa
    type(t_rpnc),intent(inout)::rpnc
    character*(*),intent(in)::s
    integer istat
    istat=rm_par(rpnc%pars,trim(adjustl(s)))
    if(istat/=0) call mess("*** Error delete_par: "//trim(s)//": code = "//trim(itoa(istat)))
  end subroutine delete_par

  subroutine set_sd(ip1,ip2,rpnc)
    use fpio, only: rzero
    integer,intent(in)::ip1,ip2
    type(t_rpnc),intent(inout),target::rpnc
    integer i,j,i2
    complex(cp) z
    logical col
    pointer(pz,z)
    type(t_rpnq),pointer::q

    if(.not.allocated(rpnc%sd%vs)) call init_sd(rpnc%sd)
    call next
    i2=min(size(rpnc%que),ip2)
    do i=ip1,i2
       q => rpnc%que(i)
       select case(q%tid)
       case(TID_VAR,TID_PAR,TID_CPAR,TID_ROVAR)
          pz=q%cid
          if(.not.col) then
             j=j+1
             if(j>2) cycle
             rpnc%sd%vs(rpnc%sd%p_vs,j)=z
             if(j==1) then
                rpnc%sd%vs(rpnc%sd%p_vs,2)=rzero
                rpnc%sd%ws(rpnc%sd%p_vs)=1.0_rp
             end if
          else
             q%tid=TID_NOP
             rpnc%sd%ws(rpnc%sd%p_vs)=realpart(z)
          end if
       case(TID_COL)
          q%tid=TID_NOP
          col=.true.
       case(TID_END)
          if(i==i2) exit
          q%tid=TID_NOP
          call next
       end select
    end do

  contains
    
    subroutine next
      if(rpnc%sd%p_vs==size(rpnc%sd%vs,1)) call inc_sd(rpnc%sd,8) ! <<<
      rpnc%sd%p_vs=rpnc%sd%p_vs+1
      j=0
      col=.false.
    end subroutine next

  end subroutine set_sd

  subroutine dump_sd(rpnc)
    use fpio, only: ztoa,rtoa
    use memio, only: itoa
    use misc, only: mess
    type(t_rpnc),intent(in)::rpnc
    integer i,f
    if(.not.allocated(rpnc%sd%vs)) return
    f=ishft(rpnc%opt,-32)
    do i=1,rpnc%sd%p_vs
       call mess(trim(itoa(i))//": "//trim(ztoa(rpnc%sd%vs(i,1),f)) &
            //", "//trim(ztoa(rpnc%sd%vs(i,2),f)) &
            //" ("//trim(rtoa(rpnc%sd%ws(i),f))//")")
    end do    
  end subroutine dump_sd

  subroutine inc_sd(sd,n)
    type(t_sd),intent(inout)::sd
    integer,intent(in)::n
    complex(cp),allocatable::tmp_vs(:,:)
    real(rp),allocatable::tmp_ws(:)
    allocate(tmp_vs(size(sd%vs,1)+n,2),tmp_ws(size(sd%ws)+n))
    tmp_vs=sd%vs
    tmp_ws=sd%ws
    deallocate(sd%vs,sd%ws)
    allocate(sd%vs(size(tmp_vs,1),2),sd%ws(size(tmp_ws)))
    sd%vs=tmp_vs
    sd%ws=tmp_ws
    deallocate(tmp_vs,tmp_ws)
  end subroutine inc_sd

  subroutine init_sd(sd)
    type(t_sd),intent(inout)::sd
    allocate(sd%vs(NUM_VS_MIN,2),sd%ws(NUM_VS_MIN))
    sd%p_vs=0
  end subroutine init_sd

  subroutine uinit_sd(sd)
    type(t_sd),intent(inout)::sd
    if(allocated(sd%vs)) deallocate(sd%vs)
    if(allocated(sd%ws)) deallocate(sd%ws)
  end subroutine uinit_sd

  subroutine reset_sd(sd)
    type(t_sd),intent(inout)::sd
    sd%p_vs=0
  end subroutine reset_sd

  subroutine put_vbuf_r(rpnc,i,v)
    use fpio, only: rzero
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(inout)::i
    real(rp),intent(in)::v
    real(rp) im
    if(iand(rpnc%opt,RPNCOPT_RATIO)==0) then
       im=rzero
    else
       im=1.0_rp
    end if
    call put_vbuf_z(rpnc,i,complex(v,im),TID_VAR)
  end subroutine put_vbuf_r

  integer function count_op(q)
    use misc, only: get_lo32
    type(t_rpnq),intent(in)::q(:)
    integer i
    count_op=0
    do i=1,size(q)
       select case(get_lo32(q(i)%tid))
       case(TID_OP,TID_IOP,TID_OPN, &
            TID_LOP,TID_ROP,TID_COP, &
            TID_AOP,TID_UFNC,TID_MAC)
          count_op=count_op+1
       end select
    end do
  end function count_op

  integer function count_tid(q,tid)
    use misc, only: get_lo32
    type(t_rpnq),intent(in)::q(:)
    integer,intent(in)::tid
    integer i
    count_tid=0
    do i=1,size(q)
       if(get_lo32(q(i)%tid)==tid) &
            count_tid=count_tid+1
    end do
  end function count_tid

  subroutine put_vbuf_z(rpnc,i,v,tid)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    complex(cp),intent(in)::v
    integer,intent(in),optional::tid
    integer t
    if(.not.associated(rpnc%vbuf).or.rpnc%p_vbuf>=size(rpnc%vbuf)) &
         STOP "*** put_vbuf_z: INTERNAL ERROR: vbuf overflow"
    rpnc%p_vbuf=rpnc%p_vbuf+1
    rpnc%vbuf(rpnc%p_vbuf)=v
    rpnc%que(i)%cid=loc(rpnc%vbuf(rpnc%p_vbuf))
    if(present(tid)) then
       t=tid
    else
       t=TID_VAR
    end if
    rpnc%que(i)%tid=t
  end subroutine put_vbuf_z

  subroutine save_par(rpnc,f)
    use misc, only: open_file
    use plist, only: dump_plist,plist_count
    type(t_rpnc),intent(in)::rpnc
    character*(*),intent(in)::f
    integer i,ou
    ou=open_file(f,.true.,.true.)
    if(ou==0) return
    do i=1,plist_count(rpnc%pars)
       call dump_plist(rpnc%pars,ent=i,out_unit=ou)
    end do
    close(ou)
  end subroutine save_par

  subroutine save_fnc(rpnc,f)
    use misc, only: open_file
    type(t_rpnc),intent(in)::rpnc
    character*(*),intent(in)::f
    integer i,ou
    ou=open_file(f,.true.,.true.)
    if(ou==0) return
    do i=1,rpnlist_count(rpnc%rl)
       call dump_rpnm(rpnc,ent=i,type=SC_FNC,out_unit=ou)
    end do
    close(ou)
  end subroutine save_fnc

  subroutine save_mac(rpnc,f)
    use misc, only: open_file
    type(t_rpnc),intent(in)::rpnc
    character*(*),intent(in)::f
    integer i,ou
    ou=open_file(f,.true.,.true.)
    if(ou==0) return
    do i=1,rpnlist_count(rpnc%rl)
       call dump_rpnm(rpnc,ent=i,type=SC_MAC,out_unit=ou)
    end do
  end subroutine save_mac

  subroutine dump_rpnm(rpnc,ent,name,type,out_unit)
    use slist, only: slist_count,get_str_ptr,dump_slist
    use misc, only: mess,messp,stdout
    use memio, only: cpstr,itoa
    type(t_rpnc),intent(in),target::rpnc
    integer,intent(in),optional::ent
    character*(*),intent(in),optional::name
    integer,intent(in),optional::type
    integer,intent(in),optional::out_unit
    integer ou
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) tmprpnc
    integer ptr,len
    integer code
    integer i,i1,i2
    integer t
    if(present(out_unit)) then
       ou=out_unit
    else
       ou=stdout
    end if
    if(present(type)) then
       t=type
    else
       t=0
    end if
    i1=1
    i2=slist_count(rpnc%rl%s)
    if(present(ent)) then
       if(ent>0) then
          i1=ent
          i2=ent
       end if
    end if
    do i=i1,i2
       if(i>size(rpnc%rl%rpnm) &
            .or.get_str_ptr(rpnc%rl%s,i,ptr,len,code)/=0) then
          if(ou==stdout) call mess("*** dump_rpnm: no such entry: "//trim(itoa(i)))
          cycle
       end if
       rpnm=>rpnc%rl%rpnm(i)
       if(iand(code,SC_MAC)/=0) then
          if(t/=0.and.t/=SC_MAC) cycle
          if(ou==stdout) call mess("MACRO entry: "//trim(itoa(i)))
       else
          if(t/=0.and.t/=SC_FNC) cycle
          if(ou==stdout) call mess("FUNCTION entry: "//trim(itoa(i)))
          if(get_str_ptr(rpnm%pnames,2,ptr,len)/=0) then
             if(ou==stdout) call mess("???") !<<<<<<<<<<<<<<<<<<<<
             cycle !<<<<<<<<<
          end if
       end if

       if(present(name)) then
          if(name/="".and.name/=cpstr(ptr,len)) cycle
       end if

       if(ou==stdout) then
          call mess("name: "//cpstr(ptr,len))
       else
          call messp(cpstr(ptr,len)//"=",ou)
       end if

       if(allocated(rpnm%pnames).and.get_str_ptr(rpnm%pnames,1,ptr,len)==0) then
          if(ou==stdout) then
             call mess("definition: "//cpstr(ptr,len))
          else
             if(t==SC_MAC) then
                call mess(""""//cpstr(ptr,len)//"""",ou)
             else
                call mess(cpstr(ptr,len),ou)
             end if
             cycle
          end if
       else
          if(ou==stdout) then
             call mess("(empty)")
          else
             call mess("",ou)
          end if
          cycle
       end if

       tmprpnc%que=>rpnm%que
       tmprpnc%vbuf=>rpnm%vbuf
       tmprpnc%p_vbuf=>rpnm%p_vbuf
       tmprpnc%pars=>rpnm%pars
       tmprpnc%answer=>rpnm%answer
       tmprpnc%tmpans=>rpnm%tmpans
       tmprpnc%rl=>rpnc%rl
       tmprpnc%rc=>rpnc%rc
       tmprpnc%pfs=>rpnc%pfs
       if(allocated(rpnm%na)) call mess("number of arguments = "//trim(itoa(rpnm%na)))
       call dump_rpnc(tmprpnc,i)
       call dump_slist(rpnm%pnames)

    end do
  end subroutine dump_rpnm
  
  subroutine dump_rpnc(rpnc,mid)
    use fpio, only: DISP_FMT_RAW,ztoa
    use misc, only: get_lo32,mess,messp
    use slist, only: get_str_ptr
    use memio, only: cpstr,itoa, DISP_FMT_HEX
    type(t_rpnc),intent(in)::rpnc
    integer,intent(in),optional::mid
    type(t_rpnm),pointer::rpnm
    type(t_rpnq),pointer::q
    integer i,t,istat
    integer ptr,len
    complex(cp) z
    complex(cp) v
    pointer(pv,v)
    call mess("rpnc dump:")
    if(.not.associated(rpnc%que).or.size(rpnc%que)<1) then
       call mess("(empty)")
       return
    end if
    if(.not.present(mid).and.iand(rpnc%opt,RPNCOPT_READY)==0) then
       call mess("(not set)")
       return
    end if
    call mess("#\ttid\tcid\tvalue")
    if(present(mid)) rpnm=>rpnc%rl%rpnm(mid)
    do i=1,size(rpnc%que)
       q => rpnc%que(i)
       t=get_lo32(q%tid)
       call messp(trim(itoa(i))//":\t"//trim(itoa(t))//"\t")
       select case(t)
       case(TID_VAR,TID_PAR,TID_CPAR,TID_FIG,TID_ROVAR,TID_LVAR_T,TID_LVAR_F)
          call messp(trim(itoa(q%cid,DISP_FMT_HEX)))
          if(present(mid)) then
             if(t/=TID_FIG) then
                istat=get_str_ptr(rpnm%pnames,q%cid,ptr,len)
                call mess(cpstr(ptr,len))
                cycle
             else
                z=rpnm%vbuf(q%cid)
             end if
          else
             if(t==TID_CPAR) then
                call messp(" (copied)")
                pv=q%cid
                z=v
             end if
             pv=q%cid
             z=v
             if(pv==0) then
                call mess(" (undef)")
                cycle
             end if
          end if
          call mess(trim(ztoa(z,fmt=DISP_FMT_RAW)))
       case(TID_DPAR)
          call mess(trim(itoa(q%cid))//" (dummy par)")
       case default
          call mess(trim(itoa(q%cid,DISP_FMT_HEX)))
       end select
    end do
    call mess("vbuf dump:")
    call mess("size used/alloc= "//trim(itoa(rpnc%p_vbuf))//"/"//trim(itoa(size(rpnc%vbuf))))
    if(rpnc%p_vbuf>0) then
       do i=1,rpnc%p_vbuf
          call mess(trim(itoa(i))//":\t"//trim(itoa(loc(rpnc%vbuf(i)),DISP_FMT_HEX))&
               //"\t"//trim(ztoa(rpnc%vbuf(i),fmt=DISP_FMT_RAW)))
       end do
    end if
    if(.not.present(mid)) call mess("")
  end subroutine dump_rpnc

  subroutine dump_rpnb(rpnb)
    use misc, only:  mess
    type(t_rpnb),intent(in),target::rpnb
    type(t_rrpnq),pointer::q(:)
    call mess("rpnb que:\n#\ttid\tp1\tp2\texpr")
    if(.not.allocated(rpnb%que).or.rpnb%p_que<1)  then
       call mess("(empty)")
    else
       q => rpnb%que
       call dump_q(rpnb%p_que)
    end if
    call mess("rpnb buf:\n#\ttid\tp1\tp2\texpr")
    if(.not.allocated(rpnb%buf).or.rpnb%p_buf<1)  then
       call mess("(empty)")
    else
       q => rpnb%buf
       call dump_q(rpnb%p_buf)
    end if
  contains
    subroutine dump_q(n)
      use misc, only: get_lo32,get_up32,messp
      use memio, only: itoa
      integer,intent(in)::n
      integer i,p1lo,p2lo,p1up,p2up,tid
      do i=1,n
         p1lo=get_lo32(q(i)%p1)
         p2lo=get_lo32(q(i)%p2)
         tid=q(i)%tid
         if(tid>0) tid=get_lo32(tid)
         call messp(trim(itoa(i))//":\t"//trim(itoa(tid))//"\t"//trim(itoa(p1lo))//"\t"//trim(itoa(p2lo))//"\t")
         if(q(i)%tid==TID_VAR) then
            call mess(rpnb%expr(p1lo:p2lo))
         else if(q(i)%tid==TID_AMAC) then
            p1up=get_up32(q(i)%p1)
            p2up=get_up32(q(i)%p2)
            call mess(rpnb%expr(p1lo:p2lo)//"\t"//rpnb%expr(p1up:p2up))
         else if(p1lo==0) then
            call mess("(no ref)")
         else if(q(i)%tid/=TID_NOP) then
            call mess(rpnb%expr(p1lo:p2lo))
         else
            call mess("")
         end if
      end do
    end subroutine dump_q
  end subroutine dump_rpnb

end module rpnd
