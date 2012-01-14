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
  use slist
  use plist
  use fpio
  implicit none

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

  integer,parameter::RPN_REC_MAX     =  256
  integer,parameter::NUM_VBUF_MIN    =   32
  integer,parameter::NUM_PBUF_MIN    =   32
  integer,parameter::NUM_RPNM_MIN    =    8
  integer,parameter::LEN_PLIST_MIN   = 1024
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
  integer,parameter::TID_BOP4    =  11  ! implicit * <<<<<<<<<<< 
  integer,parameter::TID_BOP3    =  12  ! ^,**,e
  integer,parameter::TID_BOP3U   = -12  !
  integer,parameter::TID_UOP3    =  13  ! a++
  integer,parameter::TID_UOP1    =  14  ! +a,-a,++a
  integer,parameter::TID_UOP2    =  15  ! !,!!    
  integer,parameter::TID_UOP2U   = -16  ! 
  integer,parameter::TID_IFNC    =  17  ! sin, cos,...
  integer,parameter::TID_UFNC    =  18  !
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

  integer,parameter::SC_RO  = 1
  integer,parameter::SC_MAC = 2
  integer,parameter::SC_FNC = 4

  interface put_vbuf
     module procedure put_vbuf_r
     module procedure put_vbuf_z
  end interface put_vbuf

  integer,parameter::narg_max=32

contains

  function init_rpnlist(sz,nmax)
    type(t_rpnlist) init_rpnlist
    integer,intent(in)::sz,nmax
    init_rpnlist%s=init_slist(sz)
    if(nmax>0) allocate(init_rpnlist%rpnm(nmax))
  end function init_rpnlist

  subroutine uinit_rpnms(n,rpnm)
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
    type(t_rpnlist),intent(inout),target::rl
    call uinit_slist(rl%s)
    if(.not.allocated(rl%rpnm)) return
    call uinit_rpnms(size(rl%rpnm),rl%rpnm)
  end subroutine uinit_rpnlist

  subroutine inc_vbuf(rpnc,n)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::n
    complex(cp),allocatable::vb(:)
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
  
   integer function init_rpnc(nvbuf_,szplist_,npbuf_,szrlist_,nrpnm_)
     ! type(t_rpnc) function init_rpnc causes segmentation fault
    use zmath
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    integer,intent(in),optional::nvbuf_,szplist_,npbuf_,szrlist_,nrpnm_
    integer nvbuf,szplist,npbuf,szrlist,nrpnm
    p=malloc(sizeof(rpnc))
    nvbuf=NUM_VBUF_MIN
    if(present(nvbuf_).and.nvbuf>0) then
       nvbuf=nvbuf_
    end if
    if(present(szplist_)) then
       szplist=szplist_
    else
       szplist=LEN_PLIST_MIN
    end if
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
    nullify (rpnc%que)
    nullify (rpnc%vbuf)
    allocate(rpnc%vbuf(nvbuf))
    allocate(rpnc%rl)
    allocate(rpnc%tmpans)
    allocate(rpnc%answer)
    rpnc%answer=czero
    allocate(rpnc%pars)
    allocate(rpnc%p_vbuf)
    allocate(rpnc%rc)
    allocate(rpnc%opt)
    allocate(rpnc%sd)
    allocate(rpnc%ip)
    allocate(rpnc%pfs(3))
    rpnc%pfs(1)=loc(zm_f1)
    rpnc%pfs(2)=loc(zm_f2)
    rpnc%pfs(3)=loc(zm_f3)
    rpnc%pars=init_par(rpnc,szplist,npbuf)
    rpnc%rl=init_rpnlist(szrlist,nrpnm)
    rpnc%opt=ior(RPNCOPT_NOP,ishft(X2A_DEFAULT,32))
    init_rpnc=p
  end function init_rpnc

  subroutine cp_rpnm(rpnm1,rpnm2)
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
       rpnm2%pnames=init_slist(0)
       call min_cp_slist(rpnm1%pnames,rpnm2%pnames) !<<<
    end if
  end subroutine cp_rpnm

  subroutine min_cp_rpnlist(rl1,rl2)
    type(t_rpnlist),intent(in)::rl1
    type(t_rpnlist),intent(inout)::rl2
    integer i
    call min_cp_slist(rl1%s,rl2%s)
    if(rl2%s%n<=0) return
    allocate(rl2%rpnm(rl2%s%n))
    do i=1,rl2%s%n
       call cp_rpnm(rl1%rpnm(i),rl2%rpnm(i))
    end do
  end subroutine min_cp_rpnlist

  integer function cp_rpnc(rpnc_in)
    type(t_rpnc),intent(in)::rpnc_in
    type(t_rpnc) rpnc
    integer istat
    pointer(p,rpnc)
    p=init_rpnc(size(rpnc_in%vbuf),0,0,0,0)
    call min_cp_rpnlist(rpnc_in%rl,rpnc%rl)
    call min_cp_plist(rpnc_in%pars,rpnc%pars)
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

  function init_par(rpnc,sz,nmax)
    type(t_plist) init_par
    type(t_rpnc),intent(in)::rpnc
    integer,intent(in)::sz,nmax
    integer istat
    init_par=init_plist(sz,nmax)
    if(sz==0.or.nmax==0) return
    istat=add_par_by_reference(init_par,"tmp",loc(rpnc%tmpans),.true.)
    istat=add_par_by_reference(init_par,"ans",loc(rpnc%answer),.true.)
    istat=add_par_by_value(init_par,"eps",epsilon(0.0_rp),.true.)    
    istat=add_par_by_value(init_par,"huge",huge(0.0_rp),.true.)    
    istat=add_par_by_value(init_par,"i",complex(0.0_rp,1.0_rp),.true.)
    istat=add_par_by_value(init_par,"pi",atan(1.0_rp)*4.0_rp,.true.)
    istat=add_par_by_value(init_par,"c",2.99792458e8_rp,.true.)
  end function init_par

  subroutine uinit_par(rpnc)
    type(t_rpnc),intent(inout)::rpnc
    call uinit_plist(rpnc%pars)
  end subroutine uinit_par

  subroutine delete_par_all(rpnc)
    type(t_rpnc),intent(inout)::rpnc
    call rm_par_all(rpnc%pars)
  end subroutine delete_par_all

  subroutine delete_par(rpnc,s)
    type(t_rpnc),intent(inout)::rpnc
    character*(*),intent(in)::s
    integer istat
    istat=rm_par(rpnc%pars,trim(adjustl(s)))
    if(istat/=0) write(*,*) "*** Error delete_par: "//trim(s)//": code = ",istat
  end subroutine delete_par

  subroutine set_sd(ip,rpnc)
    integer,intent(in)::ip
    type(t_rpnc),intent(inout),target::rpnc
    integer i,j,i2
    complex(cp) z
    logical col
    pointer(pz,z)
    type(t_rpnq),pointer::q

    if(.not.allocated(rpnc%sd%vs)) call init_sd(rpnc%sd)
    call next
    if(ip==0) then
       i2=size(rpnc%que)
    else
       ! the last operand
       ! ip points to next of ;;
       i2=ip-2 
    end if
    do i=1,i2
       q => rpnc%que(i)
       select case(q%tid)
       case(TID_VAR,TID_PAR,TID_CPAR,TID_ROVAR)
          q%tid=TID_NOP
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
             rpnc%sd%ws(rpnc%sd%p_vs)=realpart(z)
          end if
       case(TID_COL)
          q%tid=TID_NOP
          col=.true.
       case(TID_END)
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
    type(t_rpnc),intent(in)::rpnc
    integer i,f
    f=ishft(rpnc%opt,-32)
    do i=1,rpnc%sd%p_vs
       write(*,*) trim(itoa(i))//": "//trim(ztoa(rpnc%sd%vs(i,1),f)) &
            //", "//trim(ztoa(rpnc%sd%vs(i,2),f)) &
            //" ("//trim(rtoa(rpnc%sd%ws(i),f))//")"
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

  subroutine put_vbuf_r(rpnc,i,v)
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

  subroutine put_vbuf_z(rpnc,i,v,tid)
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in)::i
    complex(cp),intent(in)::v
    integer,intent(in),optional::tid
    integer t
    if(rpnc%p_vbuf>=size(rpnc%vbuf)) call inc_vbuf(rpnc,NUM_VBUF_MIN)
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

  subroutine dump_rpnm(rpnc,ent,name,type)
    type(t_rpnc),intent(in),target::rpnc
    integer,intent(in),optional::ent
    character*(*),intent(in),optional::name
    integer,intent(in),optional::type
    type(t_rpnm),pointer::rpnm
    type(t_rpnc) tmprpnc
    integer ptr,len
    integer code
    integer i

    do i=1,rpnc%rl%s%n
       if(present(ent)) then
          if(ent>0.and.i/=ent) cycle
       end if
       if(i>size(rpnc%rl%rpnm) &
            .or.get_str_ptr(rpnc%rl%s,i,ptr,len,code)/=0) then
          write(*,*) "*** dump_rpnm: no such entry: ",i
          cycle
       end if

       rpnm=>rpnc%rl%rpnm(i)
       if(iand(code,SC_MAC)/=0) then
          if(present(type)) then
             if(type/=SC_MAC) cycle
          end if
          write(*,*) "MACRO entry: ",i
       else
          if(present(type)) then
             if(type/=SC_FNC) cycle
          end if
          write(*,*) "FUNCTION entry: ",i
          if(get_str_ptr(rpnm%pnames,2,ptr,len)/=0) then
             write(*,*) "???"
             cycle !<<<<<<<<<
          end if
       end if

       if(present(name)) then
          if(name/="".and.name/=trim(cpstr(ptr,len))) cycle
       end if

       write(*,*) "name: "//trim(cpstr(ptr,len))
       if(allocated(rpnm%pnames).and.get_str_ptr(rpnm%pnames,1,ptr,len)==0) then
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
          if(allocated(rpnm%na)) write(*,*) "number of arguments = ",rpnm%na
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
    type(t_rpnq),pointer::q
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
       q => rpnc%que(i)
       t=get_lo32(q%tid)
       write(*,10) i,t
       select case(t)
       case(TID_VAR,TID_PAR,TID_CPAR,TID_FIG,TID_ROVAR,TID_LVAR_T,TID_LVAR_F)
          write(*,11) q%cid
          if(present(mid)) then
             if(t/=TID_FIG) then
                istat=get_str_ptr(rpnm%pnames,q%cid,ptr,len)
                write(*,*) trim(cpstr(ptr,len))
                cycle
             else
                z=rpnm%vbuf(q%cid)
             end if
          else
             if(t==TID_CPAR) then
                write(*,*) "(copied)"
                cycle
             end if
             pv=q%cid
             z=v
             if(pv==0) then
                write(*,*) "(undef)"
                cycle
             end if
          end if
          write(*,*) trim(ztoa(z,fmt=DISP_FMT_RAW))
       case(TID_OP,TID_OPN,TID_ROP)
          write(*,14) q%cid
       case(TID_DPAR)
          write(*,16) q%cid,"(dummy par)"
       case default
          write(*,14) q%cid
       end select
    end do
10  format(2(x,i4),$)
11  format(x,z16,$)
13  format(x,i4,x,z8,x,a)
14  format(x,z16)
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

  subroutine dump_rpnb(rpnb)
    type(t_rpnb),intent(in),target::rpnb
    type(t_rrpnq),pointer::q(:)
    write(*,*) "rpnb que:\n# tid p1 p2 expr"
    if(.not.allocated(rpnb%que).or.rpnb%p_que<1)  then
       write(*,*) "(empty)"
    else
       q => rpnb%que
       call dump_q(rpnb%p_que)
    end if
    write(*,*) "rpnb buf:\n# tid p1 p2 expr"
    if(.not.allocated(rpnb%buf).or.rpnb%p_buf<1)  then
       write(*,*) "(empty)"
    else
       q => rpnb%buf
       call dump_q(rpnb%p_buf)
    end if
  contains
    subroutine dump_q(n)
      integer,intent(in)::n
      integer i,p1lo,p2lo,p1up,p2up,tid
      do i=1,n
         p1lo=get_lo32(q(i)%p1)
         p2lo=get_lo32(q(i)%p2)
         tid=q(i)%tid
         if(tid>0) tid=get_lo32(tid)
         write(*,10) i,tid,p1lo,p2lo
10       format(4(x,i4),x,$)
         if(q(i)%tid==TID_VAR) then
            write(*,*) rpnb%expr(p1lo:p2lo)
         else if(q(i)%tid==TID_AMAC) then
            p1up=get_up32(q(i)%p1)
            p2up=get_up32(q(i)%p2)
            write(*,*) rpnb%expr(p1lo:p2lo)//" "//rpnb%expr(p1up:p2up)
         else if(p1lo==0) then
            write(*,*) "(no ref)"
         else if(q(i)%tid/=TID_NOP) then
            write(*,*) rpnb%expr(p1lo:p2lo)
         else
            write(*,*)
         end if
      end do
    end subroutine dump_q
  end subroutine dump_rpnb

end module rpnd
