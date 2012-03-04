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
  use plist, only: t_plist
  use fpio, only: rp,cp
  use rpng
  use rpnt
  use rpnlist, only: t_rpnlist
  implicit none

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

  integer,parameter::narg_max=32

  interface put_vbuf
     module procedure put_vbuf_r
     module procedure put_vbuf_z
  end interface put_vbuf

contains
  
  integer function init_rpnc(cp)
     ! type(t_rpnc) function init_rpnc causes segmentation fault
    use zmath, only: zm_f1,zm_f2,zm_f3
    use fpio, only: czero,X2A_DEFAULT
    use rpnlist, only: init_rpnlist
    logical,intent(in),optional::cp
    type(t_rpnc) rpnc
    pointer(p,rpnc)
    p=malloc(sizeof(rpnc))
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
    rpnc%pars=init_par(rpnc,cp)
    rpnc%rl=init_rpnlist()
    rpnc%opt=ior(RPNCOPT_NOP,ishft(X2A_DEFAULT,32))
    init_rpnc=p
  end function init_rpnc

  integer function cp_rpnc(rpnc_in)
    use rpnlist, only: cp_rpnlist
    use plist, only: add_par_by_reference,cp_plist
    type(t_rpnc),intent(in)::rpnc_in
    type(t_rpnc) rpnc
    integer istat
    pointer(p,rpnc)
    p=init_rpnc(cp=.true.)
    rpnc%rl=cp_rpnlist(rpnc_in%rl)
    rpnc%pars=cp_plist(rpnc_in%pars)
    istat=add_par_by_reference(rpnc%pars,"tmp",loc(rpnc%tmpans),.true.)
    istat=add_par_by_reference(rpnc%pars,"ans",loc(rpnc%answer),.true.)
    cp_rpnc=p
  end function cp_rpnc

  subroutine uinit_rpnc(rpnc)
    use rpnlist, only: uinit_rpnlist
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

  function init_par(rpnc,cp)
    use plist, only: init_plist,add_par_by_reference,add_par_by_value
    type(t_plist) init_par
    type(t_rpnc),intent(in)::rpnc
    logical,intent(in),optional::cp
    logical noset
    integer istat
    if(present(cp)) then
       noset=cp
    else 
       noset=.false.
    end if
    init_par=init_plist()
    if(noset) return
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

  subroutine delete_rpnm(rpnc,index,name,type)
    use rpnlist, only: rm_rpnm_entry,rm_rpnm_entry_all
    type(t_rpnc),intent(inout)::rpnc
    integer,intent(in),optional::index
    character*(*),intent(in),optional::name
    integer,intent(in),optional::type
    integer k,t,istat
    if(present(index)) then
       k=index
    else
       k=0
    end if
    if(present(type)) then
       t=type
    else
       t=0
    end if
    if(present(name).and.len_trim(name)>0) then
       istat=rm_rpnm_entry(rpnc%rl,name,t)
    else if(k==0) then
       call rm_rpnm_entry_all(rpnc%rl,t)
    else
       istat=rm_rpnm_entry(rpnc%rl,k)
    end if
  end subroutine delete_rpnm

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
    use rpnlist, only: rpnlist_count
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
    use rpnlist, only: rpnlist_count
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
    use rpnlist, only: rpnlist_count,kth_rpnlist,kth_rpnm,t_rpnm
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
    i2=rpnlist_count(rpnc%rl)
    if(present(ent)) then
       if(ent>0) then
          i1=ent
          i2=ent
       end if
    end if
    do i=i1,i2
       if(i>rpnlist_count(rpnc%rl) &
            .or.kth_rpnlist(rpnc%rl,i,ptr,len,code)/=0) then
          if(ou==stdout) call mess("*** dump_rpnm: no such entry: "//trim(itoa(i)))
          cycle
       end if
       rpnm => kth_rpnm(rpnc%rl,i)

       if(present(name)) then
          if(name/="".and.name/=cpstr(ptr,len)) cycle
       end if

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

       if(ou==stdout) then
          call mess("name: "//cpstr(ptr,len))
       else
          call messp(cpstr(ptr,len)//"=",ou)
       end if

       if(get_str_ptr(rpnm%pnames,1,ptr,len)==0) then
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

       tmprpnc%que    => rpnm%que
       tmprpnc%vbuf   => rpnm%vbuf
       tmprpnc%p_vbuf => rpnm%p_vbuf
       tmprpnc%pars   => rpnm%pars
       tmprpnc%answer => rpnm%answer
       tmprpnc%tmpans => rpnm%tmpans
       tmprpnc%rl     => rpnc%rl
       tmprpnc%rc     => rpnc%rc
       tmprpnc%pfs    => rpnc%pfs
       call mess("number of arguments = "//trim(itoa(rpnm%na)))
       call dump_rpnc(tmprpnc,i)
       call dump_slist(rpnm%pnames)

    end do
  end subroutine dump_rpnm
  
  subroutine dump_rpnc(rpnc,mid)
    use rpnlist, only: t_rpnm,kth_rpnm
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
!       return<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    end if
    call mess("#\tTID\tCID\tValue")
    if(present(mid)) rpnm => kth_rpnm(rpnc%rl,mid)
    do i=1,size(rpnc%que)
       q => rpnc%que(i)
       t=get_lo32(q%tid)
       call messp(trim(itoa(i))//":\t"//trim(itoa(t))//"\t")
       select case(t)
       case(TID_VAR,TID_PAR,TID_CPAR,TID_FIG,TID_ROVAR,TID_LVAR_T,TID_LVAR_F)
          call messp(trim(itoa(q%cid,DISP_FMT_HEX))//"\t")
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
                call messp("(copied)")
                pv=q%cid
                z=v
             end if
             pv=q%cid
             z=v
             if(pv==0) then
                call mess("(undef)")
                cycle
             end if
          end if
          call mess(trim(ztoa(z,fmt=DISP_FMT_RAW)))
       case(TID_DPAR)
          call mess(trim(itoa(q%cid))//"\t(dummy par)")
       case default
          call mess(trim(itoa(q%cid,DISP_FMT_HEX)))
       end select
    end do
    call mess("vbuf dump:")
    if(.not.associated(rpnc%vbuf)) then
       call mess("(empty)")
    else
       call mess("size used/alloc= "//trim(itoa(rpnc%p_vbuf))//"/"//trim(itoa(size(rpnc%vbuf))))
       if(rpnc%p_vbuf>0) then
          do i=1,rpnc%p_vbuf
             call mess(trim(itoa(i))//":\t"//trim(itoa(loc(rpnc%vbuf(i)),DISP_FMT_HEX))&
                  //"\t"//trim(ztoa(rpnc%vbuf(i),fmt=DISP_FMT_RAW)))
          end do
       end if
    end if
    if(.not.present(mid)) call mess("")
  end subroutine dump_rpnc

  subroutine dump_rpnb(rpnb)
    use misc, only:  mess
    type(t_rpnb),intent(in),target::rpnb
    type(t_rrpnq),pointer::q(:)
    call mess("rpnb que:\n#\tTID\tp1\tp2\tExpr.")
    if(.not.allocated(rpnb%que).or.rpnb%p_que<1)  then
       call mess("(empty)")
    else
       q => rpnb%que
       call dump_q(rpnb%p_que)
    end if
    call mess("rpnb buf:\n#\tTID\tp1\tp2\tExpr.")
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
      integer i,p1lo,p2lo,p1up,p2up,ltid,utid,tid
      do i=1,n
         p1lo=get_lo32(q(i)%p1)
         p2lo=get_lo32(q(i)%p2)
         p1up=get_up32(q(i)%p1)
         p2up=get_up32(q(i)%p2)
         tid=q(i)%tid
         if(tid>0) then
            ltid=get_lo32(tid)
            utid=get_up32(tid)
         end if
         call messp(trim(itoa(i))//":\t")
         if(tid<=0) then
            call messp(trim(itoa(tid))//"\t")
         else
            call messp(trim(itoa(utid))//":"//trim(itoa(ltid))//"\t")
         end if
         call messp(trim(itoa(p1up))//":"//trim(itoa(p1lo))//"\t" &
              //trim(itoa(p2up))//":"//trim(itoa(p1lo))//"\t")
         if(q(i)%tid==TID_VAR) then
            call mess(rpnb%expr(p1lo:p2lo))
         else if(q(i)%tid==TID_AMAC) then
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
