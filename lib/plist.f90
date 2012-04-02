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
module plist
  use fzcerr
  implicit none

  private

  interface add_par_by_value
     module procedure add_par_by_value_x
     module procedure add_par_by_value_z
     module procedure add_par_by_value_n
  end interface
  public add_par_by_value
  public add_par_by_value_r

  interface rm_par
     module procedure rm_par_s
     module procedure rm_par_k
  end interface
  public rm_par
  public rm_par_all

  public add_par_by_reference
  public add_par_by_entry
  public find_par
  public init_plist
  public dump_plist
  public uinit_plist
  public get_par_loc
  public get_par
  public remove_dup
  public cp_plist
  public sort_par
  public plist_count

  type,public::t_plist
     integer::n = 0
     type(t_pn),pointer::pn => null()
  end type t_plist

  integer,parameter,public::PK_UNDEF = 0
  integer,parameter,public::PK_COMP  = 1
  integer,parameter,public::PK_REAL  = 2
  integer,parameter,public::PK_DBLE  = 3
  integer,parameter,public::PK_INT   = 4

  interface put_par
     module procedure put_par_x
     module procedure put_par_z
     module procedure put_par_n
  end interface

  type t_vbuf
     integer::p  =0   ! pointer
     integer::sta=0   ! parameter kind and flg
     integer::pz =0   ! pointer to complex
  end type t_vbuf

  type t_pn
     type(t_vbuf),pointer::v => null()
     integer*1,allocatable::s(:)
     type(t_pn),pointer::next => null()
     type(t_pn),pointer::prev => null()
  end type t_pn

  integer,parameter::PS_NOP   = 0
  integer,parameter::PS_REF   = Z"0001"
  integer,parameter::PS_RO    = Z"0004"
  integer,parameter::PS_DUP   = Z"0008"

contains

  subroutine set_pn(pn,s)
    use memio, only: mcp
    type(t_pn),intent(inout),pointer::pn
    character*(*),intent(in)::s
    integer len
    len=len_trim(s)
    allocate(pn%s(len))
    call mcp(loc(pn%s),loc(s),len)
    allocate(pn%v)
    pn%v=init_vbuf()
  end subroutine set_pn

  function append_node(pl,s)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    type(t_pn),pointer::append_node
    type(t_pn),pointer::pn,prev
    if(.not.associated(pl%pn)) then
       pl%n=0
       allocate(pl%pn)
       prev => pl%pn
       pn => pl%pn
    else
       pn => pl%pn%prev
       allocate(pn%next)
       prev => pn
       pn => pn%next
    end if
    pn%prev => prev
    nullify(pn%next)
    pl%n=pl%n+1
    pl%pn%prev => pn
    call set_pn(pn,s)
    append_node => pn
  end function append_node

  function match_node(pl,s,k)
    type(t_plist),intent(in)::pl
    character*(*),intent(in)::s
    integer,intent(out)::k
    integer len
    type(t_pn),pointer::match_node
    type(t_pn),pointer::pn
    integer i
    nullify(match_node)
    k=0
    if(pl%n==0.or..not.associated(pl%pn)) return
    pn => pl%pn
    len=len_trim(s)
    do i=1,pl%n
       if(len==size(pn%s)) then
          if(is_matched()) then
             k=i
             match_node => pn
             return
          end if
       end if
       pn => pn%next
       if(.not.associated(pn)) return
    end do
  contains
    logical function is_matched()
      integer j
      is_matched=.false.
      do j=1,len
         if(pn%s(j)/=ichar(s(j:j))) return
      end do
      is_matched=.true.
    end function is_matched
  end function match_node

  function kth_node(pl,k)
    type(t_plist),intent(in)::pl
    integer,intent(in)::k
    type(t_pn),pointer::kth_node
    type(t_pn),pointer::pn
    integer i
    nullify(kth_node)
    if(k>pl%n.or.k<=0) return
    if(.not.associated(pl%pn)) return
    pn => pl%pn
    do i=2,k
       pn => pn%next
       if(.not.associated(pn)) return
    end do
    kth_node => pn
  end function kth_node

  pure integer function plist_count(pl)
    type(t_plist),intent(in)::pl
    plist_count=pl%n
  end function plist_count

  pure integer function get_pkind(sta)
    integer,intent(in)::sta
    get_pkind=iand(sta,Z"FFFF")
  end function get_pkind

  pure integer function get_pflg(sta)
    integer,intent(in)::sta
    get_pflg=iand(ishft(sta,-16),Z"FFFF")
  end function get_pflg

  pure subroutine set_pflg(sta,flg)
    integer,intent(inout)::sta
    integer,intent(in)::flg
    sta=ior(sta,ishft(flg,16))
  end subroutine set_pflg

  pure subroutine uset_pflg(sta,flg)
    integer,intent(inout)::sta
    integer,intent(in)::flg
    sta=iand(sta,not(ishft(flg,16)))
  end subroutine uset_pflg

  pure subroutine set_pkind(sta,pk)
    integer,intent(inout)::sta
    integer,intent(in)::pk
    sta=ior(iand(sta,Z"FFFF0000"),pk)
  end subroutine set_pkind

  pure logical function is_reference(sta)
    integer,intent(in)::sta
    is_reference=(iand(get_pflg(sta),PS_REF)/=0)
  end function is_reference

  pure logical function is_value(sta)
    integer,intent(in)::sta
    is_value=(iand(get_pflg(sta),PS_REF)==0)
  end function is_value

  pure logical function is_duplicated(sta)
    integer,intent(in)::sta
    is_duplicated=(iand(get_pflg(sta),PS_DUP)/=0)
  end function is_duplicated

  pure logical function is_single(sta)
    integer,intent(in)::sta
    is_single=(iand(get_pflg(sta),PS_DUP)==0)
  end function is_single

  pure logical function is_read_only(sta)
    integer,intent(in)::sta
    is_read_only=(iand(get_pflg(sta),PS_RO)/=0)
  end function is_read_only

  pure logical function is_writable(sta)
    integer,intent(in)::sta
    is_writable=(iand(get_pflg(sta),PS_RO)==0)
  end function is_writable

  function init_plist()
    type(t_plist) init_plist
    init_plist%n=0
  end function init_plist

  function init_vbuf()
    type(t_vbuf) init_vbuf
    init_vbuf%sta=PK_UNDEF
    init_vbuf%p=0
    init_vbuf%pz=0
  end function init_vbuf

  subroutine uinit_vbuf(v)
    type(t_vbuf),intent(inout)::v
    if(get_pkind(v%sta)/=PK_UNDEF) then
       if(is_value(v%sta).and.v%p/=0) call free(v%p)
       if(v%pz/=0) call free(v%pz)
    end if
  end subroutine uinit_vbuf

  subroutine uinit_pn(pn)
    type(t_pn),intent(inout)::pn
    if(allocated(pn%s)) deallocate(pn%s)
    if(associated(pn%v)) then
       call uinit_vbuf(pn%v)
       deallocate(pn%v)
    end if
  end subroutine uinit_pn

  subroutine uinit_plist(pl)
    type(t_plist),intent(inout)::pl
    type(t_pn),pointer::pn,next
    integer i
    if(pl%n==0) return
    if(.not.associated(pl%pn)) then
       pl%n=0
       return
    end if
    pn => pl%pn
    do i=1,pl%n
       call uinit_pn(pn)
       next => pn%next
       deallocate(pn)
       pn => next
       if(.not.associated(pn)) exit
    end do
    pl%n=0
    nullify(pl%pn)
  end subroutine uinit_plist

  subroutine cp_vbuf(v1,v2)
    use memio, only: mcp
    type(t_vbuf),intent(in)::v1
    type(t_vbuf),intent(inout)::v2
    integer sz
    call uinit_vbuf(v2)
    v2%sta=v1%sta
    if(is_reference(v1%sta)) then
       v2%p=v1%p
    else
       v2%p=palloc(get_pkind(v1%sta),sz)
       if(sz/=0) call mcp(v2%p,v1%p,sz)
    end if
  end subroutine cp_vbuf

  integer function palloc(pk,sz)
    use memio, only: mcle
    use fpio, only: dp,rp,cp
    integer,intent(in)::pk
    integer,intent(out),optional::sz
    integer sz_
    complex(cp) z
    real(rp) x
    real(dp) r
    integer n
    select case(pk)
    case(PK_COMP)
       sz_=sizeof(z)
    case(PK_REAL)
       sz_=sizeof(x)
    case(PK_DBLE)
       sz_=sizeof(r)
    case(PK_INT)
       sz_=sizeof(n)
    case default
       sz_=0
    end select
    if(sz_/=0) then
       palloc=malloc(sz_)
       call mcle(palloc,sz_)
    else
       palloc=0
    end if
    if(present(sz)) sz=sz_
  end function palloc

  subroutine change_pkind(v,pk) ! never called and not checked
    use fpio, only: dp,rp,cp,czero,rzero
    type(t_vbuf),intent(inout)::v
    integer,intent(in)::pk
    integer pk_now
    real(rp) x
    real(dp) r
    complex(cp) z
    integer n
    complex(cp) val
    pointer(px,x)
    pointer(pr,r)
    pointer(pz,z)
    pointer(pn,n)
    pk_now=get_pkind(v%sta)
    if(pk_now==pk) return
    select case(pk_now)
    case(PK_COMP)
       pz=v%p
       val=z
    case(PK_REAL)
       px=v%p
       val=complex(x,rzero)
    case(PK_DBLE)
       pr=v%p
       val=complex(real(r,kind=rp),rzero)
    case(PK_INT)
       pn=v%p
       val=complex(real(n,kind=rp),rzero)
    case(PK_UNDEF)
       val=czero
    end select
    call alloc_par(v,pk)
    select case(pk)
    case(PK_COMP)
       pz=v%p
       z=val
    case(PK_REAL)
       px=v%p
       x=realpart(val)
    case(PK_DBLE)
       pr=v%p
       x=real(realpart(val),kind=dp)
    case(PK_INT)
       pn=v%p
       n=int(realpart(val))
    case(PK_UNDEF)
    end select
  end subroutine change_pkind

  subroutine alloc_par(v,pk)
    type(t_vbuf),intent(inout)::v
    integer,intent(in)::pk
    integer pk_now
    pk_now=get_pkind(v%sta)
    if(pk_now==PK_UNDEF.or.is_reference(v%sta)) then
       v%p=palloc(pk)
    else if(pk/=pk_now) then
       call free(v%p)
       v%p=palloc(pk)
    end if
    call uset_pflg(v%sta,PS_REF)
    call set_pkind(v%sta,pk)
  end subroutine alloc_par

  subroutine rm_par_all(pl)
    type(t_plist),intent(inout)::pl
    type(t_pn),pointer::pn,next
    integer i,istat
    if(pl%n==0) return
    pn => pl%pn
    do i=1,pl%n
       next => pn%next
       istat=rm_par_pn(pl,pn)
       pn => next
       if(.not.associated(pn)) exit
    end do
  end subroutine rm_par_all

  integer function rm_par_s(pl,s)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    type(t_pn),pointer::pn
    integer k
    pn => match_node(pl,s,k)
    if(k==0) then
       rm_par_s=FZCERR_NOENT
       return
    end if
    rm_par_s=rm_par_pn(pl,pn)
  end function rm_par_s

  integer function rm_par_pn(pl,pn)
    type(t_plist),intent(inout)::pl
    type(t_pn),intent(inout),pointer::pn
    type(t_vbuf),pointer::v
    v => pn%v
    if(is_read_only(v%sta)) then
       rm_par_pn=FZCERR_RDONL
       return
    end if
    pn%prev%next => pn%next
    if(associated(pn%next)) then
       pn%next%prev => pn%prev
    else
       pl%pn%prev => pn%prev
    end if
    call uinit_pn(pn)
    deallocate(pn)
    pl%n=pl%n-1
    if(pl%n==0) nullify(pl%pn)
    rm_par_pn=0
  end function rm_par_pn

  integer function rm_par_k(pl,k)
    type(t_plist),intent(inout)::pl
    integer,intent(in)::k
    type(t_pn),pointer::pn
    pn => kth_node(pl,k)
    if(.not.associated(pn)) then
       rm_par_k=FZCERR_NOENT
       return
    end if
    rm_par_k=rm_par_pn(pl,pn)
  end function rm_par_k

  function cp_plist(pl)
    use memio, only: mcp
    type(t_plist),intent(in),target::pl
    type(t_plist) cp_plist
    type(t_pn),pointer::pn_in,pn,prev
    type(t_vbuf),pointer::v_in
    integer i
    cp_plist=init_plist()
    if(pl%n==0) return
    pn_in => pl%pn
    allocate(cp_plist%pn)
    cp_plist%n=1
    pn => cp_plist%pn
    prev => pn
    do i=1,pl%n
       v_in => pn_in%v
       if(allocated(pn_in%s)) then
          allocate(pn%s(size(pn_in%s)))
          call mcp(loc(pn%s),loc(pn_in%s),size(pn_in%s))
       end if
       if(associated(v_in)) then
          allocate(pn%v)
          call cp_vbuf(v_in,pn%v)
       end if
       pn%prev => prev
       nullify(pn%next)
       pn_in => pn_in%next
       if(.not.associated(pn_in)) exit
       allocate(pn%next)
       prev => pn
       pn => pn%next
       pn%prev => prev
       cp_plist%n=cp_plist%n+1
    end do
    cp_plist%pn%prev => pn
  end function cp_plist

  subroutine dump_plist(pl,ent,name,out_unit)
    use fpio, only: dp,rp,cp,ztoa,rtoa,DISP_FMT_RAW
    use misc, only: mess,messp,stdout
    use memio, only: cpstr,itoa,IBASE_HEX
    type(t_plist),intent(in),target::pl
    integer,intent(in),optional::ent
    character*(*),intent(in),optional::name
    integer,intent(in),optional::out_unit
    integer i,i1,i2,len,ptr
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    real(dp) r
    real(rp) x
    complex(cp) z
    integer m
    pointer(pr,r)
    pointer(pz,z)
    pointer(px,x)
    pointer(pm,m)
    integer ou
    if(present(out_unit)) then
       ou=out_unit
    else
       ou=stdout
    end if
    i1=1
    i2=pl%n
    if(present(ent)) then
       if(ent>0) then
          i1=ent
          i2=ent
       end if
    end if
    if(ou==stdout) call mess("#\tStatus:(Addr)\t\tName\tValue")
    pn => kth_node(pl,i1)
    do i=i1,i2
       if(.not.associated(pn)) exit
       v => pn%v
       ptr=loc(pn%s)
       len=size(pn%s)
       if(present(name)) then
          if(name/="".and.name/=cpstr(ptr,len)) then
             call next
             cycle
          end if
       end if
       if(ou/=stdout) then
          call messp(cpstr(ptr,len)//"=",ou)
       else
          call messp(trim(itoa(i))//":\t["//trim(itoa(v%sta,fmt="(Z6.6)"))//"]")
          if(is_reference(v%sta)) then
             call messp(":("//trim(itoa(v%p,IBASE_HEX))//")\t",ou)
          else
             call messp("\t\t")
          end if
          call messp(cpstr(ptr,len)//"\t",ou)
       end if
       select case(get_pkind(v%sta))
       case(PK_COMP)
          pz=v%p
          if(ou==stdout) then
             call mess(trim(ztoa(z,fmt=DISP_FMT_RAW)),ou)
          else
             call mess(trim(rtoa(realpart(z),fmt=DISP_FMT_RAW)) &
                  //"+"//trim(rtoa(imagpart(z),fmt=DISP_FMT_RAW))//" i",ou)
          end if
       case(PK_REAL)
          px=v%p
          call mess(trim(rtoa(x,fmt=DISP_FMT_RAW)),ou)
       case(PK_DBLE)
          pr=v%p
          call mess(trim(rtoa(real(r,kind=rp),fmt=DISP_FMT_RAW)),ou)
       case(PK_INT)
          pm=v%p
          call mess(trim(rtoa(real(m,kind=rp),fmt=DISP_FMT_RAW)),ou)
       end select
       call next
    end do

  contains

    subroutine next()
      pn => pn%next
    end subroutine next

  end subroutine dump_plist

  integer function find_par(pl,s,zout,ent)
    use fpio, only: dp,rp,cp,czero,rzero
    type(t_plist),intent(in),target::pl
    character*(*),intent(in)::s
    complex(cp),intent(out),optional::zout
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer k
    complex(cp) z
    real(rp) x
    real(dp) r
    integer m
    pointer(pz,z)
    pointer(pr,r)
    pointer(px,x)
    pointer(pm,m)
    if(present(zout)) zout=czero
    if(present(ent)) ent=0
    pn => match_node(pl,s,k)
    if(k==0) then
       find_par=FZCERR_NOENT
       return
    end if
    v => pn%v
    if(present(zout)) then
       find_par=FZCERR_NOENT
       select case(get_pkind(v%sta))
       case(PK_COMP)
          pz=v%p
          zout=z
       case(PK_REAL)
          px=v%p
          zout=complex(x,rzero)
       case(PK_DBLE)
          pr=v%p
          zout=complex(real(r,kind=rp),rzero)
       case(PK_INT)
          pm=v%p
          zout=complex(real(m,kind=rp),rzero)
       end select
    end if
    if(present(ent)) ent=k
    find_par=0
  end function find_par

  subroutine put_par_z(v,z)
    use memio, only: mcp
    use fpio, only: cp
    type(t_vbuf),intent(inout)::v
    complex(cp),intent(in)::z
    call mcp(v%p,loc(z),sizeof(z))
  end subroutine put_par_z

  subroutine put_par_x(v,x)
    use  memio, only: mcp
    use fpio, only: rp
    type(t_vbuf),intent(inout)::v
    real(rp),intent(in)::x
    call mcp(v%p,loc(x),sizeof(x))
  end subroutine put_par_x

  subroutine put_par_r(v,r)
    use memio, only: mcp
    use fpio, only: dp
    type(t_vbuf),intent(inout)::v
    real(dp),intent(in)::r
    call mcp(v%p,loc(r),sizeof(r))
  end subroutine put_par_r

  subroutine put_par_n(v,n)
    use memio, only: mcp
    type(t_vbuf),intent(inout)::v
    integer,intent(in)::n
    call mcp(v%p,loc(n),sizeof(n))
  end subroutine put_par_n

  integer function put_par_at(pl,k,z) ! for existing entry
    use fpio, only: dp,cp
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::k
    complex(cp),intent(in)::z
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    if(k>pl%n.or.k<=0) then
       put_par_at=FZCERR_NOPAR
       return
    end if
    pn => kth_node(pl,k)
    v => pn%v
    if(is_read_only(v%sta)) then
       put_par_at=FZCERR_RDONL
       return
    end if
    select case(get_pkind(v%sta))
    case(PK_COMP)
       call put_par(v,z)
    case(PK_REAL)
       call put_par(v,realpart(z))
    case(PK_DBLE)
       call put_par_r(v,real(realpart(z),kind=dp))
    case(PK_INT)
       call put_par(v,int(realpart(z)))
    case(PK_UNDEF)
       put_par_at=FZCERR_NOENT
       return
    end select
    put_par_at=0
  end function put_par_at

  integer function try_add_par(pl,s,node,ent,force,new,init)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    type(t_pn),intent(out),pointer,optional::node
    integer,intent(out),optional::ent
    logical,intent(in),optional::force
    logical,intent(out),optional::new
    logical,intent(in),optional::init
    type(t_pn),pointer::pn
    integer k
    logical f
    if(present(node)) nullify(node)
    if(present(ent)) ent=0
    if(present(new)) new=.false.
    pn => match_node(pl,s,k)
    if(.not.associated(pn)) then
       pn => append_node(pl,s)
       k=pl%n
       if(present(new)) new=.true.
    else
       if(present(force)) then
          f=force
       else
          f=.false.
       end if
       if(is_read_only(pn%v%sta).and..not.f) then
          try_add_par=FZCERR_RDONL
          return
       end if
       if(present(init)) then
          if(init) then
             call uinit_vbuf(pn%v)
             pn%v=init_vbuf()
          end if
       end if
    end if
    if(present(node)) node => pn
    if(present(ent)) ent=k
    try_add_par=0
  end function try_add_par

  integer function add_par_by_entry(pl,s,ent,ro,pk)
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    integer,intent(out)::ent
    logical,intent(in),optional::ro
    integer,intent(in),optional::pk
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer istat,k,flg,pk_set
    logical new
    ent=0
    istat=try_add_par(pl,s,pn,k,new=new,init=.false.)
    if(istat/=0) then
       add_par_by_entry=istat
       return
    end if
    ent=k
    v => pn%v
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    if(new) then
       if(present(pk)) then
          pk_set=pk
       else
          pk_set=PK_COMP
       end if
       call alloc_par(v,pk_set)
    else if(present(pk)) then
       call change_pkind(v,pk) ! <<<<<<<<<<<<<<<<<<<
    end if
    call set_pflg(v%sta,flg)
    add_par_by_entry=0
  end function add_par_by_entry

  integer function add_par_by_reference(pl,s,ptr,ro,pk,ent)
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    integer,intent(in)::ptr
    logical,intent(in),optional::ro
    integer,intent(in),optional::pk
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer istat,k,flg
    integer pk_set
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,pn,k,force=.true.,init=.true.)
    if(istat/=0) then
       add_par_by_reference=istat
       return
    end if
    if(present(ent)) ent=k
    if(present(ro).and.ro) then
       flg=ior(PS_REF,PS_RO)
    else
       flg=PS_REF
    end if
    if(present(pk)) then
       pk_set=pk
    else
       pk_set=PK_COMP
    end if
    v => pn%v
    if(get_pkind(v%sta)/=PK_UNDEF.and.is_value(v%sta)) call free(v%p)
    v%p=ptr
    call set_pkind(v%sta,pk_set)
    call set_pflg(v%sta,flg)
    add_par_by_reference=0
  end function add_par_by_reference

  integer function add_par_by_value_x(pl,s,x,ro,ent)
    use fpio, only: rp
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    real(rp),intent(in)::x
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer istat,k,flg
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,pn,k,init=.true.)
    if(istat/=0) then
       add_par_by_value_x=istat
       return
    end if
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    v => pn%v
    call alloc_par(v,PK_REAL)
    call set_pflg(v%sta,flg)
    call put_par(v,x)
    if(present(ent)) ent=k
    add_par_by_value_x=0
  end function add_par_by_value_x

  integer function add_par_by_value_r(pl,s,r,ro,ent)
    use fpio, only: dp
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    real(dp),intent(in)::r
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer istat,k,flg
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,pn,k,init=.true.)
    if(istat/=0) then
       add_par_by_value_r=istat
       return
    end if
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    v => pn%v
    call alloc_par(v,PK_DBLE)
    call set_pflg(v%sta,flg)
    call put_par_r(v,r)
    if(present(ent)) ent=k
    add_par_by_value_r=0
  end function add_par_by_value_r

  integer function add_par_by_value_n(pl,s,n,ro,ent)
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    integer,intent(in)::n
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer istat,k,flg
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,pn,k,init=.true.)
    if(istat/=0) then
       add_par_by_value_n=istat
       return
    end if
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    v => pn%v
    call alloc_par(v,PK_INT)
    call set_pflg(v%sta,flg)
    call put_par(v,n)
    if(present(ent)) ent=k
    add_par_by_value_n=0
  end function add_par_by_value_n

  integer function add_par_by_value_z(pl,s,z,ro,ent)
    use fpio, only: cp
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    complex(cp),intent(in)::z
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer istat,k,flg
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,pn,k,init=.true.)
    if(istat/=0) then
       add_par_by_value_z=istat
       return
    end if
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    v => pn%v
    call alloc_par(v,PK_COMP)
    call set_pflg(v%sta,flg)
    call put_par(v,z)
    if(present(ent)) ent=k
    add_par_by_value_z=0
  end function add_par_by_value_z

  integer function get_par_loc(pl,k,dup)
    use fpio, only: dp,rp,cp,rzero
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::k
    logical,intent(out),optional::dup
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer pk
    real(rp) x
    real(dp) r
    complex(cp) z
    integer m
    pointer(px,x)
    pointer(pr,r)
    pointer(pz,z)
    pointer(pm,m)
    if(present(dup)) dup=.false.
    get_par_loc=0
    if(k>pl%n.or.k<=0) return
    pn => kth_node(pl,k)
    v => pn%v
    if(is_duplicated(v%sta)) then
       get_par_loc=v%pz ! must not be zero
       if(present(dup)) dup=.true.
       return
    end if
    pk=get_pkind(v%sta)
    select case(pk)
    case(PK_COMP)
       get_par_loc=v%p
       return
    case(PK_UNDEF)
       return
    case default
       if(v%pz==0) v%pz=palloc(PK_COMP)
       pz=v%pz
       select case(pk)
       case(PK_REAL)
          px=v%p
          z=complex(x,rzero)
       case(PK_DBLE)
          pr=v%p
          z=complex(real(r,kind=rp),rzero)
       case(PK_INT)
          pm=v%p
          z=complex(real(m,kind=rp),rzero)
       end select
       if(present(dup)) dup=.true.
       get_par_loc=v%pz
       call set_pflg(v%sta,PS_DUP)
    end select
  end function get_par_loc

  integer function get_par(pl,k,zout)
    use fpio, only: dp,rp,cp,rzero
    type(t_plist),intent(in),target::pl
    integer,intent(in)::k
    complex(cp),intent(out)::zout
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    real(dp) r
    real(rp) x
    complex(cp) z
    integer m
    pointer(pr,r)
    pointer(pz,z)
    pointer(px,x)
    pointer(pm,m)
    if(k>pl%n.or.k<=0) then
       get_par=FZCERR_NOPAR
       return
    end if
    pn => kth_node(pl,k)
    v => pn%v
    select case(get_pkind(v%sta))
    case(PK_COMP)
       pz=v%p
       zout=z
    case(PK_REAL)
       px=v%p
       zout=complex(x,rzero)
    case(PK_DBLE)
       pr=v%p
       zout=complex(real(r,kind=rp),rzero)
    case(PK_INT)
       pm=v%p
       zout=complex(real(m,kind=rp),rzero)
    case(PK_UNDEF)
       get_par=FZCERR_NOENT
       return
    end select
    get_par=0
  end function get_par

  subroutine remove_dup(pl)
    use fpio, only: dp,rp,cp
    type(t_plist),intent(inout),target::pl
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    integer i
    real(rp) x
    real(dp) r
    complex(cp) z
    integer m
    pointer(px,x)
    pointer(pr,r)
    pointer(pz,z)
    pointer(pm,m)
    if(pl%n==0) return
    pn => pl%pn
    ! move value from pz to p
    do i=1,pl%n
       v => pn%v
       if(is_duplicated(v%sta)) then
          pz=v%pz
          select case(get_pkind(v%sta))
          case(PK_REAL)
             px=v%p
             x=realpart(z)
          case(PK_DBLE)
             pr=v%p
             r=real(realpart(z),kind=dp)
          case(PK_INT)
             pm=v%p
             m=int(realpart(z))
          case(PK_COMP)
          case(PK_UNDEF)
          end select
          call free(v%pz)
          v%pz=0
          call uset_pflg(v%sta,PS_DUP)
       end if
       pn => pn%next
       if(.not.associated(pn)) exit
    end do
  end subroutine remove_dup

  subroutine sort_par(pl,pz_in)
    use fpio, only: rp,cp,rzero
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::pz_in
    type(t_vbuf),pointer::v
    type(t_pn),pointer::pn
    real(rp) x
    complex(cp) z
    pointer(pz,z)
    pointer(px,x)
    integer i
    real(rp) x_in
    if(pl%n==0) return
    pn => pl%pn
    ! reallocate p with PK_REAL if z is real
    do i=1,pl%n
       v => pn%v
       if(v%p==pz_in) then
          pz=pz_in
          if(imagpart(z)/=rzero) return
          if(get_pkind(v%sta)/=PK_COMP) return
          x_in=realpart(z)
          call free(v%p)
          v%p=palloc(PK_REAL)
          px=v%p
          x=x_in
          call set_pkind(v%sta,PK_REAL)
       end if
       pn => pn%next
       if(.not.associated(pn)) exit
    end do

  end subroutine sort_par

end module plist
