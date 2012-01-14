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
  use fpio
  use slist
  implicit none
  
  private

  integer,parameter,public::PLERR_NOMEM = 1 + SLERR_END
  integer,parameter,public::PLERR_MEMOV = 2 + SLERR_END
  integer,parameter,public::PLERR_NOENT = 3 + SLERR_END
  integer,parameter,public::PLERR_RDONL = 4 + SLERR_END
  integer,parameter,public::PLERR_NOPAR = 5 + SLERR_END
  integer,parameter,public::PLERR_END   = 5 + SLERR_END
  
  integer,parameter,public::PK_UNDEF = 0
  integer,parameter,public::PK_COMP  = 1
  integer,parameter,public::PK_REAL  = 2
  integer,parameter,public::PK_DBLE  = 3
  integer,parameter,public::PK_INT   = 4

  integer,parameter::PS_NOP   = 0
  integer,parameter::PS_REF   = Z"0001"
  integer,parameter::PS_RO    = Z"0004"
  integer,parameter::PS_DUP   = Z"0008"

  type t_vbuf
     integer p     ! pointer
     integer sta   ! parameter kind and flg
     integer pz    ! pointer to complex
  end type t_vbuf

  type,public::t_plist
     type(t_slist) s
     type(t_vbuf),allocatable::v(:)
  end type t_plist
  
  interface rm_par
     module procedure rm_par_s
     module procedure rm_par_k
  end interface
  public rm_par
  public rm_par_all

  interface put_par
     module procedure put_par_x
     module procedure put_par_z
     module procedure put_par_n
  end interface

  interface add_par_by_value
     module procedure add_par_by_value_x
     module procedure add_par_by_value_z
     module procedure add_par_by_value_n
  end interface
  public add_par_by_value
  public add_par_by_value_r

  public add_par_by_reference
  public add_par_by_entry
  public find_par
  public init_plist
  public dump_plist
  public uinit_plist
  public get_par_loc
  public get_par
  public remove_dup
  public min_cp_plist
  public sort_par

contains

  integer function get_pkind(sta)
    integer,intent(in)::sta
    get_pkind=iand(sta,Z"FFFF")
  end function get_pkind

  integer function get_pflg(sta)
    integer,intent(in)::sta
    get_pflg=iand(ishft(sta,-16),Z"FFFF")
  end function get_pflg

  subroutine set_pflg(sta,flg)
    integer,intent(inout)::sta
    integer,intent(in)::flg
    sta=ior(sta,ishft(flg,16))
  end subroutine set_pflg

  subroutine uset_pflg(sta,flg)
    integer,intent(inout)::sta
    integer,intent(in)::flg
    sta=iand(sta,not(ishft(flg,16)))
  end subroutine uset_pflg

  subroutine set_pkind(sta,pk)
    integer,intent(inout)::sta
    integer,intent(in)::pk
    sta=ior(iand(sta,Z"FFFF0000"),pk)
  end subroutine set_pkind

  logical function is_reference(sta)
    integer,intent(in)::sta
    is_reference=(iand(get_pflg(sta),PS_REF)/=0)
  end function is_reference

  logical function is_value(sta)
    integer,intent(in)::sta
    is_value=(iand(get_pflg(sta),PS_REF)==0)
  end function is_value

  logical function is_duplicated(sta)
    integer,intent(in)::sta
    is_duplicated=(iand(get_pflg(sta),PS_DUP)/=0)
  end function is_duplicated

  logical function is_single(sta)
    integer,intent(in)::sta
    is_single=(iand(get_pflg(sta),PS_DUP)==0)
  end function is_single

  logical function is_read_only(sta)
    integer,intent(in)::sta
    is_read_only=(iand(get_pflg(sta),PS_RO)/=0)
  end function is_read_only

  logical function is_writable(sta)
    integer,intent(in)::sta
    is_writable=(iand(get_pflg(sta),PS_RO)==0)
  end function is_writable

  function init_plist(sz,nmax)
    type(t_plist) init_plist
    integer,intent(in)::sz
    integer,intent(in)::nmax
    init_plist%s=init_slist(sz)
    if(nmax>0) call alloc_vbuf(nmax,init_plist%v)
  end function init_plist

  subroutine uinit_vbuf(v)
    type(t_vbuf),intent(inout)::v
    if(get_pkind(v%sta)/=PK_UNDEF) then
       if(is_value(v%sta)) call free(v%p)
       v%sta=PK_UNDEF
       v%p=0
       v%pz=0
    end if
  end subroutine uinit_vbuf

  subroutine uinit_vbufs(n,v)
    integer,intent(in)::n
    type(t_vbuf),intent(inout)::v(:)
    integer i
    do i=1,n
       call uinit_vbuf(v(i))
    end do
  end subroutine uinit_vbufs

  subroutine uinit_plist(pl)
    type(t_plist),intent(inout)::pl
    call uinit_slist(pl%s)
    if(.not.allocated(pl%v)) return
    call uinit_vbufs(size(pl%v),pl%v)
    deallocate(pl%v)
  end subroutine uinit_plist

  subroutine mv_vbufs(n,v1,v2)
    integer,intent(in)::n
    type(t_vbuf),intent(inout)::v1(:),v2(:)
    call cp_vbufs(n,v1,v2)
    call uinit_vbufs(n,v1)
  end subroutine mv_vbufs

  subroutine mv_vbuf(v1,v2)
    type(t_vbuf),intent(inout)::v1,v2
    call cp_vbuf(v1,v2)
    call uinit_vbuf(v1)
  end subroutine mv_vbuf

  subroutine cp_vbufs(n,v1,v2)
    integer,intent(in)::n
    type(t_vbuf),intent(in)::v1(:)
    type(t_vbuf),intent(inout)::v2(:)
    integer i
    do i=1,n
       call cp_vbuf(v1(i),v2(i))
    end do
  end subroutine cp_vbufs

  subroutine cp_vbuf(v1,v2)
    use memio
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
    use memio
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
    integer i,istat
    do i=pl%s%n,1,-1
       istat=rm_par_k(pl,i)
    end do
  end subroutine rm_par_all

  integer function rm_par_s(pl,s)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    integer k
    k=find_str(pl%s,s)
    if(k==0) then
       rm_par_s=PLERR_NOENT
       return
    end if
    rm_par_s=rm_par_k(pl,k)
  end function rm_par_s

  integer function rm_par_k(pl,k)
    type(t_plist),intent(inout)::pl
    integer,intent(in)::k
    integer i,kk
    if(is_read_only(pl%v(k)%sta)) then
       rm_par_k=PLERR_RDONL
       return
    end if
    kk=k ! <<<
    rm_par_k=rm_str(pl%s,ent=kk)
    if(rm_par_k/=0) return
    call uinit_vbuf(pl%v(k))
    do i=k,pl%s%n
       call mv_vbuf(pl%v(i+1),pl%v(i))
    end do
    rm_par_k=0
  end function rm_par_k

  subroutine min_cp_plist(pl1,pl2)
    type(t_plist),intent(in),target::pl1
    type(t_plist),intent(inout),target::pl2
    type(t_vbuf),pointer::v1,v2
    integer i
    call uinit_plist(pl2)
    call min_cp_slist(pl1%s,pl2%s)
    if(pl2%s%n>0) then
       call alloc_vbuf(pl2%s%n,pl2%v)
       do i=1,pl2%s%n
          v1 => pl1%v(i)
          v2 => pl2%v(i)
          v2=v1
          if(is_value(v1%sta)) &
               call set_pflg(v2%sta,PS_REF)
       end do
    end if
  end subroutine min_cp_plist
  
  subroutine dump_plist(pl,ent,name)
    type(t_plist),intent(in),target::pl
    integer,intent(in),optional::ent
    character*(*),intent(in),optional::name
    integer i,len,istat,ptr
    type(t_vbuf),pointer::v
    real(dp) r
    real(rp) x
    complex(cp) z
    integer n
    pointer(pr,r)
    pointer(pz,z)
    pointer(px,x)
    pointer(pn,n)
    do i=1,pl%s%n
       if(present(ent)) then
          if(ent>0.and.ent/=i) cycle
       end if
       v => pl%v(i)
       istat=get_str_ptr(pl%s,i,ptr,len)
       if(present(name)) then
          if(name/=trim(cpstr(ptr,len))) cycle
       end if
       write(*,10) trim(itoa(i))//": ["//trim(itoa(v%sta,DISP_FMT_HEX))//"] "//trim(cpstr(ptr,len))
       if(is_reference(v%sta)) write(*,10) "("//trim(itoa(v%p,DISP_FMT_HEX))//")"
       select case(get_pkind(v%sta))
       case(PK_COMP)
          pz=v%p
          write(*,*) trim(ztoa(z,fmt=DISP_FMT_RAW))
       case(PK_REAL)
          px=v%p
          write(*,*) trim(rtoa(x,fmt=DISP_FMT_RAW))                
       case(PK_DBLE)
          pr=v%p
          write(*,*) trim(rtoa(real(r,kind=rp),fmt=DISP_FMT_RAW))
       case(PK_INT)
          pn=v%p
          write(*,*) trim(rtoa(real(n,kind=rp),fmt=DISP_FMT_RAW))
       end select
    end do
10  format(x,a,$)
  end subroutine dump_plist

  subroutine alloc_vbuf(n,v)
    integer,intent(in)::n
    type(t_vbuf),intent(out),allocatable::v(:) ! and not allocated
    allocate(v(n))
    v%p=0
    v%pz=0
    v%sta=PK_UNDEF
  end subroutine alloc_vbuf

  subroutine trim_plist(pl)
    use memio
    type(t_plist),intent(inout)::pl
    type(t_vbuf),allocatable::tmpv(:)
    if(allocated(pl%v)) then
       if(pl%s%n>0) then
          call alloc_vbuf(pl%s%n,tmpv)
          call mv_vbufs(pl%s%n,pl%v,tmpv)
          deallocate(pl%v)
          call alloc_vbuf(pl%s%n,pl%v)
          call mv_vbufs(pl%s%n,tmpv,pl%v)
          deallocate(tmpv)
       else
          deallocate(pl%v)
       end if
    end if
    call trim_slist(pl%s)
  end subroutine trim_plist
 
  integer function find_par(pl,s,zout,ent,code)
    type(t_plist),intent(in),target::pl
    character*(*),intent(in)::s
    complex(cp),intent(out),optional::zout
    integer,intent(out),optional::ent
    integer,intent(out),optional::code
    type(t_vbuf),pointer::v
    integer k
    complex(cp) z
    real(rp) x
    real(dp) r
    integer n
    pointer(pz,z)
    pointer(pr,r)
    pointer(px,x)
    pointer(pn,n)
    if(present(zout)) zout=czero
    if(present(ent)) ent=0
    k=find_str(pl%s,s,found_code=code)
    if(k==0) then
       find_par=PLERR_NOENT
       return
    end if
    v => pl%v(k)
    if(present(zout)) then
       find_par=PLERR_NOENT
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
          pn=v%p
          zout=complex(real(n,kind=rp),rzero)
       end select
    end if
    if(present(ent)) ent=k
    find_par=0
  end function find_par

  subroutine put_par_z(v,z)
    use memio
    type(t_vbuf),intent(inout)::v
    complex(cp),intent(in)::z
    call mcp(v%p,loc(z),sizeof(z))
  end subroutine put_par_z

  subroutine put_par_x(v,x)
    use  memio
    type(t_vbuf),intent(inout)::v
    real(rp),intent(in)::x
    call mcp(v%p,loc(x),sizeof(x))
  end subroutine put_par_x

  subroutine put_par_r(v,r)
    use memio
    type(t_vbuf),intent(inout)::v
    real(dp),intent(in)::r
    call mcp(v%p,loc(r),sizeof(r))
  end subroutine put_par_r

  subroutine put_par_n(v,n)
    use memio
    type(t_vbuf),intent(inout)::v
    integer,intent(in)::n
    call mcp(v%p,loc(n),sizeof(n))
  end subroutine put_par_n

  integer function put_par_at(pl,k,z) ! for existing entry
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::k
    complex(cp),intent(in)::z
    type(t_vbuf),pointer::v
    if(k>pl%s%n.or.k<=0) then
       put_par_at=PLERR_NOPAR
       return
    end if
    v => pl%v(k)
    if(is_read_only(v%sta)) then
       put_par_at=PLERR_RDONL
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
       put_par_at=PLERR_NOENT
       return
    end select
    put_par_at=0
  end function put_par_at

  subroutine inc_par_buf(pl,inc_n)
    type(t_plist),intent(inout)::pl
    integer,intent(in)::inc_n
    type(t_vbuf),allocatable::v(:)
    if(inc_n<=0) return
    if(allocated(pl%v)) then
       call alloc_vbuf(size(pl%v)+inc_n,v)
       call mv_vbufs(size(pl%v),pl%v,v)
       deallocate(pl%v)
       call alloc_vbuf(size(v),pl%v)
       call mv_vbufs(size(pl%v),v,pl%v)
       deallocate(v)
    else
       call alloc_vbuf(inc_n,pl%v)
    end if
  end subroutine inc_par_buf

  integer function try_add_par(pl,s,ent,force)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    integer,intent(out),optional::ent
    logical,intent(in),optional::force
    integer k
    integer istat
    logical f
    if(present(ent)) ent=0
    istat=try_add_str(pl%s,s,0,k)
    if(istat/=0) then
       try_add_par=istat
       return
    end if
    if(present(force)) then
       f=force
    else
       f=.false.
    end if
    if(k<size(pl%v)) then
       if(is_read_only(pl%v(k)%sta).and..not.f) then
          try_add_par=PLERR_RDONL
          return
       end if
    end if
    if(present(ent)) ent=k
    if(pl%s%n>size(pl%v).or..not.allocated(pl%v)) &
         call inc_par_buf(pl,max(pl%s%n-size(pl%v),8)) ! <<<
    try_add_par=0
  end function try_add_par

  integer function add_par_by_entry(pl,s,ent,ro,pk)
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    integer,intent(out)::ent
    logical,intent(in),optional::ro
    integer,intent(in),optional::pk
    type(t_vbuf),pointer::v
    integer istat,k,flg,pk_set
    ent=0
    istat=try_add_par(pl,s,k)
    if(istat/=0) then
       add_par_by_entry=istat
       return
    end if
    v => pl%v(k)
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    if(present(pk)) then
       pk_set=pk
    else
       pk_set=PK_COMP
    end if
    call alloc_par(v,pk_set)
    call set_pflg(v%sta,flg)
    ent=k
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
    integer istat,k,flg
    integer pk_set
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,k,force=.true.)
    if(istat/=0) then
       add_par_by_reference=istat
       return
    end if
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
    v => pl%v(k)
    if(get_pkind(v%sta)/=PK_UNDEF.and.is_value(v%sta)) call free(v%p)
    v%p=ptr
    call set_pkind(v%sta,pk_set)
    call set_pflg(v%sta,flg)
    if(present(ent)) ent=k
    add_par_by_reference=0
  end function add_par_by_reference

  integer function add_par_by_value_x(pl,s,x,ro,ent)
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    real(rp),intent(in)::x
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    integer istat,k,flg
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,k)
    if(istat/=0) then
       add_par_by_value_x=istat
       return
    end if
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    v => pl%v(k)
    call alloc_par(v,PK_REAL)
    call set_pflg(v%sta,flg)
    call put_par(v,x)
    if(present(ent)) ent=k
    add_par_by_value_x=0
  end function add_par_by_value_x

  integer function add_par_by_value_r(pl,s,r,ro,ent)
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    real(dp),intent(in)::r
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    integer istat,k,flg
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,k)
    if(istat/=0) then
       add_par_by_value_r=istat
       return
    end if
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    v => pl%v(k)
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
    integer istat,k,flg
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,k)
    if(istat/=0) then
       add_par_by_value_n=istat
       return
    end if
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    v => pl%v(k)
    call alloc_par(v,PK_INT)
    call set_pflg(v%sta,flg)
    call put_par(v,n)
    if(present(ent)) ent=k
    add_par_by_value_n=0
  end function add_par_by_value_n

  integer function add_par_by_value_z(pl,s,z,ro,ent)
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    complex(cp),intent(in)::z
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    integer istat,k,flg
    if(present(ent)) ent=0
    istat=try_add_par(pl,s,k)
    if(istat/=0) then
       add_par_by_value_z=istat
       return
    end if
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    v => pl%v(k)
    call alloc_par(v,PK_COMP)
    call set_pflg(v%sta,flg)
    call put_par(v,z)
    if(present(ent)) ent=k
    add_par_by_value_z=0
  end function add_par_by_value_z

  integer function get_par_loc(pl,k,dup)
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::k
    logical,intent(out),optional::dup
    type(t_vbuf),pointer::v
    integer pk
    real(rp) x
    real(dp) r
    complex(cp) z
    integer n
    pointer(px,x)
    pointer(pr,r)
    pointer(pz,z)
    pointer(pn,n)
    if(present(dup)) dup=.false.
    get_par_loc=0
    if(k>pl%s%n.or.k<=0) return
    v => pl%v(k)
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
          pn=v%p
          z=complex(real(n,kind=rp),rzero)
       end select
       if(present(dup)) dup=.true.
       get_par_loc=v%pz
       call set_pflg(v%sta,PS_DUP)
    end select
  end function get_par_loc

  integer function get_par(pl,k,zout)
    type(t_plist),intent(in),target::pl
    integer,intent(in)::k
    complex(cp),intent(out)::zout
    type(t_vbuf),pointer::v
    real(dp) r
    real(rp) x
    complex(cp) z
    integer n
    pointer(pr,r)
    pointer(pz,z)
    pointer(px,x)
    pointer(pn,n)
    if(k>pl%s%n.or.k<=0) then
       get_par=PLERR_NOPAR
       return
    end if
    v => pl%v(k)
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
       pn=v%p
       zout=complex(real(n,kind=rp),rzero)
    case(PK_UNDEF)
       get_par=PLERR_NOENT
       return
    end select
    get_par=0
  end function get_par

  subroutine remove_dup(pl)
    type(t_plist),intent(inout),target::pl
    type(t_vbuf),pointer::v
    integer i
    real(rp) x
    real(dp) r
    complex(cp) z
    integer n
    pointer(px,x)
    pointer(pr,r)
    pointer(pz,z)
    pointer(pn,n)
    ! move value from pz to p 
    do i=1,pl%s%n
       v => pl%v(i)
       if(.not.is_duplicated(v%sta)) cycle
       pz=v%pz
       select case(get_pkind(v%sta)) 
       case(PK_REAL)
          px=v%p
          x=realpart(z)
       case(PK_DBLE)
          pr=v%p
          r=real(realpart(z),kind=dp)
       case(PK_INT)
          pn=v%p
          n=int(realpart(z))
       case(PK_COMP)
       case(PK_UNDEF)
       end select
       call free(v%pz)
       v%pz=0
       call uset_pflg(v%sta,PS_DUP)
    end do
  end subroutine remove_dup

  subroutine sort_par(pl,pz_in)
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::pz_in
    type(t_vbuf),pointer::v
    real(rp) x
    complex(cp) z
    pointer(pz,z)
    pointer(px,x)
    integer i
    real x_in
    ! reallocate p with PK_REAL if z is real
    do i=1,pl%s%n
       v => pl%v(i)
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
    end do

  end subroutine sort_par

end module plist
