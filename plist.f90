module plist
  use fpio
  use slist
  implicit none
  
  private

  integer,parameter::PLERR_NOMEM = 1
  integer,parameter::PLERR_MEMOV = 2
  integer,parameter::PLERR_NOENT = 3
  integer,parameter::PLERR_RDONL = 4
  integer,parameter::PLERR_NOPAR = 5
  
  integer,parameter,public::PK_UNDEF = 0
  integer,parameter,public::PK_COMP  = 1
  integer,parameter,public::PK_REAL  = 2
  integer,parameter,public::PK_DBLE  = 3
  integer,parameter,public::PK_INT   = 4

  integer,parameter,public::PS_NOP   = 0
  integer,parameter,public::PS_REF   = Z"0001"
  integer,parameter,public::PS_NEW   = Z"0002"
  integer,parameter,public::PS_RO    = Z"0004"
  integer,parameter,public::PS_DUP   = Z"0008"

  type t_vbuf
     integer p     ! pointer
     integer sta   ! parameter kind and flg
     integer pz    ! pointer to complex
  end type t_vbuf

  type,public::t_plist
     type(t_slist) s
     type(t_vbuf),allocatable::v(:)
  end type t_plist
  
  public add_par_by_value_r
  interface add_par_by_value
     module procedure add_par_by_value_x
     module procedure add_par_by_value_z
  end interface add_par_by_value

  public add_par_by_value
  public find_par
  public add_par_by_reference
  public add_par_by_entry
  public rm_par
  public init_plist
  public dump_plist
  public uinit_plist
  public get_par_loc
  public get_par
  public alloc_par
  public remove_dup
  public min_cp_plist

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

  logical function is_duplicated(sta)
    integer,intent(in)::sta
    is_duplicated=(iand(get_pflg(sta),PS_DUP)/=0)
  end function is_duplicated

  logical function is_read_only(sta)
    integer,intent(in)::sta
    is_read_only=(iand(get_pflg(sta),PS_RO)/=0)
  end function is_read_only

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
       if(.not.is_reference(v%sta)) call free(v%p)
       v%sta=PK_UNDEF
       v%p=0
       v%pz=0
    end if
  end subroutine uinit_vbuf

  subroutine uinit_vbufs(n,v)
    integer,intent(in)::n
    type(t_vbuf),intent(inout)::v(n)
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
    type(t_vbuf),intent(inout)::v1(n),v2(n)
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
    type(t_vbuf),intent(in)::v1(n)
    type(t_vbuf),intent(inout)::v2(n)
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
    end if
    if(present(sz)) sz=sz_
  end function palloc

  integer function alloc_par(pl,k,pk)
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::k
    integer,intent(in)::pk
    type(t_vbuf),pointer::v
    integer pk_now
    if(k>size(pl%v).or.k<=0) then
       alloc_par=PLERR_NOENT
       return
    end if
    v => pl%v(k)
    pk_now=get_pkind(v%sta)
    if(pk_now==PK_UNDEF) then
       v%p=palloc(pk)
       call set_pkind(v%sta,pk)
    end if
    alloc_par=0
  end function alloc_par

  integer function rm_par(pl,s)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    integer istat
    integer k
    integer i
    istat=rm_str(pl%s,s,ent=k)
    if(istat/=0) then
       rm_par=istat
       return
    end if
    call uinit_vbuf(pl%v(k))
    do i=k,pl%s%n
       call mv_vbuf(pl%v(i+1),pl%v(i))
    end do
    rm_par=0
  end function rm_par

  subroutine min_cp_plist(pl1,pl2)
    type(t_plist),intent(in)::pl1
    type(t_plist),intent(inout)::pl2
    call uinit_plist(pl2)
    call min_cp_slist(pl1%s,pl2%s)
    if(allocated(pl2%v)) then
       call uinit_vbufs(size(pl2%v),pl2%v)
       deallocate(pl2%v)
    end if
    if(pl2%s%n>0) then
       call alloc_vbuf(pl2%s%n,pl2%v)
    end if
  end subroutine min_cp_plist
  
  subroutine dump_plist(pl)
    type(t_plist),intent(in),target::pl
    integer i,len,istat,ptr
    type(t_vbuf),pointer::v
    integer c
    real(dp) r
    real(rp) x
    complex(cp) z
    integer n
    pointer(pr,r)
    pointer(pz,z)
    pointer(px,x)
    pointer(pn,n)
    do i=1,pl%s%n
       v => pl%v(i)
       istat=get_str_ptr(pl%s,i,ptr,len,code=c)
       write(*,10) i,c,trim(cpstr(ptr,len))
       if(is_reference(v%sta)) write(*,20) v%p
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
10  format(x,i4,x,b8.8,x,a,$)
20  format(x,z16,$)
  end subroutine dump_plist

  subroutine alloc_vbuf(n,v)
    integer,intent(in)::n
    type(t_vbuf),intent(out),allocatable::v(:)
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
       if(pl%s%n>=1) then
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
 
  integer function find_par(pl,s,val,ent,code)
    type(t_plist),intent(in),target::pl
    character*(*),intent(in)::s
    complex(cp),intent(out),optional::val
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
    if(present(ent)) ent=0
    k=find_str(pl%s,s,found_code=code)
    if(k==0) then
       find_par=PLERR_NOENT
       return
    end if
    v => pl%v(k)
    if(present(val)) then
       find_par=PLERR_NOENT
       select case(get_pkind(v%sta))
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
          return
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

  integer function put_par(pl,k,z)
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::k
    complex(cp),intent(in)::z
    type(t_vbuf),pointer::v
    integer istat
    integer c
    integer p,l
    istat=get_sc(pl%s,k,c)
    if(istat/=0) then
       put_par=istat
       return
    end if
    v => pl%v(k)
    if(is_read_only(v%sta)) then
       istat=get_str_ptr(pl%s,k,p,l)
       write(*,*) "*** Parameter is read-only: "//trim(cpstr(p,l))
       put_par=PLERR_RDONL
       return
    end if
    select case(get_pkind(v%sta))
    case(PK_COMP)
       call put_par_z(v,z)
    case(PK_REAL)
       call put_par_x(v,realpart(z))
    case(PK_DBLE)
       call put_par_r(v,real(realpart(z),kind=dp))
    case(PK_INT)
       call put_par_n(v,int(realpart(z)))
    case(PK_UNDEF)
       put_par=PLERR_NOENT
       return
    end select
    put_par=0
  end function put_par

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

  integer function try_add_par(pl,s,code,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    integer,intent(in)::code
    integer,intent(out),optional::ent
    integer istat
    istat=try_add_str(pl%s,s,code,ent)
    if(istat/=0) then
       try_add_par=istat
       return
    end if
    if(pl%s%n>size(pl%v).or..not.allocated(pl%v)) call inc_par_buf(pl,4) !<<<
    try_add_par=0
  end function try_add_par

  integer function add_par_by_entry(pl,s,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    integer,intent(out)::ent
    integer c
    c=0
    add_par_by_entry=try_add_par(pl,s,c,ent)
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
    istat=try_add_par(pl,s,0,k)
    if(istat/=0) then
       add_par_by_reference=istat
       return
    end if
    v => pl%v(k)
    if(.not.is_reference(v%sta).and.get_pkind(v%sta)/=PK_UNDEF) call free(v%p)
    v%p=ptr
    call set_pkind(v%sta,pk_set)
    call set_pflg(v%sta,flg)
    if(present(ent)) ent=k
    add_par_by_reference=0
  end function add_par_by_reference

  integer function add_par_by_value_x(pl,s,x,ro,ent)
    use memio
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    real(rp),intent(in)::x
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    integer istat,k,flg,pk
    if(present(ent)) ent=0
    flg=PK_REAL
    if(present(ro).and.ro) flg=ior(flg,PS_RO)
    istat=try_add_par(pl,s,0,k)
    if(istat/=0) then
       add_par_by_value_x=istat
       return
    end if
    v => pl%v(k)
    pk=get_pkind(v%sta)
    if(pk/=PK_REAL) then
       if(pk/=PK_UNDEF) call free(v%p)
       v%p=palloc(PK_REAL)
       call set_pkind(v%sta,PK_REAL)
    end if
    call set_pflg(v%sta,flg)
    call mcp(v%p,loc(x),sizeof(x))
    if(present(ent)) ent=k
    add_par_by_value_x=0
  end function add_par_by_value_x

  integer function add_par_by_value_r(pl,s,r,ro,ent)
    use memio
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    real(dp),intent(in)::r
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    integer istat,k,flg,pk
    if(present(ent)) ent=0
    flg=PK_DBLE
    if(present(ro).and.ro) flg=ior(flg,PS_RO)
    istat=try_add_par(pl,s,0,k) ! <<<<<<<
    if(istat/=0) then
       add_par_by_value_r=istat
       return
    end if
    v => pl%v(k)
    pk=get_pkind(v%sta)
    if(pk/=PK_DBLE) then
       if(pk/=PK_UNDEF) call free(v%p)
       v%p=palloc(PK_DBLE)
       call set_pkind(v%sta,PK_DBLE)
    end if
    call set_pflg(v%sta,flg)
    call mcp(v%p,loc(r),sizeof(r))
    if(present(ent)) ent=k
    add_par_by_value_r=0
  end function add_par_by_value_r

  integer function add_par_by_value_z(pl,s,z,ro,ent)
    use memio
    type(t_plist),intent(inout),target::pl
    character*(*),intent(in)::s
    complex(cp),intent(in)::z
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    type(t_vbuf),pointer::v
    integer istat,k,flg,pk
    if(present(ent)) ent=0
    if(present(ro).and.ro) then
       flg=PS_RO
    else
       flg=PS_NOP
    end if
    istat=try_add_par(pl,s,0,k) ! <<<
    if(istat/=0) then
       add_par_by_value_z=istat
       return
    end if
    v => pl%v(k)
    pk=get_pkind(v%sta)
    if(pk/=PK_COMP) then
       if(pk/=PK_UNDEF) call free(v%p)
       v%p=palloc(PK_COMP)
       v%sta=PK_COMP
    end if
    call set_pflg(v%sta,flg)
    call mcp(v%p,loc(z),sizeof(z))
    if(present(ent)) ent=k
    add_par_by_value_z=0
  end function add_par_by_value_z

  integer function get_par_loc(pl,k)
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::k
    type(t_vbuf),pointer::v
    integer istat,pk
    integer c
    real(rp) x
    real(dp) r
    complex(cp) z
    integer n
    pointer(px,x)
    pointer(pr,r)
    pointer(pz,z)
    pointer(pn,n)
    get_par_loc=0
    if(k>pl%s%n.or.k<=0) return
    istat=get_sc(pl%s,k,c)
    if(istat/=0) return
    v => pl%v(k)
    pk=get_pkind(v%sta)
    if(pk==PK_COMP) then
       get_par_loc=v%p
       return
    end if
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
    case(PK_UNDEF)
       return
    end select
    get_par_loc=v%pz
    call set_pflg(v%sta,PS_DUP)
  end function get_par_loc

  integer function get_par(pl,k,zout)
    type(t_plist),intent(in),target::pl
    integer,intent(in)::k
    complex(cp),intent(out)::zout
    type(t_vbuf),pointer::v
    integer c
    integer istat
    real(dp) r
    real(rp) x
    complex(cp) z
    integer n
    pointer(pr,r)
    pointer(pz,z)
    pointer(px,x)
    pointer(pn,n)
    istat=get_sc(pl%s,k,c)
    if(istat/=0) then
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

  integer function remove_dup(pl)
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
    remove_dup=PLERR_NOENT
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
    remove_dup=0
  end function remove_dup

end module plist
