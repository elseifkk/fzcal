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
  
  integer,parameter,public::PK_COMP = 1
  integer,parameter,public::PK_REAL = 2
  integer,parameter,public::PK_DBLE = 3
  integer,parameter,public::PK_REF  = 4

  type t_vbuf
     complex(cp),allocatable::z
     real(rp),allocatable::x
     real(dp),allocatable::r
     integer,allocatable::p
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
  public is_double
  public get_par
  public alloc_par
  public remove_dup
  public realloc_new

contains

  function init_plist(sz,nmax)
    type(t_plist) init_plist
    integer,intent(in)::sz
    integer,intent(in)::nmax
    init_plist%s=init_slist(sz)
    allocate(init_plist%v(nmax))
  end function init_plist

  subroutine uinit_vbuf(vl)
    type(t_vbuf),intent(inout)::vl
    if(allocated(vl%z)) deallocate(vl%z)
    if(allocated(vl%x)) deallocate(vl%x)
    if(allocated(vl%r)) deallocate(vl%r)
    if(allocated(vl%p)) deallocate(vl%p)
  end subroutine uinit_vbuf

  subroutine uinit_vbufs(n,vl)
    integer,intent(in)::n
    type(t_vbuf),intent(inout)::vl(n)
    integer i
    do i=1,n
       call uinit_vbuf(vl(i))
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
    type(t_vbuf),intent(in)::v1
    type(t_vbuf),intent(inout)::v2
    call uinit_vbuf(v2)
    if(allocated(v1%z)) then
       if(.not.allocated(v2%z)) allocate(v2%z)
       v2%z=v1%z
    end if
    if(allocated(v1%x)) then
       if(.not.allocated(v2%x)) allocate(v2%z)
       v2%x=v1%x
    end if
    if(allocated(v1%r)) then
       if(.not.allocated(v2%r)) allocate(v2%r)
       v2%r=v1%r
    end if
    if(allocated(v1%p)) then
       if(.not.allocated(v2%p)) allocate(v2%p)
       v2%p=v1%p
    end if
  end subroutine cp_vbuf

  integer function alloc_par(pl,k,pk,cle)
    type(t_plist),intent(inout)::pl
    integer,intent(in)::k
    integer,intent(in)::pk
    logical,intent(in),optional::cle
    if(k>size(pl%v).or.k<=0) then
       alloc_par=PLERR_NOENT
       return
    end if
    select case(pk)
    case(PK_COMP)
       if(.not.allocated(pl%v(k)%z)) allocate(pl%v(k)%z)
       if(present(cle).and.cle) pl%v(k)%z=czero
    case(PK_REAL)
       if(.not.allocated(pl%v(k)%x)) allocate(pl%v(k)%x)
       if(present(cle).and.cle) pl%v(k)%x=rzero
    case(PK_DBLE)
       if(.not.allocated(pl%v(k)%r)) allocate(pl%v(k)%r)
       if(present(cle).and.cle) pl%v(k)%r=0.0_dp
    case(PK_REF)
       if(.not.allocated(pl%v(k)%p)) allocate(pl%v(k)%p)
       if(present(cle).and.cle) pl%v(k)%p=0
    end select
    alloc_par=add_sc(pl%s,k,SC_NEW)
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
    type(t_plist),intent(out)::pl2
    if(allocated(pl2%v)) then
       call uinit_vbufs(size(pl2%v),pl2%v)
       deallocate(pl2%v)
    end if
    if(allocated(pl1%v).and.size(pl1%v)>0) then
       allocate(pl2%v(size(pl1%v)))
       call cp_vbufs(pl1%s%n,pl1%v,pl2%v)
    end if
    call min_cp_slist(pl1%s,pl2%s)
  end subroutine min_cp_plist
  
  subroutine dump_plist(pl)
    type(t_plist),intent(in),target::pl
    integer i,len,istat,ptr
    type(t_vbuf),pointer::v
    integer*1 c
    real(dp) r
    real(rp) x
    complex(cp) z
    pointer(pr,r)
    pointer(pz,z)
    pointer(px,x)
    do i=1,pl%s%n
       v => pl%v(i)
       istat=get_str_ptr(pl%s,i,ptr,len,code=c)
       write(*,10) i,c,trim(cpstr(ptr,len))
       if(is_reference(c)) then
          if(.not.allocated(v%p)) then
             write(*,30) "(no entry)"
             cycle
          else
             write(*,20) v%p
          end if
          if(is_complex(c)) then
             pz=v%p
             write(*,*) trim(ztoa(z,fmt=DISP_FMT_RAW))
          else if(is_real(c)) then
             px=v%p
             write(*,*) trim(rtoa(x,fmt=DISP_FMT_RAW))                
          else if(is_double(c)) then
             pr=v%p
             write(*,*) trim(rtoa(real(r,kind=rp),fmt=DISP_FMT_RAW))
          end if
       else if(is_complex(c)) then
          if(.not.allocated(v%z)) then
             write(*,30) "(no entry)"
             cycle
          end if
          write(*,*) trim(ztoa(v%z,fmt=DISP_FMT_RAW))
       else if(is_real(c)) then
          if(.not.allocated(v%x)) then
             write(*,30) "(no entry)"
             cycle
          end if
          write(*,*) trim(rtoa(v%x,fmt=DISP_FMT_RAW))
       else if(is_double(c)) then
          if(.not.allocated(v%r)) then
             write(*,30) "(no entry)"
             cycle
          end if
          write(*,*) trim(rtoa(real(v%r,kind=rp),fmt=DISP_FMT_RAW))
       end if
    end do
10  format(x,i4,x,b8.8,x,a,$)
20  format(x,z16,$)
30  format(x,a)
  end subroutine dump_plist

  subroutine trim_plist(pl)
    use memio
    type(t_plist),intent(inout)::pl
    type(t_vbuf),allocatable::tmpv(:)
    if(allocated(pl%v)) then
       if(pl%s%n>=1) then
          allocate(tmpv(pl%s%n))
          call mv_vbufs(pl%s%n,pl%v,tmpv)
          deallocate(pl%v)
          allocate(pl%v(pl%s%n))
          call mv_vbufs(pl%s%n,tmpv,pl%v)
          deallocate(tmpv)
       else
          deallocate(pl%v)
       end if
    end if
    call trim_slist(pl%s)
  end subroutine trim_plist
 
  integer function find_par(pl,s,val,ent,code)
    type(t_plist),intent(in)::pl
    character*(*),intent(in)::s
    complex(cp),intent(out),optional::val
    integer,intent(out),optional::ent
    integer*1,intent(out),optional::code
    integer k
    integer*1 c
    complex(cp) z
    real(rp) x
    real(dp) r8
    pointer(pz,z)
    pointer(pr8,r8)
    pointer(px,x)
    if(present(ent)) ent=0
    k=find_str(pl%s,s,found_code=c)
    if(k==0) then
       find_par=PLERR_NOENT
       return
    end if
    if(present(code)) code=c
    if(present(val)) then
       find_par=PLERR_NOENT
       if(is_reference(c)) then
          if(.not.allocated(pl%v(k)%p)) return
          if(.not.is_double(c)) then
             if(.not.is_real(c)) then
                pz=pl%v(k)%p
                val=z
             else
                px=pl%v(k)%p
                val=x
             end if
          else
             pr8=pl%v(k)%p
             val=r8
          end if
       else 
          if(.not.allocated(pl%v(k)%z)) return
          val=pl%v(k)%z
       end if
    end if
    if(present(ent)) ent=k
    find_par=0
  end function find_par

  integer function put_par(pl,k,v)
    type(t_plist),intent(inout)::pl
    integer,intent(in)::k
    complex(cp),intent(in)::v
    integer istat
    integer*1 c
    integer p,l
    istat=get_sc(pl%s,k,c)
    if(istat/=0) then
       put_par=istat
       return
    end if
    if(is_read_only(c)) then
       istat=get_str_ptr(pl%s,k,p,l)
       write(*,*) "*** Parameter is read-only: "//trim(cpstr(p,l))
       put_par=PLERR_RDONL
       return
    end if
    put_par=PLERR_NOENT
    if(.not.is_reference(c)) then
       if(.not.allocated(pl%v(k)%z)) return
       pl%v(k)%z=v
    else
       if(.not.allocated(pl%v(k)%p)) return
       call put_par_by_reference(pl,k,v)
    end if
    put_par=0
  end function put_par

  subroutine put_par_by_reference(pl,k,v)
    type(t_plist),intent(in)::pl
    integer,intent(in)::k
    complex(cp),intent(in)::v
    complex(cp) z
    pointer(pz,z)
    pz=pl%v(k)%p
    z=v
  end subroutine put_par_by_reference

  subroutine inc_par_buf(pl,inc_n)
    type(t_plist),intent(inout)::pl
    integer,intent(in)::inc_n
    type(t_vbuf),allocatable::v(:)
    if(inc_n<=0) return
    if(allocated(pl%v)) then
       allocate(v(size(pl%v)+inc_n))
       call mv_vbufs(size(pl%v),pl%v,v)
       deallocate(pl%v)
       allocate(pl%v(size(v)))
       call mv_vbufs(size(pl%v),v,pl%v)
       deallocate(v)
    else
       allocate(pl%v(inc_n))
    end if
  end subroutine inc_par_buf

  integer function try_add_par(pl,s,code,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    integer*1,intent(in)::code
    integer,intent(out),optional::ent
    integer istat
    istat=try_add_str(pl%s,s,code,ent)
    if(istat/=0) then
       try_add_par=istat
       return
    end if
    if(pl%s%n>size(pl%v).or..not.allocated(pl%v)) call inc_par_buf(pl,4)
    try_add_par=0
  end function try_add_par

  integer function add_par_by_entry(pl,s,ent,ro)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    integer,intent(out)::ent
    logical,intent(in),optional::ro
    integer*1 c
    if(present(ro).and.ro) then
       c=SC_RO
    else
       c=0
    end if
    add_par_by_entry=try_add_par(pl,s,c,ent)
  end function add_par_by_entry

  integer function add_par_by_reference(pl,s,ptr,ro,dble,real,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    integer,intent(in)::ptr
    logical,intent(in),optional::ro
    logical,intent(in),optional::dble
    logical,intent(in),optional::real
    integer,intent(out),optional::ent
    integer istat
    integer k
    integer*1 c
    if(present(ent)) ent=0
    if(present(ro).and.ro) then
       c=ior(SC_REF,SC_RO)
    else
       c=SC_REF
    end if
    if(present(dble).and.dble) c=ior(c,ior(SC_DBLE,SC_RO)) ! <<<<
    if(present(real).and.real) c=ior(c,ior(SC_REAL,SC_RO))
    istat=try_add_par(pl,s,c,k)
    if(istat/=0) then
       add_par_by_reference=istat
       return
    end if
    if(.not.allocated(pl%v(k)%p)) allocate(pl%v(k)%p)
    pl%v(k)%p=ptr
    if(present(ent)) ent=k
    add_par_by_reference=0
  end function add_par_by_reference

  integer function add_par_by_value_x(pl,s,v,ro,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    real(rp),intent(in)::v
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    integer istat,k
    integer*1 c
    if(present(ent)) ent=0
    c=SC_REAL
    if(present(ro).and.ro) c=ior(c,SC_RO)
    istat=try_add_par(pl,s,c,k)
    if(istat/=0) then
       add_par_by_value_x=istat
       return
    end if
    if(.not.allocated(pl%v(k)%x)) allocate(pl%v(k)%x)
    pl%v(k)%x=v
    if(present(ent)) ent=k
    add_par_by_value_x=0
  end function add_par_by_value_x

  integer function add_par_by_value_r(pl,s,v,ro,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    real(dp),intent(in)::v
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    integer istat,k
    integer*1 c
    if(present(ent)) ent=0
    c=SC_DBLE
    if(present(ro).and.ro) c=ior(c,SC_RO)
    istat=try_add_par(pl,s,c,k)
    if(istat/=0) then
       add_par_by_value_r=istat
       return
    end if
    if(.not.allocated(pl%v(k)%r)) allocate(pl%v(k)%r)
    pl%v(k)%r=v
    if(present(ent)) ent=k
    add_par_by_value_r=0
  end function add_par_by_value_r

  integer function add_par_by_value_z(pl,s,v,ro,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    complex(cp),intent(in)::v
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    integer istat,k
    integer*1 c
    if(present(ent)) ent=0
    if(present(ro).and.ro) then
       c=SC_RO
    else
       c=SC_NOP
    end if
    istat=try_add_par(pl,s,c,k)
    if(istat/=0) then
       add_par_by_value_z=istat
       return
    end if
    if(.not.allocated(pl%v(k)%z)) allocate(pl%v(k)%z)
    pl%v(k)%z=v
    if(present(ent)) ent=k
    add_par_by_value_z=0
  end function add_par_by_value_z

  integer function get_par_loc(pl,k)
    type(t_plist),intent(inout),target::pl
    integer,intent(in)::k
    type(t_vbuf),pointer::v
    integer istat
    integer*1 c
    real(rp) x
    real(dp) r
    pointer(px,x)
    pointer(pr,r)
    get_par_loc=0
    if(k>pl%s%n.or.k<=0) return
    istat=get_sc(pl%s,k,c)
    if(istat/=0) return
    v => pl%v(k)
    if(is_value(c)) then
       if(is_complex(c)) then
          if(.not.allocated(v%z)) return
          get_par_loc=loc(v%z)
          return
       else if(is_real(c)) then
          if(.not.allocated(v%x)) return
          if(.not.allocated(v%z)) allocate(v%z)
          v%z=complex(v%x,rzero)
       else if(is_double(c)) then
          if(.not.allocated(v%r)) return
          if(.not.allocated(v%z)) allocate(v%z)
          v%z=complex(real(v%r,kind=rp),rzero)
       end if
    else if(.not.allocated(v%p)) then
       return
    else if(is_complex(c)) then
       get_par_loc=v%p
       return
    else if(is_real(c)) then
       if(.not.allocated(v%z)) allocate(v%z)
       px=v%p
       v%z=complex(x,rzero)
    else if(is_double(c)) then
       if(.not.allocated(v%z)) allocate(v%z)
       pr=v%p
       v%z=complex(real(r,kind=rp),rzero)
    end if
    get_par_loc=loc(v%z)
    istat=set_sc(pl%s,k,ior(c,SC_DUP))
  end function get_par_loc

  integer function get_par(pl,k,zout)
    type(t_plist),intent(in),target::pl
    integer,intent(in)::k
    complex(cp),intent(out)::zout
    type(t_vbuf),pointer::v
    integer*1 c
    integer istat
    real(dp) r
    real(rp) x
    complex(cp) z
    pointer(pr,r)
    pointer(pz,z)
    pointer(px,x)
    istat=get_sc(pl%s,k,c)
    if(istat/=0) then
       get_par=PLERR_NOPAR
       return
    end if
    get_par=PLERR_NOENT
    v => pl%v(k)
    if(is_value(c)) then
       if(is_complex(c)) then
          if(.not.allocated(v%z)) return
          zout=v%z
       else if(is_real(c)) then
          if(.not.allocated(v%x)) return
          zout=complex(v%x,rzero)
       else if(is_double(c)) then
          if(.not.allocated(v%r)) return
          zout=complex(real(v%r,kind=rp),rzero)
       end if
    else 
       if(.not.allocated(v%p)) return
       if(is_complex(c)) then
          pz=v%p
          zout=z
       else if(is_real(c)) then
          px=v%p
          zout=complex(x,rzero)
       else if(is_double(c)) then
          pr=v%p
          zout=complex(real(r,kind=rp),rzero)
       end if
    end if
    get_par=0
  end function get_par

  integer function remove_dup(pl)
    type(t_plist),intent(inout),target::pl
    type(t_vbuf),pointer::v
    integer i,istat
    integer*1 c
    real(rp) x
    real(dp) r
    pointer(px,x)
    pointer(pr,r)
    remove_dup=PLERR_NOENT
    do i=1,pl%s%n
       istat=get_sc(pl%s,i,c)
       if(istat/=0) then
          remove_dup=istat
          return
       end if
       if(.not.is_duplicated(c)) cycle
       v => pl%v(i)
       if(.not.allocated(v%z)) return
       if(is_reference(c)) then
          if(.not.allocated(v%p)) return
          if(is_real(c)) then
             px=v%p
             x=realpart(v%z)
          else if(is_double(c)) then
             pr=v%p
             r=real(realpart(v%z),kind=dp)
          else
             ! unexpected error
          end if
       else if(is_real(c)) then
          if(.not.allocated(v%x)) return
          v%x=realpart(v%z)
       else if(is_double(c)) then
          if(.not.allocated(v%r)) return
          v%r=real(realpart(v%z),kind=dp)
       else
          ! unexpected error
       end if
       deallocate(v%z)
       istat=set_sc(pl%s,i,iand(c,not(SC_DUP)))
    end do
    remove_dup=0
  end function remove_dup

  integer function realloc_new(pl)
    type(t_plist),intent(inout),target::pl
    type(t_vbuf),pointer::v
    integer i,istat
    integer*1 c
    realloc_new=PLERR_NOENT
    do i=1,pl%s%n
       istat=get_sc(pl%s,i,c)
       if(istat/=0) then
          realloc_new=istat
          return
       end if
       if(.not.is_new(c)) cycle
       if(is_complex(c)) then
          v => pl%v(i)
          if(.not.allocated(v%z)) return
          if(imagpart(v%z)==rzero) then
             if(.not.allocated(v%x)) allocate(v%x)
             v%x=realpart(v%z)
             deallocate(v%z)
             c=ior(c,SC_REAL)
          end if
       end if
       c=iand(c,not(SC_NEW))
       istat=set_sc(pl%s,i,c)
    end do
    realloc_new=0
  end function realloc_new

end module plist
