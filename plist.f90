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
  
  type,public::t_plist
     type(t_slist) s
     complex(cp),allocatable::v(:)
  end type t_plist
  
  interface add_par_by_value
     module procedure add_par_by_value_r
     module procedure add_par_by_value_c
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

contains

 function init_plist(sz,nmax)
    type(t_plist) init_plist
    integer,intent(in)::sz
    integer,intent(in)::nmax
    init_plist%s=init_slist(sz)
    allocate(init_plist%v(nmax))
  end function init_plist

  subroutine uinit_plist(pl)
    type(t_plist),intent(inout)::pl
    call uinit_slist(pl%s)
    if(allocated(pl%v)) deallocate(pl%v)
  end subroutine uinit_plist

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
    do i=k,pl%s%n
       pl%v(i)=pl%v(i+1)
    end do
    rm_par=0
  end function rm_par

  subroutine min_cp_plist(pl1,pl2)
    type(t_plist),intent(in)::pl1
    type(t_plist),intent(out)::pl2
    if(allocated(pl2%v)) deallocate(pl2%v)
    if(allocated(pl1%v).and.size(pl1%v)>0) then
       allocate(pl2%v(size(pl1%v)))
       pl2%v=pl1%v
    end if
    call min_cp_slist(pl1%s,pl2%s)
  end subroutine min_cp_plist
  
  subroutine dump_plist(pl)
    type(t_plist),intent(in)::pl
    integer i,len,istat
    integer*1 code
    integer si
    real(dp) r
    real(rp) x
    complex(cp) z
    pointer(pr,r)
    pointer(pz,z)
    pointer(ptr,si)
    pointer(px,x)
    do i=1,pl%s%n
       istat=get_str_ptr(pl%s,i,ptr,len,code=code)
       write(*,10) i,code,trim(cpstr(ptr,len))
       if(is_reference(code)) then
          ptr=loc(pl%v(i))
          write(*,20) si
          if(.not.is_double(code)) then
             if(.not.is_real(code)) then
                pz=si
                write(*,*) trim(ztoa(z,fmt=DISP_FMT_RAW))
             else
                px=si
                write(*,*) trim(rtoa(x,fmt=DISP_FMT_RAW))                
             end if
          else
             pr=si
             write(*,*) trim(rtoa(real(r,kind=rp),fmt=DISP_FMT_RAW))
          end if
       else
          write(*,*) trim(ztoa(pl%v(i),fmt=DISP_FMT_RAW))
       end if
    end do
20  format(x,z16,$)
10  format(x,i4,x,b4.4,x,a,$)
  end subroutine dump_plist

  subroutine trim_plist(pl)
    use memio
    type(t_plist),intent(inout)::pl
    complex(cp),allocatable::tmpv(:)
    integer tmpp
    integer sz

    if(allocated(pl%v)) then
       if(pl%s%n>=1) then
          allocate(tmpv(pl%s%n))
          tmpv(1:pl%s%n)=pl%v(1:pl%s%n)
          deallocate(pl%v)
          allocate(pl%v(pl%s%n))
          pl%v(1:pl%s%n)=tmpv(1:pl%s%n)
          deallocate(tmpv)
       else
          deallocate(pl%v)
       end if
    end if

    sz=pl%s%st-pl%s%p+1
    if(sz>=0) then
       tmpp=malloc(pl%s%st)
       call mcp(tmpp,pl%s%p,sz)
       call free(pl%s%p)
       pl%s%p=malloc(sz)
       call mcp(pl%s%p,tmpp,sz)
       call free(tmpp)
       pl%s%sz=pl%s%sz
    else if(pl%s%p/=0) then
       call free(pl%s%p)
       pl%s%p=0
       pl%s%st=0
       pl%s%n=0
       pl%s%sz=0
    end if
    
  end subroutine trim_plist
 
  integer function find_par(pl,s,val,ent,code)
    type(t_plist),intent(in)::pl
    character*(*),intent(in)::s
    complex(cp),intent(out),optional::val
    integer,intent(out),optional::ent
    integer*1,intent(out),optional::code
    integer k
    integer*1 c
    integer si
    complex(cp) z
    real(rp) x
    real(dp) r8
    pointer(ptr,si)
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
       if(is_reference(c)) then
          ptr=loc(pl%v(k))
          if(.not.is_double(c)) then
             if(.not.is_real(c)) then
                pz=si
                val=z
             else
                px=si
                val=x
             end if
          else
             pr8=si
             val=r8
          end if
       else
          val=pl%v(k)
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
    if(.not.is_reference(c)) then
       pl%v(k)=v
    else
       call put_par_by_reference(pl,k,v)
    end if
    put_par=0
  end function put_par

  subroutine put_par_by_reference(pl,k,v)
    type(t_plist),intent(in)::pl
    integer,intent(in)::k
    complex(cp),intent(in)::v
    integer ptr
    complex(cp) x
    pointer(pptr,ptr)
    pointer(px,x)
    pptr=loc(pl%v(k))
    px=ptr
    x=v
  end subroutine put_par_by_reference

  subroutine inc_par_buf(pl,inc_n)
    type(t_plist),intent(inout)::pl
    integer,intent(in)::inc_n
    complex(cp),allocatable::v(:)
    if(inc_n<=0) return
    if(allocated(pl%v)) then
       allocate(v(size(pl%v)+inc_n))
       v(1:size(pl%v))=pl%v
       deallocate(pl%v)
       allocate(pl%v(size(v)))
       pl%v=v
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
    integer si !<<<<<<<<< size undetermined
    integer*1 c
    pointer(p,si)
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
    pl%v(k)=0
    p=loc(pl%v(k))
    si=ptr
    if(present(ent)) ent=k
    add_par_by_reference=0
  end function add_par_by_reference

  integer function add_par_by_value_r(pl,s,v,ro,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    real(rp),intent(in)::v
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    complex(cp) z
    z=complex(v,0.)
    add_par_by_value_r=add_par_by_value_c(pl,s,z,ro,ent)
  end function add_par_by_value_r

  integer function add_par_by_value_c(pl,s,v,ro,ent)
    type(t_plist),intent(inout)::pl
    character*(*),intent(in)::s
    complex(cp),intent(in)::v
    logical,intent(in),optional::ro
    integer,intent(out),optional::ent
    integer istat
    integer k
    integer*1 c
    if(present(ent)) ent=0
    if(present(ro).and.ro) then
       c=SC_RO
    else
       c=SC_NOP
    end if
    istat=try_add_par(pl,s,c,k)
    if(istat/=0) then
       add_par_by_value_c=istat
       return
    end if
    pl%v(k)=v
    if(present(ent)) ent=k
    add_par_by_value_c=0
  end function add_par_by_value_c

  integer function get_par_loc(pl,k)
    type(t_plist),intent(in)::pl
    integer,intent(in)::k
    integer istat
    integer*1 c
    integer p
    pointer(si,p)
    get_par_loc=0
    if(k>pl%s%n.or.k<=0) return
    istat=get_sc(pl%s,k,c)
    if(istat/=0) return
    si=loc(pl%v(k))
    if(is_reference(c)) then
       get_par_loc=p
    else
       get_par_loc=si
    end if
  end function get_par_loc

  integer function get_par(pl,k,v)
    type(t_plist),intent(in)::pl
    integer,intent(in)::k
    complex(cp),intent(out)::v
    integer*1 c
    integer istat
    real(dp) r
    real(rp) x
    complex(cp) z
    integer p
    pointer(pr,r)
    pointer(pz,z)
    pointer(si,p)
    pointer(px,x)
    istat=get_sc(pl%s,k,c)
    if(istat/=0) then
       get_par=PLERR_NOPAR
       return
    end if
    if(.not.is_reference(c)) then
       ! always complex
       v=pl%v(k)
    else 
       si=loc(pl%v(k))
       if(.not.is_double(c)) then
          if(.not.is_real(c)) then
             pz=p
             v=z
          else
             px=p
             v=x
          end if
       else
          ! double real only allowed by reference
          pr=p
          v=complex(real(r,kind=rp),rzero)
       end if
    end if
    get_par=0
  end function get_par

end module plist
