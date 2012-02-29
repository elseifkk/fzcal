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
module slist
  implicit none

  private

  type t_sn
     integer*1,allocatable::s(:)
     integer:: code = 0 
     integer:: len  = 0
     type(t_sn),pointer::next => null()
  end type t_sn

  type,public::t_slist
     integer sz ! total size
     integer st ! stack top
     integer n  ! num elements
     integer p  ! str list
     type(t_sn),pointer::sn => null() 
  end type t_slist
  
  integer,parameter::SLERR_NOMEM = 1
  integer,parameter::SLERR_MEMOV = 2
  integer,parameter::SLERR_NOENT = 3
  integer,parameter,public::SLERR_END   = 3

  integer,parameter,public::LEN_SLIST_HDR=2 ! len+code
  
  !
  public get_sc
  public set_sc
  public add_sc
  public get_str_ptr
  public try_add_str
  public cpstr
  public find_str
  public rm_str
  public init_slist
  public add_str
  public min_cp_slist
  public uinit_slist
  public dump_slist
  public trim_slist
  public slist_count

contains
  
  pure integer function slist_count(sl)
    type(t_slist),intent(in)::sl
    slist_count=sl%n
  end function slist_count

  subroutine trim_slist(sl)
    type(t_slist),intent(inout)::sl
    type(t_slist) tmp
    tmp%p=0
    call min_cp_slist(sl,tmp)
    call uinit_slist(sl)
    sl=tmp
  end subroutine trim_slist

  subroutine min_cp_slist(sl1,sl2)
    use memio
    type(t_slist),intent(in)::sl1
    type(t_slist),intent(inout)::sl2
    integer sz
    sz=sl1%st-sl1%p+1
    if(sl2%p/=0) call free(sl2%p)
    if(sl1%n>0.and.sz>0) then
       sl2%p=malloc(sz)
       sl2%sz=sz
       call mcp(sl2%p,sl1%p,sz)
       sl2%st=sl2%p+sz-1
       sl2%n=sl1%n
    else
       sl2%n=0
       sl2%p=0
       sl2%st=0
       sl2%sz=0
    end if
  end subroutine min_cp_slist

  function init_slist(sz)
    type(t_slist) init_slist
    integer,intent(in)::sz
    if(sz>0) then
       init_slist%p=malloc(sz)
    else
       init_slist%p=0
    end if
    init_slist%sz=sz
    init_slist%st=init_slist%p-1
    init_slist%n=0
  end function init_slist
 
  subroutine uinit_slist(sl)
    type(t_slist),intent(inout)::sl
    integer i
    type(t_sn),pointer::sn
!!!!!!!!!!!!!
    sn => sl%sn
    do i=1,sl%n
       if(.not.associated(sn)) exit
       if(allocated(sn%s)) deallocate(sn%s)
       sn => sn%next
    end do
!!!!!!!!!!!!
    if(sl%p/=0) call free(sl%p)
    sl%p=0
    sl%sz=0
    sl%st=-1
    sl%n=0
  end subroutine uinit_slist

  pure character*256 function cpstr(ptr,len)
    integer,intent(in)::ptr
    integer,intent(in)::len
    integer i
    character*1 c
    pointer(p,c)
    cpstr=""
    p=ptr+LEN_SLIST_HDR-1 
    do i=1,len
       p=p+1
       cpstr(i:i)=c
    end do
  end function cpstr
      
  function kth_node(sl,k)
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    type(t_sn),pointer::kth_node
    type(t_sn),pointer::cur
    integer i
    nullify(kth_node)
    if(k>sl%n) return
    cur => sl%sn
    do i=2,k
       if(.not.associated(cur)) return
       cur => cur%next
    end do
    kth_node => cur
  end function kth_node
  
  integer function get_str_ptr(sl,k,ptr,len,code)
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    integer,intent(out)::ptr
    integer,intent(out),optional::len
    integer,intent(out),optional::code
    integer*1 b
    integer l,i
    pointer(p,b)
    if(present(len)) len=0
    if(sl%n<k.or.k<=0) then
       get_str_ptr=SLERR_NOENT
       return
    end if
    p=sl%p
    do i=1,k-1
       l=b
       p=p+l+LEN_SLIST_HDR
    end do
    ptr=p
    if(present(len)) len=b
    if(present(code)) then
       p=p+1
       code=b
    end if
    get_str_ptr=0
  end function get_str_ptr

  integer function get_sc(sl,k,code)
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    integer,intent(out)::code
    integer istat
    integer*1 b
    pointer(p,b)
    istat=get_str_ptr(sl,k,p)
    if(istat/=0) then
       get_sc=istat
       return
    end if
    p=p+1
    code=b
    get_sc=0
  end function get_sc

  integer function set_sc(sl,k,code)
    type(t_slist),intent(inout)::sl
    integer,intent(in)::k
    integer,intent(in)::code
    integer istat
    integer*1 b
    pointer(p,b)
    istat=get_str_ptr(sl,k,p)
    if(istat/=0) then
       set_sc=istat
       return
    end if
    p=p+1
    b=int(code,kind=1)
    set_sc=0
  end function set_sc

  integer function add_sc(sl,k,code)
    type(t_slist),intent(inout)::sl
    integer,intent(in)::k
    integer,intent(in)::code
    integer istat
    integer*1 b
    pointer(p,b)
    istat=get_str_ptr(sl,k,p)
    if(istat/=0) then
       add_sc=istat
       return
    end if
    p=p+1
    b=ior(b,int(code,kind=1))
    add_sc=0
  end function add_sc
  
  integer function rm_str(sl,s,ent)
    use memio
    type(t_slist),intent(inout)::sl
    character*(*),intent(inout),optional::s
    integer,intent(inout),optional::ent
    integer k
    integer ptr,l
    if(present(s).and.s/="") then
       k=find_str(sl,s,ptr=ptr)
       if(present(ent)) ent=k
       if(k==0) then
          rm_str=SLERR_NOENT
          return
       end if
    else if(present(ent)) then
       if(ent<=0.or.ent>sl%n.or.get_str_ptr(sl,ent,ptr,len=l)/=0) then
          rm_str=SLERR_NOENT
          return
       end if
       if(present(s)) call mcp(loc(s),ptr,min(l,len(s)))
    else
       rm_str=SLERR_NOENT
    end if
    call shift_slist()
    sl%n=sl%n-1
    rm_str=0
  contains
    subroutine shift_slist()
      integer*1 c,d
      pointer(si,c)
      pointer(di,d)
      di=ptr
      si=ptr+d+LEN_SLIST_HDR
      if(si>sl%st) then
         sl%st=di-1
      else
         do 
            d=c
            if(si==sl%st) exit
            di=di+1
            si=si+1
         end do
         sl%st=di
      end if
    end subroutine shift_slist
  end function rm_str

  integer function try_add_str(sl,s,code,ent)
    type(t_slist),intent(inout)::sl
    character*(*),intent(in)::s
    integer,intent(in)::code
    integer,intent(out),optional::ent
    integer k,istat
    integer c
    k=0
    istat=0
    if(sl%n>0) then
       k=find_str(sl,s,found_code=c)
       if(k/=0) then
          if(present(ent)) ent=k
          if(c/=code) then
             istat=set_sc(sl,k,code)
          end if
       end if
    end if
    if(k==0.and.istat==0) istat=add_str(sl,s,code,ent)
    try_add_str=istat
  end function try_add_str

  subroutine inc_slist(sl,inc_sz)
    use memio
    type(t_slist),intent(inout)::sl
    integer,intent(in)::inc_sz
    integer p
    integer std
    if(inc_sz<=0) return
    p=malloc(sl%sz+inc_sz)
    std=sl%st-sl%p
    if(sl%p/=0) then
       call mcp(p,sl%p,sl%sz)
       call free(sl%p)
    end if
    sl%p=p
    sl%st=std+p
    sl%sz=sl%sz+inc_sz
  end subroutine inc_slist

  integer function add_str(sl,s,code,ent)
    type(t_slist),intent(inout)::sl
    character*(*),intent(in)::s
    integer,intent(in)::code
    integer,intent(out),optional::ent
    integer i
    integer*1 b
    integer*1 c
    pointer(q,c)
    pointer(p,b)
    integer len
    if(present(ent)) ent=0
    if(sl%p==0) then !<<<<<<<<<<<<<<<<<
       add_str=SLERR_NOMEM
       return
    end if
    len=len_trim(s)
    if(len>255) then
       add_str=SLERR_MEMOV
       return
    end if
    if(len+LEN_SLIST_HDR+(sl%st-sl%p+1)>sl%sz) &
         call inc_slist(sl,len+LEN_SLIST_HDR)
    p=sl%st+1
    b=int(len,kind=1)
    p=p+1
    b=int(code,kind=1)
    q=loc(s)-1
    do i=1,len
       p=p+1
       q=q+1
       b=c
    end do
    sl%st=p
    sl%n=sl%n+1 
    add_str=0
    if(present(ent)) ent=sl%n
  end function add_str

  integer function find_str(sl,s,target_code,found_code,ptr)
    type(t_slist),intent(in)::sl
    character*(*),intent(in)::s
    integer,intent(in),optional::target_code
    integer,intent(out),optional::found_code
    integer,intent(out),optional::ptr
    integer i
    integer len
    integer lenp
    integer*1 cd
    integer*1 b
    integer*1 tc
    pointer(p,b)
    find_str=0
    p=sl%p
    len=len_trim(s)
    if(present(target_code)) then
       tc=int(target_code,kind=1)
    else
       tc=-1
    end if
    do i=1,sl%n
       lenp=b
       if(len==lenp) then
          if(is_matched()) then
             find_str=i
             if(present(found_code)) found_code=cd
             if(present(ptr)) ptr=p
             return
          end if
       end if
       p=p+lenp+LEN_SLIST_HDR
    end do
  contains
    logical function is_matched()
      integer*1 c
      integer j
      pointer(q,c)
      is_matched=.false.
      q=p+1 ! skip code
      cd=c
      if(tc/=-1.and.tc/=c) return
      do j=1,len
         q=q+1
         if(c/=ichar(s(j:j))) return
      end do
      is_matched=.true.
    end function is_matched
  end function find_str

  subroutine dump_slist(sl)
    use misc, only: mess,messp
    use memio, only: itoa,DISP_FMT_BIN
    type(t_slist),intent(in)::sl
    integer i
    integer*1 c
    integer ptr,len
    pointer(p,c)
    call mess("slist dump:")
    if(sl%n<=0.or.sl%p==0.or.sl%sz==0) then
      call mess("(empty)")
    end if
    call mess("size = "//trim(itoa(sl%sz)))
    call mess("capacity = "//trim(itoa(sl%sz-(sl%st-sl%p+1))))
    p=sl%p
    call mess("#\tLen\tCode\tValue")
    do i=1,sl%n
       ptr=p
       len=c
       call messp(trim(itoa(i))//":\t"//trim(itoa(len))//"\t")
       p=p+1
       call messp("["//trim(itoa(int(c),DISP_FMT_BIN))//"]\t")
       p=p+1
       call mess(trim(cpstr(ptr,len)))
       p=ptr+len+LEN_SLIST_HDR
    end do
  end subroutine dump_slist

end module slist
