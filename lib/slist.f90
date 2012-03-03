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

  public get_sc
  public set_sc
  public add_sc
  public get_str_ptr
  public try_add_str
  public find_str
  public rm_str
  public init_slist
  public add_str
  public cp_slist
  public uinit_slist
  public dump_slist
  public slist_count

  type,public::t_slist
     integer::n = 0  ! allocated sn, sn%s may not be allocated
     type(t_sn),pointer::sn => null() 
  end type t_slist

  type t_sn
     integer*1,allocatable::s(:)
     integer:: c = 0 
     type(t_sn),pointer::next => null()
     type(t_sn),pointer::prev => null() ! head%prev => tail
  end type t_sn
  
  integer,parameter::SLERR_NOMEM  = 1
  integer,parameter::SLERR_MEMOV  = 2
  integer,parameter::SLERR_NOENT  = 3
  integer,parameter::SLERR_INVARG = 4

contains
  
  pure integer function slist_count(sl)
    type(t_slist),intent(in)::sl
    slist_count=sl%n
  end function slist_count

  function cp_slist(sl)
    use memio, only: mcp
    type(t_slist),intent(in)::sl
    type(t_slist) cp_slist
    integer i
    type(t_sn),pointer::sn,prev
    type(t_sn),pointer::sn_in
    cp_slist=init_slist()
    if(sl%n==0.or..not.associated(sl%sn)) return
    allocate(cp_slist%sn)
    cp_slist%n=1
    sn => cp_slist%sn
    prev => sn
    sn_in => sl%sn
    do i=1,sl%n
       sn%c=sn_in%c
       if(allocated(sn%s)) then
          allocate(sn%s(size(sn_in%s)))
          call mcp(loc(sn%s),loc(sn_in%s),size(sn_in%s))
       end if
       sn%prev => prev
       nullify(sn%next)
       sn_in => sn_in%next
       if(.not.associated(sn_in)) exit
       allocate(sn%next)
       prev => sn
       sn => sn%next
       sn%prev => prev
       cp_slist%n=cp_slist%n+1
    end do
    cp_slist%sn%prev => sn ! last node
  end function cp_slist

  function init_slist()
    type(t_slist) init_slist
    ! for only new slist 
    init_slist%n=0
  end function init_slist
 
  subroutine uinit_slist(sl)
    type(t_slist),intent(inout)::sl
    integer i
    type(t_sn),pointer::sn,next
    if(sl%n==0) return
    sn => sl%sn
    do i=1,sl%n
       if(.not.associated(sn)) exit
       if(allocated(sn%s)) deallocate(sn%s)
       next => sn%next
       deallocate(sn)
       sn => next
    end do
    sl%n=0
  end subroutine uinit_slist

  function kth_node(sl,k)
    ! returns null if failed
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    type(t_sn),pointer::kth_node
    type(t_sn),pointer::sn
    integer i
    nullify(kth_node)
    if(k>sl%n.or.k<=0) return
    if(.not.associated(sl%sn)) return
    sn => sl%sn
    do i=2,k
       sn => sn%next
       if(.not.associated(sn)) return
    end do
    kth_node => sn
  end function kth_node

!!$  function last_node(sl,k)
!!$    ! return k=0 if failed
!!$    type(t_slist),intent(in)::sl
!!$    integer,intent(out)::k
!!$    type(t_sn),pointer::last_node
!!$    type(t_sn),pointer::sn
!!$    integer i
!!$    nullify(last_node)
!!$    k=0
!!$    if(.not.associated(sl%sn)) return
!!$    sn => sl%sn
!!$    do i=1,sl%n
!!$       if(.not.associated(sn)) return
!!$       if(.not.associated(sn%next)) then
!!$          k=i
!!$          last_node => sn
!!$          return
!!$       end if
!!$       sn => sn%next
!!$    end do
!!$  end function last_node
  
  integer function get_str_ptr(sl,k,ptr,len,code)
    ! ptr maybe 0 even if returns 0
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    integer,intent(out)::ptr
    integer,intent(out),optional::len
    integer,intent(out),optional::code
    type(t_sn),pointer::sn

    if(present(len)) len=0
    if(present(code)) code=0
    ptr=0
    if(sl%n<k.or.k<=0) then
       get_str_ptr=SLERR_NOENT
       return
    end if
   
    sn => kth_node(sl,k)
    if(.not.associated(sn)) then
       get_str_ptr=SLERR_NOENT
       return
    end if

    if(allocated(sn%s)) then
       ptr=loc(sn%s)
       if(present(len)) len=size(sn%s)
    end if
    if(present(code)) code=sn%c

    get_str_ptr=0

  end function get_str_ptr

  integer function get_sc(sl,k,code)
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    integer,intent(out)::code
    type(t_sn),pointer::sn
    sn => kth_node(sl,k)
    if(.not.associated(sn)) then
       get_sc=SLERR_NOENT
       return
    end if
    code=sn%c
    get_sc=0
  end function get_sc

  integer function set_sc(sl,k,code)
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    integer,intent(in)::code
    type(t_sn),pointer::sn
    sn => kth_node(sl,k)
    if(.not.associated(sn)) then
       set_sc=SLERR_NOENT
       return
    end if
    sn%c=code
    set_sc=0
  end function set_sc

  integer function add_sc(sl,k,code)
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    integer,intent(in)::code
    type(t_sn),pointer::sn
    sn => kth_node(sl,k)
    if(.not.associated(sn)) then
       add_sc=SLERR_NOENT
       return
    end if
    sn%c=ior(sn%c,code)
    add_sc=0
  end function add_sc
  
  integer function rm_str(sl,s,ent)
    use memio, only: mcp
    type(t_slist),intent(inout)::sl
    character*(*),intent(inout),optional::s
    integer,intent(inout),optional::ent
    integer k
    type(t_sn),pointer::sn

    rm_str=SLERR_NOENT
    if(present(s).and.s/="") then
       k=find_str(sl,s,node=sn)
       if(present(ent)) ent=k
       if(k==0) return
    else if(present(ent)) then
       if(ent<=0.or.ent>sl%n) return
       k=ent
       sn => kth_node(sl,k)
       if(.not.associated(sn)) return
       if(present(s)) call mcp(loc(s),loc(sn%s),min(size(sn%s),len(s)))
    else
       return
    end if

    sn%prev%next => sn%next
    sn%next%prev => sn%prev

    deallocate(sn)
    sl%n=sl%n-1

    rm_str=0

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
    if(sl%n>0) k=find_str(sl,s,found_code=c)
    if(k==0) then
       istat=add_str(sl,s,code,ent)
    else
       if(present(ent)) ent=k
       if(c/=code) istat=set_sc(sl,k,code)
    end if
    try_add_str=istat
  end function try_add_str

  integer function add_str(sl,s,code,ent)
    use memio, only: mcp
    type(t_slist),intent(inout)::sl
    character*(*),intent(in)::s
    integer,intent(in)::code
    integer,intent(out),optional::ent
    integer len
    type(t_sn),pointer::sn,prev

    if(present(ent)) ent=0
    len=len_trim(s)
    if(len==0) then
       add_str=SLERR_INVARG
       return
    end if

    if(sl%n==0) then
       allocate(sl%sn)
       sn => sl%sn
       prev => sn
    else
       sn => sl%sn%prev
       allocate(sn%next)
       prev => sn
       sn => sn%next
    end if
    sl%n=sl%n+1

    nullify(sn%next)
    sn%prev => prev
    sl%sn%prev => sn

    if(present(ent)) ent=sl%n
    sn%c=code
    allocate(sn%s(len))
    call mcp(loc(sn%s),loc(s),len)

    add_str=0

  end function add_str

  integer function find_str(sl,s,target_code,found_code,ptr,node)
    type(t_slist),intent(in)::sl
    character*(*),intent(in)::s
    integer,intent(in),optional::target_code
    integer,intent(out),optional::found_code
    integer,intent(out),optional::ptr
    type(t_sn),intent(out),pointer,optional::node
    integer i,len,tc
    type(t_sn),pointer::sn

    find_str=0
    if(present(found_code)) found_code=-1
    if(present(ptr)) ptr=0
    if(present(node)) nullify(node)
    if(sl%n==0) return
    len=len_trim(s)
    if(len==0) return

    if(present(target_code)) then
       tc=target_code
    else
       tc=-1
    end if

    sn => sl%sn
    do i=1,sl%n
       if(.not.associated(sn)) return
       if(len==size(sn%s).and.is_matched()) then
          find_str=i
          if(present(found_code)) found_code=sn%c
          if(present(ptr)) ptr=loc(sn%s)
          if(present(node)) node => sn
          return
       end if
       sn => sn%next
    end do

  contains

    logical function is_matched()
      integer j
      is_matched=.false.
      if(tc/=-1.and.tc/=sn%c) return
      do j=1,len
         if(sn%s(j)/=ichar(s(j:j))) return
      end do
      is_matched=.true.
    end function is_matched

  end function find_str

  subroutine dump_slist(sl)
    use misc, only: mess,messp
    use memio, only: cpstr,itoa,DISP_FMT_BIN
    type(t_slist),intent(in)::sl
    integer i
    type(t_sn),pointer::sn

    call mess("slist dump:")
    if(sl%n<=0) then
      call mess("(empty)")
      return
    end if

    call mess("#\tLen\tCode\tValue")

    sn => sl%sn
    do i=1,sl%n
       if(.not.associated(sn)) then
          call mess("(no reference)")
       else
          call messp(trim(itoa(i))//":\t"//trim(itoa(size(sn%s)))//"\t")
          call messp("["//trim(itoa(sn%c,cfmt="(B4.4)"))//"]\t")
          call mess(trim(cpstr(loc(sn%s),size(sn%s))))
       end if
       sn => sn%next
    end do

  end subroutine dump_slist

end module slist
