!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2011-2013 by Kazuaki Kumagai                            *
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
  use fzcerr
  implicit none

  private

  public get_str_ptr
  interface rm_str
     module procedure rm_str_s
     module procedure rm_str_k
  end interface
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

  integer,parameter::hdr_size=1
  integer,parameter::str_off =1
  type t_sn
     integer*1,allocatable::s(:) ! code*1+str*n
     type(t_sn),pointer::next => null()
     type(t_sn),pointer::prev => null() ! head%prev => tail
  end type t_sn

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
       if(allocated(sn_in%s)) then
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
    init_slist%n=0
    nullify(init_slist%sn)
  end function init_slist

  subroutine uinit_sn(sn)
    type(t_sn),intent(inout)::sn
    if(allocated(sn%s)) deallocate(sn%s)
  end subroutine uinit_sn

  subroutine uinit_slist(sl)
    type(t_slist),intent(inout)::sl
    integer i
    type(t_sn),pointer::sn,next
    if(sl%n==0) return
    sn => sl%sn
    do i=1,sl%n
       if(.not.associated(sn)) exit
       call uinit_sn(sn)
       next => sn%next
       deallocate(sn)
       sn => next
    end do
    sl%n=0
    nullify(sl%sn)
  end subroutine uinit_slist

  function kth_node(sl,k)
    ! returns null if failed
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    type(t_sn),pointer::kth_node
    type(t_sn),pointer::sn
    integer i
    nullify(kth_node)
    if(k>sl%n.or.k<=0.or..not.associated(sl%sn)) return
    sn => sl%sn
    do i=2,k
       sn => sn%next
       if(.not.associated(sn)) return
    end do
    kth_node => sn
  end function kth_node

  function match_node(sl,s,c,k)
    ! returns null if failed
    ! if c=0 match for s
    ! else
    ! match for c regardless of s
    type(t_slist),intent(in)::sl
    character*(*),intent(in)::s
    integer*1,intent(in)::c
    integer,intent(out),optional::k
    type(t_sn),pointer::match_node
    type(t_sn),pointer::sn
    integer i
    nullify(match_node)
    if(present(k)) k=0
    if(sl%n==0.or..not.associated(sl%sn)) return
    sn => sl%sn
    do i=1,sl%n
       if(allocated(sn%s).and.is_matched()) then
          match_node => sn
          if(present(k)) k=i
          return
       end if
       sn => sn%next
       if(.not.associated(sn)) return
    end do
  contains
    logical function is_matched()
      integer j
      if(c/=0) then
         is_matched=(c==sn%s(1))
         return
      end if
      is_matched=.false.
      if(len(s)/=size(sn%s)-hdr_size) return
      do j=1,len(s)
         if(sn%s(j+str_off)/=ichar(s(j:j))) return
      end do
      is_matched=.true.
    end function is_matched
  end function match_node

  function append_node(sl,s,c)
    type(t_slist),intent(inout)::sl
    character*(*),intent(in)::s
    integer*1,intent(in)::c
    type(t_sn),pointer::append_node
    type(t_sn),pointer::sn,prev
    if(.not.associated(sl%sn)) then
       sl%n=0
       allocate(sl%sn)
       prev => sl%sn
       sn => sl%sn
    else
       sn => sl%sn%prev
       allocate(sn%next)
       sn => sn%next
    end if
    sl%sn%prev => sn
    sl%n=sl%n+1
    nullify(sn%next)
    call set_sn(sn,s,c)
    append_node => sn
  end function append_node

  subroutine set_sn(sn,s,c)
    use memio, only: mcp
    type(t_sn),intent(inout)::sn
    character*(*),intent(in)::s
    integer*1,intent(in)::c
    integer*1 x
    pointer(px,x)
    allocate(sn%s(len(s)+hdr_size))
    px=loc(sn%s)
    x=c
    call mcp(loc(sn%s)+str_off,loc(s),len(s))
  end subroutine set_sn

  integer function get_str_ptr(sl,ent,code,ptr,len)
    ! ptr maybe 0 even if returns 0
    type(t_slist),intent(in)::sl
    integer,intent(in),optional::ent
    integer,intent(in),optional::code
    integer,intent(out),optional::ptr
    integer,intent(out),optional::len
    type(t_sn),pointer::sn
    integer k
    integer*1 c
    if(present(ent)) then
       k=ent
    else
       k=0
    end if
    if(present(code)) then
       c=code
    else
       c=0
    end if
    if(present(len)) len=0
    if(present(ptr)) ptr=0
    if(k>0.and.k<=sl%n) then
       sn => kth_node(sl,k)
    else if(c/=0) then
       sn => match_node(sl,"",c)
    else
       get_str_ptr=FZCERR_NOENT
       return
    end if
    if(.not.associated(sn)) then
       get_str_ptr=FZCERR_NOENT
       return
    end if
    if(allocated(sn%s)) then
       if(present(ptr)) ptr=loc(sn%s)+str_off
       if(present(len)) len=size(sn%s)-hdr_size
    end if
    get_str_ptr=0
  end function get_str_ptr

  integer function rm_str_s(sl,s,code)
    type(t_slist),intent(inout)::sl
    character*(*),intent(in)::s
    integer,intent(in),optional::code
    type(t_sn),pointer::sn
    integer*1 c
    if(present(code)) then
       c=code
    else
       c=0
    end if
    rm_str_s=FZCERR_NOENT
    if(sl%n==0.or..not.associated(sl%sn)) return
    sn => match_node(sl,s,c)
    if(.not.associated(sn)) return
    call rm_str_sn(sl,sn)
    rm_str_s=0
  end function rm_str_s

  integer function rm_str_k(sl,k)
    type(t_slist),intent(inout)::sl
    integer,intent(in)::k
    type(t_sn),pointer::sn
    rm_str_k=FZCERR_NOENT
    if(k<=0.or.k>sl%n) return
    sn => kth_node(sl,k)
    if(.not.associated(sn)) return
    call rm_str_sn(sl,sn)
    rm_str_k=0
  end function rm_str_k

  subroutine rm_str_sn(sl,sn)
    type(t_slist),intent(inout)::sl
    type(t_sn),intent(inout),pointer::sn
    if(allocated(sn%s)) deallocate(sn%s)
    sn%prev%next => sn%next
    if(associated(sn%next)) then
       sn%next%prev => sn%prev
    else
       sl%sn%prev => sn%prev
    end if
    deallocate(sn)
    sl%n=sl%n-1
    if(sl%n==0) nullify(sl%sn)
  end subroutine rm_str_sn

  subroutine add_str(sl,s,code,ent)
    type(t_slist),intent(inout)::sl
    character*(*),intent(in)::s
    integer,intent(in),optional::code
    integer,intent(out),optional::ent
    type(t_sn),pointer::sn
    integer*1 c
    if(present(code)) then
       c=code
    else
       c=0
    end if
    sn => match_node(sl,s,c,ent)
    if(.not.associated(sn)) then
       sn => append_node(sl,s,c)
       if(present(ent)) ent=sl%n
    else
       call uinit_sn(sn)
       call set_sn(sn,s,c)
    end if
  end subroutine add_str

  subroutine dump_slist(sl)
    use misc, only: mess,messp
    use memio, only: cpstr,itoa
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
          call messp(trim(itoa(i))//":\t" &
               //trim(itoa(size(sn%s)-hdr_size))//"\t" &
               //trim(itoa(int(sn%s(1)),fmt="(Z4.4)"))//"\t")
          call mess(cpstr(loc(sn%s)+str_off,size(sn%s)-hdr_size))
       end if
       sn => sn%next
    end do

  end subroutine dump_slist

end module slist
