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
     type(t_sn),pointer::next => null()
     type(t_sn),pointer::prev => null()
  end type t_sn

  type,public::t_slist
     integer::n = 0  ! allocated sn, sn%s may not be allocated
     type(t_sn),pointer::sn => null() 
  end type t_slist
  
  integer,parameter::SLERR_NOMEM = 1
  integer,parameter::SLERR_MEMOV = 2
  integer,parameter::SLERR_NOENT = 3
  integer,parameter,public::SLERR_END   = 3

  !
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
    sn_in => sl%sn
    nullify(prev)
    do i=1,sl%n
       sn%code=sn_in%code
       if(size(sn_in%s)>0) then
          allocate(sn%s(size(sn_in%s)))
          call mcp(loc(sn%s),loc(sn_in%s),size(sn%s))
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
  end function cp_slist

  function init_slist()
    type(t_slist) init_slist
    init_slist%n=0
  end function init_slist
 
  subroutine uinit_slist(sl)
    type(t_slist),intent(inout)::sl
    integer i
    type(t_sn),pointer::sn,next
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
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    type(t_sn),pointer::kth_node
    type(t_sn),pointer::cur
    integer i
    nullify(kth_node)
    if(k>sl%n.or.k<=0) return
    if(.not.associated(sl%sn)) return
    cur => sl%sn
    do i=2,k
       cur => cur%next
       if(.not.associated(cur)) return
    end do
    kth_node => cur
  end function kth_node

  function last_node(sl,k)
    type(t_slist),intent(in)::sl
    integer,intent(out)::k
    type(t_sn),pointer::last_node
    type(t_sn),pointer::cur
    integer i
    nullify(last_node)
    k=0
    if(.not.associated(sl%sn)) return
    cur => sl%sn
    do i=1,sl%n
       if(.not.associated(cur%next)) then
          k=i
          last_node => cur
          return
       end if
       cur => cur%next
    end do
  end function last_node
  
  integer function get_str_ptr(sl,k,ptr,len,code)
    type(t_slist),intent(in)::sl
    integer,intent(in)::k
    integer,intent(out)::ptr
    integer,intent(out),optional::len
    integer,intent(out),optional::code
    type(t_sn),pointer::sn

    if(present(len)) len=0
    if(sl%n<k.or.k<=0) then
       get_str_ptr=SLERR_NOENT
       return
    end if
   
    sn => kth_node(sl,k)
    if(.not.associated(sn)) then
       get_str_ptr=SLERR_NOENT
       return
    end if

    ptr=loc(sn%s)
    if(present(len)) len=size(sn%s)
    if(present(code)) code=sn%code

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
    code=sn%code
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
    sn%code=code
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
    sn%code=ior(sn%code,code)
    add_sc=0
  end function add_sc
  
  integer function rm_str(sl,s,ent)
    use memio
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

  integer function add_str(sl,s,code,ent)
    use memio, only: mcp
    type(t_slist),intent(inout)::sl
    character*(*),intent(in)::s
    integer,intent(in)::code
    integer,intent(out),optional::ent
    integer len
    integer k
    type(t_sn),pointer::sn,prev

    if(present(ent)) ent=0
    len=len_trim(s)

    sn => last_node(sl,k) ! k must be sl%n    

    if(k==0) then
       allocate(sl%sn)
       sn => sl%sn
       nullify(prev)
    else
       allocate(sn%next)
       prev => sn
       sn => sn%next
    end if
    sl%n=sl%n+1

    nullify(sn%next)
    sn%prev => prev

    if(present(ent)) ent=sl%n
    sn%code=code

    add_str=0

    if(len==0) return

    allocate(sn%s(len))
    call mcp(loc(sn%s),loc(s),len)

  end function add_str

  integer function find_str(sl,s,target_code,found_code,ptr,node)
    type(t_slist),intent(in)::sl
    character*(*),intent(in)::s
    integer,intent(in),optional::target_code
    integer,intent(out),optional::found_code
    integer,intent(out),optional::ptr
    type(t_sn),intent(out),pointer,optional::node
    integer i
    integer len
    integer lenp
    integer cd
    integer tc
    type(t_sn),pointer::sn

    find_str=0
    if(present(found_code)) found_code=-1
    if(present(ptr)) ptr=0
    if(present(node)) nullify(node)

    len=len_trim(s)
    if(present(target_code)) then
       tc=target_code
    else
       tc=-1
    end if

    sn => sl%sn
    do i=1,sl%n
       if(.not.associated(sn)) return
       lenp=size(sn%s)
       if(len==lenp.and.is_matched()) then
          find_str=i
          if(present(found_code)) found_code=cd
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
      cd=sn%code
      if(tc/=-1.and.tc/=cd) return
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
    integer len
    type(t_sn),pointer::sn

    call mess("slist dump:")
    if(sl%n<=0) then
      call mess("(empty)")
      return
    end if

    call mess("#\tLen\tCode\tValue")

    sn => sl%sn
    do i=1,sl%n
       if(.not.associated(sn)) exit
       call messp(trim(itoa(i))//":\t"//trim(itoa(size(sn%s)))//"\t")
       call messp("["//trim(itoa(sn%code,DISP_FMT_BIN))//"]\t")
       call mess(trim(cpstr(loc(sn%s),len)))
       sn => sn%next
    end do

  end subroutine dump_slist

end module slist
