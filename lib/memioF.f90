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
module memio
  implicit none
  public

  integer,parameter::IBASE_RAW  =  0
  integer,parameter::IBASE_BIN  =  1
  integer,parameter::IBASE_OCT  =  2
  integer,parameter::IBASE_DEC  =  3
  integer,parameter::IBASE_HEX  =  4

#ifndef _USE32_
  integer,parameter::ptrsz=selected_int_kind(20)
#else
  integer,parameter::ptrsz=selected_int_kind(10)
#endif

#ifndef _NO_ASM_
  interface
     subroutine mcp(dst,src,len)
       integer,intent(in),value::dst,src,len
     end subroutine mcp
  end interface
  interface
     subroutine mcle(dst,len)
       integer,intent(in),value::dst,len
     end subroutine mcle
  end interface
#endif

  interface atoi
     module procedure atoi_4
     module procedure atoi_8
  end interface

  interface itoa
     module procedure itoa_4
     module procedure itoa_8
  end interface

contains

  function cpstr(ptr,len)
    integer,intent(in)::ptr
    integer,intent(in)::len
    character(len=len) cpstr
    if(len==0.or.ptr==0) return
    call mcp(loc(cpstr),ptr,len)
  end function cpstr

  integer*4 function atoi_4(a,n_dummy,base,ist)
    character*(*),intent(in)::a
    integer*4,intent(in)::n_dummy
    integer,intent(in),optional::base
    integer,intent(out),optional::ist
    atoi_4=int(atoi_8(a,0_8,base,ist),kind=4)
  end function atoi_4

  integer*8 function atoi_8(a,n_dummy,base,ist)
    character*(*),intent(in)::a
    integer*8,intent(in)::n_dummy
    integer,intent(in),optional::base
    integer,intent(out),optional::ist
    integer istat,b
    character*32 f
    if(present(base)) then
       b=base
    else
       b=IBASE_RAW
    end if
    select case(b)
    case(10,IBASE_RAW)
       read(a,*,iostat=istat) atoi_8
       if(present(ist)) ist=istat
       return
    case(IBASE_BIN)
       f="(B"
    case(IBASE_OCT)
       f="(O"
    case(IBASE_HEX)
       f="(Z"
    case default
       STOP "*** atoi_8: ERROR: NOIMPL"
    end select
    f=trim(f)//trim(itoa(len_trim(a)))//")"
    read(a,f,iostat=istat) atoi_8
    if(present(ist)) ist=istat
  end function atoi_8

  character*32 function itoa_4(i,base,fmt,len)
    integer*4,intent(in)::i
    integer,intent(in),optional::base
    character*(*),intent(in),optional::fmt
    integer,intent(in),optional::len
    itoa_4=itoa_8(int(i,kind=8),base,fmt,len)
  end function itoa_4

  character*32 function itoa_8(i,base,fmt,len)
    integer*8,intent(in)::i
    integer,intent(in),optional::base
    character*(*),intent(in),optional::fmt
    integer,intent(in),optional::len
    integer b,istat
    character*32 f
    itoa_8=""
    if(present(base)) then
       b=base
    else
       b=IBASE_DEC
    end if
    if(present(fmt)) then
       f=fmt
    else
       select case(b)
       case(IBASE_RAW)
          write(itoa_8,*,iostat=istat) i
          if(istat==0) call adj
          return
       case(IBASE_DEC)
          f="(I0)"
       case(IBASE_HEX)
          f="(Z0)"
       case(IBASE_BIN)
          f="(B0)"
       case(IBASE_OCT)
          f="(O0)"
       case default
          itoa_8=int_to_str(i,b)
       end select
    end if
    write(itoa_8,f,iostat=istat) i
    if(istat==0) call adj
  contains
    subroutine adj
      integer lp
      itoa_8=adjustl(itoa_8)
      if(.not.present(len)) return
      lp=len-len_trim(itoa_8)
      if(lp<=0) return
      itoa_8=repeat(" ",lp)//trim(itoa_8)
    end subroutine adj
  end function itoa_8

#ifdef _NO_ASM_
  subroutine mcp(dst,src,len)
    integer,intent(in)::dst,src,len
    integer*1 s,d
    integer i
    pointer(si,s)
    pointer(di,d)
    si=src-1
    di=dst-1
    do i=1,len
       si=si+1
       di=di+1
       d=s
    end do
  end subroutine mcp

  subroutine mcle(dst,len)
    integer,intent(in)::dst,len
    integer*1 d
    integer i
    pointer(di,d)
    di=dst-1
    do i=1,len
       di=di+1
       d=int(0,kind=1)
    end do
  end subroutine mcle
#endif

  integer function str_len_trim(str)
    character*(*),intent(in)::str
    integer i
    integer*1 c
    pointer(pc,c)
    str_len_trim=0
    pc=loc(str)-1
    do i=1,len(str)
       pc=pc+1
       select case(c)
       case(0)
          str_len_trim=i-1
          return
       case(32,9)
       case default
          str_len_trim=i
       end select
    end do
  end function str_len_trim

  character*32 function int_to_str(n,b)
    integer*8,intent(in)::n
    integer,intent(in)::b ! b must be <= 16 and > 1
    character*(*),parameter::f="0123456789ABCDEF"
    integer r,i,m,k,p1
    if(n==0) then
       int_to_str="0"
       return
    end if
    if(n>0) then
       p1=1
       m=n
       int_to_str=""
    else
       p1=2
       m=-n
       int_to_str="-"
    end if
    k=len(int_to_str)+1
    do while(m>0)
       i=m/b
       r=m-i*b+1
       k=k-1
       int_to_str(k:k)=f(r:r)
       m=i
    end do
    if(k>1) int_to_str(p1:)=int_to_str(k:)
  end function int_to_str

end module memio
