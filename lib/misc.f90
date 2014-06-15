!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2012,2014 by Kazuaki Kumagai                            *
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
module misc
  implicit none

  interface is_alpha
     module procedure is_alpha_c
     module procedure is_alpha_i
  end interface

  interface is_number
     module procedure is_number_c
     module procedure is_number_i
  end interface

  interface is_numeric
     module procedure is_numeric_c
     module procedure is_numeric_i
  end interface

  integer,parameter::stdin=5
  integer,parameter::stdout=6
  integer,parameter::stderr=0

contains

  subroutine ins(s,u,i)
    character*(*),intent(out)::s
    integer,intent(in),optional::u
    integer,intent(out),optional::i
    integer unit,istat
    if(present(u)) then
       unit=u
    else
       unit=stdin
    end if
    read(unit,10,iostat=istat) s
    if(present(i)) i=istat
    if(istat/=0) s=""
10  format(a)
  end subroutine ins

  subroutine messp(s,u)
    character*(*),intent(in)::s
    integer,intent(in),optional::u
    integer unit
    if(present(u)) then
       unit=u
    else
       unit=stdout
    end if
    write(unit,10) s
10  format(a,$)
  end subroutine messp

  subroutine mess(s,u)
    character*(*),intent(in)::s
    integer,intent(in),optional::u
    integer unit
    if(present(u)) then
       unit=u
    else
       unit=stdout
    end if
    write(unit,10) s
10  format(a)
  end subroutine mess

  pure logical function is_set(a,x)
    integer,intent(in)::a,x
    is_set=(iand(a,x)/=0)
  end function is_set

  pure logical function is_not_set(a,x)
    integer,intent(in)::a,x
    is_not_set=(iand(a,x)==0)
  end function is_not_set

  pure subroutine set_flg(a,x)
    integer,intent(inout)::a
    integer,intent(in)::x
    a=ior(a,x)
  end subroutine set_flg

  pure subroutine cle_flg(a,x)
    integer,intent(inout)::a
    integer,intent(in)::x
    a=iand(a,not(x))
  end subroutine cle_flg

  pure integer function get_lo32(cid)
    integer,intent(in)::cid
    get_lo32=iand(cid,int(Z"FFFF",kind=4))
  end function get_lo32

  pure integer function get_up32(cid)
    integer,intent(in)::cid
    get_up32=iand(ishft(cid,-16),int(Z"FFFF",kind=4))
  end function get_up32

  pure integer function get_i32(lo,up)
    integer,intent(in)::lo,up
    get_i32=ior(lo,ishft(up,16))
  end function get_i32

  pure logical function is_symbol(a)
    integer,intent(in)::a
    select case(a)
    case(95,36) ! _, $
       is_symbol=.true.
    case default
       is_symbol=.false.
    end select
  end function is_symbol

  pure logical function is_alpha_c(c)
    character*1,intent(in)::c
    is_alpha_c=is_alpha_i(ichar(c))
  end function is_alpha_c
  pure logical function is_alpha_i(i)
    integer,intent(in)::i
    integer j
    j=ior(i,32)
    is_alpha_i=(j>=97.and.j<=122)
  end function is_alpha_i

  pure logical function is_hex_number(a)
    integer,intent(in)::a
    is_hex_number=(a>=65.and.a<=70).or.(a>=97.and.a<=102).or.is_number(a)
  end function is_hex_number

  pure logical function is_oct_number(a)
    integer,intent(in)::a
    is_oct_number=(a>=48.and.a<=55)
  end function is_oct_number

  pure logical function is_bin_number(a)
    integer,intent(in)::a
    is_bin_number=(a>=48.and.a<=49)
  end function is_bin_number

  pure logical function is_number_c(c)
    character*1,intent(in)::c
    is_number_c=is_number_i(ichar(c))
  end function is_number_c
  pure logical function is_number_i(i)
    integer,intent(in)::i
    is_number_i=(i>=48.and.i<=57)
  end function is_number_i

  pure logical function is_numeric_c(c)
    character*1,intent(in)::c
    is_numeric_c=is_numeric_i(ichar(c))
  end function is_numeric_c
  pure logical function is_numeric_i(i)
    integer,intent(in)::i
    is_numeric_i=(is_number_i(i).or.i==46)
  end function is_numeric_i

  pure character*4 function i2str(i)
    integer,intent(in)::i
    i2str=log2str(i/=0)
  end function i2str

  pure character*4 function log2str(log)
    logical,intent(in)::log
    if(log) then
       log2str="on"
    else
       log2str="off"
    end if
  end function log2str

  subroutine c2fstr(pstr,s)
    use iso_c_binding, only: C_SIZE_T
    integer(C_SIZE_T),intent(in),value::pstr
    character*(*),intent(out)::s
    integer i
    character*1 c
    pointer(p,c)
    s=""
    p=pstr-1
    do i=1,len(s)
       p=p+1
       if(c==char(0)) exit
       s(i:i)=c
    end do
  end subroutine c2fstr

  integer function replace(s1,p,ln,s2,ln2)
    character*(*),intent(inout)::s1
    character*(*),intent(in)::s2
    integer,intent(in),optional::ln2
    integer ls2,ls1
    integer,intent(in)::p,ln ! p > 0, len >= 0
    replace=0
    if(p<=0.or.ln<0) return
    if(present(ln2)) then
       ls2=ln2
    else
       ls2=len_trim(s2)
    end if
    if(p+ls2-ln>len(s1)) return
    ls1=len_trim(s1)
    s1(p:)=s2(1:ls2)//s1(p+ln:ls1)
    replace=ls2+ls1-ln
  end function replace

  integer function strip(sin,sout)
    character*(*),intent(in)::sin
    character*(*),intent(out)::sout
    integer i,k,wc
    logical first
    k=0
    wc=0
    first=.true.
    do i=1,len(sin)
       select case(sin(i:i))
       case(" ","\t")
          if(first) cycle
          wc=wc+1
          if(wc>1) cycle
       case(char(0))
          exit
       case default
          wc=0
          first=.false.
       end select
       k=k+1
       sout(k:k)=sin(i:i)
    end do
    if(wc/=0) k=k-1 ! remove the last blank
    strip=k
  end function strip

  integer function get_open_unit()
    integer unit
    logical opened
    get_open_unit=-1
    do unit=10,99
       inquire(unit=unit,opened=opened)
       if(.not.opened) then
          get_open_unit=unit
          return
       end if
    end do
  end function get_open_unit

  integer function open_file(f,print_error,ask_overwrite,read_only,stat,acce)
    character*(*),intent(in)::f
    logical,intent(in),optional::print_error,ask_overwrite,read_only
    character*(*),intent(in),optional::stat,acce
    integer istat,unit
    logical exist
    character ans
    character(len=16) s,a,c
    character(len=256) iomsg

    if(present(stat)) then
       s=stat
    else
       s="unknown"
    end if
    if(present(acce)) then
       a=acce
    else
       a="sequential"
    end if
    if(present(read_only).and.read_only) then
       c="read"
    else
       c="readwrite"
    end if

    open_file=0
    if(present(ask_overwrite).and.ask_overwrite) then
       inquire(file=f,exist=exist)
       if(exist) then
          call messp("overwrite: "//trim(f)//" ? => ")
          call ins(ans)
          if(ans/="Y".and.ans/="y") return
       end if
    end if

    unit=get_open_unit()
    open(unit=unit,file=f,iostat=istat,status=s,iomsg=iomsg,access=a,action=c)
    if(istat/=0) then
       if(present(print_error).and.print_error)&
            call mess("*** open_file: Error: opening file: "//trim(f)//"\n" &
            //"*** "//trim(iomsg))
       open_file=0
       return
    else
       open_file=unit
    end if
  end function open_file

end module misc
