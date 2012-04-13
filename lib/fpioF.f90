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
module fpio
  use memio
  implicit none
  public
  integer,parameter::sp=selected_real_kind(6,37)
  integer,parameter::dp=selected_real_kind(15,307)
  integer,parameter::ep=selected_real_kind(18)
  integer,parameter::qp=selected_real_kind(33,4931)
#if !defined _NO_REAL16_
  integer,parameter::rp=qp
  integer,parameter::cp=qp
  character*(*),parameter::cfmt="(ES41.33e4)"
  real(rp),parameter::int_max=2.0_rp**63.0_rp-1.0_rp
#elif !defined _NO_REAL10_
  integer,parameter::rp=ep
  integer,parameter::cp=ep
  character*(*),parameter::cfmt="(ES26.18e4)"
  real(rp),parameter::int_max=2.0_rp**63.0_rp-1.0_rp
#else
#define _RP_IS_DP_
  integer,parameter::rp=dp
  integer,parameter::cp=dp
  real(rp),parameter::int_max=2.0_rp**31_rp-1.0_rp
  character*(*),parameter::cfmt="(ES25.16e4)"
#endif
  real(rp),parameter::rzero=0.0_rp
  real(rp),parameter::runit=1.0_rp
  integer,parameter::min_digit=3
  integer,parameter::max_digit=precision(rzero)
  real(rp),parameter::eps=epsilon(rzero)
  complex(cp),parameter::cunit=complex(rzero,runit)
  complex(cp),parameter::czero=complex(rzero,rzero)
  real(rp),public,parameter::pi =4.0_rp*atan(1.0_rp)
  real(rp),public,parameter::pi2=2.0_rp*pi
  real(rp),public,parameter::pi_2=pi/2.0_rp
  complex,public,parameter::ztrue=complex(1.0_rp,rzero)
  complex,public,parameter::zfalse=czero

  integer,parameter::si_prefix_off=9
  integer,parameter::si_prefix_max=24
  character*(*),parameter::si_prefix="yzafpnum kMGTPEZY"

  integer,parameter::DISP_FMT_RAW=-1
  integer,parameter::LEN_STR_ANS_MAX=128
  ! |  flag | base | digit|
  ! | FFFFF | B    | DD   |
  integer,parameter:: digit_mask=not(Z"FF")
  integer,parameter:: base_mask=not(Z"F00")
  integer,parameter:: X2A_ALLOW_ORDINARY = Z"00001000"
  integer,parameter:: X2A_TRIM_ZERO      = Z"00002000"
  integer,parameter:: X2A_SHOW_E0        = Z"00004000"
  integer,parameter:: X2A_FIX            = Z"00008000"
  integer,parameter:: X2A_ENG            = Z"00010000"
  integer,parameter:: X2A_DMS            = Z"00020000"
  integer,parameter:: X2A_RAW            = Z"00040000"
  integer,parameter:: X2A_DEFAULT        = ior(max_digit,ishft(IBASE_DEC,8))

  character*(*),parameter,private::NAN_STR="NaN"

  interface is_integer
     module procedure is_integer_z
     module procedure is_integer_x
  end interface

contains

  ! workaround
  real(rp) function nan()
    real(rp) a,b
    a=rzero
    b=rzero
    nan=a/b
  end function nan

  real(rp) function atox(a,stat)
    character*(*),intent(in)::a
    integer,intent(out),optional::stat
    integer s
    read(a,*,iostat=s) atox
    if(present(stat)) stat=s
  end function atox

  integer function get_digit(flg)
    integer,intent(in)::flg
    get_digit=iand(flg,not(digit_mask))
  end function get_digit

  integer function get_base(flg)
    integer,intent(in)::flg
    get_base=ishft(iand(flg,not(base_mask)),-8)
  end function get_base

  subroutine set_digit(flg,n)
    integer,intent(inout)::flg
    integer,intent(in)::n
    flg=ior(iand(digit_mask,flg),n)
  end subroutine set_digit

  subroutine set_base(flg,n)
    integer,intent(inout)::flg
    integer,intent(in)::n
    integer m
    if(n==16) then
       m=IBASE_HEX
    else if(n>1.and.n<16) then
       m=n
    else
       return
    end if
    flg=ior(iand(base_mask,flg),ishft(m,8))
  end subroutine set_base

  logical function is_integer_z(z,n)
    complex(cp),intent(in)::z
    integer*8,intent(out),optional::n
    is_integer_z=.false.
    if(imagpart(z)/=rzero) return
    is_integer_z=is_integer_x(realpart(z),n)
  end function is_integer_z

  logical function is_integer_x(x,n)
    real(rp),intent(in)::x
    integer*8,intent(out),optional::n
    integer*8 m
    real(rp) d
    is_integer_x=.false.
    if(abs(x)>int_max) return
    m=int(x,kind=8)
    d=x-real(m,kind=8)
    if(d==rzero) then
       is_integer_x=.true.
       if(present(n)) n=m
    end if
  end function is_integer_x

  character(LEN_STR_ANS_MAX) function trim_zero(a)
    character*(*),intent(in)::a
    integer i,k,ka,ke
    k=index(a,"e")
    if(k==0) k=index(a,"E")
    if(k/=0) then
       ke=k
       k=k-1
    else
       ke=0
       k=len_trim(a)
    end if
    ka=0
    do i=k,1,-1
       select case(a(i:i))
       case("0")
          cycle
       case(".")
          ka=i+1
          exit
       case default
          ka=i
          exit
       end select
    end do
    if(ka==0) then
       trim_zero="0"
       return
    end if
    trim_zero=a(1:ka)
    if(ke/=0) then
       trim_zero(ka+1:)=trim(a(ke:))
    end if
  end function trim_zero

  character(LEN_STR_ANS_MAX) function rtoa(x,fmt)
    real(rp),intent(in)::x
    integer,intent(in),optional::fmt
    integer istat,f
    integer*8 n
    if(present(fmt)) then
       f=fmt
    else
       f=X2A_DEFAULT
    end if
    if(f==DISP_FMT_RAW) then
       if(x/=rzero) then
          write(rtoa,*,iostat=istat) x
          if(istat==0) rtoa=adjustl(rtoa)
          call rmzero(rtoa)
       else
          rtoa="0"
       end if
       return
    else if(is_integer(x,n)) then
       rtoa=itoa(n,get_base(f))
       return
    end if
    rtoa=xtoa(x,f)
  end function rtoa

  character(LEN_STR_ANS_MAX) function ztoa(z,fmt)
    use misc, only: is_set
    complex(cp),intent(in)::z
    integer,intent(in),optional::fmt
    if(present(fmt)) then
       if(fmt==DISP_FMT_RAW) then
          ztoa="( "//trim(rtoa(realpart(z),fmt))//", "&
               //trim(rtoa(imagpart(z),fmt))//" )"
          return
       else if(is_set(fmt,X2A_DMS)) then
          call todms
          return
       end if
    end if
    if(imagpart(z)/=rzero) then
       ztoa=trim(rtoa(imagpart(z),fmt))//" i"
       if(realpart(z)/=rzero) then
          if(imagpart(z)>rzero) then
             ztoa=" + "//trim(ztoa)
          else if(ztoa(1:1)=="-") then
             ztoa=" - "//trim(ztoa(2:))
          else
             ztoa=" - "//trim(ztoa)
          end if
       else
          return
       end if
    else
       ztoa=""
    end if
    ztoa=trim(rtoa(realpart(z),fmt))//trim(ztoa)

    contains

      subroutine todms
        integer d,m
        real(rp) x,s
        character*(*),parameter::ifmt="(i2.2)"
        x=realpart(z)
        d=x/(60.0_rp*60.0_rp)
        m=(x-(d*60.0_rp*60._rp))/60.0_rp
        s=x-60.0_rp*(m+d*60.0_rp)
        ztoa=trim(itoa(d))//":"//trim(itoa(m))//":"//trim(rtoa(s,fmt))
      end subroutine todms

  end function ztoa

  subroutine rmzero(s)
    character*(*),intent(inout)::s
    integer i,k,j
    integer p
    p=index(s,"e")
    if(p==0) p=index(s,"E")
    if(p==0) then
       k=len_trim(s)
    else
       k=p-1
    end if
    j=0
    do i=k,1,-1
       select case(s(i:i))
       case("0")
          s(i:i)=" "
       case(".")
          s(i+1:i+1)="0"
          j=i+1
          exit
       case default
          j=i
          exit
       end select
    end do
    if(j==0) then
       s="0"
    else if(p/=0) then
       s(j+1:)=s(p:)
    end if
  end subroutine rmzero

!!$  character(LEN_STR_ANS_MAX) function rtoha(r)
!!$    real(rp),intent(in)::r
!!$    real(rp) f
!!$    integer i,n,m,k
!!$    n=r
!!$    rtoha=trim(itoa(n,fmt=DISP_FMT_HEX))//"."
!!$    k=len_trim(rtoha)
!!$    f=r-n
!!$    do i=1,10
!!$       if(f==rzero) exit
!!$       f=f*16
!!$       m=f
!!$       k=k+1
!!$       if(m<=9) then
!!$          rtoha(k:k)=achar(m+z"30")
!!$       else
!!$          rtoha(k:k)=achar(m+z"41"-10)
!!$       end if
!!$       f=f-m
!!$    end do
!!$  end function rtoha

#define is_set(x) (iand(opt,(x))/=0)
#define is_uset(x) (iand(opt,(x))==0)

  character(LEN_STR_ANS_MAX) function xtos(x,e,dot,neg)
    real(rp),intent(in)::x
    integer,intent(out),optional::e
    logical,intent(in),optional::dot
    logical,intent(out),optional::neg
    logical s
    real(rp) xx
    integer istat
    integer p1
    ! 12345..
    ! +x.xx..E+xxxx
    if(isnan(x)) then
       xtos=NAN_STR
       return
    end if
    xtos=""
    p1=2
    if(x>=rzero) then
       xx=x
       s=.false.
    else
       xx=-x
       s=.true.
    end if
    if(present(neg)) then
       p1=1
       neg=s
    else
       p1=2
    end if

    write(xtos(p1:),cfmt,iostat=istat) xx
    if(istat/=0) return
    if(xtos(p1:p1)=="*") then
       write(xtos(p1:),*,iostat=istat) xx
       if(istat/=0) return
    end if
    xtos(p1:)=adjustl(xtos(p1:))
    if(p1==2) then
       if(.not.s) then
          xtos(1:1)="+"
       else
          xtos(1:1)="-"
       end if
    end if
    if(present(e)) call get_exp()
    if(present(dot).and..not.dot) xtos=xtos(1:p1)//xtos(p1+2:len_trim(xtos))

  contains

    subroutine get_exp()
      integer k
      e=0
      k=index(xtos(p1:),"E")
      if(k==0) return
      read(xtos(k+p1-1+1:),*,iostat=istat) e
      if(istat==0) xtos(k+p1-1:)=" "
    end subroutine get_exp

  end function xtos

  character(LEN_STR_ANS_MAX) function xtoa(x,opt)
    real(rp),intent(in)::x
    integer,intent(in)::opt
    integer sd
    character(LEN_STR_ANS_MAX) ns
    integer p1,p2
    integer k,d,dd
    logical cr,neg
    integer*1 c
    integer e,ee,r
    integer len,len0
    pointer(pc,c)
    if(x==rzero.and.is_set(X2A_ALLOW_ORDINARY)) then
       xtoa="0"
       return
    end if
    xtoa=""
    ns=xtos(x,e,.false.,neg)
    if(ns==NAN_STR) then
       xtoa=NAN_STR
       return
    end if
    if(neg) then
       p1=2
       xtoa(1:1)="-"
    else
       p1=1
    end if
    p2=len_trim(ns)
! x.xxxx...xE+xxxx
    len=len_trim(ns)
    len0=len_trim0(ns)
    if(is_set(X2A_ALLOW_ORDINARY).and.e>=0.and.e<max_digit) then
       if(e>=len0-1) then
          ! integer
          xtoa(p1:)=ns(1:1+e)
          return
       end if
    end if

    sd=iand(opt,int(Z"FF"))
    if(sd<=0) sd=max_digit
    if(is_uset(X2A_FIX)) then
       sd=min(max(sd,min_digit),len)
       d=sd
    else
       d=e+1+sd
       if(d<1) then
          if(d==0.and.ichar(ns(1:1))>=z"35") then
             ! 0.09 => 0.1
             xtoa(p1:)="0."//repeat("0",sd-1)//"1"
          else
             xtoa(p1:)="0."//repeat("0",sd)
          end if
          return
       end if
   end if

    if(d<len) then
       pc=loc(ns)+d
       cr=.false.
       if(c>=z"35") then
          do k=1,d
             pc=pc-1
             if(c/=z"39") then
                c=c+1
                cr=.false.
             else
                c=z"30"
                cr=.true.
             end if
             if(.not.cr) exit
          end do
       end if
       if(cr) then
          ! 9.99... => 10.0...
          ns="1"//ns(1:d-1)
          e=e+1
          if(is_set(X2A_FIX)) then
             d=d+1
             ns(d:d)="0"
          end if
       end if
    end if
    if(abs(e)<d.and.is_set(X2A_ALLOW_ORDINARY).or.is_set(X2A_FIX)) then
       if(e>=0) then
          if(e+2<=d) then
             xtoa(p1:)=ns(1:1+e)//"."//ns(1+e+1:d)
             if(is_uset(X2A_FIX)) call trimz
          else
             xtoa(p1:)=ns(1:d)
          end if
       else
          xtoa(p1:)="0."//repeat("0",-e-1)//ns(1:d)
          if(is_uset(X2A_FIX)) call trimz
       end if
       return
    end if

    if(is_set(X2A_TRIM_ZERO)) then
       dd=0
       pc=loc(ns)+d
       do k=sd,1,-1
          pc=pc-1
          if(c/=z"30") then
             dd=k
             exit
          end if
       end do
       if(dd==0) then
          xtoa="0"
          return
       end if
       if(dd==1) dd=2
    else
       dd=d
    end if

    if(is_set(X2A_ENG).and.abs(e)<si_prefix_max+2) then
       r=mod(e,3)
       ee=e/3
       if(e<0) then
          if(r/=0) then
             r=3+r
             ee=ee-1
          end if
       end if
       k=ee+si_prefix_off

       xtoa(p1:)=ns(1:r+1)
       p2=p1+r+1
       if(r+2<=dd) then
          xtoa(p2:)="."//ns(r+2:dd)
          p2=p2+(dd-(r+2)+1)+1
       end if
       if(ee/=0) xtoa(p2:)="_"//si_prefix(k:k)
    else if(e==0.and.is_uset(X2A_SHOW_E0)) then
       xtoa(p1:)=ns(1:1)//"."//ns(2:dd)
    else
       xtoa(p1:)=ns(1:1)//"."//ns(2:dd)//"e"//trim(itoa(e))
    end if

    contains

      subroutine trimz
        integer ii
        do ii=len_trim(xtoa),1,-1
           select case(xtoa(ii:ii))
           case("0")
           case(".")
              xtoa=xtoa(1:ii-1)
              return
           case default
              xtoa=xtoa(1:ii)
              return
           end select
        end do
        xtoa="0"
      end subroutine trimz

  end function xtoa

  integer function len_trim0(a)
    character*(*),intent(in)::a
    integer i
    do i=len_trim(a),1,-1
       if(a(i:i)/="0") then
          len_trim0=i
          return
       end if
    end do
    len_trim0=0
  end function len_trim0

!!$  subroutine to_intfrac(x,i,f)
!!$    real(rp),intent(in)::x
!!$    real(rp),intent(out),optional::i
!!$    real(rp),intent(out),optional::f
!!$    character(LEN_STR_ANS_MAX) s
!!$    integer d,e,istat
!!$    s=xtos(x,e,.false.)
!!$    d=len_trim0(s)-1 ! excluding sign
!!$    ! 123.456789
!!$    ! +1234567890...
!!$    ! e=2
!!$    ! d=9
!!$    if(present(i)) then
!!$       if(e>=0) then
!!$          read(s(2:e+2),*,iostat=istat) i
!!$          f=x-i
!!$       else
!!$          i=rzero
!!$          f=x
!!$       end if
!!$    end if
!!$  end subroutine to_intfrac

end module fpio
