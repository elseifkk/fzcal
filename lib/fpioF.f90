!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2011-2014 by Kazuaki Kumagai                            *
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
  integer,parameter::ip=8 ! integer kind; 8-byte
  integer(ip),parameter::int_max=huge(int(0,kind=ip))
#if !defined _NO_REAL16_
  integer,parameter::rp=qp
  integer,parameter::cp=qp
  character*(*),parameter::cfmt="(ES41.33e4)"
#elif !defined _NO_REAL10_
  integer,parameter::rp=ep
  integer,parameter::cp=ep
  character*(*),parameter::cfmt="(ES26.18e4)"
#else
#define _RP_IS_DP_
  integer,parameter::rp=dp
  integer,parameter::cp=dp
  character*(*),parameter::cfmt="(ES25.16e4)"
#endif
  real(rp),parameter::rzero=0.0_rp
  real(rp),parameter::runit=1.0_rp
  integer,parameter::min_digit=1
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
  integer,parameter:: X2A_DEFAULT        = ior(max_digit,ishft(IBASE_RAW,8))

  character*(*),parameter,private::NAN_STR="NaN"
  character*(*),parameter,private::INF_STR="Inf"

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
  ! workaround
  real(rp) function inf()
    inf=exp(huge(rzero))
  end function inf

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
    flg=ior(iand(flg,base_mask),ishft(n,8))
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

  character(LEN_STR_ANS_MAX) function rtoa(x,fmt)
    use misc, only: is_set
    real(rp),intent(in)::x
    integer,intent(in),optional::fmt
    integer istat,f,b
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
    else 
       b=get_base(f)
       if(is_integer(x,n).and.(is_set(f,X2A_ALLOW_ORDINARY).or.b>0)) then
          rtoa=itoa(n,b)
          return
       end if
    end if
    rtoa=xtoa(x,f)
  end function rtoa

  character(LEN_STR_ANS_MAX) function ztoa(z,fmt)
    use misc, only: is_set
    complex(cp),intent(in)::z
    integer,intent(in),optional::fmt

    if(isnan(realpart(z))) then
       ztoa=NAN_STR
       return
    else if(abs(realpart(z))>huge(rzero)) then
       ztoa=INF_STR
       return
    end if

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
        character*32 dd,ss
        x=realpart(z)
        d=x/(60.0_rp*60.0_rp)
        m=(x-(d*60.0_rp*60._rp))/60.0_rp
        s=x-60.0_rp*(m+d*60.0_rp)
        dd=trim(itoa(d))
        if(len_trim(dd)==1) dd="0"//trim(dd)
        ss=trim(rtoa(s,fmt))
        if(ss(2:2)==".".or.len_trim(ss)==1) ss="0"//trim(ss)
        ztoa=trim(dd)//":"//trim(itoa(m,fmt="(i2.2)"))//":"//trim(ss)
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
    use misc, only: is_set,is_not_set
    real(rp),intent(in)::x
    integer,intent(in)::opt
    logical fix
    integer sd
    character(LEN_STR_ANS_MAX) ns
    integer p1,p2
    integer k,d,dd
    logical cr,neg
    integer e,ee,r
    integer len,len0

    if(x==rzero.and.is_set(opt,X2A_ALLOW_ORDINARY)) then
       xtoa="0"
       return
    end if
    xtoa=""
    ns=xtos(x,e,.false.,neg)
    if(neg) then
       p1=2
       xtoa(1:1)="-"
    else
       p1=1
    end if
    p2=len_trim(ns)
! x.xxxx...xE+xxxx
    len=len_trim(ns)
    call rm9s(ns,len)
    len0=len_trim0(ns)
    if(is_set(opt,X2A_ALLOW_ORDINARY).and.e>=0.and.e<max_digit) then
       if(e>=len0-1) then
          ! integer
          xtoa(p1:)=ns(1:1+e)
          return
       end if
    end if

    fix=is_set(opt,X2A_FIX)
    if(e>max_digit) fix=.false.

    sd=iand(opt,int(Z"FF"))
    if(.not.fix) then
       if(sd==0) sd=max_digit
       sd=min(max(sd,min_digit),len)
       d=sd
    else
       if(sd>max_digit) sd=max_digit
       d=e+1+sd
       if(d<1) then
          if(d==0.and.ichar(ns(1:1))>=z"35") then
             ! 0.09 => 0.1
             if(sd>0) then
                xtoa(p1:)="0."//repeat("0",sd-1)//"1"
             else
                xtoa(p1:)="1"
             end if
          else if(sd>0) then
             xtoa(p1:)="0."//repeat("0",sd)
          else
             xtoa(p1:)="0"
          end if
          return
       end if
    end if
    if(d<len) then
       if(ichar(ns(d+1:d+1))>=z"35") ns(d+1:d+1)="9"
       cr=adc(ns,d+1)
       if(cr) then
          ! 9.99... => 10.0...
          ns="1"//ns(1:d-1)
          e=e+1
          if(fix) then
             d=d+1
             ns(d:d)="0"
          end if
       end if
    end if

    if(abs(e)<d.and.is_set(opt,X2A_ALLOW_ORDINARY).or.fix) then
       if(e>=0) then
          if(e+2<=d) then
             xtoa(p1:)=ns(1:1+e)//"."//ns(1+e+1:d)
             if(.not.fix) call trimz
          else
             xtoa(p1:)=ns(1:d)
          end if
       else
          xtoa(p1:)="0."//repeat("0",-e-1)//ns(1:d)
          if(.not.fix) call trimz
       end if
       return
    end if

    if(is_set(opt,X2A_TRIM_ZERO)) then
       dd=0
       do k=sd,1,-1
          if(ns(d+k-sd:d+k-sd)/="0") then
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

    if(is_set(opt,X2A_ENG).and.abs(e)<si_prefix_max+2) then
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
    else
       xtoa(p1:)=ns(1:1)
       p1=p1+1
       if(dd>1) then
          xtoa(p1:)="."//ns(2:dd)
          p1=p1+dd
       end if
       if(e/=0.or..not.is_not_set(opt,X2A_SHOW_E0)) then
          xtoa(p1:)="e"//trim(itoa(e))
       end if
    end if

    contains

      subroutine rm9s(s,l)
        character*(*),intent(inout)::s
        integer,intent(in)::l
        integer ii,nc,ll
        logical retlog
        ! 1.49999999999993 => 1.5
        nc=0
        if(s(l:l)/="9") then
           ll=l-1
        else
           ll=l
        end if
        do ii=ll,2,-1
           if(s(ii:ii)=="9") then
              nc=nc+1
           else
              if(nc>=2) then
                 if(ll/=l) nc=nc+1
                 s(ii+1:l)=repeat("0",nc)
                 retlog=adc(s,ii)
              end if
              exit
           end if
        end do
      end subroutine rm9s

      recursive function adc(s,i) result(crf)
        character*(*),intent(inout)::s
        integer,intent(in)::i
        integer*1 c
        logical crf
        pointer(pc,c)
        if(i==0) then
           crf=.true.
           return
        end if
        pc=loc(s(i:i))
        if(c==z"39") then
           c=z"30"
           crf=adc(s,i-1)
        else
           c=c+1
           crf=.false.
        end if
      end function adc

      subroutine trimz
        ! 1.00000 => 1
        ! 1.20000 => 1.2
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
