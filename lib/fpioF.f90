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
  integer,parameter::max_digit=33
  character*(*),parameter::cfmt="(ES41.33e4)"
  real(rp),parameter::int_max=2.0_rp**63.0_rp-1.0_rp
#elif !defined _NO_REAL10_
  integer,parameter::rp=ep
  integer,parameter::cp=ep
  integer,parameter::max_digit=18
  character*(*),parameter::cfmt="(ES26.18e4)"
  real(rp),parameter::int_max=2.0_rp**63.0_rp-1.0_rp
#else
#define _RP_IS_DP_
  integer,parameter::rp=dp
  integer,parameter::cp=dp
  integer,parameter::max_digit=18 !15
  real(rp),parameter::int_max=2.0_rp**31_rp-1.0_rp
  character*(*),parameter::cfmt="(ES26.18e4)" !"(ES23.15e4)"
#endif
  integer,parameter::min_digit=3

  real(rp),parameter::eps=epsilon(0.0_rp)
  real(rp),parameter::pre=precision(0.0_rp)
  real(rp),parameter::rzero=0.0_rp
  complex(cp),parameter::czero=complex(rzero,rzero)

  integer,parameter::si_prefix_off=9
  integer,parameter::si_prefix_max=24
  character*(*),parameter::si_prefix="yzafpnum kMGTPEZY"

  integer,parameter::LEN_STR_ANS_MAX=128
  
  integer*8,parameter:: X2A_ALLOW_ORDINARY = Z"0100"
  integer*8,parameter:: X2A_TRIM_ZERO      = Z"0200"
  integer*8,parameter:: X2A_SHOW_E0        = Z"0400"
  integer*8,parameter:: X2A_FIX            = Z"0800"
  integer*8,parameter:: X2A_ENG            = Z"1000"
  integer*8,parameter:: X2A_DEFAULT        = max_digit

  interface is_integer
     module procedure is_integer_z
     module procedure is_integer_x
  end interface

contains

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
    f=X2A_DEFAULT
    n=0
    if(present(fmt)) then
       if(fmt>=0.or.is_integer(x,n)) f=fmt
       if(f==DISP_FMT_NORM) f=X2A_DEFAULT 
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
    end if
    rtoa=""
    if(f>0) then
       rtoa=xtoa(x,f)
    else
       rtoa=itoa(n,f)
    end if
  end function rtoa
  
  character(LEN_STR_ANS_MAX) function ztoa(z,fmt)
    complex(cp),intent(in)::z
    integer,intent(in),optional::fmt
    if(present(fmt).and.fmt==DISP_FMT_RAW) then
       ztoa="( "//trim(rtoa(realpart(z),fmt))//", "&
            //trim(rtoa(imagpart(z),fmt))//" )"
       return
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

  character(LEN_STR_ANS_MAX) function xtos(x,e,dot)
    real(rp),intent(in)::x
    integer,intent(out),optional::e
    logical,intent(in),optional::dot
    real(rp) xx
    logical neg
    integer istat
    ! 12345..
    ! +x.xx..E+xxxx
    xtos=""
    if(x>=rzero) then
       xx=x
       neg=.false.
    else
       xx=-x
       neg=.true.
    end if
    write(xtos(2:),cfmt,iostat=istat) xx
    if(istat/=0) return
    if(xtos(2:2)=="*") then
       write(xtos(2:),*,iostat=istat) xx
       if(istat/=0) return
    end if
    xtos(2:)=adjustl(xtos(2:))
    if(.not.neg) then
       xtos(1:1)="+"
    else
       xtos(1:1)="-"
    end if
    if(present(e)) call get_exp()
    if(present(dot).and..not.dot) xtos=xtos(1:2)//xtos(4:len_trim(xtos))

  contains

    subroutine get_exp()
      integer k
      e=0
      k=index(xtos(2:),"E")
      if(k==0) return
      read(xtos(k+1+1:),*,iostat=istat) e
      if(istat==0) xtos(k+1:)=" "
    end subroutine get_exp

  end function xtos

  character(LEN_STR_ANS_MAX) function xtoa(x,opt)
    real(rp),intent(in)::x
    integer,intent(in)::opt
    integer sd
    character(LEN_STR_ANS_MAX) ns
    integer p1,p2
    integer k,d,dd
    logical cr
    integer*1 c
    integer e,ee,r
    integer len,len0
    pointer(pc,c)
    if(x==rzero.and.is_set(X2A_ALLOW_ORDINARY)) then
       xtoa="0"
       return
    end if
    xtoa=""
    ns=xtos(x,e,.false.)
    if(ns(1:1)=="-") then
       p1=2
       xtoa(1:1)="-"
    else
       p1=1
    end if
    p2=len_trim(ns)
! x.xxxx...xE+xxxx
    len=len_trim(ns)-1   ! digits excluding sign
    len0=len_trim0(ns)-1 ! excluding sign
    if(is_set(X2A_ALLOW_ORDINARY).and.e>=0.and.e<max_digit) then
       if(e>=len0-1) then
          ! integer
          xtoa(p1:)=ns(2:2+e)
          return 
       end if
    end if

    sd=min(max(iand(opt,int(Z"FF")),min_digit),len)
    if(is_uset(X2A_FIX)) then
       d=sd
    else
       d=e+1+sd
       if(d<1) then
          xtoa(p1:)="0."//repeat("0",sd)
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
