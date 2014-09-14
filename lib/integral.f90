!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2012,2014 by Kazuaki Kumagai                                 *
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
module integral
  use fpio, only: rp,cp,czero,rzero,pi,pi_2
  implicit none
  private

  integer,parameter::MAXNMAX  = 1000000
  integer,parameter::KMAX     = 18
  integer,parameter::RCMAX    = 8 ! maximum recursion count
  integer,target::ibuf(RCMAX)
  integer,target::kbuf(RCMAX)
  integer::rc=0
  complex(cp),target::zbuf(MAXNMAX,2,RCMAX)

  public deSdx

contains

  ! one dimensional integration by double exponential formula.
  function deSdx(ptr_c,ptr_f,a,b,nv,eps,ans) result(istat)
    ! declaration as recursive cause segmentation fault with gfortran 4.7.2
    ! build for arm-linux-gnueabi.
    use misc, only: mess
    integer istat
    integer,intent(in)::ptr_c
    integer,intent(in)::ptr_f
    real(rp),intent(in)::a,b
    integer,intent(in)::nv ! 0 if X_lo or X_up are unnecessary
    real(rp),intent(in)::eps
    complex(cp),intent(out)::ans
    interface
       recursive function f(c,n,x) result(z)
         use fpio, only: rp,cp
         complex(cp) z
         integer,intent(in)::c     ! = ptr_c
         ! if f has singularity at bound, use b-X or X-a
         ! instead of X
         ! Ex., for 1/sqrt(1-X**2) over (-1,1)
         ! use 1/sqrt((1-X)*(1+x)) = 1/sqrt(X_lo*X_up)
         integer,intent(in)::n     ! = 3
         real(rp),intent(in)::x(n) ! = [X, b-X, X-a]
       end function f
    end interface
    pointer(pf,f)
    real(rp),parameter::XMAX = 6.0_rp
    real(rp),parameter::HMAX = XMAX
    real(rp),parameter::HMIN = 1.0e-3_rp
    integer,pointer::i
    real(rp) x
    complex(cp) s1,s2
    real(rp) alpha,beta
    real(rp) h
    complex(cp) Ih,Ih2
    integer n
    integer,pointer::k
    integer nmax
    real(rp) sqrteps
    complex(cp),pointer::buf(:,:)
    real(rp) phi
    real(rp) aa,bb
    complex(cp) fa,fb

    rc=rc+1
    ans=czero

    if(rc>RCMAX) then
       call mess("*** deSdx: too deep recursion")
       istat=2
       goto 999
    end if

    i => ibuf(rc)
    k => kbuf(rc)
    buf => zbuf(:,:,rc)
    istat=0
    if(b>a) then
       aa=a
       bb=b
    else if(a>b) then
       aa=b
       bb=a
    else
       goto 999
    end if

    h=(bb-aa)/2.0_rp

    if(h>XMAX) then
       h=HMAX
    else if(h<HMIN) then
       h=HMIN
    end if

    sqrteps=sqrt(eps)
    alpha=(bb-aa)/2.0_rp
    beta=(aa+bb)/2.0_rp
    nmax=(XMAX/h)
    n=0

    pf=ptr_f
    if(nv==0) then
       fa=f(ptr_c,1,[aa])
       fb=f(ptr_c,1,[bb])
    end if

    do i=1,nmax
       x=real(i,kind=rp)*h
       if(.not.do_sum(x)) exit
    end do
    call sum_buf
    Ih=(s1+s2+f(ptr_c,3,[beta,alpha,alpha])*pi_2)*h*alpha
    k=0

    istat=1
    do k=1,KMAX
       h=h/2.0_rp
       nmax=(XMAX/h+1.0_rp)/2.0_rp
       if(MAXNMAX<nmax) THEN
          call mess("*** deSdx: BUFFER OVERFLOW")
          exit
       END if
       n=0
       do i=1,nmax
          x=real(2*i-1,kind=rp)*h
          if(.not.do_sum(x)) exit
       end do
       call sum_buf
       Ih2=(Ih/2.0_rp+(s1+s2)*h*alpha)
       if(is_converged()) then
          istat=0
          exit
       end if
       Ih=Ih2
    end do

    ans=Ih2
    if(a>b) ans=-ans

999 continue

    rc=rc-1

  contains

     logical function is_converged()
       is_converged=&
            (abs(realpart(Ih2))<eps &
            .and.abs(realpart(Ih))<eps &
            .or.abs(realpart(Ih2-Ih))<sqrteps*abs(realpart(Ih2))) &
            .and.(abs(imagpart(Ih2))<eps &
            .and.abs(imagpart(Ih))<eps &
            .or.abs(imagpart(Ih2-Ih))<sqrteps*abs(imagpart(Ih2)))
     end function is_converged

     logical function do_sum(x)
       real(rp),intent(in)::x
       real(rp) az12
       real(rp) x1(3),x2(3)
       complex(cp) z1,z2
       do_sum=.true.
       phi=de(x)
       call get_xi(x,x1)
       x2(1) = beta-x1(1)
       x1(1) = beta+x1(1)
       x2(2) = x1(3)
       x2(3) = x1(2)
       if(nv==0.and.x1(1)==bb) then
          z1=fb
       else
          z1=f(ptr_c,3,x1)
       end if
       if(nv==0.and.x2(1)==aa) then
          z2=fa
       else
          z2=f(ptr_c,3,x2)
       end if
       az12=realpart(z1)+realpart(z2)+imagpart(z1)+imagpart(z2)
       if(isnan(az12)) then
          do_sum=.false.
          return
       end if
       n=i
       buf(i,2)=z2*phi
       buf(i,1)=z1*phi
     end function do_sum

     subroutine sum_buf()
       s1=czero
       s2=czero
       do i=n,1,-1
          s1=s1+buf(i,1)
          s2=s2+buf(i,2)
       end do
     end subroutine sum_buf

     real(rp) function de(x)
       real(rp),intent(in)::x
       de=pi*cosh(x)/(cosh(pi*sinh(x))+1.0_rp)
     end function de

     subroutine get_xi(x,xi)
       real(rp),intent(in)::x
       real(rp),intent(out)::xi(3)
       real(rp) t,c
       t=pi_2*sinh(x)
       xi(1)=tanh(t)
       if(nv>0) then
          c=cosh(t)
          xi(2)=exp( t)/c
          xi(3)=exp(-t)/c
       end if
       xi=xi*alpha
     end subroutine get_xi

   end function deSdx

end module integral
