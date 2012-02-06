module integral
  use fpio
  implicit none
  private
  
  integer,parameter::MAXNMAX  = 1000000
  integer,parameter::KMAX     = 18

  public deSdx
  
contains
  
! one dimensional integration by double exponential formula.
! Automatic integrator utilizing Double exponential formula.
! Itretively decreasing mesh width with convergence condition such that
! abs(I1-I2)<sqrt(eps). Summation cuts off when the value is less than eps.
! Summationn also ends when it is expected to be over 10**308, namely, x>6.1.
! It cannot be used for infinite interval integration.
  integer function deSdx(ptr_c,ptr_f,a,b,eps,ans)
    integer,intent(in)::ptr_c
    integer,intent(in)::ptr_f
    real(rp),intent(in)::a,b,eps
    real(rp),intent(out)::ans
    interface 
       real(rp) function f(c,n,x)
         use fpio, only: rp
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
    real(rp),parameter::XMAX=6.0_rp
    real(rp),parameter::HMAX = XMAX
    integer i
    real(rp) x,s1,s2
    real(rp) alpha,beta
    real(rp) h, Ih, Ih2, z1, z2
    integer n,k
    integer nmax
    real(rp) sqrteps
    real(rp) buffer1(MAXNMAX)
    real(rp) buffer2(MAXNMAX)
    real(rp) phi
    
    deSdx=0
    ans=rzero

    h=(b-a)/2.0_rp
    if(h==0.0) return
    if(h<-XMAX) then
       h=-HMAX
    else if(XMAX<h) then
       h=HMAX
    end if
    sqrteps=sqrt(eps)
    alpha=(b-a)/2.0_rp
    beta=(a+b)/2.0_rp
    nmax=(XMAX/abs(h))
    if(MAXNMAX<nmax) then
       WRITE(*,*) "*** deSdx: BUFFER OVERFLOW"
       nmax=MAXNMAX
    end if
    n=0
    pf=ptr_f
    do i=1,nmax
       x=real(i,kind=rp)*h
       if(.not.do_sum(x)) exit
    end do
    call sum_buf
    Ih=(s1+s2+f(ptr_c,3,[beta,alpha,alpha])*pi_2)*h*alpha
    k=0
    deSdx=1
    do k=1,KMAX
       h=h/2.0_rp
       nmax=(XMAX/abs(h)+1.0_rp)/2.0_rp
       if(MAXNMAX<nmax) THEN
          WRITE(*,*) "*** deSdx: BUFFER OVERFLOW"
          nmax=MAXNMAX
       END if
       n=0
       do i=1,nmax
          x=real(2*i-1,kind=rp)*h
          if(.not.do_sum(x)) exit
       end do
       call sum_buf
       Ih2=(Ih/2.0_rp+(s1+s2)*h*alpha)
       if(abs(Ih2)<eps.and.abs(Ih)<eps &
            .or.abs(Ih2-Ih)<=sqrteps*abs(Ih2)) then
          deSdx=0
          exit
       end if
       Ih=Ih2
    end do

    ans=Ih2

  contains
    
    logical function do_sum(x)
      real(rp),intent(in)::x
      real(rp) az12
      real(rp) x1(3),x2(3)
      do_sum=.true.
      phi=de(x)
      call get_xi(x,x1)
      x2(1) = beta-x1(1)
      x1(1) = beta+x1(1)
      x2(2) = x1(3)
      x2(3) = x1(2)
      z1=f(ptr_c,3,x1)*phi
      z2=f(ptr_c,3,x2)*phi
      az12=abs(z1)+abs(z2)
      if(is_nan(az12)) then
         do_sum=.false.
         return
      end if
      n=i
      buffer2(i)=z2
      buffer1(i)=z1
    end function do_sum

    subroutine sum_buf()
      s1=rzero
      s2=rzero
      do i=n,1,-1
         s1=s1+buffer1(i)
         s2=s2+buffer2(i)
      end do
    end subroutine sum_buf

    real(rp) function de(x)
      real(rp),intent(in)::x
      de=pi*cosh(x)/(cosh(pi*sinh(x))+1.0_rp)
    end function de

    subroutine get_xi(x,xi)
      real(rp),intent(in)::x
      real(rp),intent(out)::xi(3)
      real(rp) a,c
      a=pi_2*sinh(x)
      xi(1)=tanh(a)
      c=cosh(a)
      xi(2)=exp( a)/c
      xi(3)=exp(-a)/c
      xi=xi*alpha
    end subroutine get_xi

  end function deSdx
  
end module integral
