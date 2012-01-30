module integral
  use fpio
  implicit none
  private
  
  real(rp),parameter::XMAX = 6.0
  real(rp),parameter::HMAX = XMAX
  integer,parameter::MAXNMAX  = 100000
  integer,parameter::KMAX  = 10000
  integer,parameter::SNMAX = 10

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
       real(rp) function f(c,x)
         use fpio, only: rp
         integer,intent(in)::c
         real(rp),intent(in)::x
       end function f
    end interface
    pointer(pf,f)
    integer i
    real(rp) x,s1,s2
    real(rp) alpha,beta
    real(rp) h, Ih, Ih2, z1, z2
    integer n,k
    integer nmax
    real(rp) sqrteps
    real(rp) buffer1(MAXNMAX+1)
    real(rp) buffer2(MAXNMAX+1)
    real(rp) phi,xx
    
    deSdx=0
    
    h=(b-a)/2.0_rp
    if(h==0.0) then
       ans=rzero
       return
    end if
    if(h<-XMAX) then
       h=-HMAX
    else if(XMAX<h) then
       h=HMAX
    end if
    sqrteps=sqrt(eps)
    alpha=(b-a)/2.0_rp
    beta=(a+b)/2.0_rp
    nmax=(XMAX/abs(h))
    if(MAXNMAX<nmax) nmax=MAXNMAX
    n=0
    i=0
    pf=ptr_f
    do
       i=i+1
       x=real(i,kind=rp)*h
       phi=pi*cosh(x)/(cosh(pi*sinh(x))+1.0_rp)
       xx=alpha*tanh(pi_2*sinh(x))
       n=i
       z1=f(ptr_c,xx+beta)*phi
       buffer1(i)=z1
       z2=f(ptr_c,-xx+beta)*phi
       buffer2(i)=z2
       z1=abs(z1)+abs(z2)
       if(z1<eps) exit
       if(nmax<=i) then
          deSdx=1
          !ErrorHandler(ERRID_DESDX_NMAX_OVER,nmax);
          exit
       end if
    end do
    s1=rzero
    s2=rzero
    do i=n,1,-1
       s1=s1+buffer1(i)
       s2=s2+buffer2(i)
    end do
    Ih=(s1+s2+f(ptr_c,beta)*pi_2)*h*alpha
    k=0
    do
       k=k+1
       h=h/2.0_rp
       nmax=(XMAX/abs(h)+1.0_rp)/2.0_rp
       if(MAXNMAX<nmax) nmax=MAXNMAX
       n=0
       i=0
       do
          i=i+1
          x=real(2*i-1,kind=rp)*h
          phi=pi*cosh(x)/(cosh(pi*sinh(x))+1.0_rp)
          xx=alpha*tanh(pi_2*sinh(x))
          n=i
          z1=f(ptr_c,xx+beta)*phi
          buffer1(i)=z1
          z2=f(ptr_c,-xx+beta)*phi
          buffer2(i)=z2
          z1=abs(z1)+abs(z2)
          if(z1<eps) exit
          if(nmax<=i) then
             deSdx=1
             !ErrorHandler(ERRID_DESDX_NMAX_OVER,nmax);
             exit
          end if
       end do
       s1=rzero
       s2=rzero
       do i=n,1,-1
          s1=s1+buffer1(i)
          s2=s2+buffer2(i)
       end do
       Ih2=(Ih/2.0_rp+(s1+s2)*h*alpha)
       if(Ih==rzero .and. Ih2==rzero) then
          ans=rzero
          return
       end if
       if(abs(Ih2-Ih)<=sqrteps*abs(Ih2)) exit
       if(k>KMAX) then
          deSdx=1
          !ErrorHandler(ERRID_DESDX_KMAX_OVER,KMAX);
          exit
       end if
       Ih=Ih2
    end do
    ans=Ih2
  end function deSdx
  
end module integral
