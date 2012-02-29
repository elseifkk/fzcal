module integral
  implicit none
  private
  
  integer,parameter::MAXNMAX  = 1000000
  integer,parameter::KMAX     = 18

  public deSdx
  
contains
  
! one dimensional integration by double exponential formula.
  integer function deSdx(ptr_c,ptr_f,a,b,eps,ans)
    use fpio, only: rp,cp,czero,pi,pi_2
    use misc, only: mess
    integer,intent(in)::ptr_c
    integer,intent(in)::ptr_f
    real(rp),intent(in)::a,b,eps
    complex(cp),intent(out)::ans
    interface 
       complex(cp) function f(c,n,x)
         use fpio, only: rp,cp
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
    real(rp) x
    complex(cp) s1,s2
    real(rp) alpha,beta
    real(rp) h
    complex(cp) Ih,Ih2
    integer n,k
    integer nmax
    real(rp) sqrteps
    complex(cp) buf(MAXNMAX,2)
    real(rp) phi
    
    deSdx=0
    ans=czero

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
       call mess("*** deSdx: BUFFER OVERFLOW")
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
          call mess("*** deSdx: BUFFER OVERFLOW")
          nmax=MAXNMAX
       END if
       n=0
       do i=1,nmax
          x=real(2*i-1,kind=rp)*h
          if(.not.do_sum(x)) exit
       end do
       call sum_buf
       Ih2=(Ih/2.0_rp+(s1+s2)*h*alpha)
       if(is_converged()) then
          deSdx=0
          exit
       end if
       Ih=Ih2
    end do

    ans=Ih2

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
      use fpio, only: is_nan
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
      z1=f(ptr_c,3,x1)*phi
      z2=f(ptr_c,3,x2)*phi
      az12=abs(z1)+abs(z2)
      if(is_nan(az12)) then
         do_sum=.false.
         return
      end if
      n=i
      buf(i,2)=z2
      buf(i,1)=z1
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
