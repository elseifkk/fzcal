module zmath
  use fpio
  implicit none

  private

  public zm_mov
  public zm_add
  public zm_sub
  public zm_mul
  public zm_div
  public zm_pow
  public zm_exp10

  public zm_nop
  public zm_neg
  public zm_fac
  public zm_dfac

  public zm_sin
  public zm_cos
  public zm_tan
  public zm_asin
  public zm_acos
  public zm_atan
  public zm_sinh
  public zm_cosh
  public zm_tanh
  public zm_asinh
  public zm_acosh
  public zm_atanh
  public zm_exp
  public zm_log
  public zm_log10
  public zm_sqrt
  public zm_abs
  public zm_int
  public zm_frac
  public zm_nint
  public zm_conjg
  public zm_real
  public zm_imag
  public zm_mag
  public zm_arg
  public zm_gamma
  public zm_lgamma

  public zm_min
  public zm_max

  real(rp),parameter::pi =4.0_rp*atan(1.0_rp)
  real(rp),parameter::pi2=2.0_rp*pi
contains

  logical function is_integer(z,n)
    complex(cp),intent(in)::z
    integer,intent(out),optional::n
    integer m
    real(rp) x
    is_integer=.false.
    if(imagpart(z)/=rzero) return
    x=realpart(z)
    m=int(x)
    x=x-m
    if(x==0) then 
       is_integer=.true.
       if(present(n)) n=m
    end if
  end function is_integer
  
  complex(cp) function zm_mov(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_mov=z2
  end function zm_mov

  complex(cp) function zm_add(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_add=z1+z2
  end function zm_add
  
  complex(cp) function zm_sub(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_sub=z1-z2
  end function zm_sub
  
  complex(cp) function zm_mul(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_mul=z1*z2
  end function zm_mul
  
  complex(cp) function zm_div(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_div=z1/z2
  end function zm_div
  
  complex(cp) function zm_pow(z1,z2)
    complex(cp),intent(in)::z1,z2
    integer n1,n2
    if(is_integer(z2,n2)) then
       if(is_integer(z1,n1)) then
          zm_pow=real(n1,kind=rp)**real(n2,kind=rp)
       else
          zm_pow=z1**real(n2,kind=rp)
       end if
    else
       zm_pow=z1**z2
    end if
  end function zm_pow

  complex(cp) function zm_exp10(z1,z2)
    complex(cp),intent(in)::z1,z2
    integer n1,n2
    if(is_integer(z2,n2)) then
       if(is_integer(z1,n1)) then
          zm_exp10=real(n1,kind=rp)*10.0_rp**real(n2,kind=rp)
       else
          zm_exp10=z1*10.0_rp**real(n2,kind=rp)
       end if
    else
       zm_exp10=z1*10.0_rp**z2
    end if  
  end function zm_exp10

  complex(cp) function zm_nop(z1)
    complex(cp),intent(in)::z1
    zm_nop=z1
  end function zm_nop

  complex(cp) function zm_neg(z1)
    complex(cp),intent(in)::z1
    zm_neg=-z1
  end function zm_neg

  complex(cp) function zm_fac(z1)
    complex(cp),intent(in)::z1
    integer i,n
    n=int(z1)
    if(n<=0) then
       zm_fac=czero
       return
    end if
    zm_fac=real(n,kind=rp)       
    do i=2,n-1
       zm_fac=zm_fac*real(i,kind=rp)
    end do
  end function zm_fac

  complex(cp) function zm_dfac(z1)
    complex(cp),intent(in)::z1
    integer i,n
    n=int(z1)
    if(n<=0) then
       zm_dfac=czero
       return
    end if
    zm_dfac=real(n,kind=rp)
    do i=n-2,2,-2
       zm_dfac=zm_dfac*real(i,kind=rp)
    end do
  end function zm_dfac

  complex(cp) function zm_sin(z1)
    complex(cp),intent(in)::z1
    zm_sin=sin(z1)
  end function zm_sin

  complex(cp) function zm_cos(z1)
    complex(cp),intent(in)::z1
    zm_cos=cos(z1)
  end function zm_cos

  complex(cp) function zm_tan(z1)
    complex(cp),intent(in)::z1
    zm_tan=tan(z1)
  end function zm_tan

  complex(cp) function zm_asin(z1)
    complex(cp),intent(in)::z1
    zm_asin=asin(z1)
  end function zm_asin

  complex(cp) function zm_acos(z1)
    complex(cp),intent(in)::z1
    zm_acos=acos(z1)
  end function zm_acos

  complex(cp) function zm_atan(z1)
    complex(cp),intent(in)::z1
    zm_atan=atan(z1)
  end function zm_atan

  complex(cp) function zm_sinh(z1)
    complex(cp),intent(in)::z1
    zm_sinh=sinh(z1)
  end function zm_sinh

  complex(cp) function zm_cosh(z1)
    complex(cp),intent(in)::z1
    zm_cosh=cosh(z1)
  end function zm_cosh

  complex(cp) function zm_tanh(z1)
    complex(cp),intent(in)::z1
    zm_tanh=tanh(z1)
  end function zm_tanh

  complex(cp) function zm_asinh(z1)
    complex(cp),intent(in)::z1
    zm_asinh=log(z1+sqrt(z1*z1+1.0_rp))
  end function zm_asinh

  complex(cp) function zm_acosh(z1)
    complex(cp),intent(in)::z1
    zm_acosh=log(z1+sqrt(z1*z1-1.0_rp))
  end function zm_acosh

  complex(cp) function zm_atanh(z1)
    complex(cp),intent(in)::z1
    zm_atanh=log((1.0_rp+z1)/(1.0_rp-z1))/2.0_rp
  end function zm_atanh

  complex(cp) function zm_exp(z1)
    complex(cp),intent(in)::z1
    zm_exp=exp(z1)
  end function zm_exp

  complex(cp) function zm_log(z1)
    complex(cp),intent(in)::z1
    zm_log=log(z1)
  end function zm_log

  complex(cp) function zm_log10(z1)
    complex(cp),intent(in)::z1
    zm_log10=log(z1)/log(10.0_rp)
  end function zm_log10

  complex(cp) function zm_sqrt(z1)
    complex(cp),intent(in)::z1
    zm_sqrt=sqrt(z1)
  end function zm_sqrt

  complex(cp) function zm_abs(z1)
    complex(cp),intent(in)::z1
    zm_abs=abs(z1)
  end function zm_abs
  
  complex(cp) function zm_int(z1)
    complex(cp),intent(in)::z1
    zm_int=int(z1)
  end function zm_int

  complex(cp) function zm_frac(z1)
    complex(cp),intent(in)::z1
    zm_frac=z1-int(z1)
  end function zm_frac

  complex(cp) function zm_nint(z1)
    complex(cp),intent(in)::z1
    zm_nint=int(z1+0.5_rp)
  end function zm_nint

  complex(cp) function zm_conjg(z1)
    complex(cp),intent(in)::z1
    zm_conjg=conjg(z1)
    if(abs(imagpart(zm_conjg))==rzero)  then
       zm_conjg=complex(realpart(zm_conjg),rzero)
    end if
  end function zm_conjg

  complex(cp) function zm_real(z1)
    complex(cp),intent(in)::z1
    zm_real=realpart(z1)
  end function zm_real

  complex(cp) function zm_imag(z1)
    complex(cp),intent(in)::z1
    zm_imag=imagpart(z1)
  end function zm_imag

  complex(cp) function zm_mag(z1)
    complex(cp),intent(in)::z1
    zm_mag=sqrt(realpart(z1)**2.0_rp+imagpart(z1)**2.0_rp)
  end function zm_mag
  
  complex(cp) function zm_arg(z1)
    complex(cp),intent(in)::z1
    zm_arg=atan(imagpart(z1),realpart(z1))
  end function zm_arg

  complex(cp) function zm_min(z1,z2)
    complex(cp),intent(in)::z1,z2
    real(rp) r1,r2,i1,i2
    r1=realpart(z1)
    r2=realpart(z2)
    i1=imagpart(z1)
    i2=imagpart(z2)
    if(i1==rzero.and.i2==rzero) then
       zm_min=complex(min(r1,r2),rzero)
    else if(r1==rzero.and.r2==rzero) then
       zm_min=complex(rzero,min(i1,i2))
    else
       if(abs(z1)<abs(z2)) then
          zm_min=z1
       else
          zm_min=z2
       end if
    end if
  end function zm_min

  complex(cp) function zm_max(z1,z2)
    complex(cp),intent(in)::z1,z2
    real(rp) r1,r2,i1,i2
    r1=realpart(z1)
    r2=realpart(z2)
    i1=imagpart(z1)
    i2=imagpart(z2)
    if(i1==rzero.and.i2==rzero) then
       zm_max=complex(max(r1,r2),rzero)
    else if(r1==rzero.and.r2==rzero) then
       zm_max=complex(rzero,max(i1,i2))
    else
       if(abs(z1)>abs(z2)) then
          zm_max=z1
       else
          zm_max=z2
       end if
    end if
  end function zm_max

  complex(cp) function zm_lgamma(z1)
    complex(cp),intent(in)::z1
    real(rp),parameter::a(7)=[&
         1.0_rp/12.0_rp,&
         1.0_rp/30.0_rp,&
         53.0_rp/210.0_rp,&
         195.0_rp/371.0_rp,&
         22999.0_rp/22737.0_rp,&
         29944523.0_rp/19733142.0_rp,&
         109535241009.0_rp/48264275462.0_rp]
    real(rp),parameter::log_pi2_2=log(pi2)*0.5_rp
    complex(cp) z,d,r
    real(rp) r1
    integer i
    integer n
    if(imagpart(z1)==0.0_rp) then
       zm_lgamma=lgamma(realpart(z1))
       return
    end if
    r1=realpart(z1)
    if(r1<rzero) then
       n=-int(r1)+1
       z=z1+real(n,kind=rp)
    else
       n=0
       z=z1
    end if
    if(realpart(z)<100.0_rp) then
       n=n+100
       z=z+100.0_rp
    end if
    d=a(7)/z
    do i=6,1,-1
       d=a(i)/(d+z)
    end do
    r=z-(z-0.5_rp)*log(z)-log_pi2_2
    zm_lgamma=d-r ! log gamma(z+n) = log (...)gamma(z)
    if(n/=0) zm_lgamma=zm_lgamma-log(redfac())
  contains
    complex(cp) function redfac()
      integer j
      redfac=z1
      do j=1,n-1
         redfac=redfac*(z1+real(j,kind=rp))
      end do
    end function redfac
  end function zm_lgamma
  
  complex(cp) function zm_gamma(z1)
    complex(cp),intent(in)::z1
    zm_gamma=exp(zm_lgamma(z1))
  end function zm_gamma

end module zmath
