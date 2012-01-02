module zmath
  use fpio
  implicit none

  private

  public zm_ran

  public zm_f1
  public zm_f2
  public zm_f3

  public zmr_eq
  public zmr_neq
  public zmr_gt
  public zmr_lt
  public zmr_ge
  public zmr_le

  public zm_add_f
  public zm_sub_f
  public zm_mul_f
  public zm_div_f

  public zm_mov
  public zm_add
  public zm_sub
  public zm_mul
  public zm_div
  public zm_pow
  public zm_invpow
  public zm_exp10

  public zm_nop
  public zm_inc_f
  public zm_dec_f
  public zm_neg_f
  public zm_neg
  public zm_fac
  public zm_dfac
  public zm_inc
  public zm_dec
  public zm_sum
  public zm_ave
  public zm_var
  public zm_uvar
  public zm_sum2

  public zms_n
  public zms_sum_x
  public zms_ave_x
  public zms_var_x
  public zms_uvar_x
  public zms_sum2_x
  public zms_sum_y
  public zms_ave_y
  public zms_var_y
  public zms_uvar_y
  public zms_sum2_y
  public zms_sum_xy
  public zms_a
  public zms_b

  public zm_sin
  public zm_cos
  public zm_tan
  public zm_asin
  public zm_acos
  public zm_atan

  public zm_sind
  public zm_cosd
  public zm_tand
  public zm_asind
  public zm_acosd
  public zm_atand

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
  public zm_cbrt
  public zm_abs
  public zm_int
  public zm_frac
  public zm_nint
  public zm_conjg
  public zm_re
  public zm_im
  public zm_mag
  public zm_arg
  public zm_gamma
  public zm_lgamma
  public zm_gami
  public zm_psy

  public zm_mod
  public zm_min
  public zm_max

  public zm_deint

  real(rp),parameter::pi =4.0_rp*atan(1.0_rp)
  real(rp),parameter::pi2=2.0_rp*pi
  complex,public,parameter::ztrue=complex(1.0_rp,rzero)
  complex,public,parameter::zfalse=czero

  logical::random_seed_init=.false.

contains

  subroutine init_random_seed()
    integer::i,n,clock
    integer,allocatable::seed(:)
    call random_seed(size = n)
    allocate(seed(n))
    call system_clock(count=clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(put = seed)
    deallocate(seed)
    random_seed_init=.true.
  end subroutine init_random_seed
     
  integer function get_gcd(a,b)
    integer,intent(in)::a,b
    integer i1,i2,r ! i1>i2
    if(a>b) then
       i1=a
       i2=b
    else
       i1=b
       i2=a
    end if
    if(i2==0) then
       get_gcd=i1
       return
    end if
    do
       r=mod(i1,i2)
       i1=i2
       if(r==0) exit
       i2=r
    end do
    get_gcd=i1
  end function get_gcd

  complex(cp) function reduce_f(z)
    complex(cp),intent(in)::z
    integer gcd
    reduce_f=z
    if(realpart(z)/=0.and.imagpart(z)>1) then
       gcd=get_gcd(int(realpart(z)),int(imagpart(z)))
       if(gcd>1) &
            reduce_f=complex(int(realpart(z))/gcd,int(imagpart(z)/gcd))
    end if
  end function reduce_f

  complex(cp) function zm_ran()
    real(rp) x
    if(.not.random_seed_init) call init_random_seed()
    call random_number(x)
    zm_ran=complex(x,rzero)
  end function zm_ran

  complex(cp) function zm_f1(n,pzs)
    integer,intent(in)::n,pzs(0:n)
    complex(cp) z1
    pointer(p1,z1)
    interface
       function f1(z1)
         use fpio, only: cp
         complex(cp) f1,z1
       end function f1
    end interface
    pointer(pf1,f1)
    pf1=pzs(0)
    p1=pzs(1)
    zm_f1=f1(z1)
  end function zm_f1

  complex(cp) function zm_f2(n,pzs)
    integer,intent(in)::n,pzs(0:n)
    complex(cp) z1,z2
    pointer(p1,z1)
    pointer(p2,z2)
    interface
       function f2(z1,z2)
         use fpio, only: cp
         complex(cp) f2,z1,z2
       end function f2
    end interface
    pointer(pf2,f2)
    pf2=pzs(0)
    p1=pzs(1)
    p2=pzs(2)
    zm_f2=f2(z1,z2)
  end function zm_f2

  complex(cp) function zm_f3(n,pzs)
    integer,intent(in)::n,pzs(0:n)
    complex(cp) z1,z2,z3
    pointer(p1,z1)
    pointer(p2,z2)
    pointer(p3,z3)
    interface
       function f3(z1,z2,z3)
         use fpio, only: cp
         complex(cp) f3,z1,z2,z3
       end function f3
    end interface
    pointer(pf3,f3)
    pf3=pzs(0)
    p1=pzs(1)
    p2=pzs(2)
    p3=pzs(3)
    zm_f3=f3(z1,z2,z3)
  end function zm_f3

  complex(cp) function zmr_eq(z1,z2)
    complex(cp),intent(in)::z1,z2
    if(z1==z2) then
       zmr_eq=ztrue
    else
       zmr_eq=zfalse
    end if
  end function zmr_eq

  complex(cp) function zmr_neq(z1,z2)
    complex(cp),intent(in)::z1,z2
    if(z1/=z2) then
       zmr_neq=ztrue
    else
       zmr_neq=zfalse
    end if
  end function zmr_neq

  complex(cp) function zmr_gt(z1,z2)
    complex(cp),intent(in)::z1,z2
    if(realpart(z1)>realpart(z2)) then
       zmr_gt=ztrue
    else
       zmr_gt=zfalse
    end if
  end function zmr_gt

  complex(cp) function zmr_lt(z1,z2)
    complex(cp),intent(in)::z1,z2
    if(realpart(z1)<realpart(z2)) then
       zmr_lt=ztrue
    else
       zmr_lt=zfalse
    end if
  end function zmr_lt

  complex(cp) function zmr_ge(z1,z2)
    complex(cp),intent(in)::z1,z2
    if(realpart(z1)>=realpart(z2)) then
       zmr_ge=ztrue
    else
       zmr_ge=zfalse
    end if
  end function zmr_ge

  complex(cp) function zmr_le(z1,z2)
    complex(cp),intent(in)::z1,z2
    if(realpart(z1)<=realpart(z2)) then
       zmr_le=ztrue
    else
       zmr_le=zfalse
    end if
  end function zmr_le
  
  complex(cp) function zm_mov(z1_unused,z2)
    complex(cp),intent(in)::z1_unused,z2
    zm_mov=z2
  end function zm_mov

  complex(cp) function zm_add_f(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_add_f=reduce_f(complex(realpart(z1)*imagpart(z2)+imagpart(z1)*realpart(z2),&
         imagpart(z1)*imagpart(z2)))
  end function zm_add_f
  
  complex(cp) function zm_sub_f(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_sub_f=reduce_f(complex(realpart(z1)*imagpart(z2)-imagpart(z1)*realpart(z2),&
         imagpart(z1)*imagpart(z2)))
  end function zm_sub_f
  
  complex(cp) function zm_mul_f(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_mul_f=reduce_f(complex(realpart(z1)*realpart(z2),imagpart(z1)*imagpart(z2)))
  end function zm_mul_f
  
  complex(cp) function zm_div_f(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_div_f=reduce_f(complex(realpart(z1)*imagpart(z2),imagpart(z1)*realpart(z2)))
  end function zm_div_f

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

  complex(cp) function zm_invpow(z2,z1)
    complex(cp),intent(in)::z2,z1
    integer n1,n2
    if(is_integer(z2,n2)) then
       if(is_integer(z1,n1)) then
          zm_invpow=real(n1,kind=rp)**(1.0_rp/real(n2,kind=rp))
       else
          zm_invpow=z1**(1.0_rp/real(n2,kind=rp))
       end if
    else
       zm_invpow=z1**(1.0_rp/z2)
    end if
  end function zm_invpow

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
    ! gfortran returns -0 for 0
    if(abs(realpart(z1))==rzero) then
       zm_neg=complex(rzero,imagpart(zm_neg))
    end if
    if(abs(imagpart(z1))==rzero) then
       zm_neg=complex(realpart(zm_neg),rzero)
    end if
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

  complex(cp) function zm_sind(z1)
    complex(cp),intent(in)::z1
    zm_sind=sin(z1/180.0_rp*pi)
  end function zm_sind

  complex(cp) function zm_cosd(z1)
    complex(cp),intent(in)::z1
    zm_cosd=cos(z1/180.0_rp*pi)
  end function zm_cosd

  complex(cp) function zm_tand(z1)
    complex(cp),intent(in)::z1
    zm_tand=tan(z1/180.0_rp*pi)
  end function zm_tand

  complex(cp) function zm_asind(z1)
    complex(cp),intent(in)::z1
    zm_asind=asin(z1)/pi*180.0_rp
  end function zm_asind

  complex(cp) function zm_acosd(z1)
    complex(cp),intent(in)::z1
    zm_acosd=acos(z1)/pi*180.0_rp
  end function zm_acosd

  complex(cp) function zm_atand(z1)
    complex(cp),intent(in)::z1
    zm_atand=atan(z1)/pi*180.0_rp
  end function zm_atand

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

  complex(cp) function zm_cbrt(z1)
    complex(cp),intent(in)::z1
    zm_cbrt=(z1)**(1.0_rp/3.0_rp)
  end function zm_cbrt

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

  complex(cp) function zm_re(z1)
    complex(cp),intent(in)::z1
    zm_re=realpart(z1)
  end function zm_re

  complex(cp) function zm_im(z1)
    complex(cp),intent(in)::z1
    zm_im=imagpart(z1)
  end function zm_im

  complex(cp) function zm_mag(z1)
    complex(cp),intent(in)::z1
    zm_mag=sqrt(realpart(z1)**2.0_rp+imagpart(z1)**2.0_rp)
  end function zm_mag
  
  complex(cp) function zm_arg(z1)
    complex(cp),intent(in)::z1
    zm_arg=atan(imagpart(z1),realpart(z1))
  end function zm_arg

  complex(cp) function zm_mod(z1,z2)
    complex(cp),intent(in)::z1,z2
    zm_mod=mod(int(z1),int(z2))
  end function zm_mod

  complex(cp) function zm_inc_f(z)
    complex(cp),intent(in)::z
    zm_inc_f=complex(realpart(z)+imagpart(z),imagpart(z))
  end function zm_inc_f

  complex(cp) function zm_dec_f(z)
    complex(cp),intent(in)::z
    zm_dec_f=complex(realpart(z)-imagpart(z),imagpart(z))
  end function zm_dec_f

  complex(cp) function zm_neg_f(z)
    complex(cp),intent(in)::z
    zm_neg_f=complex(-realpart(z),imagpart(z))
  end function zm_neg_f

  complex(cp) function zm_inc(z)
    complex(cp),intent(in)::z
    zm_inc=z+1.0_rp
  end function zm_inc

  complex(cp) function zm_dec(z)
    complex(cp),intent(in)::z
    zm_dec=z-1.0_rp
  end function zm_dec

  complex(cp) function zm_min(n,pzs)
    integer,intent(in)::n
    integer,intent(in)::pzs(0:n)
    integer i
    real(rp) r1,r2,i1,i2
    complex(cp) z
    pointer(pz,z)
    pz=pzs(1)
    zm_min=z
    do i=2,n
       pz=pzs(i)
       r1=realpart(zm_min)
       i1=imagpart(zm_min)
       r2=realpart(z)
       i2=imagpart(z)
       if(i1==rzero.and.i2==rzero) then
          zm_min=complex(min(r1,r2),rzero)
       else if(r1==rzero.and.r2==rzero) then
          zm_min=complex(rzero,min(i1,i2))
       else if(abs(z)<abs(zm_min)) then
             zm_min=z
       end if
    end do
  end function zm_min

  complex(cp) function zm_max(n,pzs)
    integer,intent(in)::n
    integer,intent(in)::pzs(0:n)
    integer i
    real(rp) r1,r2,i1,i2
    complex(cp) z
    pointer(pz,z)
    pz=pzs(1)
    zm_max=z
    do i=2,n
       pz=pzs(i)
       r1=realpart(zm_max)
       i1=imagpart(zm_max)
       r2=realpart(z)
       i2=imagpart(z)
       if(i1==rzero.and.i2==rzero) then
          zm_max=complex(max(r1,r2),rzero)
       else if(r1==rzero.and.r2==rzero) then
          zm_max=complex(rzero,max(i1,i2))
       else if(abs(z)>abs(zm_max)) then
          zm_max=z
       end if
    end do
  end function zm_max

  complex(cp) function zm_sum(n,pzs)
    integer,intent(in)::n
    integer,intent(in)::pzs(0:n)
    integer i
    complex(cp) z
    pointer(pz,z)
    zm_sum=czero
    do i=1,n
       pz=pzs(i)
       zm_sum=zm_sum+z
    end do
  end function zm_sum

  complex(cp) function zm_sum2(n,pzs)
    integer,intent(in)::n
    integer,intent(in)::pzs(0:n)
    integer i
    complex(cp) z
    pointer(pz,z)
    zm_sum2=czero
    do i=1,n
       pz=pzs(i)
       zm_sum2=zm_sum2+z*z
    end do
  end function zm_sum2

  complex(cp) function zm_ave(n,pzs)
    integer,intent(in)::n
    integer,intent(in)::pzs(0:n)
    zm_ave=zm_sum(n,pzs)/real(n,kind=rp)
  end function zm_ave
    
  complex(cp) function zm_var(n,pzs)
    integer,intent(in)::n
    integer,intent(in)::pzs(0:n)
    integer i
    complex(cp) s,s2
    complex(cp) z
    pointer(pz,z)
    s=czero
    s2=czero
    do i=1,n
       pz=pzs(i)
       s=s+z
       s2=s2+z*z
    end do
    zm_var=sqrt(s2/real(n,kind=rp)-(s/(real(n,kind=rp)))**2.0_rp)
  end function zm_var

  complex(cp) function zm_uvar(n,pzs)
    integer,intent(in)::n
    integer,intent(in)::pzs(0:n)
    zm_uvar=zm_var(n,pzs)*sqrt(real(n,kind=rp)/real(n-1,kind=rp))
  end function zm_uvar

  complex(cp) function zms_n(n,vs_unused,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs_unused(n,2)
    real(rp),intent(in)::ws(n)
    zms_n=sum(ws)
  end function zms_n

  complex(cp) function zms_ave_x(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    zms_ave_x=sum(vs(:,1)*ws)/sum(ws)
  end function zms_ave_x

  complex(cp) function zms_var_x(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    complex(cp) s
    real wn
    wn=sum(ws)
    s=sum(vs(:,1)*ws)
    zms_var_x=sqrt(s**2.0_rp/wn-(s/wn)**2.0_rp)
  end function zms_var_x

  complex(cp) function zms_uvar_x(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    real wn
    wn=sum(ws)
    zms_uvar_x=zms_var_x(n,vs,ws)*sqrt(wn/(wn-1.0_rp))
  end function zms_uvar_x
  
  complex(cp) function zms_sum_x(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    zms_sum_x=sum(vs(:,1)*ws)
  end function zms_sum_x

  complex(cp) function zms_sum2_x(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    zms_sum2_x=sum(vs(:,1)**2.0_rp*ws)
  end function zms_sum2_x

  complex(cp) function zms_ave_y(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    zms_ave_y=sum(vs(:,2)*ws)/sum(ws)
  end function zms_ave_y

  complex(cp) function zms_var_y(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    complex(cp) s
    real(rp) wn
    wn=sum(ws)
    s=sum(vs(:,2)*ws)
    zms_var_y=sqrt(s**2.0_rp/wn-(s/wn)**2.0_rp)
  end function zms_var_y

  complex(cp) function zms_uvar_y(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    real(rp) wn
    wn=sum(ws)
    zms_uvar_y=zms_var_y(n,vs,ws)*sqrt(wn/(wn-1.0_rp))
  end function zms_uvar_y
  
  complex(cp) function zms_sum_y(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    zms_sum_y=sum(vs(:,2)*ws)
  end function zms_sum_y

  complex(cp) function zms_sum2_y(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    zms_sum2_y=sum(vs(:,2)**2.0_rp*ws)
  end function zms_sum2_y

  complex(cp) function zms_sum_xy(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in)::vs(n,2)
    real(rp),intent(in)::ws(n)
    zms_sum_xy=sum(vs(:,1)*vs(:,2)*ws)
  end function zms_sum_xy

  complex(cp) function zms_a(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in),target::vs(n,2)
    real(rp),intent(in)::ws(n)
    complex(cp),pointer::x(:),y(:)
    complex(cp) d
    x => vs(:,1)
    y => vs(:,2)
    d=sum(ws)*sum(x**2.0_rp*ws)-sum(x*ws)**2.0_rp
    zms_a = (sum(x**2.0_rp*ws)*sum(y*ws)-sum(x*ws)*sum(x*y*ws))/d
  end function zms_a

  complex(cp) function zms_b(n,vs,ws)
    integer,intent(in)::n
    complex(cp),intent(in),target::vs(n,2)
    real(rp),intent(in)::ws(n)
    complex(cp),pointer::x(:),y(:)
    complex(cp) d
    x => vs(:,1)
    y => vs(:,2)
    d=sum(ws)*sum(x**2.0_rp*ws)-sum(x*ws)**2.0_rp
    zms_b = (-sum(x*ws)*sum(y*ws)+sum(ws)*sum(x*y*ws))/d
  end function zms_b

!!!!!!---------------------------------------------------------------------!!!!!!
  ! GAMMA FUNCTION AND RELATED FUNCTIONS OF COMPLEX
!!!---------------------------------------------------------------------------!!!
  
!!!---------------------------------------------------------------------------!!!
  complex(cp) function zm_psy(z)
    complex(cp),intent(in)::z
    integer,parameter::num_poly=3
    real(rp),parameter::a(num_poly)=[&
         -1.0_rp/12.0_rp,&
         +1.0_rp/120.0_rp,&
         -.0_rp/252.0_rp]
    real(rp),parameter::ncr2=100000.0_rp**2.0_rp ! <<<<<<<<<<<<<<<<<<<<
    integer i
    complex(cp) p,zz,zn,z0
    logical neg
    integer n
    real(rp) abs2,zzi,zzr

    if(realpart(z)<rzero) then
       neg=.true.
       zz=1.0_rp-z
    else
       neg=.false.
       zz=z
    end if

    zzr=realpart(zz)
    zzi=imagpart(zz)
    abs2=zzr*zzr+zzi*zzi
    if(abs2<ncr2) then
       n=int(sqrt(ncr2-abs2))
       z0=zz
       zz=zz+n
    else
       n=0
    end if

    p=czero
    zn=zz*zz
    do i=1,num_poly
       p=p+a(i)/zn
       zn=zn*zz*zz
    end do
    zm_psy=log(zz)-1.0_rp/(zz*2.0_rp)+p
    if(n/=0) then
       p=czero
       do i=n-1,1,-1
          p=p+1.0_rp/(z0+real(i,kind=rp))
       end do
       p=p+1.0_rp/z0
       zm_psy=zm_psy-p
    end if
    if(neg) zm_psy=zm_psy-pi/tan(pi*zz)
  end function zm_psy

!!!---------------------------------------------------------------------------!!!
  complex(cp) function zm_gami(a,z)
    ! Computes complex incomplete gamma function by series developments; HMF(6.5.29)262
    ! i dont know domein of definition for this
    complex(cp),intent(in)::a
    complex(cp),intent(in)::z
    integer i
    complex(cp) p,p1,p2,p_old
    logical ok
    integer,parameter::NUM_ITERATION_MAX=1000000 ! <<<<<<<<<<<<<<
    p2=1.0_rp
    p1=1.0_rp+z/(a+1.0_rp)
    p_old=czero
    ok=.false.
    do i=2,NUM_ITERATION_MAX
       p=z/(a+real(i,kind=rp))*(p1-p2)+p1
       if(abs(p-p_old)<eps.and.i/=1) then
          ok=.true.
          exit
       end if
       p2=p1
       p1=p
       p_old=p
    end do
    if(ok) then
       zm_gami=p*z**a*EXP(-z)/a
    else
       write(*,*) "*** zm_gami: Number of iteration exceeded:",NUM_ITERATION_MAX
    end if
  end function zm_gami
  
  complex(cp) function zm_lgamma(z1)
      ! Computes logarithm of complex gamma function
      ! Algorithm:
      ! For all cases where argument is mathmatically valid, ln(gamma(z)) is computed 
      ! utilising continued fraction representation; HMF(6.1.48)258, defined for 
      ! Real(z)>0. When this condition is not satisfied, ln(gamma(1-z)) is computed 
      ! first and then get ln(gamma(z)) using reflection formula; HMF(6.1.17)256. Since 
      ! the continued fraction must be terminated somewhere, absolute value of argument 
      ! should be large enough to ensure requested tolerance. If abs(z) is smaller than 
      ! some value of criterion we compute the ln(gamma(z+n)) and use recurrence formula
      ! to get down to ln(gamma(z)).  
      ! Reference
      ! 1) Handbook of Mathematical Functions, Milton Abramowitz et al. Dover publications, 
      ! INC., New York, 1970(?) 
    complex(cp),intent(in)::z1
!!$  Numerators and denominators for continued fraction
!!$  http://oeis.org/A005146
!!$  n  a(n)
!!$  0  1
!!$  1  1
!!$  2  53
!!$  3  195
!!$  4  22999
!!$  5  29944523
!!$  6  109535241009
!!$  7  29404527905795295658
!!$  8  455377030420113432210116914702
!!$  9  26370812569397719001931992945645578779849
!!$  10 152537496709054809881638897472985990866753853122697839
!!$  11 100043420063777451042472529806266909090824649341814868347109676190691
!!$
!!$  n  b(n)
!!$  0  12
!!$  1  30
!!$  2  210
!!$  3  371
!!$  4  22737
!!$  5  19733142
!!$  6  48264275462
!!$  7  9769214287853155785
!!$  8  113084128923675014537885725485
!!$  9  5271244267917980801966553649147604697542
!!$  10 24274291553105128438297398108902195365373879212227726
!!$  11 13346384670164266280033479022693768890138348905413621178450736182873
!!$     1234567890123456789012345678901234567890123456789012345678901234567890
!!$     1         2         3         4         5         6         7
    integer,parameter::num_frac=10
    real(rp),parameter::a(num_frac)=[&
         1.0_rp/12.0_rp,&
         1.0_rp/30.0_rp,&
         53.0_rp/210.0_rp,&
         195.0_rp/371.0_rp,&
         22999.0_rp/22737.0_rp,&
         29944523.0_rp/19733142.0_rp,&
         109535241009.0_rp/48264275462.0_rp,&
         29404527905795295658.0_rp/9769214287853155785.0_rp,&
         455377030420113432210116914702.0_rp/113084128923675014537885725485.0_rp,&
         26370812569397719001931992945645578779849.0_rp/5271244267917980801966553649147604697542.0_rp]
    real(rp),parameter::log_pi2_2=log(pi2)*0.5_rp
    real(rp),parameter::ncr2=40.0_rp**2.0_rp
    complex(cp) z,d,r
    real(rp) r1,i1,abs2
    integer i
    integer n
    if(imagpart(z1)==0.0_rp) then
       zm_lgamma=lgamma(realpart(z1))
       return
    end if
    r1=realpart(z1)
    i1=imagpart(z1)
    if(r1<=rzero) then
       z=1.0_rp-z1
    else
       z=z1
    end if
    abs2=r1*r1+i1*i1
    if(abs2<ncr2) then
       abs2=sqrt(ncr2-abs2)+0.5_rp
       n=int(abs2)
       z=z+real(n,kind=rp)
    else
       n=0
    end if
    d=a(7)/z
    do i=9,1,-1
       d=a(i)/(d+z)
    end do
    r=z-(z-0.5_rp)*log(z)-log_pi2_2
    zm_lgamma=d-r
    if(n/=0) zm_lgamma=zm_lgamma-redfac()
    if(r1<0.0_rp) zm_lgamma=pi/(sin(pi*z)*zm_lgamma)
  contains
    complex(cp) function redfac()
      integer j
      redfac=log(z1)
      do j=1,n-1
         redfac=redfac+log(z1+real(j,kind=rp))
      end do
    end function redfac

  end function zm_lgamma
  
  complex(cp) function zm_gamma(z1)
    complex(cp),intent(in)::z1
    zm_gamma=exp(zm_lgamma(z1))
  end function zm_gamma

  complex(cp) function zm_deint(a,b,ptr_integrand)
    complex(cp),intent(in)::a,b
    integer,intent(in)::ptr_integrand
    interface
       function f(z)
         use fpio, only: cp
         complex(cp) f
         complex(cp) z
       end function f
    end interface
    pointer(pf,f)
    pf=ptr_integrand
    zm_deint=(f(a)+f(b))/2.0_rp ! <<<<<< test
  end function zm_deint

end module zmath
