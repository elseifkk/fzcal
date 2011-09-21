module fpio
  implicit none
  public
  integer,parameter::sp=selected_real_kind(6,37)
  integer,parameter::dp=selected_real_kind(15,307)
  integer,parameter::ep=selected_real_kind(18)
  integer,parameter::qp=selected_real_kind(33,4931)
#if !defined _NO_REAL16_
  integer,parameter::rp=qp
  integer,parameter::cp=qp
#elif !defined _NO_REAL10_
  integer,parameter::rp=ep
  integer,parameter::cp=ep
#else
#define _RP_IS_DP_
  integer,parameter::rp=dp
  integer,parameter::cp=dp
#endif
#if !defined _NO_REAL10_
  real(ep),parameter::disp_huge=huge(0.0_ep)
  integer,parameter::dispp=ep
#else
  real(dp),parameter::disp_huge=huge(0.0_dp)
  integer,parameter::dispp=dp
#endif

  real(rp),parameter::eps=epsilon(0.0_rp)
  real(rp),parameter::pre=precision(0.0_rp)
  real(rp),parameter::rzero=0.0_rp
  complex(cp),parameter::czero=0.0_cp

  integer,parameter::LEN_STR_ANS_MAX=128
  
  integer,parameter::DISP_FMT_RAW  = 0
  integer,parameter::DISP_FMT_NORM = 1
  integer,parameter::DISP_FMT_SCI  = 2
  integer,parameter::DISP_FMT_ENG  = 3
  character*9,parameter::sfmt(3)=[&
       "(G64.38) ",&
       "(ES64.38)",&
       "(EN64.38)"]

  interface f2str
     integer function f2str(pf,pstr,opt)
       integer,intent(in),value::pf,pstr,opt
     end function f2str
  end interface f2str
  integer,parameter::FP2A_TRIM_TRAILING_ZEROS         =  Z"000100"
  integer,parameter::FP2A_ALLOW_INTEGER_EXPRESSION    =  Z"000200"
  integer,parameter::FP2A_ALLOW_ORDINARY_EXPRESSION   =  Z"000400"
  integer,parameter::FP2A_FORCE_NOT_SHOW_EXPSIGN      =  Z"000800"
  integer,parameter::FP2A_FORCE_SHOW_SIGN             =  Z"001000"
  integer,parameter::FP2A_KEEP_LEADING_ZEROS          =  Z"002000" ! for exponential digits
  integer,parameter::FP2A_ALLOW_ENGINEERING_NOTATION  =  Z"004000"
  integer,parameter::FP2A_INPUT_REAL10                =  Z"008000"
  integer,parameter::FP2A_SUPRESS_E0                  =  Z"010000"
  integer,parameter::FP2A_NULLTERM                    =  Z"020000"
  integer,parameter::FP2A_TRIM_ALL_TRAILING_ZEROS     =  Z"040000" ! 1.0 become 1.
  integer,parameter::FP2A_ADJUSTR                     =  Z"080000" ! LSB 4bit represents digit.
  integer,parameter::FP2A_INPUT_REAL4                 =  Z"100000"
  integer,parameter::FP2A_DEFAULT                     = ior(18,&
       ior(FP2A_ALLOW_ORDINARY_EXPRESSION,&
       ior(FP2A_FORCE_NOT_SHOW_EXPSIGN,&
       ior(FP2A_SUPRESS_E0,&
       FP2A_TRIM_TRAILING_ZEROS))))

contains

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

  character(LEN_STR_ANS_MAX) function rtoa(x,ok,fmt)
    real(rp),intent(in)::x
    logical,intent(out),optional::ok
    integer,intent(in),optional::fmt
    integer istat,f
#ifndef _NO_ASM_
    integer len,opt
#endif
    real(dispp) xx
    if(present(fmt)) then
       f=fmt
    else
       f=DISP_FMT_NORM
    end if
    if(abs(x)>disp_huge) f=DISP_FMT_RAW

    if(f==DISP_FMT_RAW) then
       write(rtoa,*,iostat=istat) x
       if(istat==0) rtoa=adjustl(rtoa)
       if(present(ok)) ok=(istat==0)
       return
    end if

    xx=real(x,kind=dispp)
    rtoa=""
#ifdef _NO_ASM_
    write(rtoa,sfmt(f),iostat=istat) x
    if(istat==0) rtoa=adjustl(rtoa)
    rtoa=trim_zero(rtoa)
#else
    opt=FP2A_DEFAULT
    if(dispp==ep) opt=ior(opt,FP2A_INPUT_REAL10)
    len=f2str(loc(xx),loc(rtoa),opt)
    if(len>0) then
       istat=0
    else
       istat=-1
    end if
#endif
    if(present(ok)) ok=(istat==0)
  end function rtoa
  
  character(LEN_STR_ANS_MAX) function ztoa(z,ok,fmt)
    complex(cp),intent(in)::z
    logical,intent(out),optional::ok
    integer,intent(in),optional::fmt
    logical retlog
    if(z==0) then
       ztoa="0"
       if(present(ok)) ok=.true.
       return
    end if
    if(present(fmt).and.fmt==DISP_FMT_RAW) then
       ztoa="( "//trim(rtoa(realpart(z),ok,fmt))//", "&
            //trim(rtoa(imagpart(z),ok,fmt))//" )"
       return
    end if
    if(imagpart(z)/=rzero) then
       ztoa=trim(rtoa(imagpart(z),retlog))//" i"
       if(.not.retlog) then
          if(present(ok)) ok=retlog
          return
       end if
       if(realpart(z)/=rzero) then
          if(imagpart(z)>rzero) then
             ztoa=" + "//trim(ztoa)
          else if(ztoa(1:1)=="-") then
             ztoa=" - "//trim(ztoa(2:))
          else
             ztoa=" - "//trim(ztoa)
          end if
       end if
    else
       ztoa=""
    end if
    if(realpart(z)==rzero) return
    ztoa=trim(rtoa(realpart(z),retlog))//trim(ztoa)
    if(present(ok)) ok=retlog
  end function ztoa

end module fpio
