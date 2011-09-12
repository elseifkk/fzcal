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
  integer,parameter::rp=dp
  integer,parameter::cp=dp
#endif
#if !defined _NO_REAL10_
  real(ep),parameter::disp_huge=huge(0.0_ep)
#else
  real(dp),parameter::disp_huge=huge(0.0_dp)
#endif

  real(rp),parameter::eps=epsilon(0.0_rp)
  real(rp),parameter::pre=precision(0.0_rp)
  real(rp),parameter::rzero=0.0_rp
  complex(cp),parameter::czero=0.0_cp

  integer,parameter::LEN_STR_ANS_MAX=128

contains
  
  character(LEN_STR_ANS_MAX) function rtoa(x,ok,fmt)
    real(rp),intent(in)::x
    logical,intent(out),optional::ok
    logical,intent(in),optional::fmt
    integer istat
    if(present(fmt).and..not.fmt) then
       write(rtoa,*,iostat=istat) x
       if(istat==0) rtoa=adjustl(rtoa)
       if(present(ok)) ok=(istat==0)
       return
    end if
    if(abs(x)<eps) then
       rtoa="0"
       if(present(ok)) ok=.true.
       return
    end if
    rtoa=""
    if(abs(x)<disp_huge) then
       write(rtoa,"(G32.16)",iostat=istat) x
    else
       write(rtoa,*,iostat=istat) x
    end if
    if(present(ok)) ok=(istat==0)
    if(istat==0) rtoa=adjustl(rtoa)
  end function rtoa
  
  character(LEN_STR_ANS_MAX) function ztoa(z,ok,fmt)
    complex(cp),intent(in)::z
    logical,intent(out),optional::ok
    logical,intent(in),optional::fmt
    logical retlog
    if(z==0) then
       ztoa="0"
       if(present(ok)) ok=.true.
       return
    end if
    if(present(fmt).and..not.fmt) then
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
