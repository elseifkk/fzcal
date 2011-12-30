module com
  use rpnd
  implicit none
  integer,parameter::CID_INV     = -1
  integer,parameter::CID_NOP     =  0
  integer,parameter::CID_DEL     =  1
  integer,parameter::CID_DEL_P   =  2
  integer,parameter::CID_DEL_F   =  3
  integer,parameter::CID_DEL_M   =  4
  integer,parameter::CID_DUMP_P  =  6
  integer,parameter::CID_DUMP_F  =  7
  integer,parameter::CID_DUMP_M  =  8
  integer,parameter::CID_INI     = 14
  integer,parameter::CID_EXIT    = 16
  integer,parameter::CID_SCLE    = 17
  integer,parameter::CID_DONE    =999
  integer*8,parameter::digit_mask=not(ishft(Z"FF",32))
contains

#define set_opt(x) rpnc%opt=ior(rpnc%opt,(x))
#define cle_opt(x) rpnc%opt=iand(rpnc%opt,not(x))
#define set_disp_opt(x) rpnc%opt=ior(rpnc%opt,ishft((x),32))
#define cle_disp_opt(x) rpnc%opt=iand(rpnc%opt,not(ishft((x),32)))
#define cle_disp_opt(x) rpnc%opt=iand(rpnc%opt,not(ishft((x),32)))
#define put_disp_digit(x) rpnc%opt=ior(iand(rpnc%opt,digit_mask),ishft((x),32))

  integer function parse_command(rpnc,a,karg)
    use fpio
    ! a must not include dup white and must be left adjusted
    type(t_rpnc),intent(inout)::rpnc
    character*(*),intent(inout)::a
    integer,intent(out)::karg
    integer p1,p2
    integer*8 n
    parse_command=CID_NOP

    if(len_trim(a)<=1) return
    if(a(1:1)/=".") return
    p2=1
    p1=get_arg(p2)

    parse_command=CID_DONE
    select case(a(p1:p2))
    case("opt")
       write(*,"(Z16.16)") rpnc%opt
    case("q","quit")
       parse_command=CID_EXIT
    case("eng")
       set_disp_opt(ior(X2A_ENG,X2A_TRIM_ZERO))
       cle_disp_opt(ior(X2A_FIX,ior(X2A_SHOW_E0,X2A_ALLOW_ORDINARY)))
       call set_disp_digit()
    case("fix")
       set_disp_opt(X2A_FIX)
       cle_disp_opt(ior(X2A_ENG,X2A_SHOW_E0))
       call set_disp_digit()
    case("exp")
       cle_disp_opt(ior(X2A_FIX,ior(X2A_ALLOW_ORDINARY,X2A_TRIM_ZERO)))
       call set_disp_digit()
    case("fig")
       set_disp_opt(ior(X2A_ALLOW_ORDINARY,X2A_TRIM_ZERO))
       cle_disp_opt(ior(X2A_FIX,ior(X2A_ENG,X2A_SHOW_E0)))
       n=max_digit
       put_disp_digit(n)
    case("DEC")
       cle_opt(ior(RPNCOPT_INM,RPNCOPT_OUTM))
    case("HEX")
       set_opt(ior(RPNCOPT_IHEX,RPNCOPT_OHEX))
    case("OCT")
       set_opt(ior(RPNCOPT_IOCT,RPNCOPT_OOCT))
    case("BIN")
       set_opt(ior(RPNCOPT_IBIN,RPNCOPT_OBIN))
    case("Dec")
       cle_opt(RPNCOPT_INM)
    case("Hex")
       set_opt(RPNCOPT_IHEX)
    case("Oct")
       set_opt(RPNCOPT_IOCT)
    case("Bin")
       set_opt(RPNCOPT_IBIN)
    case("dec")
       cle_opt((RPNCOPT_OUTM))
    case("hex")
       set_opt(RPNCOPT_OHEX)
    case("oct")
       set_opt(RPNCOPT_OOCT)
    case("bin")
       set_opt(RPNCOPT_OBIN)
    case("deg")
       set_opt(RPNCOPT_DEG)
    case("rad")
       cle_opt(RPNCOPT_DEG)
    case("dbg","debug")
       set_opt(RPNCOPT_DEBUG)
    case("cle")
       parse_command=CID_SCLE
    case("s","sta","stat")
       set_opt(RPNCOPT_STA)
       cle_opt(RPNCOPT_DAT) ! <<<
    case("d","dat","data")
       set_opt(RPNCOPT_DAT)
    case("n","norm")
       cle_opt(ior(RPNCOPT_DAT,RPNCOPT_STA))
    case("nodbg","nodebug")
       cle_opt(RPNCOPT_DEBUG)
    case("r","ratio")
       set_opt(RPNCOPT_RATIO)
    case("f","frac")
       cle_opt(RPNCOPT_RATIO)











    case("del","delete")
    case("md")
       parse_command=CID_DUMP_M
    case("fd")
       parse_command=CID_DUMP_F
    case("pd")
       parse_command=CID_DUMP_P
    case("dm")
       parse_command=CID_DEL_M
    case("df")
       parse_command=CID_DEL_F
    case("dp")
       parse_command=CID_DEL_P
    case("init")
       parse_command=CID_INI
    case default
       parse_command=CID_INV
    end select

  contains
    
    subroutine set_disp_digit()
      integer*8 nn
      p1=get_arg(p2)
      if(p1>0) then
         nn=atoi(a(p1:p2))
         put_disp_digit(nn)
      end if
    end subroutine set_disp_digit

    integer function get_arg(pp2)
      integer,intent(out)::pp2
      integer kk
      do kk=p2+1,len_trim(a)
         select case(a(kk:kk))
         case(" ","\t")
         case default
            get_arg=kk
            pp2=index(a(kk:)," ")
            if(pp2==0) then
               pp2=len_trim(a)
            else
               pp2=pp2-1+kk-1
            end if
            return
         end select
      end do
      get_arg=0
    end function get_arg

  end function parse_command

end module com
