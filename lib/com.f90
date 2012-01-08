module com
  use rpnd
  implicit none
  integer,parameter::CID_INV         =  -999
  integer,parameter::CID_NOP         =   0
  integer,parameter::CID_DEL         =   1
  integer,parameter::CID_DEL_PAR     =   2 
  integer,parameter::CID_DEL_FNC     =   3 
  integer,parameter::CID_DEL_MAC     =   4 
  integer,parameter::CID_DEL_PAR_ALL =   5 
  integer,parameter::CID_DEL_FNC_ALL =   6 
  integer,parameter::CID_DEL_MAC_ALL =   7 
  integer,parameter::CID_PRI         =   8 
  integer,parameter::CID_PRI_PAR     =   9
  integer,parameter::CID_PRI_FNC     =  10 
  integer,parameter::CID_PRI_MAC     =  11 
  integer,parameter::CID_PRI_DAT     =  12 
  integer,parameter::CID_INI         =  13 
  integer,parameter::CID_EXIT        =  14
  integer,parameter::CID_SCLE        =  15
  integer,parameter::CID_LOAD        =  16
  integer,parameter::CID_ECHO        =  17
  integer,parameter::CID_ECHO_ON     =  18
  integer,parameter::CID_ECHO_OFF    =  19
  integer,parameter::CID_SET_PROMPT  =  20
  integer,parameter::CID_WRITE       =  21
  integer,parameter::CID_READ        =  22
  integer,parameter::CID_DONE        = 999

  integer,parameter::AK_INV    =  -1
  integer,parameter::AK_ALL    =   0
  integer,parameter::AK_PAR    =   1
  integer,parameter::AK_FNC    =   2
  integer,parameter::AK_MAC    =   3
  integer,parameter::AK_DAT    =   4
  integer,parameter::AK_ANY    =   5
  integer,parameter::AK_ON     =   6
  integer,parameter::AK_OFF    =   7

  integer*8,parameter::digit_mask=not(ishft(Z"FF",32))

contains

#define set_opt(x) rpnc%opt=ior(rpnc%opt,(x))
#define cle_opt(x) rpnc%opt=iand(rpnc%opt,not(x))
#define set_disp_opt(x) rpnc%opt=ior(rpnc%opt,ishft((x),32))
#define cle_disp_opt(x) rpnc%opt=iand(rpnc%opt,not(ishft((x),32)))
#define cle_disp_opt(x) rpnc%opt=iand(rpnc%opt,not(ishft((x),32)))
#define put_disp_digit(x) rpnc%opt=ior(iand(rpnc%opt,digit_mask),ishft((x),32))

  integer function parse_command(rpnc,a,p1arg,p2arg,b)
    use fpio
    ! a must not include dup white and must be left adjusted
    type(t_rpnc),intent(inout)::rpnc
    character*(*),intent(in)::a
    integer,intent(out),optional::p1arg,p2arg
    character*(*),intent(inout),optional::b
    integer p1,p2
    integer*8 n
    integer lencom

    parse_command=CID_NOP
    if(present(p1arg)) p1arg=0
    if(present(p2arg)) p2arg=0
    lencom=str_len_trim(a)
    if(lencom<=1) return
    if(a(1:1)/=".") return
    p2=1
    p1=get_arg(p2)

    do
       select case(parse_command)
       case(CID_NOP)
          select case(a(p1:p2))
          case("read")
             call read_arg
             parse_command=CID_READ
          case("write")
             parse_command=-CID_WRITE
          case("prompt")
             parse_command=-CID_SET_PROMPT
          case("echo")
             parse_command=-CID_ECHO
          case("load")
             parse_command=-CID_LOAD
          case("opt")
             write(*,"(Z16.16)") rpnc%opt
             parse_command=CID_DONE
          case("q","quit")
             parse_command=CID_EXIT
          case("eng")
             cle_opt((RPNCOPT_OUTM))
             set_disp_opt(ior(X2A_ENG,X2A_TRIM_ZERO))
             cle_disp_opt(ior(X2A_FIX,ior(X2A_SHOW_E0,X2A_ALLOW_ORDINARY)))
             call set_disp_digit()
             parse_command=CID_DONE
          case("fix")
             cle_opt((RPNCOPT_OUTM))
             set_disp_opt(X2A_FIX)
             cle_disp_opt(ior(X2A_ENG,X2A_SHOW_E0))
             call set_disp_digit()
             parse_command=CID_DONE
          case("exp")
             cle_opt((RPNCOPT_OUTM))
             cle_disp_opt(ior(X2A_FIX,ior(X2A_ENG,ior(X2A_ALLOW_ORDINARY,X2A_TRIM_ZERO))))
             call set_disp_digit()
             parse_command=CID_DONE
          case("fig")
             cle_opt((RPNCOPT_OUTM))
             set_disp_opt(ior(X2A_ALLOW_ORDINARY,X2A_TRIM_ZERO))
             cle_disp_opt(ior(X2A_FIX,ior(X2A_ENG,X2A_SHOW_E0)))
             n=max_digit
             put_disp_digit(n)
             parse_command=CID_DONE
          case("DEC")
             cle_opt(ior(RPNCOPT_INM,RPNCOPT_OUTM))
             parse_command=CID_DONE
          case("HEX")
             set_opt(ior(RPNCOPT_IHEX,RPNCOPT_OHEX))
             parse_command=CID_DONE
          case("OCT")
             set_opt(ior(RPNCOPT_IOCT,RPNCOPT_OOCT))
             parse_command=CID_DONE
          case("BIN")
             set_opt(ior(RPNCOPT_IBIN,RPNCOPT_OBIN))
             parse_command=CID_DONE
          case("Dec")
             cle_opt(RPNCOPT_INM)
             parse_command=CID_DONE
          case("Hex")
             set_opt(RPNCOPT_IHEX)
             parse_command=CID_DONE
          case("Oct")
             set_opt(RPNCOPT_IOCT)
             parse_command=CID_DONE
          case("Bin")
             set_opt(RPNCOPT_IBIN)
             parse_command=CID_DONE
          case("dec")
             cle_opt((RPNCOPT_OUTM))
             parse_command=CID_DONE
          case("hex")
             set_opt(RPNCOPT_OHEX)
             parse_command=CID_DONE
          case("oct")
             set_opt(RPNCOPT_OOCT)
             parse_command=CID_DONE
          case("bin")
             set_opt(RPNCOPT_OBIN)
             parse_command=CID_DONE
          case("deg")
             set_opt(RPNCOPT_DEG)
             parse_command=CID_DONE
          case("rad")
             cle_opt(RPNCOPT_DEG)
             parse_command=CID_DONE
          case("dbg","debug")
             set_opt(RPNCOPT_DEBUG)
             parse_command=CID_DONE
          case("cle")
             parse_command=CID_SCLE
          case("s","sta","stat")
             set_opt(RPNCOPT_STA)
             cle_opt(RPNCOPT_DAT) ! <<<
             parse_command=CID_DONE
          case("d","dat","data")
             set_opt(RPNCOPT_DAT)
             parse_command=CID_DONE
          case("n","norm")
             cle_opt(ior(RPNCOPT_DAT,RPNCOPT_STA))
             parse_command=CID_DONE
          case("nodbg","nodebug")
             cle_opt(RPNCOPT_DEBUG)
             parse_command=CID_DONE
          case("r","ratio")
             set_opt(RPNCOPT_RATIO)
             parse_command=CID_DONE
          case("f","frac")
             cle_opt(RPNCOPT_RATIO)
             parse_command=CID_DONE
          case("del","delete")
             parse_command=-CID_DEL
          case("dm")
             parse_command=-CID_DEL_MAC
          case("df")
             parse_command=-CID_DEL_FNC
          case("dp")
             parse_command=-CID_DEL_PAR
          case("p","print")
             parse_command=-CID_PRI
          case("pm")
             parse_command=-CID_PRI_MAC
          case("pf")
             parse_command=-CID_PRI_FNC
          case("pp")
             parse_command=-CID_PRI_PAR   
          case("pd")
             parse_command=CID_PRI_DAT   
          case("init")
             parse_command=CID_INI
          case default
             parse_command=CID_INV
             exit
          end select
       case(-CID_ECHO)
          select case(get_ak(a(p1:p2)))
          case(AK_ON)
             parse_command=CID_ECHO_ON
          case(AK_OFF)
             parse_command=CID_ECHO_OFF
          case default
             parse_command=CID_INV
             exit
          end select
       case(-CID_DEL)
          select case(get_ak(a(p1:p2)))
          case(AK_PAR)
             parse_command=-CID_DEL_PAR
          case(AK_MAC)
             parse_command=-CID_DEL_MAC
          case(AK_FNC)
             parse_command=-CID_DEL_FNC
          case default
             parse_command=CID_INV
             exit
          end select
       case(-CID_PRI)
          select case(get_ak(a(p1:p2)))
          case(AK_PAR)
             parse_command=-CID_PRI_PAR
          case(AK_MAC)
             parse_command=-CID_PRI_MAC
          case(AK_FNC)
             parse_command=-CID_PRI_FNC
          case(AK_DAT)
             parse_command=CID_PRI_DAT
          case default
             parse_command=CID_INV
             exit
          end select
       case(-CID_WRITE)
          if(present(p1arg)) p1arg=p1
          if(present(p2arg)) p2arg=lencom
          parse_command=CID_WRITE
       case(-CID_PRI_PAR,-CID_PRI_FNC,-CID_PRI_MAC,-CID_LOAD,-CID_SET_PROMPT)
          if(present(p1arg)) p1arg=p1
          if(present(p2arg)) p2arg=p2
          parse_command=-parse_command
       case(-CID_DEL_PAR,-CID_DEL_FNC,-CID_DEL_MAC)
          parse_command=-parse_command
          if(get_ak(a(p1:p2))==AK_ALL) then
             parse_command=3+parse_command ! <<<
          else
             if(present(p1arg)) p1arg=p1
             if(present(p1arg)) p2arg=p2
          end if
       case default
          stop "internal error"
       end select

       if(parse_command>=0) exit

       p1=get_arg(p2)
       if(p1==0) exit

    end do

  contains
    
    subroutine set_disp_digit()
      integer*8 nn
      p1=get_arg(p2)
      if(p1>0) then
         nn=atoi(a(p1:p2),nn)
         put_disp_digit(nn)
      end if
    end subroutine set_disp_digit

    integer function get_arg(pp2)
      integer,intent(out)::pp2
      integer kk
      do kk=p2+1,lencom
         select case(a(kk:kk))
         case(" ","\t")
         case default
            get_arg=kk
            pp2=index(a(kk:)," ")
            if(pp2==0) then
               pp2=lencom
            else
               pp2=pp2-1+kk-1
            end if
            return
         end select
      end do
      get_arg=0
    end function get_arg

    integer function get_ak(ss)
      character*(*),intent(in)::ss
      get_ak=AK_INV
      select case(ss)
      case("f","func","function")
         get_ak=AK_FNC
      case("m","mac","macro")
         get_ak=AK_MAC
      case("p","par","parameter")
         get_ak=AK_PAr
      case("d","dat","data")
         get_ak=AK_DAT
      case(".all")
         get_ak=AK_ALL
      case(".any")
         get_ak=AK_ANY
      case("on")
         get_ak=AK_ON
      case("off")
         get_ak=AK_OFF
      end select
    end function get_ak

    subroutine read_arg()
      integer kk,jj
      character*128 ans ! <<<<<<<<<<
      character(LEN_FORMULA_MAX) str
      if(.not.present(b)) return
      write(*,"(a)") "Input: "//trim(adjustl(a(p2+1:lencom)))
      jj=0
      do kk=p2+1,lencom
         select case(a(kk:kk))
         case("?")
            read(*,"(a)") ans
            str(jj+1:)=trim(ans)
            jj=jj+len_trim(ans)
         case default
            jj=jj+1
            str(jj:jj)=a(kk:kk)
         end select
      end do
      b=trim(str)
    end subroutine read_arg

  end function parse_command

end module com
