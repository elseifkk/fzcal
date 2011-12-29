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

contains

  integer function parse_command(rpnc,a,karg)
    ! a must not include dup white and must be left adjusted
    type(t_rpnc),intent(inout)::rpnc
    character*(*),intent(inout)::a
    integer,intent(out)::karg
    integer k,ko,ke,cid

    parse_command=CID_NOP

    if(len_trim(a)<=1) return
    if(a(1:1)/=".") return

    k=index(a," ")-1
    if(k<=0) then
       ke=len_trim(a)
       karg=0
    else
       ke=k
       karg=k+1
    end if

    select case(a(2:ke))
    case("q","quit")
       parse_command=CID_EXIT
    case("DEC")
       rpnc%opt=iand(rpnc%opt,not(ior(RPNCOPT_INM,RPNCOPT_OUTM)))
       parse_command=CID_DONE
    case("HEX")
       rpnc%opt=ior(rpnc%opt,ior(RPNCOPT_IHEX,RPNCOPT_OHEX))
       parse_command=CID_DONE
    case("OCT")
       rpnc%opt=ior(rpnc%opt,ior(RPNCOPT_IOCT,RPNCOPT_OOCT))
       parse_command=CID_DONE
    case("BIN")
       rpnc%opt=ior(rpnc%opt,ior(RPNCOPT_IBIN,RPNCOPT_OBIN))
       parse_command=CID_DONE
    case("Dec")
       rpnc%opt=iand(rpnc%opt,not(RPNCOPT_INM))
       parse_command=CID_DONE
    case("Hex")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_IHEX)
       parse_command=CID_DONE
    case("Oct")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_IOCT)
       parse_command=CID_DONE
    case("Bin")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_IBIN)
       parse_command=CID_DONE
    case("dec")
       rpnc%opt=iand(rpnc%opt,not(RPNCOPT_OUTM))
       parse_command=CID_DONE
    case("hex")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_OHEX)
       parse_command=CID_DONE
    case("oct")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_OOCT)
       parse_command=CID_DONE
    case("bin")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_OBIN)
       parse_command=CID_DONE
    case("deg")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_DEG)
       parse_command=CID_DONE
    case("rad")
       rpnc%opt=iand(rpnc%opt,not(RPNCOPT_DEG))
       parse_command=CID_DONE
    case("dbg","debug")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_DEBUG)
       parse_command=CID_DONE 
    case("cle")
       parse_command=CID_SCLE
    case("s","sta","stat")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_STA)
       rpnc%opt=iand(rpnc%opt,not(RPNCOPT_DAT)) ! <<<
       parse_command=CID_DONE
    case("d","dat","data")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_DAT)
       parse_command=CID_DONE
    case("n","norm")
       rpnc%opt=iand(rpnc%opt,not(ior(RPNCOPT_DAT,RPNCOPT_STA)))
       parse_command=CID_DONE
    case("nodbg","nodebug")
       rpnc%opt=iand(rpnc%opt,not(RPNCOPT_DEBUG))
       parse_command=CID_DONE
    case("del","delete")
       cid=CID_DEL
    case("r","ratio")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_RATIO)
       parse_command=CID_DONE
    case("f","frac")
       rpnc%opt=iand(rpnc%opt,not(RPNCOPT_RATIO))
       parse_command=CID_DONE
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

  end function parse_command

end module com
