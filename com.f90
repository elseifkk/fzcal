module com
  use rpn
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
    case("deg")
       rpnc%opt=ior(rpnc%opt,RPNCOPT_DEG)
       parse_command=CID_DONE
    case("rad")
       rpnc%opt=iand(rpnc%opt,not(RPNCOPT_DEG))
       parse_command=CID_DONE
    case("dbg","debug")
       rpnc%opt=iand(rpnc%opt,RPNCOPT_DEBUG)
       parse_command=CID_DONE
    case("nodbg","nodebug")
       rpnc%opt=iand(rpnc%opt,not(RPNCOPT_DEBUG))
       parse_command=CID_DONE
    case("del","delete")
       cid=CID_DEL
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
