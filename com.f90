module com
  implicit none
  integer,parameter::CID_INV    = -1
  integer,parameter::CID_NOP    =  0
  integer,parameter::CID_DEL    =  1
  integer,parameter::CID_DEL_P  =  2
  integer,parameter::CID_DEL_F  =  3
  integer,parameter::CID_DEL_M  =  4
  integer,parameter::CID_DMP    =  5
  integer,parameter::CID_DMP_P  =  6
  integer,parameter::CID_DMP_F  =  7
  integer,parameter::CID_DMP_M  =  8
  integer,parameter::CID_DMP_C  =  9
  integer,parameter::CID_DMP_B  = 10
  integer,parameter::CID_SET    = 11
  integer,parameter::CID_SET_DM = 12
  integer,parameter::CID_SET_MD = 13
  integer,parameter::CID_INI    = 14
  integer,parameter::CID_DBG    = 15
  integer,parameter::CID_EXI    = 16

contains

  integer function parse_command(a,karg)
    character*(*),intent(inout)::a
    integer,intent(out)::karg
    integer k,ko,ke,cid

    parse_command=CID_NOP

    if(len_trim(a)==0) return
    k=index(a," ")-1
    if(k<=0) then
       ke=len_trim(a)
       karg=0
    else
       ke=k
       karg=k+1
    end if

    cid=CID_NOP
    select case(a(1:ke))
    case("q","quit")
       parse_command=CID_EXI
       return
    case("del","delete")
       if(karg<=0) return
       cid=CID_DEL
    case("debug","dbg")
       parse_command=CID_DBG
       a(1:ke)=""
       return
    case("d")
       parse_command=CID_SET_DM
       return
    case("dump")
       if(k<=0) return
       cid=CID_DMP
    case("md")
       parse_command=CID_DMP_M
       return       
    case("fd")
       parse_command=CID_DMP_F
       return       
    case("pd")
       parse_command=CID_DMP_P
       return
    case("dm")
       parse_command=CID_DEL_M
       return       
    case("df")
       parse_command=CID_DEL_F
       return       
    case("dp")
       parse_command=CID_DEL_P
       return
    case("set","s")
       if(k<=0) return
       cid=CID_SET
    case("init")
       parse_command=CID_INI
       return
    case default
       parse_command=CID_INV
       return
    end select

    if(cid==CID_NOP) return

    karg=0
    ko=get_next_str()
    if(ko==0) return

    k=index(a(ko:)," ")-1
    if(k<=0) then
       ke=len_trim(a)
       karg=0
    else
       ke=k+ko-1
       karg=ke+1
    end if

    select case(cid)
    case(CID_DMP)
       select case(a(ko:ke))
       case("p","par","parameter")
          parse_command=CID_DMP_P
          return
       case("m","macro","mac")
          parse_command=CID_DMP_M
          return
       case("f","fnc","function")
          parse_command=CID_DMP_F
          return
       end select
    case(CID_DEL)
       select case(a(ko:ke))
       case("p","par","parameter")
          parse_command=CID_DEL_P
          return
       case("m","macro","mac")
          parse_command=CID_DEL_M
          return
       case("f","fnc","function")
          parse_command=CID_DEL_F
          return
       end select
    case(CID_SET)
       select case(a(ko:ke))
       case("d","disp","display")
          cid=CID_SET_DM
       case("m","mode")
          cid=CID_SET_MD
       end select
    end select

  contains
    
    integer function get_next_str()
      integer ii
      get_next_str=0
      do ii=ke+1,len_trim(a)
         select case(a(ii:ii))
         case(" ","\t")
         case default
            get_next_str=ii
            return
         end select
      end do
    end function get_next_str


  end function parse_command
  
  

end module com
