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
  integer,parameter::CID_INI    = 13

contains

  integer function parse_command(a,karg)
    character*(*),intent(in)::a
    integer,intent(out)::karg
    integer k,ko,ke,cid
    karg=0
    parse_command=CID_NOP
    if(len_trim(a)==0) return
    k=index(a," ")-1
    if(k<=0) then
       ke=len_trim(a)
    else
       ke=k
    end if
    parse_command=CID_INV
    select case(a(1:ke))
    case("del","delete")
       if(k<=0) return
       cid=CID_DEL
    case("dump")
       if(k<=0) return
       cid=CID_DMP
    case("dm")
       parse_command=CID_DMP_M
       return       
    case("df")
       parse_command=CID_DMP_F
       return       
    case("dp")
       parse_command=CID_DMP_P
       return
    case("set","s")
       if(k<=0) return
       cid=CID_SET
    case("init")
       parse_command=CID_INI
       return
    end select

    ko=get_next_str()
    if(ko==0) return
    k=index(a(ko:)," ")-1
    if(k<=0) then
       ke=len_trim(a)
    else
       ke=k+ko-1
    end if

    select case(cid)
    case(CID_DMP)
       select case(a(ko:ke))
       case("p","par","parameter")
       case("m","macro","mac")
       case("f","fnc","function")
       end select
    case(CID_DEL)
    case(CID_SET)
    end select

  contains
    
    integer function get_next_str()
      integer ii
      get_next_str=0
      do ii=k+1,len_trim(a)
         select case(a(ii:ii))
         case(" ","\t")
         case default
            get_next_str=ii
         end select
      end do
    end function get_next_str


  end function parse_command
  
  

end module com
