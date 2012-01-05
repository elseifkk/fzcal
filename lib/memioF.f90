module memio
  implicit none
  public

  integer,parameter::DISP_FMT_RAW   = 0
  integer,parameter::DISP_FMT_NORM  = 1

  integer,parameter::DISP_FMT_BIN   = -1
  integer,parameter::DISP_FMT_OCT   = -2
  integer,parameter::DISP_FMT_DEC   = -3
  integer,parameter::DISP_FMT_HEX   = -4

#ifndef _USE32_
  integer,parameter::ptrsz=selected_int_kind(20)
#else
  integer,parameter::ptrsz=selected_int_kind(10)
#endif

#ifndef _NO_ASM_
  interface
     subroutine mcp(dst,src,len)
       integer,intent(in),value::dst,src,len
     end subroutine mcp
  end interface
  interface
     subroutine mcle(dst,len)
       integer,intent(in),value::dst,len
     end subroutine mcle
  end interface
#endif

  interface atoi
     module procedure atoi_4
     module procedure atoi_8
  end interface

  interface itoa
     module procedure itoa_4
     module procedure itoa_8
  end interface

contains

  integer*4 function atoi_4(a,n_dummy,fmt,ist)
    character*(*),intent(in)::a
    integer*4,intent(in)::n_dummy
    integer,intent(in),optional::fmt
    integer,intent(out),optional::ist
    atoi_4=int(atoi_8(a,0_8,fmt,ist),kind=4)
  end function atoi_4

  integer*8 function atoi_8(a,n_dummy,fmt,ist)
    character*(*),intent(in)::a
    integer*8,intent(in)::n_dummy
    integer,intent(in),optional::fmt
    integer,intent(out),optional::ist
    integer istat,f
    character*32 sfmt
    if(present(fmt)) then
       f=fmt
    else
       f=DISP_FMT_DEC
    end if
    select case(f)
    case(DISP_FMT_DEC,DISP_FMT_NORM,DISP_FMT_RAW)
       read(a,*,iostat=istat) atoi_8
       if(present(ist)) ist=istat
       return
    case(DISP_FMT_BIN)
       sfmt="(B"
    case(DISP_FMT_OCT)
       sfmt="(O"
    case(DISP_FMT_HEX)
       sfmt="(Z"
    end select
    sfmt=trim(sfmt)//trim(itoa(len_trim(a)))//")"
    read(a,sfmt,iostat=istat) atoi_8
    if(present(ist)) ist=istat
  end function atoi_8


  character*32 function itoa_4(i,fmt) 
    integer*4,intent(in)::i
    integer,intent(in),optional::fmt
    itoa_4=itoa_8(int(i,kind=8),fmt)
  end function itoa_4

  character*32 function itoa_8(i,fmt) 
    integer*8,intent(in)::i
    integer,intent(in),optional::fmt
    integer f,istat
    character*32 sfmt
    itoa_8=""
    if(present(fmt)) then
       f=fmt
    else
       f=DISP_FMT_DEC
    end if
    select case(f)
    case(DISP_FMT_RAW)
       write(itoa_8,*,iostat=istat) i
       if(istat==0) itoa_8=adjustl(itoa_8)
       return
    case(DISP_FMT_DEC,DISP_FMT_NORM)
       sfmt="(I0)"
    case(DISP_FMT_HEX)
       sfmt="(Z0)"
    case(DISP_FMT_BIN)
       sfmt="(B0)"
    case(DISP_FMT_OCT)
       sfmt="(O0)"
    end select
    write(itoa_8,sfmt,iostat=istat) i
    if(istat==0) itoa_8=adjustl(itoa_8)
  end function itoa_8

#ifdef _NO_ASM_
  subroutine mcp(dst,src,len)
    integer,intent(in)::dst,src,len
    integer*1 s,d
    integer i
    pointer(si,s)
    pointer(di,d)
    si=src-1
    di=dst-1
    do i=1,len
       si=si+1
       di=di+1
       d=s
    end do
  end subroutine mcp

  subroutine mcle(dst,len)
    integer,intent(in)::dst,len
    integer*1 d
    integer i
    pointer(di,d)
    di=dst-1
    do i=1,len
       di=di+1
       d=int(0,kind=1)
    end do
  end subroutine mcle
#endif
  
end module memio
