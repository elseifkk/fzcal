module memio
  implicit none
  public

  interface mcp
#ifdef _NO_ASM_
     module procedure cpmem
#else
     subroutine mcp(dst,src,len)
       integer,intent(in),value::dst,src,len
     end subroutine mcp
#endif
  end interface
  
#ifndef _NO_ASM_
  interface dw2str
     integer function dw2str(dw,pstr)
       integer,intent(in),value::dw
       integer,intent(in),value::pstr
     end function dw2str
  end interface dw2str
#endif

contains

#ifndef _NO_ASM_
  character*32 function itoa(i)
    integer,intent(in)::i
    integer len
    itoa=""
    len=dw2str(i,loc(itoa))
  end function itoa
#else
  character*32 function itoa(i)
    integer,intent(in)::i
    integer len,istat
    itoa=""
    write(itoa,*,iostat=istat) i
    if(istat==0) itoa=adjustl(itoa)
  end function itoa
#endif

#ifdef _NO_ASM_
  subroutine cpmem(dst,src,len)
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
  end subroutine cpmem
#endif
  
end module memio
