module misc
implicit none

contains

  subroutine messp(s,u)
    character*(*),intent(in)::s
    integer,intent(in),optional::u
    integer unit
    if(present(u)) then
       unit=u
    else
       unit=0
    end if
    write(unit,10) s
10  format(a,$)
  end subroutine messp

  subroutine mess(s,u)
    character*(*),intent(in)::s
    integer,intent(in),optional::u
    integer unit
    if(present(u)) then
       unit=u
    else
       unit=0
    end if
    write(unit,10) s
10  format(a)
  end subroutine mess

  pure logical function is_set(a,x)
    integer*8,intent(in)::a,x
    is_set=(iand(a,x)/=0)
  end function is_set

  pure logical function is_not_set(a,x)
    integer*8,intent(in)::a,x
    is_not_set=(iand(a,x)==0)
  end function is_not_set

  pure subroutine set_opt(a,x)
    integer*8,intent(inout)::a
    integer*8,intent(in)::x
    a=ior(a,x)
  end subroutine set_opt

  pure subroutine cle_opt(a,x)
    integer*8,intent(inout)::a
    integer*8,intent(in)::x
    a=iand(a,not(x))
  end subroutine cle_opt

  pure integer function get_lo32(cid)
    integer,intent(in)::cid
    get_lo32=iand(cid,int(Z"FFFF",kind=4))
  end function get_lo32

  pure integer function get_up32(cid)
    integer,intent(in)::cid
    get_up32=iand(ishft(cid,-16),int(Z"FFFF",kind=4))
  end function get_up32

  pure integer function get_i32(lo,up)
    integer,intent(in)::lo,up
    get_i32=ior(lo,ishft(up,16))
  end function get_i32

  pure logical function is_symbol(a)
    integer,intent(in)::a
    select case(a)
    case(95,36) ! _, $
       is_symbol=.true.
    case default
       is_symbol=.false.
    end select
  end function is_symbol

  pure logical function is_alpha(a)
    integer,intent(in)::a
    integer b
    b=ior(a,32)
    is_alpha=(b>=97.and.b<=122)
  end function is_alpha
  
  pure logical function is_hex_number(a)
    integer,intent(in)::a
    is_hex_number=(a>=65.and.a<=70).or.(a>=97.and.a<=102).or.is_number(a)
  end function is_hex_number

  pure logical function is_oct_number(a)
    integer,intent(in)::a
    is_oct_number=(a>=48.and.a<=55)
  end function is_oct_number

  pure logical function is_bin_number(a)
    integer,intent(in)::a
    is_bin_number=(a>=48.and.a<=49)
  end function is_bin_number

  pure logical function is_number(a)
    integer,intent(in)::a
    is_number=(a>=48.and.a<=57)
  end function is_number
  
  pure logical function is_numeric(a)
    integer,intent(in)::a
    is_numeric=(is_number(a).or.a==46)
  end function is_numeric

  pure character*4 function i2str(i)
    integer,intent(in)::i
    i2str=log2str(i/=0)
  end function i2str

  pure character*4 function log2str(log)
    logical,intent(in)::log
    if(log) then
       log2str="on"
    else
       log2str="off"
    end if
  end function log2str

  subroutine c2fstr(pstr,s)
    use iso_c_binding, only: C_SIZE_T
    integer(C_SIZE_T),intent(in),value::pstr
    character*(*),intent(out)::s
    integer i
    character*1 c
    pointer(p,c)
    s=""
    p=pstr-1
    do i=1,len(s)
       p=p+1
       if(c==achar(0)) exit
       s(i:i)=c
    end do
  end subroutine c2fstr

  integer function replace(s1,p,ln,s2,ln2)
    character*(*),intent(inout)::s1
    character*(*),intent(in)::s2
    integer,intent(in),optional::ln2
    integer ls2,ls1
    integer,intent(in)::p,ln ! p > 0, len >= 0
    replace=0
    if(p<=0.or.ln<0) return
    if(present(ln2)) then
       ls2=ln2
    else
       ls2=len_trim(s2)
    end if
    if(p+ls2-ln>len(s1)) return
    ls1=len_trim(s1)
    s1(p:)=s2(1:ls2)//s1(p+ln:ls1)
    replace=ls2+ls1-ln
  end function replace

  integer function strip(s)
    character*(*),intent(inout)::s
    integer i,k,wc
    k=0
    wc=0
    do i=1,len(s)
       select case(s(i:i))
       case(" ","\t")
          wc=wc+1
          if(wc>1) cycle
       case(achar(0))
          exit
       case default
          wc=0
       end select
       k=k+1
       if(k/=i) s(k:k)=s(i:i)
    end do
    if(wc/=0) k=k-1
    strip=k
  end function strip

  integer function get_open_unit()
    integer unit
    logical opened
    get_open_unit=-1
    do unit=10,99
       inquire(unit=unit,opened=opened)
       if(.not.opened) then
          get_open_unit=unit
          return
       end if
    end do
  end function get_open_unit

  integer function open_file(f,print_error,ask_overwrite)
    character*(*),intent(in)::f
    logical,intent(in)::print_error,ask_overwrite
    integer istat,unit
    logical exist
    character ans
    open_file=0
    if(ask_overwrite) then
       inquire(file=f,exist=exist)
       if(exist) then
          write(*,"(a,$)") "overwrite: "//trim(f)//" ? => "
          read(*,*) ans
          if(ans/="Y".and.ans/="y") return
       end if
    end if
    unit=get_open_unit()
    open(unit=unit,file=f,iostat=istat)
    if(istat/=0) then
       if(print_error) write(*,*) "*** open_file: Error: opening file: "//trim(f)//": code = ",istat 
       open_file=0
       return
    else
       open_file=unit
    end if
  end function open_file

end module misc
