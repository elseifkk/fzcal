program print_power_of_ten
  implicit none
  interface 
     subroutine poweroften(pn,ps)
       integer,intent(in),value::pn,ps
     end subroutine poweroften
  end interface

  integer i,n,istat,len
  character*128 str
  integer*1 buf(10)
  integer,parameter::ep=selected_real_kind(18)
  integer,parameter::qp=selected_real_kind(33,4931)
  real(qp) v16
  real(ep) x,v10
  pointer(pv10,v10)
  pointer(px,x)
  integer*16 fp

  n=command_argument_count()
  if(n<=0) stop "gimme number"
  
  call get_command_argument(1,str,len)
  if(len==20) then
     read(str,"(Z20.20)",iostat=istat) fp
     px=loc(fp)
     write(*,*) x
     stop
  end if

  read(str,*,iostat=istat) n
  if(istat/=0) stop "invalid number"

  pv10=loc(buf)
  v16=10.0_qp**real(n,kind=qp)
  v10=real(v16,kind=ep)
  call print_result()

  call poweroften(loc(n),loc(buf))
  call print_result()

contains

  subroutine print_result()
    do i=10,1,-1
       write(*,"(x,z2.2,$)") buf(i)
    end do
    write(*,*)
    
    px=loc(buf)
    write(*,*) x
  end subroutine print_result

end program print_power_of_ten
