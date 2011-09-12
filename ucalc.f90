program ucalc
  use rpn
  use plist ! for test
  use fpio
  use memio ! for test
  implicit none
  type(t_rpnc) rpnc
  pointer(p,rpnc)
  character*16384 str
  integer istat
  logical drpnb
  integer i

  p=init_rpnc()

  do
     drpnb=.false.
     write(*,10) "> "
10   format(x,a,$)
     read(*,"(a)") str
     if(str=="") cycle
     select case(str)
     case("quit","exit","q")
        exit
     case("pdump","pd")
        call dump_plist(rpnc%pars)
        cycle
     end select
     
     if(str(1:4)=="mac ") then
        read(str(5:),*,iostat=istat) i
        if(istat==0) call dump_rpnm(rpnc,i)
        cycle
     end if

     if(str(1:4)=="del ".and.len_trim(str)>=5) then
        call delete_par(rpnc,str(5:))
        cycle
     end if

     if(str(1:5)=="dump ".and.len_trim(str)>=6) then
        drpnb=.true.
        str(1:4)=""
     end if
     
     str=trim(str)//achar(0)
     istat=rpn_set_formula(loc(str),loc(rpnc))
!     istat=parse_formula(str,rpnc,drpnb)

     if(istat>0) then
        write(*,*)  "*** parse_formula failed: code = ",istat
        call dump_rpnc(rpnc)
     else if(istat==0) then
        if(drpnb) then
           write(*,*) "=== Before eval ==="
           call dump_rpnc(rpnc)
        end if
        
        istat=rpn_eval(loc(rpnc))
        
        if(istat/=0) then
           write(*,*) "*** rpn_eval failed: code = ",istat
           call dump_rpnc(rpnc)
        else
           if(drpnb) then
              write(*,*) "=== After eval ==="
              call dump_rpnc(rpnc)
           end if
           write(*,*) trim(ztoa(rpn_ans(rpnc)))
        end if
     end if
  end do
  
end program ucalc
