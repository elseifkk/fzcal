program fzcal
  use com
  use rpn
  use plist ! for test
  use memio ! for test
  use fpio  ! for test
  implicit none
  type(t_rpnc) rpnc
  pointer(p,rpnc)
  character*16384 str
  integer istat
  integer i
  integer ka
  integer cid
  integer*8 aaa
  p=init_rpnc()

  do
     rpnc%opt=iand(rpnc%opt,not(RPNCOPT_DBG))
     write(*,10) "> "
10   format(x,a,$)
     read(*,"(a)") str
     if(str=="") cycle
     str=adjustl(str)
     cid=parse_command(str,ka)
     select case(cid)
     case(CID_EXI)
        exit
     case(CID_DMP_P)
        call dump_plist(rpnc%pars)
        cycle
     case(CID_DMP_F)
        call dump_rpnm(rpnc,atoi(trim(adjustl(str(ka:)))))
        cycle
     case(CID_DMP_M)
        call dump_rpnm(rpnc,atoi(trim(adjustl(str(ka:)))))
        cycle
     case(CID_DBG)
        rpnc%opt=ior(rpnc%opt,RPNCOPT_DBG)
     case(CID_DEL_P)
        call delete_par(rpnc,trim(adjustl(str(ka:))))
        cycle
     case(CID_DEL_M)
     case(CID_DEL_F)
     end select
     
     istat=rpn_set_formula(loc(str),loc(rpnc))
!     istat=parse_formula(str,rpnc,drpnb)

     if(istat>0) then
        write(*,*)  "*** parse_formula failed: code = ",istat
        call dump_rpnc(rpnc)
     else if(istat==0) then
        if(iand(rpnc%opt,RPNCOPT_DBG)/=0) then
           write(*,*) "=== Before eval ==="
           call dump_rpnc(rpnc)
        end if
        
        istat=rpn_eval(loc(rpnc))
        
        if(istat/=0) then
           write(*,*) "*** rpn_eval failed: code = ",istat
           call dump_rpnc(rpnc)
        else
           if(iand(rpnc%opt,RPNCOPT_DBG)/=0) then
              write(*,*) "=== After eval ==="
              call dump_rpnc(rpnc)
           end if
           write(*,*) trim(ztoa(rpn_ans(rpnc)))
        end if
     end if
  end do
  
end program fzcal
