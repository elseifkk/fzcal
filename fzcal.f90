program fzcal
  use memio ! for test
  use fpio  ! for test
  use plist ! for test
  use com
  use rpn
  implicit none
  type(t_rpnc) rpnc
  pointer(p,rpnc)
  character*16384 str
  integer istat
  integer ka
  integer cid

  p=init_rpnc()

  main: do
     write(*,10) "> "
10   format(x,a,$)
     read(*,"(a)") str
     if(str=="") cycle
     str=adjustl(str)
     cid=parse_command(rpnc,str,ka)
     select case(cid)
     case(CID_EXIT)
        exit main
     case(CID_DUMP_P)
        call dump_plist(rpnc%pars)
        cycle main
     case(CID_DUMP_F)
        call dump_rpnm(rpnc,atoi(trim(adjustl(str(ka:)))))
        cycle
     case(CID_DUMP_M)
        call dump_rpnm(rpnc,atoi(trim(adjustl(str(ka:)))))
        cycle
     case(CID_DEL_P)
        call delete_par(rpnc,trim(adjustl(str(ka:))))
        cycle
     case(CID_DEL_M)
     case(CID_DEL_F)
     case(CID_DONE)
        cycle
     end select
     
     istat=parse_formula(rpnc,str)

     if(istat>0) then
        write(*,*)  "*** parse_formula failed: code = ",istat
        call dump_rpnc(rpnc)
     else if(istat==0) then
        if(iand(rpnc%opt,RPNCOPT_DEBUG)/=0) then
           write(*,*) "=== Before eval ==="
           call dump_rpnc(rpnc)
        end if
        
        istat=eval(rpnc)

        if(istat/=0) then
           write(*,*) "*** rpn_eval failed: code = ",istat
           call dump_rpnc(rpnc)
        else
           if(iand(rpnc%opt,RPNCOPT_DEBUG)/=0) then
              write(*,*) "=== After eval ==="
              call dump_rpnc(rpnc)
           end if
           write(*,*) trim(ztoa(rpn_ans(rpnc)))
        end if
     end if
  end do main
  
end program fzcal
