program fzcal
  use memio ! for test
  use fpio  ! for test
  use plist ! for test
  use com
  use rpnd
  use rpne
  use rpnp
  implicit none
  type(t_rpnc) rpnc
  pointer(p,rpnc)
  character*16384 str
  integer istat
  integer ka,n,kb
  integer cid

  p=init_rpnc()

  main: do
     write(*,10) "> "
10   format(x,a,$)
     read(*,"(a)") str
     if(str=="") cycle
     str=adjustl(str)
     cid=parse_command(rpnc,str,ka,kb)
     if(ka/=0) then
        n=atoi(str(ka:kb),ist=istat)
        if(istat/=0) then
           n=0
        else
           str(ka:kb)=""
        end if
     end if
     select case(cid)
     case(CID_EXIT)
        exit
     case(CID_SCLE)
        call reset_dat(rpnc)
        cycle
     case(-CID_PRI)
        cycle
     case(-CID_PRI_PAR)
        call dump_plist(rpnc%pars)
        cycle
     case(CID_PRI_PAR)
        call dump_plist(rpnc%pars,n,str(ka:kb))
        cycle
     case(-CID_PRI_FNC)
        call dump_rpnm(rpnc,type=SC_FNC)
        cycle
     case(CID_PRI_FNC)
        call dump_rpnm(rpnc,n,str(ka:kb),type=SC_FNC)
        cycle
     case(-CID_PRI_MAC)
        call dump_rpnm(rpnc,type=SC_MAC)
        cycle
     case(CID_PRI_MAC)
        call dump_rpnm(rpnc,n,str(ka:kb),type=SC_MAC)
        cycle
     case(CID_DEL_PAR)
        call delete_par(rpnc,str(ka:kb))
        cycle
     case(CID_DEL_PAR_ALL)
        call delete_par_all(rpnc)
        cycle
     case(CID_DEL_MAC)
     case(CID_DEL_FNC)
     case(CID_DONE,CID_INV)
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
           write(*,*) trim(rpn_sans(rpnc))
        end if
     end if
  end do main
  
end program fzcal
