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
  logical calc,echo
  integer fu
  character*32 prompt

  p=init_rpnc()

  fu=0
  prompt=">"
  main: do
     calc=.false.
     if(fu==0) then
        write(*,10) trim(prompt)//" "
10      format(x,a,$)
        read(*,"(a)") str
     else
        call read_line()
        if(istat/=0) then
           close(fu)
           fu=0
           cycle
        end if
     end if
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
        call reset_sd(rpnc%sd)
     case(-CID_PRI)
     case(-CID_PRI_PAR)
        call dump_plist(rpnc%pars)
     case(CID_PRI_PAR)
        call dump_plist(rpnc%pars,n,str(ka:kb))
     case(-CID_PRI_FNC)
        call dump_rpnm(rpnc,type=SC_FNC)
     case(CID_PRI_FNC)
        call dump_rpnm(rpnc,n,str(ka:kb),type=SC_FNC)
     case(-CID_PRI_MAC)
        call dump_rpnm(rpnc,type=SC_MAC)
     case(CID_PRI_MAC)
        call dump_rpnm(rpnc,n,str(ka:kb),type=SC_MAC)
     case(CID_DEL_PAR)
        call delete_par(rpnc,str(ka:kb))
     case(CID_DEL_PAR_ALL)
        call delete_par_all(rpnc)
     case(CID_PRI_DAT)
        call dump_sd(rpnc)
     case(CID_DEL_MAC)
     case(CID_DEL_FNC)
     case(CID_LOAD)
        if(fu/=0) cycle ! <<<<<<<<<<<<<<<<<<<<<<
        fu=123
        open(unit=fu,iostat=istat,file=str(ka:kb),status="old")
        if(istat/=0) then
           write(*,*) "Cannot open: "//str(ka:kb)
           fu=0
        end if
     case(-CID_ECHO)
        write(*,*) "echo is "//trim(log2str(echo)) 
     case(CID_ECHO_OFF)
        echo=.false.
     case(CID_ECHO_ON)
        echo=.true.
     case(-CID_SET_PROMPT)
     case(CID_SET_PROMPT)
        prompt=str(ka:kb)
     case(-CID_WRITE)
        write(*,*) 
     case(CID_WRITE)
        write(*,*) trim(str(ka:kb))
     case(CID_DONE)
     case(CID_INV)
        write(*,*) "Invalid command: "//trim(str)
     case default
        calc=.true.
     end select
     
     if(.not.calc) cycle

     if(echo) write(*,*) trim(prompt)//" "//trim(str)
     istat=parse_formula(rpnc,str)

     if(istat>0) then
        write(*,*)  "*** parse_formula failed: code = ",istat
        call dump_rpnc(rpnc)
     else if(istat==0) then
        if(iand(rpnc%opt,RPNCOPT_DEBUG)/=0) then
           write(*,*) "=== Before eval ==="
           call dump_rpnc(rpnc)
        end if
        
        do while(rpnc%ip/=0)
           istat=eval(rpnc)
           if(istat/=0) then
              write(*,*) "*** rpn_eval failed: code = ",istat
              call dump_rpnc(rpnc)
              exit
           else
              if(iand(rpnc%opt,RPNCOPT_DEBUG)/=0) then
                 write(*,*) "=== After eval ==="
                 call dump_rpnc(rpnc)
              end if
              write(*,*) trim(rpn_sans(rpnc))
           end if
        end do

     end if

  end do main
  
contains
  
  character*4 function log2str(log)
    logical,intent(in)::log
    if(log) then
       log2str="on"
    else
       log2str="off"
    end if
  end function log2str

  subroutine read_line()
    read(fu,"(a)",iostat=istat) str
  end subroutine read_line

end program fzcal
