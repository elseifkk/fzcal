!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2011-2012 by Kazuaki Kumagai                            *
! *   elseifkk@users.sf.net                                                 *
! *                                                                         *
! *   This program is free software; you can redistribute it and/or modify  *
! *   it under the terms of the GNU General Public License as published by  *
! *   the Free Software Foundation; either version 2 of the License, or     *
! *   (at your option) any later version.                                   *
! *                                                                         *
! *   This program is distributed in the hope that it will be useful,       *
! *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
! *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
! *   GNU General Public License for more details.                          *
! *                                                                         *
! *   You should have received a copy of the GNU General Public License     *
! *   along with this program; if not, write to the                         *
! *   Free Software Foundation, Inc.,                                       *
! *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
program fzcal
  use memio ! for test
  use fpio  ! for test
  use plist ! for test
  use com
  use rpnd
  use rpne
  use rpnp
  use integral ! for test
  implicit none
  type(t_rpnc) rpnc
  pointer(p,rpnc)
  character(LEN_FORMULA_MAX) str
  integer istat
  integer ka,n,kb
  integer cid
  logical calc,echo
  integer fu
  character*32 prompt
  integer i

  p=init_rpnc()
  call init_rpne

  fu=0
  echo=.false.
  prompt=">"
  i=0
  main: do
     calc=.false.
     if(fu==0) then
        if(command_argument_count()==0) then
           write(*,10) trim(prompt)//" "
10         format(x,a,$)
           read(*,"(a)") str
        else
           i=i+1
           if(i>command_argument_count()) exit
           call get_command_argument(i,str)
           if(trim(adjustl(str))=="-") then
              read(*,"(a)") str
           end if
        end if
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
     cid=parse_command(rpnc,str,ka,kb,str)
     if(ka/=0) then
        n=atoi(str(ka:kb),n,ist=istat)
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
