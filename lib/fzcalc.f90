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
program fzcalc
  use memio ! for test
  use fpio  ! for test
  use plist ! for test
  use com
  use rpnd
  use rpne
  use rpnp
  use misc
  use integral ! for test
  implicit none
  type(t_rpnc) rpnc
  pointer(p,rpnc)
  character(LEN_FORMULA_MAX) str
  integer istat
  integer ka,n,kb
  integer cid
  logical calc,echo
  integer fu,hu
  character*32 prompt
  integer i

  p=init_rpnc()
  call init_rpne

  hu=0
  fu=0
  echo=.false.
  prompt=">"
  i=0
  main: do
     calc=.false.
     if(fu==0) then
        if(command_argument_count()==0) then
           call messp(trim(prompt)//" ")
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
     call write_hist(str)
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
     case(-CID_SAVE,-CID_SAVE_PAR,-CID_SAVE_FNC,-CID_SAVE_MAC)
     case(CID_SAVE_PAR)
        call save_par(rpnc,str(ka:kb))
     case(CID_SAVE_FNC)
        call save_fnc(rpnc,str(ka:kb))
     case(CID_SAVE_MAC)
        call save_mac(rpnc,str(ka:kb))
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
           call mess("Cannot open: "//str(ka:kb))
           fu=0
        end if
     case(-CID_ECHO)
        call mess("echo is "//trim(log2str(echo))) 
     case(CID_ECHO_OFF)
        echo=.false.
     case(CID_ECHO_ON)
        echo=.true.
     case(-CID_HIST)
        call mess("hist is "//trim(i2str(hu))) 
     case(CID_HIST_OFF)
        call enable_hist(.false.)
     case(CID_HIST_ON)
        call enable_hist(.true.)
     case(-CID_SET_PROMPT)
     case(CID_SET_PROMPT)
        prompt=str(ka:kb)
     case(-CID_WRITE)
        write(*,*) 
     case(CID_WRITE)
        write(*,*) trim(str(ka:kb))
     case(CID_DONE)
     case(CID_INV)
        call mess("Invalid command: "//trim(str))
     case default
        calc=.true.
     end select
     
     if(.not.calc) cycle

     if(echo) call mess(trim(prompt)//" "//trim(str))
     istat=parse_formula(rpnc,str)

     if(istat>0) then
        call mess("*** parse_formula failed: code = "//trim(itoa(istat)))
        call dump_rpnc(rpnc)
     else if(istat==0) then
        if(iand(rpnc%opt,RPNCOPT_DEBUG)/=0) then
           call mess("=== Before eval ===")
           call dump_rpnc(rpnc)
        end if
        
        do while(rpnc%ip/=0)
           istat=eval(rpnc)
           if(istat/=0) then
              call mess("*** rpn_eval failed: code = "//trim(itoa(istat)))
              call dump_rpnc(rpnc)
              exit
           else
              if(iand(rpnc%opt,RPNCOPT_DEBUG)/=0) then
                 call mess("=== After eval ===")
                 call dump_rpnc(rpnc)
              end if
              call mess(trim(rpn_sans(rpnc)))
           end if
        end do

     end if

  end do main
  
  call enable_hist(.false.)

contains
  
  subroutine enable_hist(on)
    logical,intent(in)::on
    character*(*),parameter::histfile=".fzcalc-hist"
    character(len=1024) f
    integer istat
    if(on) then
       call getenv("HOME",f)
       if(f/="") then
          f=trim(adjustl(f))//"/"//histfile
       else
          f=histfile
       end if
       hu=1210
       open(unit=hu,file=f,iostat=istat)
       if(istat/=0) then
          call mess("*** Error: cannot open file: "//trim(f))
          hu=0
       end if
    else
       close(hu)
    end if
  end subroutine enable_hist

  subroutine write_hist(h)
    character*(*),intent(in)::h
    if(hu==0) return
    write(hu,"(a)") trim(h)
    flush(hu)
  end subroutine write_hist

  subroutine read_line()
    read(fu,"(a)",iostat=istat) str
  end subroutine read_line

end program fzcalc
