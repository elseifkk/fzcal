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
  use rpnd
  use rpne
  use memio, only: itoa
  use misc, only: mess
  implicit none
  type(t_rpnc) rpnc
  integer istat

  rpnc=init_rpnc()
  call init_rpne
  call open_hist(.true.)

  main: do

     istat=get_formula(rpnc)
     if(istat/=0) then
        select case(istat)
        case(FZCERR_EMPTY_INPUT)
        case(FZCERR_READ)
           exit
        case default
           call mess("*** set_formula failed: code = "//trim(itoa(istat)))
        end select
        cycle
     end if
     
     istat=rpn_run(rpnc)
     if(istat==0) then
     else if(istat>0) then
        call mess("*** rpn_run failed: code = "//trim(itoa(istat)))
        cycle
     else if(istat==RPNSTA_EXIT) then
        exit
     end if

  end do main
  
  call open_hist(.false.)

end program fzcalc
