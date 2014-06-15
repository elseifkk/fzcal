!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2011-2014 by Kazuaki Kumagai                            *
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
  use fzcerr
  use misc, only: set_flg,cle_flg,is_set
  implicit none
  type(t_rpnc) rpnc
  integer istat
  integer,parameter::IM_INTERACTIVE= 1
  integer,parameter::IM_CMDLINE    = 2
  integer,parameter::IM_DONE       = 4
  integer,parameter::IM_FILE       = 8
  integer,parameter::IM_SET        =16
  integer::imode=0

  call init
  if(imode==IM_DONE) call exit

  if(is_set(imode,IM_INTERACTIVE)) then
     call open_hist(.true.)
     call set_prompt(rpnc,bname//"> ")
     ! this will fail after reading from stdin
     istat=read_and_run()
     call open_hist(.false.)
  end if

contains
  
  subroutine print_the_version
    use com, only: print_version
    call print_version
  end subroutine print_the_version

  subroutine print_usage
    use misc, only: mess
    call mess( &
         "Usage: fzcalc.bin [options]\n"// &
         "Options:\n"// &
         "\t-e expression   evaluate expression\n"// &
         "\t-f file         read file and evaluate line by line\n"// &
         "\t-i              invoke interactive mode\n"// &
         "\t-h              print usage\n"// &
         "\t-v              print version\n"// &
         "\t-               read from stdin (pipe)\n"// &
         "\tInteractive mode fails to start after reading from stdin.\n"// &
         "\tAny other strings are passed as expression to evaluate.\n\n"//&
         "Examples:\n"//&
         "\tExpressions will be evaluated from left to right in the command line.\n"//&
         "\t\techo ""a=1;b=2;"" | fzcalc.bin - .fig a+b\n"//&
         "\tprints 3 and\n"//&
         "\t\techo ""1;2;3;4;5;6;7;8;9;10;"" | fzcalc.bin .dat - .sta .fig sum\n"//&
         "\tprints 55.")
  end subroutine print_usage

  subroutine init
    use iso_fortran_env
    use rpng, only: LEN_STR_MAX,LEN_FORMULA_MAX
    integer i,n,len,k
    character(LEN_STR_MAX) a
    rpnc=init_rpnc()
    n=command_argument_count()
    call init_rpne
    do i=1,n
       call get_command_argument(i,a,len)
       if(is_set(imode,IM_CMDLINE)) then
          istat=set_and_run(a)
          call cle_flg(imode,IM_CMDLINE)
       else if(is_set(imode,IM_FILE)) then
          istat=load_and_run(a)
          call cle_flg(imode,IM_FILE)          
       else
          if(a(1:1)=="-") then
             if(len>1) then
                do k=2,len
                   select case(a(k:k))
                   case("v")
                      call print_the_version
                      imode=ior(imode,IM_DONE)
                   case("h")
                      call print_usage
                      imode=ior(imode,IM_DONE)
                   case("i")
                      call set_flg(imode,IM_INTERACTIVE)
                   case("e")
                      call set_flg(imode,ior(IM_CMDLINE,IM_SET))
                   case("f")
                      call set_flg(imode,ior(IM_FILE,IM_SET))
                   case default
                   end select
                end do
             else ! STDIN
                call set_flg(imode,IM_SET)
                istat=read_and_run()
             end if
          else ! CMDLINE
             call set_flg(imode,IM_SET)
             istat=set_and_run(a)
          end if
       end if
    end do
    if(imode==0) imode=IM_INTERACTIVE
  end subroutine init
  
  integer function load_and_run(f) result(istat)
    use misc, only: open_file,ins
    character*(*),intent(in)::f
    integer u
    character(LEN_STR_MAX) e
    u=open_file(f,print_error=.true.,read_only=.true.,stat="old")
    istat=-1
    if(u==0) return
    do
       call ins(e,u,istat)
       if(istat/=0) exit
       istat=set_and_run(e)
       if(istat==-1) exit
    end do
    close(u)
  end function load_and_run
  
  integer function set_and_run(e) result(istat)
    use misc, only: mess
    use rpnp, only: set_formula
    character*(*),intent(in)::e
    istat=set_formula(rpnc,e)
    if(istat/=0) then
       select case(istat)
       case(FZCERR_EMPTY_INPUT)
       case(FZCERR_READ)
          istat=-1
       case default
          call mess("*** set_formula failed: "//trim(error_str(istat)))
       end select
       return
    end if
    istat=run()
  end function set_and_run

  integer function read_and_run() result(istat)
    do
       istat=get_formula(rpnc)
       if(istat/=0) exit
       istat=run()
       if(istat==-1) exit
    end do
  end function read_and_run

  integer function run() result(istat)
    use misc, only: mess
    istat=rpn_run(rpnc)
    if(istat>0) then
       call mess("*** rpn_run failed: "//trim(error_str(istat)))
       return
    else if(istat==RPNSTA_EXIT) then
       istat=-1
    end if
  end function run
  
end program fzcalc
