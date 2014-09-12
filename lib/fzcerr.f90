!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2012 by Kazuaki Kumagai                                 *
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
module fzcerr
  implicit none

  public

  integer,parameter::FZCERR_NOENT        =  1
  integer,parameter::FZCERR_NOOP         =  2
  integer,parameter::FZCERR_NOPAR        =  3
  integer,parameter::FZCERR_NOFNC        =  4
  integer,parameter::FZCERR_ADDPAR       =  5
  integer,parameter::FZCERR_INVASN       =  6
  integer,parameter::FZCERR_INVOP        =  7
  integer,parameter::FZCERR_INVFNC       =  8
  integer,parameter::FZCERR_PARSER       =  9
  integer,parameter::FZCERR_ADDSTR       = 10
  integer,parameter::FZCERR_MEMOV        = 11
  integer,parameter::FZCERR_RECOV        = 12
  integer,parameter::FZCERR_NARG         = 13
  integer,parameter::FZCERR_GETPAR       = 14
  integer,parameter::FZCERR_TOO_MANY_ARG = 15
  integer,parameter::FZCERR_TOO_FEW_ARG  = 16
  integer,parameter::FZCERR_NOARG        = 17
  integer,parameter::FZCERR_INVARG       = 18
  integer,parameter::FZCERR_INVFIG       = 19
  integer,parameter::FZCERR_READ         = 20
  integer,parameter::FZCERR_NOMEM        = 21
  integer,parameter::FZCERR_RDONL        = 22
  integer,parameter::FZCERR_TOO_LONG_STR = 23
  integer,parameter::FZCERR_INVCOM       = 24
  integer,parameter::FZCERR_NOFILE       = 25
  integer,parameter::FZCERR_EMPTY_INPUT  = 26
  integer,parameter::FZCERR_INTERNAL     = 27

  integer,parameter::LEN_STR_ERR_MAX=64

contains

  character(len=LEN_STR_ERR_MAX) function error_str(ec)
    integer,intent(in)::ec
    select case(ec)
    case(FZCERR_NOENT)
       error_str="No suchh entry"
    case(FZCERR_NOOP)
       error_str="Some operands missing"
    case(FZCERR_NOPAR)
       error_str="No such parameter"
    case(FZCERR_NOFNC)
       error_str="No such function"
    case(FZCERR_ADDPAR)
       error_str="Error during add_par"
    case(FZCERR_INVASN) ! not used yet
       error_str="Invalid assignment"
    case(FZCERR_INVOP)  ! not used yet
       error_str="Invalid operator"
    case(FZCERR_INVFNC) ! not used yet
       error_str="Invalid function"
    case(FZCERR_PARSER)
       error_str="Error in parser"
    case(FZCERR_ADDSTR) ! not used yet
       error_str="Error during add_str"
    case(FZCERR_MEMOV)
       error_str="Memory overflow"
    case(FZCERR_RECOV)
       error_str="Too much recursion"
    case(FZCERR_NARG)
       error_str="Inconsistent number of arguments"
    case(FZCERR_GETPAR)
       error_str="Error during get_par"
    case(FZCERR_TOO_MANY_ARG)
       error_str="Too many arguments"
    case(FZCERR_TOO_FEW_ARG)
       error_str="Too few arguments"
    case(FZCERR_NOARG)
       error_str="No arguments passed"
    case(FZCERR_INVARG)
       error_str="Invalid arguments"
    case(FZCERR_INVFIG)
       error_str="Invalid figure"
    case(FZCERR_READ)
       error_str="Error during read"
    case(FZCERR_NOMEM) ! not used yet
       error_str="No memory"
    case(FZCERR_RDONL)
       error_str="Read only"
    case(FZCERR_TOO_LONG_STR)
       error_str="Too long strings"
    case(FZCERR_INVCOM)
       error_str="Invalid command"
    case(FZCERR_NOFILE)
       error_str="No such file"
    case(FZCERR_EMPTY_INPUT)
       error_str="Empty input"
    case(FZCERR_INTERNAL)
       error_str="Internal Error"
    case default
       error_str="Unknown error code"
    end select
  end function error_str

end module fzcerr

