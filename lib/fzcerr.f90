module fzcerr
  implicit none
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
end module fzcerr
