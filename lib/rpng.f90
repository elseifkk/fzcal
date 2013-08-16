!/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *   Copyright (C) 2012-2013 by Kazuaki Kumagai                            *
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
module rpng
  implicit none

  integer,parameter::RPNSTA_COMOK  = -7
  integer,parameter::RPNSTA_LOAD   = -6
  integer,parameter::RPNSTA_COMSET = -5
  integer,parameter::RPNSTA_EXIT   = -4
  integer,parameter::RPNSTA_EMPTY  = -3
  integer,parameter::RPNSTA_MACSET = -2
  integer,parameter::RPNSTA_FNCSET = -1
  integer,parameter::RPNSTA_OK     =  0

  integer,parameter::RPN_REC_MAX     =  256
  integer,parameter::NUM_VS_MIN      =   32
  integer,parameter::LEN_STR_MAX     = 1024
  integer,parameter::LEN_FORMULA_MAX = LEN_STR_MAX

  ! meta tid
  integer,parameter::TID_NOP   =     0
  integer,parameter::TID_FIN   =   100
  integer,parameter::TID_UNDEF =   101
  integer,parameter::TID_INV   =   102
  integer,parameter::TID_LAST  =   103

  !! priority table begin
  ! asign and conditional
  integer,parameter::TID_ASN    =   1  !R =
  integer,parameter::TID_ASNU   =  -1
  integer,parameter::TID_AOP    =   2  !R
  integer,parameter::TID_TOP1   =   3  !R ?
  ! logical
  integer,parameter::TID_LOP4    =  4  !L eq,neq
  integer,parameter::TID_LOP3    =  5  !L or
  integer,parameter::TID_LOP2    =  6  !L and
  integer,parameter::TID_LOP1    =  7  !R not, ~
  integer,parameter::TID_LOP1U   = -7  ! ~
  integer,parameter::TID_ROP     =  8  !L ==, ~=, <=, >=,...
  ! unary, binary and functions
  integer,parameter::TID_BOP1    =   9  !L +,-
  integer,parameter::TID_BOP1U   =  -9  !
  integer,parameter::TID_BOP2    =  10  !L *,/,&P,&C
  integer,parameter::TID_BOP2U   = -10  !
  integer,parameter::TID_BOP4    =  11  !L implicit * <<<<<<<<<<<
  integer,parameter::TID_BOP3    =  12  !R ^,**,e
  integer,parameter::TID_BOP3U   = -12  !
  integer,parameter::TID_UOP1    =  13  !R +a,-a,++a
  integer,parameter::TID_UOP2    =  14  !L !,!!
  integer,parameter::TID_UOP3    =  15  !L a++
  integer,parameter::TID_UOP2U   = -15  !
  integer,parameter::TID_IFNC    =  16  !L sin, cos,...
  integer,parameter::TID_UFNC    =  17  !L
  integer,parameter::TID_PRI_MAX =  18  !
  !! priority tabel end

  integer,parameter::TID_PAR   =  32   ! a,b,c,...
  integer,parameter::TID_PARU  = -32   ! a,b,c,...
  integer,parameter::TID_FIG   =  33   ! 1,2,3,...
  integer,parameter::TID_VAR   =  34   ! fig in rbuf
  integer,parameter::TID_MAC   =  35
  integer,parameter::TID_OP    =  36   ! operators
  integer,parameter::TID_COP   =  37
  integer,parameter::TID_OPN   =  38
  integer,parameter::TID_APAR  =  39   ! par assign
  integer,parameter::TID_AMAC  =  40
  integer,parameter::TID_AFNC  =  41
  integer,parameter::TID_DPAR  =  42   ! dummy par
  integer,parameter::TID_END   =  43
  integer,parameter::TID_ROVAR =  44
  integer,parameter::TID_LVAR_T = 45
  integer,parameter::TID_LVAR_F = 46
  integer,parameter::TID_LOP    = 47
  integer,parameter::TID_SOP    = 48
  integer,parameter::TID_POP    = 49
  integer,parameter::TID_CPAR   = 50 ! copied par
  integer,parameter::TID_NPAR   = 51 ! just assigned par
  integer,parameter::TID_IOP    = 52 ! integral
  integer,parameter::TID_IVAR1  = 53 ! dummy par in integrand x
  integer,parameter::TID_IVAR1L = 54 ! dummy par in integrand b-x
  integer,parameter::TID_IVAR1U = 55 ! dummy par in integrand x-a
  integer,parameter::TID_AT     = 56 ! @
  integer,parameter::TID_MSCL   = 57 ! ; in macro definition
  integer,parameter::TID_SHRP   = 58 ! #
  integer,parameter::TID_IGNORE = 59
  integer,parameter::TID_EMAC   = 60 ! $mac to be expanded

  ! braket and delimiters
  integer,parameter::TID_SCL   =  64   ! ;
  integer,parameter::TID_COL   =  65   ! : PUSHED!
  integer,parameter::TID_TCOL  =  66   ! : PUSHED!
  integer,parameter::TID_IBRA  =  67   ! implicit (
  integer,parameter::TID_BRA   =  68   ! ( PUSHED!
  integer,parameter::TID_KET   =  69   ! )
  integer,parameter::TID_QTN   =  70   ! "
  integer,parameter::TID_QEND  =  71
  integer,parameter::TID_QSTA  =  72   ! PUSHED!
  integer,parameter::TID_COMA  =  73   ! ,
  integer,parameter::TID_DLM1  =  75
  integer,parameter::TID_DLM2  =  76   ! ket
  integer,parameter::TID_BLK   =  77   ! space and tab
  integer,parameter::TID_HKET  =  78   ! }
  integer,parameter::TID_USCR  =  79   ! _
  integer,parameter::TID_ISTA  =  80
  integer,parameter::TID_IEND  =  81
  integer,parameter::TID_COM   =  82
  integer,parameter::TID_MCOM  =  83
  integer,parameter::TID_SQ1   =  84 ! first '
  integer,parameter::TID_SQ2   =  85 ! last '
  integer,parameter::TID_DQ1   =  86 ! first "
  integer,parameter::TID_DQ2   =  87 ! last "

  character*1,parameter::STID_SQ1  = char(1) ! first '
  character*1,parameter::STID_SQ2  = char(4) ! last '
  character*1,parameter::STID_DQ1  = char(2) ! first "
  character*1,parameter::STID_DQ2  = char(3) ! last "

  integer,parameter::LOID_NOT = 1
  integer,parameter::LOID_AND = 2
  integer,parameter::LOID_OR  = 3
  integer,parameter::LOID_EQ  = 4
  integer,parameter::LOID_NEQ = 5

  ! RPNFs
  integer,parameter::RCF_NOP              =  0
  ! stat
  integer,parameter::RCS_READY            =  Z"00000001"
  integer,parameter::RCS_NEW_PAR          =  Z"00000002"
  integer,parameter::RCS_ANS_SET          =  Z"00000004"
  integer,parameter::RCS_FNC_SET          =  Z"00000008"
  integer,parameter::RCS_MAC_SET          =  Z"00000010"
  integer,parameter::RCS_PRINT_ANS_REQ    =  Z"00000020"
  ! public mode
  integer,parameter::RCM_ECHO            =  Z"00000001"
  integer,parameter::RCM_HIST            =  Z"00000002"
  integer,parameter::RCM_DEBUG           =  Z"00000004"
  integer,parameter::RCM_DEG             =  Z"00000008"
  integer,parameter::RCM_RATIO           =  Z"00000010"
  integer,parameter::RCM_DAT             =  Z"00000020"
  integer,parameter::RCM_STA             =  Z"00000040"
  integer,parameter::RCM_BYTE            =  Z"00000080" ! for SI prefix k to be 1024
  integer,parameter::RCM_IBIN            =  Z"00000100"
  integer,parameter::RCM_IOCT            =  Z"00000200"
  integer,parameter::RCM_IHEX            =  Z"00000400"
  integer,parameter::RCM_INM = ior(RCM_IHEX,ior(RCM_IOCT,RCM_IBIN))
  ! private mode
  integer,parameter::RCPM_NO_AUTO_ADD_PAR =  Z"00000800"
  integer,parameter::RCPM_NO_WARN         =  Z"00001000"
  integer,parameter::RCPM_NO_STDIN        =  Z"00002000"
  integer,parameter::RCPM_NO_STDOUT       =  Z"00004000"

  integer,parameter::AID_NOP = 0
  integer,parameter::OID_NOP = 0
  integer,parameter::OID_CND = 1

  integer,parameter::PID_yoc =  1
  integer,parameter::PID_zep =  2
  integer,parameter::PID_a   =  3
  integer,parameter::PID_f   =  4
  integer,parameter::PID_pi  =  5
  integer,parameter::PID_n   =  6
  integer,parameter::PID_u   =  7
  integer,parameter::PID_mi  =  8
  integer,parameter::PID_    =  9
  integer,parameter::PID_k   = 10
  integer,parameter::PID_M   = 11
  integer,parameter::PID_G   = 12
  integer,parameter::PID_T   = 13
  integer,parameter::PID_P   = 14
  integer,parameter::PID_E   = 15
  integer,parameter::PID_Z   = 16
  integer,parameter::PID_Y   = 17
  integer,parameter::PID_END = 17
  integer,parameter::PID_INPUT  = 18
  integer,parameter::PID_EMAC   = 19

  integer,parameter::SC_RO     = 1
  integer,parameter::SC_MAC    = 2
  integer,parameter::SC_FNC    = 4
  integer,parameter::SC_PROMPT = 1

#if defined _VERSION_
      character*(*),parameter::version=_VERSION_
#else
      character*(*),parameter::version="?"
#endif
      character*(*),parameter::bname="fzcalc-"//version
#if defined __GNUC__
#if defined __GNUC_MINOR__
#if defined __GNUC_PATCHLEVEL__
      character*(*),parameter::cversion=char(__GNUC__+48)//"."//char(__GNUC_MINOR__+48)//"."//char(__GNUC_PATCHLEVEL__+48)
      character*(*),parameter::cname="gcc-"//cversion
#else
      character*(*),parameter::cname="gcc"
      character*(*),parameter::cversion=""
#endif
#else
      character*(*),parameter::cname="gcc"
      character*(*),parameter::cversion=""
#endif
#else
      character*(*),parameter::cname="?"
      character*(*),parameter::cversion=""
#endif

end module rpng
