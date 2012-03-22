module rpng
  implicit none
  
  integer,parameter::RPNSTA_LOAD   = -5
  integer,parameter::RPNSTA_COMSET = -4
  integer,parameter::RPNSTA_EXIT   = -3
  integer,parameter::RPNSTA_EMPTY  = -2
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
  integer,parameter::TID_UOP1    =  11  !R +a,-a,++a
  integer,parameter::TID_BOP4    =  12  !L implicit * <<<<<<<<<<< 
  integer,parameter::TID_BOP3    =  13  !R ^,**,e
  integer,parameter::TID_BOP3U   = -13  !
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
  integer,parameter::TID_MAC   =  36   
  integer,parameter::TID_OP    =  37   ! operators
  integer,parameter::TID_COP   =  38   
  integer,parameter::TID_OPN   =  39   
  integer,parameter::TID_APAR  =  40   ! par assign
  integer,parameter::TID_AMAC  =  41   
  integer,parameter::TID_AFNC  =  42   
  integer,parameter::TID_DPAR  =  43   ! dummy par
  integer,parameter::TID_END   =  44
  integer,parameter::TID_ROVAR =  45 
  integer,parameter::TID_LVAR_T = 46
  integer,parameter::TID_LVAR_F = 47
  integer,parameter::TID_LOP    = 48
  integer,parameter::TID_SOP    = 49
  integer,parameter::TID_POP    = 50
  integer,parameter::TID_CPAR   = 51 ! copied par
  integer,parameter::TID_NPAR   = 52 ! just assigned par
  integer,parameter::TID_IOP    = 53 ! integral
  integer,parameter::TID_IVAR1  = 54 ! dummy par in integrand x
  integer,parameter::TID_IVAR1L = 55 ! dummy par in integrand b-x
  integer,parameter::TID_IVAR1U = 56 ! dummy par in integrand x-a
  integer,parameter::TID_AT     = 57 ! @
  integer,parameter::TID_MSCL   = 58 ! ; in macro definition
  integer,parameter::TID_SHRP   = 59 ! #
  integer,parameter::TID_IGNORE = 60 
  integer,parameter::TID_EMAC   = 61 ! $mac to be expanded

  ! braket and delimiters
  integer,parameter::TID_SCL   =  64   ! ;
  integer,parameter::TID_COL   =  65   ! : PUSHED!
  integer,parameter::TID_IBRA  =  66   ! implicit (
  integer,parameter::TID_BRA   =  67   ! ( PUSHED!
  integer,parameter::TID_KET   =  69   ! )
  integer,parameter::TID_QTN   =  70   ! "
  integer,parameter::TID_QEND  =  71
  integer,parameter::TID_QSTA  =  72   ! PUSHED!
  integer,parameter::TID_COMA  =  73   ! ,
!  integer,parameter::TID_MASN  =  74   ! = for macro PUSHED!
  integer,parameter::TID_DLM1  =  75   
  integer,parameter::TID_DLM2  =  76   ! ket
  integer,parameter::TID_BLK   =  77   ! space and tab
  integer,parameter::TID_HKET  =  78   ! }
  integer,parameter::TID_USCR  =  79   ! _
  integer,parameter::TID_ISTA  =  80
  integer,parameter::TID_IEND  =  81
  integer,parameter::TID_COM   =  82
  integer,parameter::TID_SQ1   =  83 ! first '
  integer,parameter::TID_SQ2   =  84 ! last '
  integer,parameter::TID_DQ1   =  85 ! first "
  integer,parameter::TID_DQ2   =  86 ! last "
!  integer,parameter::TID_FASN  =  87

  character*1,parameter::STID_SQ1  = char(1) ! first '
  character*1,parameter::STID_SQ2  = char(4) ! last '
  character*1,parameter::STID_DQ1  = char(2) ! first " 
  character*1,parameter::STID_DQ2  = char(3) ! last "

  integer,parameter::LOID_NOT = 1
  integer,parameter::LOID_AND = 2
  integer,parameter::LOID_OR  = 3
  integer,parameter::LOID_EQ  = 4
  integer,parameter::LOID_NEQ = 5

  integer*8,parameter::RPNCOPT_NOP             =  0
  integer*8,parameter::RPNCOPT_DEBUG           =  Z"08000000"
  integer*8,parameter::RPNCOPT_READY           =  Z"00000001"
  integer*8,parameter::RPNCOPT_DEG             =  Z"00000002"
  integer*8,parameter::RPNCOPT_NEW             =  Z"00000004"
  integer*8,parameter::RPNCOPT_NO_AUTO_ADD_PAR =  Z"00000008"
  integer*8,parameter::RPNCOPT_RATIO           =  Z"00000010"
  integer*8,parameter::RPNCOPT_NO_WARN         =  Z"00000020"
  integer*8,parameter::RPNCOPT_DAT             =  Z"00000040"
  integer*8,parameter::RPNCOPT_STA             =  Z"00000080"
  integer*8,parameter::RPNCOPT_OBIN            =  Z"00000100"
  integer*8,parameter::RPNCOPT_OOCT            =  Z"00000200"
  integer*8,parameter::RPNCOPT_OHEX            =  Z"00000400"
  integer*8,parameter::RPNCOPT_OUTM = ior(RPNCOPT_OHEX,ior(RPNCOPT_OOCT,RPNCOPT_OBIN))     
  integer*8,parameter::RPNCOPT_IBIN            =  Z"00000800"
  integer*8,parameter::RPNCOPT_IOCT            =  Z"00001000"
  integer*8,parameter::RPNCOPT_IHEX            =  Z"00002000"
  integer*8,parameter::RPNCOPT_INM = ior(RPNCOPT_IHEX,ior(RPNCOPT_IOCT,RPNCOPT_IBIN))     
  integer*8,parameter::RPNCOPT_BYTE            =  Z"00004000" ! for SI prefix k to be 1024
  integer*8,parameter::RPNCOPT_NO_STDIN        =  Z"00008000"
  integer*8,parameter::RPNCOPT_NO_STDOUT       =  Z"00010000"
  integer*8,parameter::RPNCOPT_EXECOM          =  Z"00020000"
  integer*8,parameter::RPNCOPT_ECHO            =  Z"00040000"
  integer*8,parameter::RPNCOPT_HIST            =  Z"00080000"
  integer*8,parameter::RPNCOPT_PRINT_ANS_REQ   =  Z"00100000"
  integer*8,parameter::RPNCOPT_ANS_SET         =  Z"00200000"

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

  integer,parameter::SC_RO  = 1
  integer,parameter::SC_MAC = 2
  integer,parameter::SC_FNC = 4

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