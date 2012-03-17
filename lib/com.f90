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
module com
  implicit none

  private

  public parse_command
  public exe_com
  
  integer,parameter::CID_NOP         =    0
  integer,parameter::CID_DEL         =    1
  integer,parameter::CID_DEL_PAR     =    2 
  integer,parameter::CID_DEL_FNC     =    3 
  integer,parameter::CID_DEL_MAC     =    4 
  integer,parameter::CID_DEL_PAR_ALL =    5 
  integer,parameter::CID_DEL_FNC_ALL =    6 
  integer,parameter::CID_DEL_MAC_ALL =    7 
  integer,parameter::CID_PRI         =    8 
  integer,parameter::CID_PRI_PAR     =    9
  integer,parameter::CID_PRI_FNC     =   10 
  integer,parameter::CID_PRI_MAC     =   11 
  integer,parameter::CID_PRI_DAT     =   12 
  integer,parameter::CID_INI         =   13 
  integer,parameter,public::CID_EXIT        =   14
  integer,parameter::CID_SCLE        =   15
  integer,parameter,public::CID_LOAD        =   16
  integer,parameter::CID_ECHO        =   17
  integer,parameter::CID_ECHO_ON     =   18
  integer,parameter::CID_ECHO_OFF    =   19
  integer,parameter::CID_SET_PROMPT  =   20
  integer,parameter::CID_WRITE       =   21
  integer,parameter::CID_HIST        =   23
  integer,parameter::CID_HIST_ON     =   24
  integer,parameter::CID_HIST_OFF    =   25
  integer,parameter::CID_SAVE        =   26
  integer,parameter::CID_SAVE_PAR    =   27
  integer,parameter::CID_SAVE_FNC    =   28
  integer,parameter::CID_SAVE_MAC    =   29
  integer,parameter::CID_VER         =   30
  integer,parameter::CID_HELP        =   31
  integer,parameter::CID_BYTE        =   32      
  integer,parameter::CID_NOBYTE      =   33       
  integer,parameter::CID_OPT         =   35               
  integer,parameter::CID_DMS         =   36   
  integer,parameter::CID_NODMS       =   37    
  integer,parameter::CID_ENG         =   38
  integer,parameter::CID_NOENG       =   39     
  integer,parameter::CID_FIX         =   40                  
  integer,parameter::CID_EXP         =   41             
  integer,parameter::CID_FIG         =   42              
  integer,parameter::CID_BIN_IO      =   43                      
  integer,parameter::CID_OCT_IO      =   44
  integer,parameter::CID_DEC_IO      =   45
  integer,parameter::CID_HEX_IO      =   46
  integer,parameter::CID_BIN_I       =   47                    
  integer,parameter::CID_OCT_I       =   48
  integer,parameter::CID_DEC_I       =   49
  integer,parameter::CID_HEX_I       =   50
  integer,parameter::CID_BIN_O       =   51                      
  integer,parameter::CID_OCT_O       =   52
  integer,parameter::CID_DEC_O       =   53
  integer,parameter::CID_HEX_O       =   54
  integer,parameter::CID_DEG         =   55           
  integer,parameter::CID_RAD         =   56         
  integer,parameter::CID_DBG         =   57                
  integer,parameter::CID_NODBG       =   58               
  integer,parameter::CID_DAT         =   59                 
  integer,parameter::CID_NORM        =   60           
  integer,parameter::CID_STA         =   61              
  integer,parameter::CID_FRAC        =   62                
  integer,parameter::CID_RATIO       =   63      
  integer,parameter::CID_SHELL       =   64      

  integer,parameter::CID_INV         =   99
  integer,parameter::CID_LAST        =  100
  integer,parameter::CID_HELP_OFF    = 1000

  integer,parameter::AK_INV    =  -1
  integer,parameter::AK_ALL    =   0
  integer,parameter::AK_PAR    =   1
  integer,parameter::AK_FNC    =   2
  integer,parameter::AK_MAC    =   3
  integer,parameter::AK_DAT    =   4
  integer,parameter::AK_ANY    =   5
  integer,parameter::AK_ON     =   6
  integer,parameter::AK_OFF    =   7

  integer*8,parameter::digit_mask=not(ishft(Z"FF",32))

contains

  integer function exe_com(rpnc,i)
    use fpio
    use rpnd
    use memio, only: atoi
    use misc, only: mess,log2str,i2str,is_set,cle_opt,set_opt,get_lo32,get_up32
    ! return value:
    ! 0< : command to be processed outside
    ! 0  : command proccessed
    type(t_rpnc),intent(inout),target::rpnc
    integer,intent(in)::i
    type(t_rpnq),pointer::q
    integer istat
    integer n
    character(len=LEN_STR_MAX) str
    integer cid,lenarg
    logical help

    exe_com=0
    help=.false.
    q => rpnc%que(i)
    cid=get_lo32(q%tid)-TID_LAST

    if(cid>=CID_HELP_OFF) then
       help=.true.
       cid=cid-CID_HELP_OFF
    end if
    if(cid>CID_LAST) then
       cid=cid-CID_LAST
       cid=-cid
    end if
    if(help) then
       if(cid==CID_NOP) then
          call print_com_list
       else
          call print_help
       end if
       return
    end if

    lenarg=get_up32(q%tid)
    str=""
    n=0

    if(lenarg>0) then
       str=cpstr(q%cid,lenarg)
       n=atoi(str,n,ist=istat)
       if(istat/=0) then
          n=0
       else
          str=""
       end if
    end if
    
    select case(cid)
    case(CID_EXIT)
       exe_com=CID_EXIT
    case(CID_BYTE)
       call set_opt(rpnc%opt,RPNCOPT_BYTE)
    case(CID_NOBYTE)
       call cle_opt(rpnc%opt,RPNCOPT_BYTE)
    case(CID_OPT)
       call mess("Opt-word: "//trim(itoa(rpnc%opt,cfmt="(Z16.16)")))
    case(CID_NODMS)
       call cle_opt(rpnc%opt,RPNCOPT_OUTM)
       call cle_disp_opt(X2A_DMS)            
    case(CID_DMS)
       call cle_opt(rpnc%opt,RPNCOPT_OUTM)
       call set_disp_opt(X2A_DMS)
       call cle_disp_opt(X2A_ENG)            
       call put_disp_digit(n)
    case(CID_NOENG)
       call cle_opt(rpnc%opt,RPNCOPT_OUTM)
       call cle_disp_opt(X2A_ENG)
    case(CID_ENG)
       call cle_opt(rpnc%opt,RPNCOPT_OUTM)
       call set_disp_opt(X2A_ENG)
       call cle_disp_opt(ior(X2A_DMS,X2A_ALLOW_ORDINARY)) ! <<<<<<<<<<<<<<<
       call put_disp_digit(n)
    case(CID_FIX)
       call cle_opt(rpnc%opt,RPNCOPT_OUTM)
       call set_disp_opt(X2A_FIX)
       call cle_disp_opt(X2A_SHOW_E0)
       call put_disp_digit(n)
    case(CID_EXP)
       call cle_opt(rpnc%opt,RPNCOPT_OUTM)
       call cle_disp_opt(ior(X2A_FIX,ior(X2A_ALLOW_ORDINARY,X2A_TRIM_ZERO)))
       call put_disp_digit(n)
    case(CID_FIG)
       call cle_opt(rpnc%opt,RPNCOPT_OUTM)
       call set_disp_opt(ior(X2A_ALLOW_ORDINARY,X2A_TRIM_ZERO))
       call cle_disp_opt(ior(X2A_FIX,X2A_SHOW_E0))
       call put_disp_digit(0)
    case(CID_DEC_IO)
       call cle_opt(rpnc%opt,ior(RPNCOPT_INM,RPNCOPT_OUTM))
    case(CID_HEX_IO)
       call set_opt(rpnc%opt,ior(RPNCOPT_IHEX,RPNCOPT_OHEX))
    case(CID_OCT_IO)
       call set_opt(rpnc%opt,ior(RPNCOPT_IOCT,RPNCOPT_OOCT))
    case(CID_BIN_IO)
       call set_opt(rpnc%opt,ior(RPNCOPT_IBIN,RPNCOPT_OBIN))
    case(CID_DEC_I)
       call cle_opt(rpnc%opt,RPNCOPT_INM)
    case(CID_HEX_I)
       call set_opt(rpnc%opt,RPNCOPT_IHEX)
    case(CID_OCT_I)
       call set_opt(rpnc%opt,RPNCOPT_IOCT)
    case(CID_BIN_I)
       call set_opt(rpnc%opt,RPNCOPT_IBIN)
    case(CID_DEC_O)
       call cle_opt(rpnc%opt,(RPNCOPT_OUTM))
    case(CID_HEX_O)
       call set_opt(rpnc%opt,RPNCOPT_OHEX)
    case(CID_OCT_O)
       call set_opt(rpnc%opt,RPNCOPT_OOCT)
    case(CID_BIN_O)
       call set_opt(rpnc%opt,RPNCOPT_OBIN)
    case(CID_DEG)
       call set_opt(rpnc%opt,RPNCOPT_DEG)
    case(CID_RAD)
       call cle_opt(rpnc%opt,RPNCOPT_DEG)
    case(CID_STA)
       call set_opt(rpnc%opt,RPNCOPT_STA)
       call cle_opt(rpnc%opt,RPNCOPT_DAT) ! <<<
    case(CID_DAT)
       call set_opt(rpnc%opt,RPNCOPT_DAT)
    case(CID_NORM)
       call cle_opt(rpnc%opt,ior(RPNCOPT_DAT,RPNCOPT_STA))
    case(CID_DBG)
       call set_opt(rpnc%opt,RPNCOPT_DEBUG)
    case(CID_NODBG)
       call cle_opt(rpnc%opt,RPNCOPT_DEBUG)
    case(CID_RATIO)
       call set_opt(rpnc%opt,RPNCOPT_RATIO)
    case(CID_FRAC)
       call cle_opt(rpnc%opt,RPNCOPT_RATIO)
    case(CID_SCLE)
       call reset_sd(rpnc%sd)
    case(-CID_SAVE,-CID_SAVE_PAR,-CID_SAVE_FNC,-CID_SAVE_MAC)
    case(CID_SAVE_PAR)
       call save_par(rpnc,str)
    case(CID_SAVE_FNC)
       call save_fnc(rpnc,str)
    case(CID_SAVE_MAC)
       call save_mac(rpnc,str)
    case(-CID_PRI)
    case(-CID_PRI_PAR)
       call dump_par(rpnc)
    case(CID_PRI_PAR)
       call dump_par(rpnc,n,str)
    case(-CID_PRI_FNC)
       call dump_rpnm(rpnc,type=SC_FNC)
    case(CID_PRI_FNC)
       call dump_rpnm(rpnc,n,str,type=SC_FNC)
    case(-CID_PRI_MAC)
       call dump_rpnm(rpnc,type=SC_MAC)
    case(CID_PRI_MAC)
       call dump_rpnm(rpnc,n,str,type=SC_MAC)
    case(CID_DEL_PAR)
       call delete_par(rpnc,str)
    case(CID_DEL_PAR_ALL)
       call delete_par_all(rpnc)
    case(CID_PRI_DAT)
       call dump_sd(rpnc)
    case(CID_DEL_MAC_ALL)
       call delete_rpnm(rpnc,0,"",type=SC_MAC)
    case(CID_DEL_FNC_ALL)
       call delete_rpnm(rpnc,0,"",type=SC_FNC)
    case(CID_DEL_MAC)
       call delete_rpnm(rpnc,n,str,type=SC_MAC)
    case(CID_DEL_FNC)
       call delete_rpnm(rpnc,n,str,type=SC_FNC)
    case(CID_LOAD)
       exe_com=CID_LOAD
    case(-CID_ECHO)
       call mess("echo is "//trim(log2str(is_set(rpnc%opt,RPNCOPT_ECHO))))
    case(CID_ECHO_OFF)
       call cle_opt(rpnc%opt,RPNCOPT_ECHO)
    case(CID_ECHO_ON)
       call set_opt(rpnc%opt,RPNCOPT_ECHO)
    case(-CID_HIST)
       call mess("hist is "//trim(log2str(is_set(rpnc%opt,RPNCOPT_HIST))))
    case(CID_HIST_OFF)
       call cle_opt(rpnc%opt,RPNCOPT_HIST)
    case(CID_HIST_ON)
       call set_opt(rpnc%opt,RPNCOPT_HIST)
    case(-CID_SET_PROMPT)
    case(CID_SET_PROMPT)
       call stripq
       call set_prompt(rpnc,str(1:lenarg))
    case(-CID_WRITE)
       call mess("")
    case(CID_WRITE)
       call stripq
       call mess(str(:lenarg))
    case(CID_VER)
       call print_version
    case(CID_SHELL)
       call restoreq
       call system(str(1:lenarg))
    case default
       STOP "exe_com: UNEXPECTED ERROR: invalid cid"
    end select

  contains

    subroutine restoreq()
      use rpng, only: STID_SQ1,STID_SQ2,STID_DQ1,STID_DQ2
      character*1 c
      integer ii,jj
      jj=0
      do ii=1,lenarg
         c=str(ii:ii)
         select case(c)
         case(STID_SQ1,STID_SQ2)
            c="'"
         case(STID_DQ1,STID_DQ2)
            c=""""
         end select
         jj=jj+1
         str(jj:jj)=c
      end do
      lenarg=jj
    end subroutine restoreq

    subroutine stripq()
      use rpng, only: STID_SQ1,STID_SQ2,STID_DQ1,STID_DQ2
      integer ii,jj
      jj=0
      do ii=1,lenarg
         select case(str(ii:ii))
         case(STID_SQ1,STID_SQ2,STID_DQ1,STID_DQ2)
            cycle
         case default
            jj=jj+1
            str(jj:jj)=str(ii:ii)
         end select
      end do
      lenarg=jj
    end subroutine stripq
     
    subroutine print_version()
#if defined _VERSION_
      character*(*),parameter::v=_VERSION_
#else
      character*(*),parameter::v="version unknown"
#endif
#if defined __GNUC__
#if defined __GNUC_MINOR__
#if defined __GNUC_PATCHLEVEL__
      character*(*),parameter::cv=char(__GNUC__+48)//"."//char(__GNUC_MINOR__+48)//"."//char(__GNUC_PATCHLEVEL__+48)
#else
      character*(*),parameter::cv=""
#endif
#else
      character*(*),parameter::cv=""
#endif
#else
      character*(*),parameter::cv=""
#endif
      call mess("fzcalc "//v)
      call mess("gcc "//cv)
    end subroutine print_version

    subroutine cle_disp_opt(x)
      integer*8,intent(in)::x
      rpnc%opt=iand(rpnc%opt,not(ishft(x,32)))
    end subroutine cle_disp_opt

    subroutine put_disp_digit(x)
      integer,intent(in)::x
      integer*8 xx
      xx=x
      rpnc%opt=ior(iand(rpnc%opt,digit_mask),ishft(xx,32))
    end subroutine put_disp_digit

    subroutine set_disp_opt(x)
      integer*8,intent(in)::x
      rpnc%opt=ior(rpnc%opt,ishft((x),32))
    end subroutine set_disp_opt

    subroutine print_com_list()
      use misc, only: messp
      integer i,j
      character*(*),parameter::coms(12*4)=[&
           "help     ",&    
           "version  ",&      
           "[no]debug",&      
           "opt      ",&        
           "quit     ",&            
           "init     ",&            
           "         ",&                       
           "read     ",&                 
           "write    ",&                    
           "save     ",&                 
           "load     ",&                 
           "prompt   ",&                  
           !
           "echo     ",&                 
           "print    ",&                 
           "[no]dms  ",&                   
           "[no]eng  ",&                         
           "fix      ",&                   
           "exp      ",&                    
           "fig      ",&                    
           "[no]byte ",&                    
           "         ",&               
           "bin      ",&                     
           "oct      ",&                  
           "dec      ",&               
           !
           "hex      ",&                  
           "Bin      ",&
           "Oct      ",&                  
           "Dec      ",&                  
           "Hex      ",&                  
           "BIN      ",&                  
           "OCT      ",&                
           "DEC      ",&                  
           "HEX      ",&                  
           "deg      ",&                  
           "rad      ",&                  
           "ratio    ",&                      
           !        
           "frac     ",&                
           "stat     ",&                
           "data     ",&                     
           "clear    ",&                    
           "norm     ",&                
           "         ",&
           "         ",&
           "         ",&
           "         ",&
           "         ",&
           "         ",&
           "         "]
      call mess("* Commands begin with `.'")
      call mess("* Shell commands begin with `!'")
      call mess("* Comments begin with `#'\n")
      do j=1,12
         do i=1,4
            call messp(coms(j+(i-1)*12)//"\t")
         end do
         call mess("")
      end do
      
    end subroutine print_com_list

    subroutine print_help
      select case(cid)
      case(CID_EXIT)
        str="quit:\n\t.{q|quit}"
     case(CID_INI)
        str="initialize all the status"
     case(CID_SCLE)
        str="clear data of statistical mode:\n\t.{cle|clear}"
     case(-CID_SAVE,-CID_SAVE_PAR,-CID_SAVE_FNC,-CID_SAVE_MAC)
        str="save parameters, functions or macros on a file:\n\t.save obj file"
     case(CID_SAVE_PAR)
        str="save parameters on a file:\n\t.save {p|par|parameter} file"
     case(CID_SAVE_FNC)
        str="save parameters on a file:\n\t.save {f|func|function} file"
     case(CID_SAVE_MAC)
        str="save parameters on a file:\n\t.save {m|mac|macro} file"
     case(-CID_PRI)
        str="print parameters, functions, macros or data:\n\t.{p|pri|print} obj [name]"
     case(-CID_PRI_PAR,CID_PRI_PAR)
        str="print parameters, pp:\n\t.{p|pri|print} {p|par|parameter} [name]"
     case(-CID_PRI_FNC,CID_PRI_FNC)
        str="print functions, pf:\n\t.{p|pri|print} {f|func|function} [name]"
     case(-CID_PRI_MAC,CID_PRI_MAC)
        str="print macros, pm:\n\t.{p|pri|print} {m|mac|macro} [name]"
     case(CID_PRI_DAT)
        str="print data, pd:\n\t.{p|pri|print} {d|dat|data}"
     case(-CID_DEL)
        str="delete parameter, function or macro\n\t.{del|delete} obj {name|.all}"
     case(-CID_DEL_PAR,CID_DEL_PAR)
        str="delete a parameter, dp:\n\t.{del|delete} {p|par|parameter} {name|.all}"
     case(-CID_DEL_MAC,CID_DEL_MAC)
        str="delete a macro, dm:\n\t.{del|delete} {m|mac|macros} {name|.all}"
     case(-CID_DEL_FNC,CID_DEL_FNC)
        str="delete a function, df:\n\t.{del|delete} {f|func|function} {name|.all}"
     case(CID_DEL_PAR_ALL)
        str="delete all parameters:\n\t.{del|delete} {p|par|parameter} .all"
     case(CID_LOAD)
        str="load a file:\n  .load file"
     case(-CID_ECHO,CID_ECHO_OFF,CID_ECHO_ON)
        str="sets echo mode on/off:\n\t.echo [on|off]"
     case(-CID_HIST,CID_HIST_OFF,CID_HIST_ON)
        str="sets history save mode on/off:\n\t.hist [on|off]"
     case(-CID_SET_PROMPT,CID_SET_PROMPT)
        str="sets prompt string:\n\t.prompt sting"
     case(-CID_WRITE,CID_WRITE)
        str="writes string:\n\t.write string"
     case default
        str="???"
     end select
     call mess(trim(str))
   end subroutine print_help

  end function exe_com
  
  integer function parse_command(a,p1arg,p2arg)
    ! a must not include dup white and must be left adjusted
    ! return value:
    !  0< : cammnd id
    !  0  : not a command
    ! -1  : invalid command
    use memio, only: itoa
    use misc, only: set_opt,cle_opt,mess,messp,is_alpha
    character*(*),intent(in)::a
    integer,intent(out)::p1arg,p2arg
    integer p1,p2
    integer lencom
    logical help
    integer cid

    parse_command=0
    cid=CID_NOP
    p1arg=0
    p2arg=0
    lencom=len_trim(a)

    if(lencom<=1) return
    if(a(1:1)=="!") then
       parse_command=CID_SHELL
       p1arg=2
       p2arg=lencom
       return
    end if
    if(a(1:1)/=".".or..not.is_alpha(a(2:2))) return

    p2=1
    p1=get_arg(p2)
    help=.false.
    cid=CID_NOP

    do
       select case(cid)
       case(CID_NOP)
          select case(a(p1:p2))
          case("ver","version")
             cid=CID_VER
          case("h","help")
             help=.true.
          case("byte")
             cid=CID_BYTE
          case("nobyte")
             cid=CID_NOBYTE
          case("opt")
             cid=CID_OPT
          case("q","quit")
             cid=CID_EXIT
          case("nodms")
             cid=CID_NODMS
          case("dms") ! degree minute second
             cid=CID_DMS
             call set_disp_arg()
          case("noeng")
             cid=CID_NODBG
          case("eng")
             cid=CID_ENG
             call set_disp_arg()
          case("fix")
             cid=CID_FIX
             call set_disp_arg()
          case("exp")
             cid=CID_EXP
             call set_disp_arg()
          case("fig")
             cid=CID_FIG
             call set_disp_arg()
          case("DEC")
             cid=CID_DEC_IO
          case("HEX")
             cid=CID_HEX_IO
          case("OCT")
             cid=CID_OCT_IO
          case("BIN")
             cid=CID_BIN_IO
          case("Dec")
             cid=CID_DEC_I
          case("Hex")
             cid=CID_HEX_I
          case("Oct")
             cid=CID_OCT_I
          case("Bin")
             cid=CID_BIN_I
          case("dec")
             cid=CID_BIN_O
          case("hex")
             cid=CID_BIN_O
          case("oct")
             cid=CID_BIN_O
          case("bin")
             cid=CID_BIN_O
          case("deg")
             cid=CID_DEG
          case("rad")
             cid=CID_RAD
          case("dbg","debug")
             cid=CID_DBG
          case("cle","clear")
             cid=CID_SCLE
          case("s","sta","stat")
             cid=CID_STA 
          case("d","dat","data")
             cid=CID_DAT
          case("n","norm")
             cid=CID_NORM
          case("nodbg","nodebug")
             cid=CID_NODBG
          case("r","ratio")
             cid=CID_RATIO
          case("f","frac")
             cid=CID_FRAC
          case("write")
             cid=-CID_WRITE
          case("prompt")
             cid=-CID_SET_PROMPT
          case("hist") 
             cid=-CID_HIST
          case("echo")
             cid=-CID_ECHO
          case("load")
             cid=-CID_LOAD
          case("save")
             cid=-CID_SAVE             
          case("del","delete")
             cid=-CID_DEL
          case("dm")
             cid=-CID_DEL_MAC
          case("df")
             cid=-CID_DEL_FNC
          case("dp")
             cid=-CID_DEL_PAR
          case("p","pri","print")
             cid=-CID_PRI
          case("pm")
             cid=-CID_PRI_MAC
          case("pf")
             cid=-CID_PRI_FNC
          case("pp")
             cid=-CID_PRI_PAR   
          case("pd")
             cid=CID_PRI_DAT   
          case("init")
             cid=CID_INI
          case default
             cid=CID_INV
          end select
       case(-CID_HIST)
          select case(get_ak(a(p1:p2)))
          case(AK_ON)
             cid=CID_HIST_ON
          case(AK_OFF)
             cid=CID_HIST_OFF
          case default
             cid=CID_INV
          end select
       case(-CID_ECHO)
          select case(get_ak(a(p1:p2)))
          case(AK_ON)
             cid=CID_ECHO_ON
          case(AK_OFF)
             cid=CID_ECHO_OFF
          case default
             cid=CID_INV
          end select
       case(-CID_DEL)
          select case(get_ak(a(p1:p2)))
          case(AK_PAR)
             cid=-CID_DEL_PAR
          case(AK_MAC)
             cid=-CID_DEL_MAC
          case(AK_FNC)
             cid=-CID_DEL_FNC
          case default
             cid=CID_INV
          end select
       case(-CID_SAVE)
          select case(get_ak(a(p1:p2)))
          case(AK_PAR)
             cid=-CID_SAVE_PAR
          case(AK_MAC)
             cid=-CID_SAVE_MAC
          case(AK_FNC)
             cid=-CID_SAVE_FNC
          case default
             cid=CID_INV
          end select
       case(-CID_PRI)
          select case(get_ak(a(p1:p2)))
          case(AK_PAR)
             cid=-CID_PRI_PAR
          case(AK_MAC)
             cid=-CID_PRI_MAC
          case(AK_FNC)
             cid=-CID_PRI_FNC
          case(AK_DAT)
             cid=CID_PRI_DAT
          case default
             cid=CID_INV
          end select
       case(-CID_WRITE)
          p1arg=p1
          p2arg=lencom
          cid=CID_WRITE
       case(-CID_PRI_PAR,-CID_PRI_FNC,-CID_PRI_MAC,-CID_LOAD,-CID_SET_PROMPT,&
            -CID_SAVE_PAR,-CID_SAVE_FNC,-CID_SAVE_MAC)
          p1arg=p1
          p2arg=p2
          cid=-cid
       case(-CID_DEL_PAR,-CID_DEL_FNC,-CID_DEL_MAC)
          cid=-cid
          if(get_ak(a(p1:p2))==AK_ALL) then
             cid=3+cid ! <<<
          else
             p1arg=p1
             p2arg=p2
          end if
       case default
          call mess("cid = "//trim(itoa(cid)))
          STOP "*** cid: UNEXPECTED ERROR: unknown cid"
       end select

       if(cid>=0.and..not.help) exit

       p1=get_arg(p2)
       if(p1==0.and.p2==0) exit

    end do

    if(cid==CID_INV) then
       parse_command=-1
    else
       if(cid<0) cid=-cid+CID_LAST
       if(help) cid=cid+CID_HELP_OFF
       parse_command=cid
    end if

  contains
    
    subroutine set_disp_arg()
      integer pp1,pp2
      pp1=get_arg(pp2)
      if(pp1>0) then
         p1arg=pp1
         p2arg=pp2
      end if
    end subroutine set_disp_arg

    integer function get_arg(pp2)
      use rpng, only: STID_SQ1,STID_SQ2,STID_DQ1,STID_DQ2
      integer,intent(out)::pp2
      integer kk
      integer k1,k2
      k1=0
      k2=0
      do kk=p2+1,lencom
         select case(a(kk:kk))
         case(" ")
            cycle
         case(STID_SQ1)
            k1=kk
            k2=index(a(k1:),STID_SQ2)
            exit
         case(STID_DQ1)
            k1=kk
            k2=index(a(k1:),STID_DQ2)
            exit
         case default
            k1=kk
            k2=index(a(k1:)," ")
            exit
         end select
      end do
      if(k2==0) then
         if(k1==lencom) then
            k2=k1
         else if(k1/=0) then
            k2=lencom
         end if
      else
         k2=k2+k1-1
      end if
      get_arg=k1
      pp2=k2
    end function get_arg

    integer function get_ak(ss)
      character*(*),intent(in)::ss
      get_ak=AK_INV
      select case(ss)
      case("f","func","function")
         get_ak=AK_FNC
      case("m","mac","macro")
         get_ak=AK_MAC
      case("p","par","parameter")
         get_ak=AK_PAr
      case("d","dat","data")
         get_ak=AK_DAT
      case(".all")
         get_ak=AK_ALL
      case(".any")
         get_ak=AK_ANY
      case("on")
         get_ak=AK_ON
      case("off")
         get_ak=AK_OFF
      end select
    end function get_ak

  end function parse_command

end module com
