module rpnt
  use slist, only: t_slist
  use plist, only: t_plist
  use fpio, only: rp,cp
  use rpng, only: LEN_FORMULA_MAX
  implicit none
  
  type t_rrpnq
     integer tid
     integer p1,p2
  end type t_rrpnq
  
  type t_rpnq
     integer tid
     integer cid ! oid or fid or pointer to value
  end type t_rpnq
  
  type t_rpnb
     character(LEN_FORMULA_MAX) expr
     integer len_expr
     integer cur_pos
     integer old_pos
     integer old_tid
     type(t_rrpnq),allocatable::que(:)
     type(t_rrpnq),allocatable::buf(:)
     integer p_buf,p_que
     integer*8 opt
  end type t_rpnb
  
  type t_rpnm
     type(t_rpnq),allocatable::que(:)  ! allocated
     complex(cp),allocatable::vbuf(:)  ! allocated
     integer,allocatable::p_vbuf       ! allocated
     type(t_plist),pointer::pars       ! => rpnc%pars
     complex(cp),pointer::answer       ! => rpnc%answer
     complex(cp),pointer::tmpans       ! => rpnc%tmpans
     type(t_slist),allocatable::pnames ! allocated
     integer,allocatable::na               ! num arg
  end type t_rpnm
    
  type t_sd
     real(rp),allocatable::ws(:)
     complex(cp),allocatable::vs(:,:) ! n:2
     integer p_vs
  end type t_sd  

end module rpnt
