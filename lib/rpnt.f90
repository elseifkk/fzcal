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
     integer cid
  end type t_rpnq
  
  integer,parameter::NUM_RRPNQ_MAX=1024
  type t_rpnb
     character(LEN_FORMULA_MAX),pointer::expr => null()
     integer,pointer::len_expr => null()
     integer cur_pos
     integer old_tid
     type(t_rrpnq) que(NUM_RRPNQ_MAX)
     type(t_rrpnq) buf(NUM_RRPNQ_MAX)
     integer p_buf,p_que
     integer*8 opt
  end type t_rpnb
      
  type t_sd
     real(rp),allocatable::ws(:)
     complex(cp),allocatable::vs(:,:) ! n:2
     integer p_vs
  end type t_sd  

end module rpnt
