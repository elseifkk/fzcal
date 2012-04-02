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
module rpnt
  use slist, only: t_slist
  use plist, only: t_plist
  use fpio, only: rp,cp
  use rpng, only: LEN_FORMULA_MAX
  implicit none

  type t_rrpnq
     integer::tid = 0
     integer::p1  = 0
     integer::p2  = 0
  end type t_rrpnq

  type t_rpnq
     integer::tid = 0
     integer::cid = 0
  end type t_rpnq

  type t_rpnf
     integer::mode  = 0
     integer::dmode = 0
     integer::sta   = 0
  end type t_rpnf

  integer,parameter::NUM_RRPNQ_MAX=1024
  type t_rpnb
     character(LEN_FORMULA_MAX),pointer::expr => null()
     integer,pointer::len_expr => null()
     integer::cur_pos = 0
     integer::old_tid = 0
     type(t_rrpnq) que(NUM_RRPNQ_MAX)
     type(t_rrpnq) buf(NUM_RRPNQ_MAX)
     integer::p_buf = 0
     integer::p_que = 0
     integer*8::opt = 0
     type(t_rpnf) flg
     integer::err = 0
  end type t_rpnb

  type t_sd
     real(rp),allocatable::ws(:)
     complex(cp),allocatable::vs(:,:) ! n:2
     integer p_vs
  end type t_sd

end module rpnt
