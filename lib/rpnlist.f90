module rpnlist
  use fzcerr
  use fpio, only: cp
  use rpnt, only: t_rpnq
  use slist, only: t_slist
  use plist, only: t_plist
  implicit none

  private

  public add_rpnm_entry
  public init_rpnlist
  public uinit_rpnlist
  public rpnlist_count
  public kth_rpnm
  public kth_rpnlist
  public find_rpnlist
  public kth_na
  public cp_rpnlist
  interface rm_rpnm_entry
     module procedure rm_rpnm_entry_s
     module procedure rm_rpnm_entry_k
  end interface
  public rm_rpnm_entry
  public rm_rpnm_entry_all

  type,public::t_rpnlist
     integer::n = 0
     type(t_rn),pointer::rn => null()
  end type t_rpnlist

  type,public::t_rpnm
     type(t_rpnq),allocatable::que(:)
     complex(cp),allocatable::vbuf(:)
     type(t_slist) pnames
     integer::p_vbuf = 0
     integer::na     = 0 ! num arg
  end type t_rpnm

  type t_rn
     integer*1,allocatable::s(:)
     integer c
     type(t_rpnm),pointer::rpnm => null()
     type(t_rn),pointer::next => null()
     type(t_rn),pointer::prev => null()
  end type t_rn

contains

  integer function rpnlist_count(rl)
    type(t_rpnlist),intent(in)::rl
    rpnlist_count=rl%n
  end function rpnlist_count

  function init_rpnlist()
    type(t_rpnlist) init_rpnlist
    init_rpnlist%n=0
    nullify(init_rpnlist%rn)
  end function init_rpnlist

  function init_rpnm()
    use slist, only: init_slist
    type(t_rpnm) init_rpnm
    init_rpnm%pnames=init_slist()
    init_rpnm%p_vbuf=0
    init_rpnm%na=0
  end function init_rpnm

  subroutine uinit_rpnm(rpnm)
    use slist, only: uinit_slist
    type(t_rpnm),intent(inout)::rpnm
    if(allocated(rpnm%que)) deallocate(rpnm%que)
    if(allocated(rpnm%vbuf))deallocate(rpnm%vbuf)
    call uinit_slist(rpnm%pnames)
  end subroutine uinit_rpnm

  subroutine uinit_rn(rn)
    type(t_rn),intent(inout)::rn
    if(associated(rn%rpnm)) then
       call uinit_rpnm(rn%rpnm)
       deallocate(rn%rpnm)
    end if
    if(allocated(rn%s)) deallocate(rn%s)
  end subroutine uinit_rn

  subroutine uinit_rpnlist(rl)
    type(t_rpnlist),intent(inout)::rl
    integer i
    type(t_rn),pointer::rn,next
    if(rl%n==0) return
    rn => rl%rn
    do i=1,rl%n
       if(.not.associated(rn)) exit
       call uinit_rn(rn)
       next => rn%next
       deallocate(rn)
       rn => next
    end do
    rl%n=0
    nullify(rl%rn)
  end subroutine uinit_rpnlist

  function kth_node(rl,k)
    type(t_rpnlist),intent(in)::rl
    integer,intent(in)::k
    type(t_rn),pointer::kth_node
    type(t_rn),pointer::rn
    integer i
    nullify(kth_node)
    if(k<=0.or.k>rl%n) return
    rn => rl%rn
    if(.not.associated(rn)) return
    do i=2,k
       rn => rn%next
       if(.not.associated(rn)) return
    end do
    kth_node => rn
  end function kth_node

  function match_node(rl,s,c,k)
    type(t_rpnlist),intent(in)::rl
    character*(*),intent(in)::s
    integer,intent(in)::c
    integer,intent(out),optional::k
    type(t_rn),pointer::match_node
    type(t_rn),pointer::rn
    integer i
    nullify(match_node)
    if(present(k)) k=0
    if(rl%n==0) return
    rn => rl%rn
    if(.not.associated(rn)) return
    do i=1,rl%n
       if(allocated(rn%s)) then
          if(len(s)==size(rn%s).and.c==rn%c.and.is_matched()) then
             match_node => rn
             if(present(k)) k=i
             return
          end if
       end if
       rn => rn%next
       if(.not.associated(rn)) return
    end do
  contains
    logical function is_matched()
      integer j
      is_matched=.false.
      do j=1,size(rn%s)
         if(ichar(s(j:j))/=rn%s(j)) return
      end do
      is_matched=.true.
    end function is_matched
  end function match_node

  function append_node(rl,s,c)
    type(t_rpnlist),intent(inout)::rl
    character*(*),intent(in)::s
    integer,intent(in)::c
    type(t_rn),pointer::append_node
    type(t_rn),pointer::rn,prev
    if(.not.associated(rl%rn)) then
       rl%n=0
       allocate(rl%rn)
       prev => rl%rn
       rn => rl%rn
    else
       rn => rl%rn%prev ! tail
       allocate(rn%next)
       prev => rn
       rn => rn%next
    end if
    rn%prev => prev
    nullify(rn%next)
    rl%n=rl%n+1
    rl%rn%prev => rn

    call set_rn(rn,s,c)

    append_node => rn

  end function append_node

  subroutine set_rn(rn,s,c)
    use memio, only: mcp
    type(t_rn),intent(inout)::rn
    character*(*),intent(in)::s
    integer,intent(in)::c
    allocate(rn%s(len_trim(s)))
    call mcp(loc(rn%s),loc(s),len_trim(s))
    rn%c=c
    allocate(rn%rpnm)
    rn%rpnm=init_rpnm()
  end subroutine set_rn

  subroutine add_rpnm_entry(rl,name,code,ent,rpnm)
    type(t_rpnlist),intent(inout)::rl
    character*(*),intent(in)::name
    integer,intent(in)::code
    integer,intent(out),optional::ent
    type(t_rpnm),intent(out),pointer,optional::rpnm
    type(t_rn),pointer::rn
    integer k
    rn => match_node(rl,name,code,k)
    if(.not.associated(rn)) then
       rn => append_node(rl,name,code)
       k=rl%n
    else
       call uinit_rn(rn) !<<<<<<<<<<<<<<<<<<
       call set_rn(rn,name,code)!<<<<<<<<<<<<<<<<<
    end if
    if(present(ent)) ent=k
    if(present(rpnm)) rpnm => rn%rpnm
  end subroutine add_rpnm_entry

  function kth_rpnm(rl,k)
    type(t_rpnlist),intent(in)::rl
    integer,intent(in)::k
    type(t_rpnm),pointer::kth_rpnm
    type(t_rn),pointer::rn
    nullify(kth_rpnm)
    if(k<=0.or.k>rl%n) return
    rn => kth_node(rl,k)
    if(.not.associated(rn)) return
    if(.not.associated(rn%rpnm)) return
    kth_rpnm => rn%rpnm
  end function kth_rpnm

  integer function kth_rpnlist(rl,k,ptr,len,code,node)
    type(t_rpnlist),intent(in)::rl
    integer,intent(in)::k
    integer,intent(out),optional::ptr,len,code
    type(t_rn),intent(out),pointer,optional::node
    type(t_rn),pointer::rn
    if(present(ptr)) ptr=0
    if(present(len)) len=0
    if(present(code)) code=0
    kth_rpnlist=FZCERR_NOENT
    rn => kth_node(rl,k)
    if(.not.associated(rn)) return
    if(present(ptr)) ptr=loc(rn%s)
    if(present(len)) len=size(rn%s)
    if(present(code)) code=rn%c
    if(present(node)) nullify(node)
    kth_rpnlist=0
  end function kth_rpnlist

  integer function find_rpnlist(rl,s,c,rpnm)
    type(t_rpnlist),intent(in)::rl
    character*(*),intent(in)::s
    integer,intent(in)::c
    type(t_rpnm),intent(out),pointer,optional::rpnm
    integer k
    type(t_rn),pointer::rn
    find_rpnlist=0
    if(present(rpnm)) nullify(rpnm)
    rn => match_node(rl,s,c,k)
    if(.not.associated(rn)) return
    if(present(rpnm)) rpnm => rn%rpnm
    find_rpnlist=k
  end function find_rpnlist

  integer function kth_na(rl,k)
    type(t_rpnlist),intent(in)::rl
    integer,intent(in)::k
    type(t_rn),pointer::rn
    kth_na=0
    if(k<=0.or.k>rl%n) return
    rn => kth_node(rl,k)
    if(.not.associated(rn)) return
    if(.not.associated(rn%rpnm)) return
    kth_na=rn%rpnm%na
  end function kth_na

  function cp_rpnm(rpnm)
    use slist, only: cp_slist
    type(t_rpnm),intent(in)::rpnm
    type(t_rpnm) cp_rpnm
    cp_rpnm=init_rpnm()
    if(allocated(rpnm%que)) then
       allocate(cp_rpnm%que(size(rpnm%que)))
       cp_rpnm%que=rpnm%que
    end if
    if(allocated(rpnm%vbuf)) then
       allocate(cp_rpnm%vbuf(size(rpnm%vbuf)))
       cp_rpnm%vbuf=rpnm%vbuf
    end if
    cp_rpnm%na=rpnm%na
    cp_rpnm%p_vbuf=rpnm%p_vbuf
    cp_rpnm%pnames=cp_slist(rpnm%pnames)
  end function cp_rpnm

  function cp_rpnlist(rl)
    use memio, only: mcp
    type(t_rpnlist),intent(in)::rl
    type(t_rpnlist) cp_rpnlist
    type(t_rn),pointer::rn_in,rn,prev
    integer i
    cp_rpnlist=init_rpnlist()
    if(rl%n==0) return
    if(.not.associated(rl%rn)) return
    rn_in => rl%rn
    allocate(cp_rpnlist%rn)
    rn => cp_rpnlist%rn
    nullify(rn%next)
    rn%prev => rn
    cp_rpnlist%n=1
    do i=1,rl%n
       rn%c=rn_in%c
       if(allocated(rn_in%s)) then
          allocate(rn%s(size(rn_in%s)))
          call mcp(loc(rn%s),loc(rn_in%s),size(rn_in%s))
       end if
       if(associated(rn_in%rpnm)) then
          allocate(rn%rpnm)
          rn%rpnm=cp_rpnm(rn_in%rpnm)
       end if
       rn_in => rn_in%next
       if(.not.associated(rn_in)) exit
       prev => rn
       allocate(rn%next)
       rn => rn%next
       rn%prev => prev
       nullify(rn%next)
       cp_rpnlist%n=cp_rpnlist%n+1
    end do
    cp_rpnlist%rn%prev => rn
  end function cp_rpnlist

  integer function rm_rpnm_entry_s(rl,s,c)
    type(t_rpnlist),intent(inout)::rl
    character*(*),intent(in)::s
    integer,intent(in)::c
    type(t_rn),pointer::rn
    rm_rpnm_entry_s=FZCERR_NOENT
    if(rl%n==0) return
    rn => match_node(rl,s,c)
    if(.not.associated(rn)) return
    call rm_rpnm_entry_rn(rl,rn)
    rm_rpnm_entry_s=0
  end function rm_rpnm_entry_s

  integer function rm_rpnm_entry_k(rl,k)
    type(t_rpnlist),intent(inout)::rl
    integer,intent(in)::k
    type(t_rn),pointer::rn
    rm_rpnm_entry_k=FZCERR_NOENT
    if(k<=0.or.k>rl%n) return
    rn => kth_node(rl,k)
    if(.not.associated(rn)) return
    call rm_rpnm_entry_rn(rl,rn)
    rm_rpnm_entry_k=0
  end function rm_rpnm_entry_k

  subroutine rm_rpnm_entry_rn(rl,rn)
    type(t_rpnlist),intent(inout)::rl
    type(t_rn),intent(inout),pointer::rn
    call uinit_rn(rn)
    rn%prev%next => rn%next
    if(associated(rn%next)) then
       rn%next%prev => rn%prev
    else
       ! the tail is about removed
       rl%rn%prev => rn%prev
    end if
    deallocate(rn)
    rl%n=rl%n-1
    if(rl%n==0) nullify(rl%rn)
  end subroutine rm_rpnm_entry_rn

  subroutine rm_rpnm_entry_all(rl,code)
    type(t_rpnlist),intent(inout)::rl
    integer,intent(in),optional::code
    type(t_rn),pointer::rn,next
    integer i
    integer m
    if(rl%n==0) return
    rn => rl%rn
    if(.not.associated(rn)) return
    m=0
    do i=1,rl%n
       next => rn%next
       if(.not.present(code).or.rn%c==code) then
          m=m+1
          call rm_rpnm_entry_rn(rl,rn)
       end if
       if(.not.associated(next)) exit
       rn => next
    end do
    rl%n=rl%n-m
  end subroutine rm_rpnm_entry_all

end module rpnlist
