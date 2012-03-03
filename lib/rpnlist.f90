module rpnlist
  use rpnt, only: t_rpnm
  use slist, only: t_slist
  implicit none

  private

  type,public::t_rpnlist
     type(t_rpnm),allocatable::rpnm(:)
     type(t_slist) s
  end type t_rpnlist

end module rpnlist
