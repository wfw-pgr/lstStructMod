module lstStructMod

  ! ------------------------------------------------------ !
  ! --- [1] list element                               --- !
  ! ------------------------------------------------------ !
  type :: list_type
     type(list_type), pointer :: next
     integer                  :: groupNum
     integer                  :: value
  end type list_type
  
contains

  
  ! ====================================================== !
  ! === make new node of a list                        === !
  ! ====================================================== !
  subroutine make__nodeInList( list, next, value )
    implicit none
    type(list_type), pointer,           intent(out) :: list
    type(list_type), pointer, optional, intent(in ) :: next
    integer        ,          optional, intent(in ) :: value

    ! ------------------------------------------------------ !
    ! --- [1] list allocation check                      --- !
    ! ------------------------------------------------------ !
    if ( associated(list) ) then
       write(6,*) "[make__nodeInList] list is allocated.... [ERROR] "
    endif

    ! ------------------------------------------------------ !
    ! --- [2] allocation of a node                       --- !
    ! ------------------------------------------------------ !
    allocate( list      )
    nullify ( list%next )

    ! ------------------------------------------------------ !
    ! --- [3] store next or value                        --- !
    ! ------------------------------------------------------ !
    if ( present( next  ) ) then
       list%next => next
    endif
    if ( present( value ) ) then
       list%value = value
    endif
    return
  end subroutine make__nodeInList


  ! ====================================================== !
  ! === delete the list                                === !
  ! ====================================================== !
  subroutine delete__list( list )
    implicit none
    type(list_type), pointer, intent(inout) :: list
    type(list_type), pointer                :: iter, temp

    if ( .not.( associated(list) ) ) then
       return
    endif

    iter => list
    do while( associated( iter%next ) )
       temp => iter
       iter => iter%next
       deallocate( temp )
    enddo

    nullify( list )
    return
  end subroutine delete__list


  ! ====================================================== !
  ! === return the pointer of a list                   === !
  ! ====================================================== !
  function tail__nodeInList(list) result(iter)
    type(list_type), pointer, intent(in) :: list
    type(list_type), pointer             :: iter
    
    iter => list
    do while( associated(iter%next) )
       iter => iter%next
    end do
    return
  end function tail__nodeInList


  ! ====================================================== !
  ! ===  append node to a list                         === !
  ! ====================================================== !
  subroutine append__nodeInList( list, value )
    implicit none
    integer        ,          intent(in) :: value
    type(list_type), pointer, intent(in) :: list
    type(list_type), pointer             :: iter

    iter => tail__nodeInList( list )
    call make__nodeInList( iter%next, value=value )
    return
  end subroutine append__nodeInList


  ! ====================================================== !
  ! === insert of a node at the index                  === !
  ! ====================================================== !
  subroutine insert__nodeInList( list, index, value )
    implicit none
    type(list_type), pointer, intent(inout) :: list
    integer                 , intent(in)    :: index
    integer                 , intent(in)    :: value
    integer                                 :: ik
    type(list_type), pointer                :: iter, prev, node

    ! ------------------------------------------------------ !
    ! --- [1] index starts from 1                        --- !
    ! ------------------------------------------------------ !
    if ( index < 1 ) then
       write(6,*) "[insert__nodeInList] index ERROR. no such index. [ERROR]"
       write(6,*) "[insert__nodeInList] index == ", index
       stop
    endif

    ! ------------------------------------------------------ !
    ! --- [2] skip until index                           --- !
    ! ------------------------------------------------------ !
    nullify( prev )
    iter => list
    do ik=1, index-1
       if ( .not.( associated( iter%next ) ) ) then
          write(6,*) "[insert__nodeInList] index ERROR. no such index. [ERROR]"
          write(6,*) "[insert__nodeInList] index == ", index
          stop
       endif
       prev => iter
       iter => iter%next
    enddo

    ! ------------------------------------------------------ !
    ! --- [3] create a new node                          --- !
    ! ------------------------------------------------------ !
    nullify( node )
    call make__nodeInList( node, next=iter, value=value )
    if ( .not.associated( prev ) ) then ! -- this means first node -- !
       list => node
    else
       prev%next => node
    endif
    
  end subroutine insert__nodeInList



  ! ====================================================== !
  ! === remove a node at a given index                 === !
  ! ====================================================== !
  subroutine remove__nodeInList( list, index )
    implicit none
    type(list_type), pointer, intent(inout) :: list
    integer        ,          intent(in)    :: index
    integer                                 :: ik
    type(list_type), pointer                :: iter, prev

    ! ------------------------------------------------------ !
    ! --- [1] index starts from 1                        --- !
    ! ------------------------------------------------------ !
    if ( index < 1 ) then
       write(6,*) "[remove__nodeInList] index ERROR. no such index. [ERROR]"
       write(6,*) "[remove__nodeInList] index == ", index
       stop
    endif

    ! ------------------------------------------------------ !
    ! --- [2] skip until index                           --- !
    ! ------------------------------------------------------ !
    nullify( prev )
    iter => list
    do ik=1, index-1
       if ( .not.( associated( iter%next ) ) ) then
          write(6,*) "[remove__nodeInList] index ERROR. no such index. [ERROR]"
          write(6,*) "[remove__nodeInList] index == ", index
          stop
       endif
       prev => iter
       iter => iter%next
    enddo

    ! ------------------------------------------------------ !
    ! --- [3] remove a node to be removed                --- !
    ! ------------------------------------------------------ !
    if ( .not.associated( prev ) ) then ! -- this means first node -- !
       list => iter%next
    else
       prev%next => iter%next
    endif
    deallocate(iter)

  end subroutine remove__nodeInList
  

  ! ====================================================== !
  ! === show the contents of the list                  === !
  ! ====================================================== !
  subroutine show__nodeInList( list )
    implicit none
    type(list_type), pointer, intent(in) :: list
    type(list_type), pointer             :: iter

    if ( associated( list ) ) then
       iter => list
       write(6,"(i8)",advance="no") iter%value
       do while( associated(iter%next) )
          iter => iter%next
          write(6,"(i8)",advance="no") iter%value
       enddo
       
    else
       write(6,*) "[show__nodeInList] list is empty.... "
       return
    endif

    write(6,*)
    return
  end subroutine show__nodeInList

  
end module lstStructMod


! ====================================================== !
! === main                                           === !
! ====================================================== !
program main
  use lstStructMod
  implicit none
  type(list_type), pointer :: list

  nullify( list )

  call make__nodeInList( list, value=10 )
  call show__nodeInList( list )
  call append__nodeInList( list, 20 )
  call show__nodeInList( list )
  call append__nodeInList( list, 30 )
  call show__nodeInList( list )
  call insert__nodeInList( list, 3, 25 )
  call show__nodeInList( list )
  call remove__nodeInList( list, 3  )
  call show__nodeInList( list )
  call delete__list( list )
  call show__nodeInList( list )
  

end program main
