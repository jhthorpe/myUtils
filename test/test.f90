PROGRAM test
  USE myUtils
  IMPLICIT NONE
  integer(kind=4), dimension(:,:) , allocatable :: A
  integer :: k

  CALL say_hi()
  k=5

  allocate(A(0:5,10:20))
  A(1,11) = -42

  WRITE(*,*) LBOUND(A,1)
  WRITE(*,*) LBOUND(A,2)

  write(*,*) ALLOCATED(A)

  CALL int4_2Dgrow1(A)
END PROGRAM
