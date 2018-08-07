PROGRAM test
  USE myUtils
  IMPLICIT NONE
  
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: A,S,T
  REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: V,W,X,Y
  INTEGER(KIND=4) :: n,m,i,j

  n = 2
  m = 2

  ALLOCATE(A(0:n-1,0:n-1))
  ALLOCATE(S(0:m-1,0:n-1))
  ALLOCATE(T(0:m-1,0:m-1))
  ALLOCATE(V(0:n-1))
  ALLOCATE(W(0:n-1))
  ALLOCATE(X(0:n-1))
  ALLOCATE(Y(0:n-1))

  A(0,0) = 1
  A(1,0) = 0
  A(0,1) = 0
  A(1,1) = 2 

  V(0) = 1.0D0
  V(1:n-1) = (/ (0.0D0, i=1,n-1) /)
  CALL linal_lanczos_symreal_2Dreal8(A,n,m,V,W,X,S,T)

  WRITE(*,*) "S is:"
  do i=0,n-1
    write(*,*) S(:,i)
  end do  
  
  write(*,*) 
  WRITE(*,*) "T is:"
  do i=0,m-1
    write(*,*) T(:,i)
  end do
  

END PROGRAM test
