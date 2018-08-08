PROGRAM test
  USE myUtils
  IMPLICIT NONE
  
  !lanczos
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: A,S
  REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: V,W,X,Y,Td,Ts
  INTEGER(KIND=4) :: n,m,i,j

  !gs
  REAL(KIND=8), DIMENSION(0:2,0:2) :: B
  REAL(KIND=8), DIMENSION(0:2) :: u,aba,gaba

  !testing lanczos

  n = 3
  m = 2

  ALLOCATE(A(0:n-1,0:n-1))
  ALLOCATE(S(0:m-1,0:n-1))
  ALLOCATE(Td(0:m-1))
  ALLOCATE(Ts(0:m-2))
  ALLOCATE(V(0:n-1))
  ALLOCATE(W(0:n-1))
  ALLOCATE(X(0:n-1))
  ALLOCATE(Y(0:n-1))

  !A = TRANSPOSE(RESHAPE((/ 93,57,93,57,2,12,93,12,19/),(/n,n/)))
  A = TRANSPOSE(RESHAPE((/ 25,1,0,1,32,0,0,0,93/),(/n,n/)))
 
  DO i=0,n-1
      WRITE(*,*) A(:,i)
  END DO

  V(0) = 1.0D0
  V(1:n-1) = (/ (0.0D0, i=1,n-1) /)
  CALL linal_lanczos_symreal_2Dreal8(A,n,m,V,W,X,S,Td,Ts)

  WRITE(*,*) "S is:"
  do i=0,n-1
    write(*,*) S(:,i)
  end do  
  
  write(*,*) 
  WRITE(*,*) "eigenvalues are"
  do i=0,m-1
    write(*,*) Td(i)
  end do

  ! testing GS
  !WRITE(*,*) 
  !WRITE(*,*) "////// TESTING GS //////"

  !B = TRANSPOSE(RESHAPE((/1,2,3,0,0,0,0,0,0/),SHAPE(B)))
  
  !WRITE(*,*) "Currect array:"
  !DO i=0,SIZE(B(0,:))-1
  !    WRITE(*,*) B(:,i)
  !END DO

  !WRITE(*,*) "New vector 1"
  !CALL linal_onvec_2Dreal8(B,u,SIZE(w)-1,1)
  !DO i=0,SIZE(u)-1
  !  WRITE(*,*) u(i) 
  !END DO

  !B(1,:) = u

  !WRITE(*,*) "New Vector 2"
  !CALL linal_onvec_2Dreal8(B,u,SIZE(w)-1,2)
  !DO i=0,SIZE(u)-1
  !  WRITE(*,*) u(i) 
  !END DO

  !aba = [1,1,0]
  !gaba = [1,2,3]

  !WRITE(*,*) 
  !WRITE(*,*) SUM(aba*gaba)/SUM(gaba*gaba)*gaba 

END PROGRAM test
