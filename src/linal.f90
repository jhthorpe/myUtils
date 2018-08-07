!/////////////////////////////////////////////////////////////////////
!//             Module containing linear algebra subroutines 
!//
!//                     James H Thorpe, in Group of John Stanton
!//                     The University of Florida
!//
!/////////////////////////////////////////////////////////////////////

! GENERAL STRUCTURE:
!       linal_operation_matType_dataType
!       example: Lanczos eigenvalues of a Hermitian 2D real*8 matrix 
!               linal_lanczos_hermitian_2Dreal8

MODULE linal
  USE dynamic_arrays
  CONTAINS

!---------------------------------------------------------------------
!	linal_lanczos_symreal_2Dreal8
!		James H. Thorpe
!		July 21, 2018
!	- Performs Lanczos to find lowest m eigenvalues of ...
!	  ... a (nxn) symmetric, real valued, 2D real8 array 
!	- takes V as an input vector of length n. Program checks if it...
!	  ... its Euclidean Norm is 1
!	- Currently uses my implementation of matrix vector multiplication
!	- DGEM would be prefered, and should perhaps be replaced
!	- T = S*AS
!---------------------------------------------------------------------
  ! Values
  ! A		:	2D real8, nxn symetric, real valued array to be tridiagonalized
  ! n		:	int4, size of arrays
  ! m		:	int4, number of lowest eigenvalues to be found
  ! V		:	1D real8, length n input vector 
  ! W		:	1D real8, length n working vector
  ! X		:	1D real8, length n working vector
  ! S		:	2D real8, nxm (row x col) colomns of X's 
  ! T		:	2D real8, mxm tridiagonal	

  SUBROUTINE linal_lanczos_symreal_2Dreal8(A,n,m,V,W,X,S,T)
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: tol=1.0D-16
    REAL(KIND=8), DIMENSION(0:,0:), INTENT(INOUT) :: S,T
    REAL(KIND=8), DIMENSION(0:,0:), INTENT(IN) :: A    
    REAL(KIND=8), DIMENSION(0:), INTENT(INOUT) :: V,W,X
    INTEGER(KIND=4), INTENT(IN) :: n,m
    
    INTEGER(KIND=4) :: i,j
    REAL(KIND=8) :: b,c

    CALL real8_2Dzero(T)

    !check input vector has Euclidian norm of 1
    b = linal_eunorm_1Dreal8(V,n) 
    IF ( ABS(b - 1.0D0) .GT. tol) THEN
      V(0) = 1.0D0
      V(1:n-1) = (/ (0.0D0, i=1, n-1) /) 
    END IF

    !Initial step
    W = MATMUL(A,V)
    c = SUM(W*V) !this works because we are real valued
    X = W - c*V
    S(0,:) = V
    T(0,0) = c

    DO i=1,m-1
      b = linal_eunorm_1Dreal8(X,n)
      IF (ABS(b - 0.0D0) .LT. tol) THEN
        V = X/b
      ELSE
        !this is currently broken, and should be fixed
        V = (/ (0.0D0, j=0,n-1) /)
        V(1) = 1.0D0
        IF ( j .GT. 1) STOP "We have gone beyond my hardcoded case"
      END IF
      W = MATMUL(A,V)
      c = SUM(W*V) !this works because we are real valued 
      X = W - c*V-b*S(i-1,:)
      S(i,:) = V
      T(i,i) = c
      T(i-1,i) = b 
      T(i,i-1) = b
    END DO 

  END SUBROUTINE linal_lanczos_symreal_2Dreal8

!---------------------------------------------------------------------
!	linal_eunorm_1Dreal8
!		James H. Thorpe
!		July 22, 2018
!	- function that returns the euclidean norm of a real8 vector
!	- might consider changing this for greater numerical stability?
!	- certainly consider changing this for better performance
!---------------------------------------------------------------------
  ! Values
  ! A		:	1D real8, vector who's norm to return
  ! n		:	int4, lenth of vector

  REAL(KIND=8) FUNCTION linal_eunorm_1Dreal8(A,n)
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(0:), INTENT(IN) :: A
    INTEGER(KIND=4), INTENT(IN) :: n
    
    INTEGER(KIND=4) :: i
    REAL(KIND=8) :: temp

    temp = 0.0D0
    DO i=0, n-1
      temp = temp + A(i)**2.0D0 
    END DO 
   
    temp = SQRT(temp)
    linal_eunorm_1Dreal8 = temp

  END FUNCTION linal_eunorm_1Dreal8
!---------------------------------------------------------------------

END MODULE linal
