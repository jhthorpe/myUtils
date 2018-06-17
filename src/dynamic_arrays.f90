!/////////////////////////////////////////////////////////////////////
!//             Module containing dynamic array subroutines  
!//
!//                     James H Thorpe, in Group of John Stanton
!//                     The University of Florida
!//
!/////////////////////////////////////////////////////////////////////


MODULE dynamic_arrays

!---------------------------------------------------------------------
!       Generate table of auxilary functions, RNLMj 
!               James H. Thorpe
!               June 16, 2018
!       -double the first dimension of a 2d, int(kind=4) array
!---------------------------------------------------------------------
  CONTAINS

  SUBROUTINE int4_2Dgrow1(A)
    IMPLICIT NONE
    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: A
    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:) :: B
    INTEGER :: stat,n,m,i,j,k,l1,l2,u1,u2
    !get shape of A
    l1 = LBOUND(A,1)
    l2 = LBOUND(A,2)
    WRITE(*,*) "l1=", l1,", l2=", l2
    WRITE(*,*) A(1,11)
    !check if allocatable
    IF (.NOT. ALLOCATED(A)) THEN
      WRITE(*,*) "myUtil:int4_2Dgrow1 - array A not allocated"
      STOP
    ELSE
      ALLOCATE(B(0:n-1,0:m-1),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:int4_2Dgrow1 - could not allocate array B"
        STOP
      END IF
      !temp copy
      B(:,:) = A(:,:)
      DEALLOCATE(A,STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "util:int4_2Dgrow1 - could not deallocate array A"
        STOP
      END IF
      !resize
      ALLOCATE(A(0:2*n-1,0:m-1),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "util:int4_2Dgrow1 - could not allocate array A"
        STOP
      END IF
      !move the data
      DO j=0,m-1
        DO i=0,n-1
          A(i,j) = B(i,j)
        END DO
      END DO
      DEALLOCATE(B)
    END IF
  END SUBROUTINE int4_2Dgrow1

END MODULE dynamic_arrays
