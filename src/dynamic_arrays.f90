!/////////////////////////////////////////////////////////////////////
!//             Module containing dynamic array subroutines  
!//
!//                     James H Thorpe, in Group of John Stanton
!//                     The University of Florida
!//
!/////////////////////////////////////////////////////////////////////

! GENERAL STRUCUTRE
!       type_#D/operation/dimension
!       example: grow the 1st dimension of an int4 2D array
!               int4_2Dgrow1 


MODULE dynamic_arrays

  CONTAINS

!---------------------------------------------------------------------
!      int4_2Dgrow1 
!               James H. Thorpe
!               June 16, 2018
!       -double the first dimension of a 2d, int(kind=4) array
!---------------------------------------------------------------------
  SUBROUTINE int4_2Dgrow1(A)
    ! Variables
    ! A		:	2D int(4) array

    IMPLICIT NONE
    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: A
    INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:) :: B
    INTEGER :: stat,i,j,l1,l2,u1,u2

    !get shape of A
    l1 = LBOUND(A,1)
    l2 = LBOUND(A,2)
    u1 = UBOUND(A,1)
    u2 = UBOUND(A,2)

    !check if allocatable
    IF (.NOT. ALLOCATED(A)) THEN
      WRITE(*,*) "myUtil:int4_2Dgrow1 - array A not allocated"
      STOP

    ELSE
      ALLOCATE(B(l1:u1,l2:u2),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:int4_2Dgrow1 - could not allocate array B"
        STOP
      END IF

      !temp copy
      B = A

      DEALLOCATE(A,STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:int4_2Dgrow1 - could not deallocate array A"
        STOP
      END IF

      !resize
      ALLOCATE(A(l1:(u1-l1)+u1+1,l2:u2),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:int4_2Dgrow1 - could not allocate array A"
        STOP
      END IF

      !move the data
      DO j=l2,u2
        DO i=l1,u1
          A(i,j) = B(i,j)
        END DO
      END DO

      DEALLOCATE(B)
    END IF
  END SUBROUTINE int4_2Dgrow1

!---------------------------------------------------------------------
!	chr8_1Dgrow
!               James H. Thorpe
!               June 17, 2018
!       -double the first dimension of a 1d, char(len=8) array
!---------------------------------------------------------------------
! Variables
!       A       :       2D chr(8), array to be reallocated

  SUBROUTINE chr8_1Dgrow(A)
    IMPLICIT NONE
    CHARACTER(LEN=8), ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: A
    CHARACTER(LEN=8), ALLOCATABLE, DIMENSION(:) :: B
    INTEGER :: stat,l1,u1,i

    !get shape of A
    l1 = LBOUND(A,1)
    u1 = UBOUND(A,1)

    !check if allocatable
    IF (.NOT. ALLOCATED(A)) THEN
      WRITE(*,*) "myUtil:chr8_1Dgrow - array A not allocated"
      STOP
    ELSE
      ALLOCATE(B(l1:u1),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:chr8_1Dgrow - could not allocate array B"
        STOP
      END IF

      !temp copy
      B = A

      DEALLOCATE(A,STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:chr8_1Dgrow - could not deallocate array A"
        STOP
      END IF

      !resize
      ALLOCATE(A(l1:(u1-l1)+u1+1),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:chr8_1Dgrow - could not allocate array A"
        STOP
      END IF

      !move the data
      DO i=l1,u1
        A(i) = B(i)
      END DO

      DEALLOCATE(B)

    END IF

  END SUBROUTINE chr8_1Dgrow

!---------------------------------------------------------------------
!	real8_2Dgrow1
!               James H. Thorpe
!               June 17, 2018
!       -double the first dimension of a 2d, real(kind=8) array
!---------------------------------------------------------------------
  ! Variables
  ! A		:	2D real(8), array to be reallocated

  SUBROUTINE real8_2Dgrow1(A)
    IMPLICIT NONE
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: A
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: B
    INTEGER :: stat,l1,l2,u1,u2,i,j

    !get shape of A
    l1 = LBOUND(A,1)
    l2 = LBOUND(A,2)
    u1 = UBOUND(A,1)
    u2 = UBOUND(A,2)

    !check if allocatable
    IF (.NOT. ALLOCATED(A)) THEN
      WRITE(*,*) "myUtil:real8_2Dgrow1 - array A not allocated"
      STOP
    ELSE
      ALLOCATE(B(l1:u1,l2:u2),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:real8_2Dgrow1 - could not allocate array B"
        STOP
      END IF

      !temp copy
      B = A

      DEALLOCATE(A,STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:real8_2Dgrow1 - could not deallocate array A"
        STOP
      END IF

      !resize
      ALLOCATE(A(l1:(u1-l1)+u1+1,l2:u2),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:real8_2Dgrow1 - could not allocate array A"
        STOP
      END IF

      !move the data
      DO j=l2,u2
        DO i=l1,u1
          A(i,j) = B(i,j)
        END DO
      END DO

      DEALLOCATE(B)

    END IF

  END SUBROUTINE real8_2Dgrow1

!---------------------------------------------------------------------
!	real8_3Dgrow1
!               James H. Thorpe
!               June 17, 2018
!       -double the first dimension of a 3d, real(kind=8) array
!---------------------------------------------------------------------
  ! Variables
  ! A       :       3D real(8), array to be reallocated

  SUBROUTINE real8_3Dgrow1(A)
    IMPLICIT NONE
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:), INTENT(INOUT) :: A
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:) :: B
    INTEGER :: stat,i,j,k,u1,l1,u2,l2,u3,l3

    !get shape of A
    l1 = LBOUND(A,1)
    l2 = LBOUND(A,2)
    l3 = LBOUND(A,3)
    u1 = UBOUND(A,1)
    u2 = UBOUND(A,2)
    u2 = UBOUND(A,3)

    !check if allocatable
    IF (.NOT. ALLOCATED(A)) THEN
      WRITE(*,*) "myUtil:real8_3Dgrow1 - array A not allocated"
      STOP

    ELSE
      ALLOCATE(B(l1:u1,l2:u2,l3:u3),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:real8_3Dgrow1 - could not allocate array B"
        STOP
      END IF

      !temp copy
      B = A

      DEALLOCATE(A,STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:real8_3Dgrow1 - could not deallocate array A"
        STOP
      END IF

      !resize
      ALLOCATE(A(l1:(u1-l1)+u1+1,l2:u2,l3:u3),STAT=stat)
      IF (stat .NE. 0) THEN
        WRITE(*,*) "myUtil:real8_3Dgrow1 - could not allocate array A"
        STOP
      END IF

      !move the data
      DO k=l3,u3
        DO j=l2,u2
          DO i=l1,u1
            A(i,j,k) = B(i,j,k)
          END DO
        END DO
      END DO
      DEALLOCATE(B)

    END IF

  END SUBROUTINE real8_3Dgrow1

!---------------------------------------------------------------------
!	real8_2Dzero
!               James H. Thorpe
!               June 17, 2018
!       -zero the elements of a real8_2D array
!---------------------------------------------------------------------
! Variables
!       A       :       2D real(8), array to be zeroed

  SUBROUTINE real8_2Dzero(A)
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(0:,0:), INTENT(INOUT) :: A
    INTEGER :: i,j

    DO j=0,SIZE(A(0,:))-1
      A(:,j) = (/ (0.0D0, i=0, SIZE(A(:,0))-1 ) /)
    END DO

  END SUBROUTINE real8_2Dzero

!---------------------------------------------------------------------
!	bool_2Dgrow1
!               James H. Thorpe
!               June 18, 2018
!       -double the first dimension of a 2D boolean array 
!       -note that this makes all new elements .F.
!---------------------------------------------------------------------
  ! Variables
  ! A           :       2D bool, the table 
  ! c           :       bool, T = grow down, F = grow up

  SUBROUTINE bool_2Dgrow1(A,c)
  IMPLICIT NONE
  LOGICAL, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: A
  LOGICAL, INTENT(IN) :: c
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: B
  INTEGER :: stat,i,j,k,l1,u1,l2,u2

  !get shape
  l1 = LBOUND(A,1)   
  u1 = UBOUND(A,1)
  l2 = LBOUND(A,2)   
  u2 = UBOUND(A,2)
 
  IF (.NOT. ALLOCATED(A)) THEN
    WRITE(*,*) "myUtil:bool_2Dgrow1 - array A not allocated"
    STOP
  END IF

  !temp copy
  ALLOCATE(B(l1:u1,l2:u2),STAT=stat)
  IF (stat .NE. 0) THEN
    WRITE(*,*) "myUtil:bool_2Dgrow1 - could not allocate array B"
    STOP
  END IF

  B = A

  DEALLOCATE(A,STAT=stat)
  IF (stat .NE. 0) THEN
    WRITE(*,*) "myUtil:bool_2Dgrow1 - could not deallocate array A"
    STOP
  END IF

  !resize
  IF (c) THEN !we go down

    ALLOCATE(A(l1-(u1-l1)-1:u1,l2:u2),STAT=stat)
    IF (stat .NE. 0) THEN
      WRITE(*,*) "myUtil:bool_2Dgrow1 - could not reallocate array A"
      STOP
    END IF

    !move data
    DO j=l2,u2
      A(l1-(u1-l1)-1:l1-1,j) = (/ ( .FALSE. , i=l1-(u1-l1)-1,l1-1) /)
      A(l1:u1,j) = (/ ( B(i,j), i=l1,u1 ) /)
    END DO

  ELSE !we go up

    ALLOCATE(A(l1:u1+(u1-l1)+1,l2:u2),STAT=stat)
    IF (stat .NE. 0) THEN
      WRITE(*,*) "myUtil:bool_2Dgrow1 - could not reallocate array A"
      STOP
    END IF

    DO j=l2,u2
      A(l1:u1,j) = (/ ( B(i,j) , i=l1, u1) /)
      A(u1+1:u1+(u1-l1)+1,j) = (/ ( .FALSE. , i=u1+1, u1+(u1-l1)+1 ) /)
    END DO

  END IF

  DEALLOCATE(B)

END SUBROUTINE bool_2Dgrow1 


!---------------------------------------------------------------------
END MODULE dynamic_arrays
