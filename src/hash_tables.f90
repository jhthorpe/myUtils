!/////////////////////////////////////////////////////////////////////
!//             Module containing hash table subroutines 
!//
!//                     James H Thorpe, in Group of John Stanton
!//                     The University of Florida
!//
!/////////////////////////////////////////////////////////////////////

! GENERAL STRUCTURE:
!       hash_operation_keytype_valtype
!       example: insert a real8 value for an int4 key, quadratic probing
!               hash_qinsert_int4_real8

MODULE hash_tables
  USE dynamic_arrays

  CONTAINS
!---------------------------------------------------------------------
!       hash_qinit_1Dint4_bool        
!               James H. Thorpe
!               June 18, 2018
!       - initialize hash tables for key:1Dint4, value: bool array 
!       - builds as m=2^p, m > n, the users minimum size request 
!       - this allows it to be compatible with my quadratic hashing
!       - on input, n is the minimum size of the array
!       - on intput, m is the size of the integer key array
!       - Note: upon testing, it seems that storing in this style:
!               B(index, key array)
!               iterate: index, check key=test 
!         offers the best performance with gfortran and intel
!---------------------------------------------------------------------
  ! Varaibles
  ! A           :       1D bool, hash table (T,F array)
  ! B           :       2D int4, hashed keys (index, key), size: n,m
  ! C           :       1D bool, hashed values
  ! n           :       int4, minimum size of array
  ! m           :       int4, size of integer key array

  SUBROUTINE hash_qinit_2Dint4_bool(A,B,C,n,m)
    IMPLICIT NONE
    INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: B 
    LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: A,C
    INTEGER(KIND=4), INTENT(INOUT) :: n,m
    INTEGER(KIND=4) :: i,stat

    !get next largest power of two via bitops - no lookup required!
    !"https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2"
    n = n - 1 
    DO i=0,BIT_SIZE(n)
      n = IOR(n, ISHFT(n, -(2**i))) 
    END DO 
    n = n + 1
   
    !initialize A
    ALLOCATE(A(0:n-1),STAT=stat)
    IF ( stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qinit_1Dint4_bool - could not allocate array A"
      STOP
    ELSE
      A = .FALSE.
    END IF

    ALLOCATE(B(0:n-1,0:m-1),STAT=stat)
    IF ( stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qinit_1Dint4_bool - could not allocate array B"
      STOP
    END IF

    ALLOCATE(C(0:m-1),STAT=stat)
    IF ( stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qinit_1Dint4_bool - could not allocate array C"
      STOP
    END IF

  END SUBROUTINE hash_qinit_2Dint4_bool

!---------------------------------------------------------------------
!       hash_qinsert_1Dint4_bool        
!               James H. Thorpe
!               June 18, 2018
!       - insert key value pair for key:1Dint4, value:bool array
!       - quadratic probing
!       - use fancy bitops for modules, as we know we have 2^n size
!---------------------------------------------------------------------
  ! Varaibles
  ! A           :       1D bool, hash table (T,F array)
  ! B           :       2D int4, hashed keys (index,key)
  ! C           :       1D bool, hashed values
  ! key         :       1D int4, key to insert       
  ! val         :       bool, values to insert
  ! idx         :       int4, result of has function, between 0 and 2^n-1
  ! n           :       int4, size of the matrix

  SUBROUTINE hash_qinsert_2Dint4_bool(A,B,C,key,val,idx,n)
    IMPLICIT NONE
    INTEGER(KIND=4), DIMENSION(0:,0:), INTENT(INOUT) :: B 
    INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: key
    LOGICAL, DIMENSION(0:), INTENT(INOUT) :: A,C
    INTEGER(KIND=4), INTENT(IN) :: idx,n
    LOGICAL, INTENT(IN) :: val
    INTEGER(KIND=4) :: j,k,i,l

    j = 1    
    k = idx
    i = IAND(k,n-1) !bitwise modulus of 2^n, fancy stuff :) 
    l = -1

    !find empty slot, quadratic probing 
    DO WHILE (A(i) .EQV. .TRUE.)
      IF ( l .EQ. i) THEN       !the full cycle has been searched, quadratic ftw
        WRITE(*,*) "myUtil:hash_qinsert_2Dint4_bool - no insertion point found"
        STOP
      ELSE IF ( ALL(B(i,:) .EQ. key(:) ) ) THEN !key already exists
        RETURN
      END IF
      k = k + j  
      j = j + 1
      l = i
      i = IAND(k,n-1)
    END DO 

    !if we got out, then we have any empty slot
    WRITE(*,*) 
    WRITE(*,*) LBOUND(B,1), UBOUND(B,1)
    WRITE(*,*) LBOUND(B,2), UBOUND(B,2)
    A(i) = .TRUE.
    B(i,:) = key(:)
    C(i) = val

    WRITE(*,*) "Found at i=",i
      
    
  END SUBROUTINE hash_qinsert_2Dint4_bool

!---------------------------------------------------------------------


END MODULE hash_tables

