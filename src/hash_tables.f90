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

! USER DEFINED HASH FUNCTIONS
!       user must define a function that is passed in for the args
!       This is referenced through the interface, and thus must only 
!       depend on the elements/size of the array handed to it
!
!       NOTE: you do not need to mod by n=2^p in your hash function, I
!       have done that for you with fancy bitops

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

  SUBROUTINE hash_qinit_1Dint4_bool(A,B,C,n,m)
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

    ALLOCATE(C(0:n-1),STAT=stat)
    IF ( stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qinit_1Dint4_bool - could not allocate array C"
      STOP
    END IF

  END SUBROUTINE hash_qinit_1Dint4_bool

!---------------------------------------------------------------------
!       hash_qinsert_1Dint4_bool        
!               James H. Thorpe
!               June 18, 2018
!       - insert key value pair for key:1Dint4, value:bool array
!       - quadratic probing
!       - use fancy bitops for modules, as we know we have n=2^p size
!---------------------------------------------------------------------
  ! Varaibles
  ! A           :       1D bool, hash table (T,F array)
  ! B           :       2D int4, hashed keys (index,key)
  ! C           :       1D bool, hashed values
  ! key         :       1D int4, key to insert       
  ! val         :       bool, values to insert
  ! n           :       int4, size of the matrix
  ! q           :       int4, current number of inserted keys 

  SUBROUTINE hash_qinsert_1Dint4_bool(A,B,C,key,val,idx,n,q,hash_1Dint4)
    IMPLICIT NONE

    INTERFACE
      INTEGER(KIND=4) FUNCTION hash_1Dint4(A)
        INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: A
      END FUNCTION hash_1Dint4
    END INTERFACE

    INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: B 
    LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: A,C
    INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: key
    INTEGER(KIND=4), INTENT(INOUT) :: n,idx
    INTEGER(KIND=4), INTENT(INOUT) :: q
    LOGICAL, INTENT(IN) :: val
    INTEGER(KIND=4) :: j,k,i,l

    IF ( 1.0E0*q/n .GT. 0.5E0) CALL hash_qrehash_1Dint4_bool(A,B,C,n,hash_1Dint4)

    !get initial guess index
    idx = hash_1Dint4(key)

    j = 1    
    k = idx
    i = IAND(k,n-1) !bitwise modulus of size n=2^p, fancy stuff :) 
    l = -1

    !find empty slot, quadratic probing 
    DO WHILE (A(i) .EQV. .TRUE.)
      IF ( l .EQ. i) THEN       !the full cycle has been searched, quadratic ftw
        WRITE(*,*) "myUtil:hash_qinsert_1Dint4_bool - no insertion point found"
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
    A(i) = .TRUE.
    B(i,:) = key(:)
    C(i) = val
    idx = i
    q = q + 1

  END SUBROUTINE hash_qinsert_1Dint4_bool

!---------------------------------------------------------------------
!       hash_qinsert_1Dint4_bool_coll        
!               James H. Thorpe
!               June 18, 2018
!       - insert key value pair for key:1Dint4, value:bool array
!       - quadratic probing
!       - use fancy bitops for modules, as we know we have n=2^p size
!       - tracks number of collisions
!---------------------------------------------------------------------
  ! Varaibles
  ! A           :       1D bool, hash table (T,F array)
  ! B           :       2D int4, hashed keys (index,key)
  ! C           :       1D bool, hashed values
  ! key         :       1D int4, key to insert       
  ! val         :       bool, values to insert
  ! n           :       int4, size of the matrix
  ! q           :       int4, current number of inserted keys 
  ! x           :       int4, number of collisions

  SUBROUTINE hash_qinsert_1Dint4_bool_col(A,B,C,key,val,idx,n,q,hash_1Dint4,x)
    IMPLICIT NONE

    INTERFACE
      INTEGER(KIND=4) FUNCTION hash_1Dint4(A)
        INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: A
      END FUNCTION hash_1Dint4
    END INTERFACE

    INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: B 
    LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: A,C
    INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: key
    INTEGER(KIND=4), INTENT(INOUT) :: n,idx,q,x
    LOGICAL, INTENT(IN) :: val
    INTEGER(KIND=4) :: j,k,i,l

    IF ( 1.0E0*q/n .GT. 0.5E0) CALL hash_qrehash_1Dint4_bool(A,B,C,n,hash_1Dint4)

    !get initial guess index
    idx = hash_1Dint4(key)

    j = 1    
    k = idx
    i = IAND(k,n-1) !bitwise modulus of size n=2^p, fancy stuff :) 
    l = -1

    x = 0
    !find empty slot, quadratic probing 
    DO WHILE (A(i) .EQV. .TRUE.)
      IF ( l .EQ. i) THEN       !the full cycle has been searched, quadratic ftw
        WRITE(*,*) "myUtil:hash_qinsert_1Dint4_bool - no insertion point found"
        STOP
      ELSE IF ( ALL(B(i,:) .EQ. key(:) ) ) THEN !key already exists
        RETURN
      END IF
      k = k + j  
      j = j + 1
      x = x + 1
      l = i
      i = IAND(k,n-1)
    END DO 

    !if we got out, then we have any empty slot
    A(i) = .TRUE.
    B(i,:) = key(:)
    C(i) = val
    idx = i
    q = q + 1

  END SUBROUTINE hash_qinsert_1Dint4_bool_col

!---------------------------------------------------------------------
!       hash_qrehash_1Dint4_bool        
!               James H. Thorpe
!               June 18, 2018
!       - rehash the hash table after doubling size
!       - quadratic probing
!       - use fancy bitops for modulus, as we know we have n=2^p size
!       - changes val to value of pair
!---------------------------------------------------------------------
  ! Variables
  ! A           :       1D bool, hash table (T,F array)
  ! B           :       2D int4, hashed keys (index,key)
  ! C           :       1D bool, hashed values
  ! key         :       1D int4, key to insert       
  ! val         :       bool, values to insert
  ! idx         :       int4, result of has function, between 0 and n=2^p-1
  ! n           :       int4, size of the matrix

  SUBROUTINE hash_qrehash_1Dint4_bool(A,B,C,n,hash_1Dint4)
    IMPLICIT NONE

    INTERFACE
      INTEGER(KIND=4) FUNCTION hash_1Dint4(A)
        INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: A
      END FUNCTION hash_1Dint4 
    END INTERFACE

    INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: B 
    LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: A,C
    INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE :: nB
    LOGICAL, DIMENSION(:), ALLOCATABLE:: nA,nC
    INTEGER(KIND=4), INTENT(INOUT) :: n
    INTEGER(KIND=4) :: i,m,stat,l,q

    n = 2*n
    m = SIZE(B(0,:))
    q = 0

    !create temporary arrays
    CALL hash_qinit_1Dint4_bool(nA,nB,nC,n,m)

    !rehash
    DO i=0,(n/2)-1
      IF ( A(i) .EQV. .TRUE.) THEN
        CALL hash_qinsert_1Dint4_bool(nA,nB,nC,B(i,:),C(i),l,n,q,hash_1Dint4)
      END IF 
    END DO

    !deallocate
    DEALLOCATE(A,STAT=stat) 
    IF (stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qrehash_1Dint4_bool - could not deallocate A"
      STOP
    END IF
    DEALLOCATE(B,STAT=stat) 
    IF (stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qrehash_1Dint4_bool - could not deallocate B"
      STOP
    END IF
    DEALLOCATE(C,STAT=stat) 
    IF (stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qrehash_1Dint4_bool - could not deallocate C"
      STOP
    END IF
    
    !reallocate
    ALLOCATE(A(0:n-1),STAT=stat)
    IF (stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qrehash_1Dint4_bool - could not reallocate A"
      STOP
    END IF
    A = nA
    ALLOCATE(B(0:n-1,0:m-1),STAT=stat)
    IF (stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qrehash_1Dint4_bool - could not reallocate B"
      STOP
    END IF
    B = nB
    ALLOCATE(C(0:n-1),STAT=stat)
    IF (stat .NE. 0) THEN
      WRITE(*,*) "myUtil:hash_qrehash_1Dint4_bool - could not reallocate C"
      STOP
    END IF
    C = nC

    DEALLOCATE(nA)
    DEALLOCATE(nB)
    DEALLOCATE(nC)

  END SUBROUTINE hash_qrehash_1Dint4_bool

!---------------------------------------------------------------------
!       hash_qsearch_1Dint4_bool        
!               James H. Thorpe
!               June 18, 2018
!       - find key:values pair for key:1Dint4, value:bool array
!       - quadratic probing
!       - use fancy bitops for modules, as we know we have n=2^p size
!       - changes val to value of pair
!---------------------------------------------------------------------
  ! Varaibles
  ! A           :       1D bool, hash table (T,F array)
  ! B           :       2D int4, hashed keys (index,key)
  ! C           :       1D bool, hashed values
  ! key         :       1D int4, key to insert       
  ! val         :       bool, values to insert
  ! idx         :       int4, result of has function, between 0 and n=2^p-1
  ! n           :       int4, size of the matrix
  ! fnd         :       bool, returns if key was found or not

  SUBROUTINE hash_qsearch_1Dint4_bool(A,B,C,key,val,idx,n,fnd,hash_1Dint4)
    IMPLICIT NONE

    INTERFACE
      INTEGER(KIND=4) FUNCTION hash_1Dint4(A)
        INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: A
      END FUNCTION hash_1Dint4 
    END INTERFACE

    INTEGER(KIND=4), DIMENSION(0:,0:), INTENT(IN) :: B 
    INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: key
    LOGICAL, DIMENSION(0:), INTENT(IN) :: A,C
    INTEGER(KIND=4), INTENT(INOUT) :: idx
    INTEGER(KIND=4), INTENT(IN) :: n
    LOGICAL, INTENT(INOUT) :: val,fnd
    INTEGER(KIND=4) :: j,k,i,l

    idx = hash_1Dint4(key)
    fnd = .FALSE.

    j = 1    
    k = idx
    i = IAND(k,n-1) !bitwise modulus of n=2^p, fancy stuff :) 
    l = -1

    !find empty slot, quadratic probing 
    DO WHILE ( .NOT. ALL(B(i,:) .EQ. key(:) ) )
      IF ( l .EQ. i .OR. A(i) .EQV. .FALSE.) THEN       !the full cycle has been searched, quadratic ftw
        RETURN 
      END IF
      k = k + j  
      j = j + 1
      l = i
      i = IAND(k,n-1)
    END DO 

    !if we got out, then we have any empty slot
    val = C(i)
    idx = i
    fnd = .TRUE.
    
  END SUBROUTINE hash_qsearch_1Dint4_bool

!---------------------------------------------------------------------

END MODULE hash_tables

