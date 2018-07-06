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

    IF ( 1.0E0*q/n .GT. 0.49E0) CALL hash_qrehash_1Dint4_bool(A,B,C,n,hash_1Dint4)

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

    IF ( 1.0E0*q/n .GT. 0.49E0) CALL hash_qrehash_1Dint4_bool(A,B,C,n,hash_1Dint4)

    !get initial guess index
    !WRITE(99,*) "inserting..."
    idx = hash_1Dint4(key)

    j = 1    
    k = idx
    i = IAND(k,n-1) !bitwise modulus of size n=2^p, fancy stuff :) 
    l = -1
    !WRITE(99,*) "initial ins index is...", i

    !find empty slot, quadratic probing 
    !if (i .EQ. n/2) x = x + 1
    !if (i .EQ. n/2+1) x = x + 1
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
      !TESTING
      x = x + 1
      !WRITE(99,*) i
      !TESTING
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

    !WRITE(99,*) "rehashing"

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


    !WRITE(99,*) "searching..."
    idx = hash_1Dint4(key)
    fnd = .FALSE.

    j = 1    
    k = idx
    i = IAND(k,n-1) !bitwise modulus of n=2^p, fancy stuff :) 
    l = -1

    ! initial search check
    IF ( A(i) .EQV. .FALSE.) RETURN

    !find empty slot, quadratic probing 
    DO WHILE ( .NOT. ALL(B(i,:) .EQ. key(:))  )
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
!       hash_FNV1a_1Dint4
!               James H. Thorpe
!               Jul 2, 2018
!       Simple implementation of FNV-1a hash
!       All credit goes to the original authors, Fowler-Noll-Vo
!       http://www.isthe.com/chongo/tech/comp/fnv/
!       - uses a slightly smaller prime number for 32 byte SIGNED ints
!       -Optimized for GCC -O3 compilers
!---------------------------------------------------------------------
  ! Varaibles
  ! A           :       1D int4, integer array to be hashed

  INTEGER(KIND=4) FUNCTION hash_FNV1a_1Dint4(A)
    IMPLICIT NONE
    INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: A
    INTEGER(KIND=4) :: i,val

    val = 2147483647 !the only difference, I only have 31 bits

    DO i=0,SIZE(A)-1
      ! This is for non-gfortran -O3
      !val = IEOR(val,IBITS(A(i),0,7))*16777619
      !val = IEOR(val,IBITS(A(i),8,7))*16777619
      !val = IEOR(val,IBITS(A(i),16,7))*16777619
      !val = IEOR(val,IBITS(A(i),24,7))*16777619

      ! gfortran -O3 optimization
      val = IEOR(val,IBITS(A(i),0,7))
      val = val+ISHFT(val,1)+ISHFT(val,4)+ISHFT(val,7)+ISHFT(val,8)+ISHFT(val,24)
      val = IEOR(val,IBITS(A(i),8,7))
      val = val+ISHFT(val,1)+ISHFT(val,4)+ISHFT(val,7)+ISHFT(val,8)+ISHFT(val,24)
      val = IEOR(val,IBITS(A(i),16,7))
      val = val+ISHFT(val,1)+ISHFT(val,4)+ISHFT(val,7)+ISHFT(val,8)+ISHFT(val,24)
      val = IEOR(val,IBITS(A(i),24,7))
      val = val+ISHFT(val,1)+ISHFT(val,4)+ISHFT(val,7)+ISHFT(val,8)+ISHFT(val,24)
    END DO

    val = ABS(val) !a sad requirement
    hash_FNV1a_1Dint4 = val
  
  END FUNCTION hash_FNV1a_1Dint4

!---------------------------------------------------------------------
!       hash_xxHash_1Dint4
!               James H. Thorpe
!               Jul 2, 2018
!       My implementation of xxHash for Fortran 90
!       All credit goes to Yann Collet, https://github.com/Cyan4973/xxHash
!	And to Stephan Brumme, who's code I translated, http://create.stephan-brumme.com/xxhash/
!       - I use slightly smaller primes that have not been tested
!---------------------------------------------------------------------
  ! A		:	1D int4, array of values to be hashed

  INTEGER(KIND=4) FUNCTION hash_xxHash_1Dint4(A)
    IMPLICIT NONE
    INTEGER(KIND=4), DIMENSION(0:), INTENT(IN) :: A
    INTEGER(KIND=4), DIMENSION(0:3) :: state
    INTEGER(KIND=4), PARAMETER :: p1,p2,p3,p4,p5
    INTEGER(KIND=4) :: nbytes,sizeBuff,maxBuff,s0,s1,s2,s3 
    INTEGER(KIND=4) :: val
    INTEGER(KIND=4) :: i

    !set the primes, these need testing and changing
    p1 = 2147475521 !this one is meh
    p2 = 2147470769 !maybe okay
    p3 = 2147483647 !this one is meh 
    p4 = 668265263  !fine
    p5 = 374761393  !fine

    !initialize the state with seed 137 (why not?)
    state(0) =  137 + p1 + p2
    state(1) = 137 + p2
    state(2) = 137
    state(3) = p1 - 137    ! this will be wrong if 
    maxBuff = 16
    sizeBuff = 0
    nbytes = SIZE(A)*4  !defined by array 
    
    !Add in data
    DO i=0,nbytes-1
       
    END DO 

    !Process the information streams
    val = nbytes 
    IF (nbytes .GE. maxBuff) THEN
      !"fold 128 bit state into 32 bit value"
          ! "rotate left"
          ! (x << bits | x >> (32 - bits))
          ! supposedly this happens in 1 CPU instruction?
      val = val &
        + IOR(ISHFT(state(0),1),ISHFT(state(0),-(32-1))) &
        + IOR(ISHFT(state(1),7),ISHFT(state(1),-(32-7))) & 
        + IOR(ISHFT(state(2),12),ISHFT(state(2),-(32-12))) & 
        + IOR(ISHFT(state(3),18),ISHFT(state(3),-(32-18)))  
    ELSE
      ! "internal state wasn't set in the addition section",
      !  so therefore the seed is in state(2)
      ! I don't really understand this
      val = val + state[2] + p5
    END IF
    

    WRITE(*,*) "---------------"
    WRITE(*,*) "xxHash_1Dint4"
    WRITE(*,*) "Things to check"
    WRITE(*,*) "1) additions of primes 1&2 are within range"
    WRITE(*,*) "2) rotate left uses IOR and not IEOR"
    WRITE(*,*) "3) rotate left using left and right shift correctly"
    WRITE(*,*) "4) state 3 is defined correctly"
    WRITE(*,*) 
    WRITE(*,*) "Things to do"
    WRITE(*,*) "Add data section"

    STOP "must adress above problems"

  END FUNCTION hash_xxHash_1Dint4
!---------------------------------------------------------------------
END MODULE hash_tables

