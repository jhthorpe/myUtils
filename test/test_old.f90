PROGRAM test
  USE myUtils
  IMPLICIT NONE
  integer(kind=4), dimension(:,:) , allocatable :: B
  logical, dimension(:),allocatable :: A,C 
  integer(kind=4), dimension(:), allocatable :: key
  logical :: val,fnd
  integer :: k,n,m,q,loc


  n = 10
  m = 4
  val = .TRUE.
  call hash_qinit_1Dint4_bool(A,B,C,n,m)
  WRITE(*,*) SIZE(A(:))
  WRITE(*,*) SIZE(B(:,0))
  WRITE(*,*) SIZE(B(0,:))
  WRITE(*,*) SIZE(C(:))

  allocate(key(0:m-1))
  CALL int4_1Dzero(key)
  key(3) = 42
  write(*,*) "key is:", key
  WRITE(*,*) "key is..."
  WRITE(*,*) key
  write(*,*) 
  
  q = 2
  loc = 0
  call hash_qinsert_1Dint4_bool(A,B,C,key,val,loc,n,q,myHash2)
  key(1) = 24
  q = q + 1
  loc = 1
  call hash_qinsert_1Dint4_bool(A,B,C,key,val,loc,n,q,myHash2)
  key(2) = 23
  q = q + 1
  loc = 2
  call hash_qinsert_1Dint4_bool(A,B,C,key,val,loc,n,q,myHash2)


  WRITE(*,*) 
  write(*,*) "B(i,:) is"
  DO k=0,n
    write(*,*) B(k,:)
  end do
  WRITE(*,*)

  WRITE(*,*) "search before rehash"
  key = [0,0,0,42]
  call hash_qsearch_1Dint4_bool(A,B,C,key,val,loc,n,fnd,myHash2)
  key = [0,24,0,42]
  call hash_qsearch_1Dint4_bool(A,B,C,key,val,loc,n,fnd,myHash2)
  key = [0,24,23,42]
  call hash_qsearch_1Dint4_bool(A,B,C,key,val,loc,n,fnd,myHash2)

  call hash_qrehash_1Dint4_bool(A,B,C,n,myHash2)

  WRITE(*,*) "search after rehash"
  key = [0,0,0,42]
  call hash_qsearch_1Dint4_bool(A,B,C,key,val,loc,n,fnd,hash_FNV1a_1Dint4)
  key = [0,24,0,42]
  call hash_qsearch_1Dint4_bool(A,B,C,key,val,loc,n,fnd,myHash2)
  key = [0,24,23,42]
  call hash_qsearch_1Dint4_bool(A,B,C,key,val,loc,n,fnd,myHash2)

  write(*,*) 
  write(*,*) "B(k,:) is"
  DO k=0,n
    write(*,*) B(k,:)
  end do

  contains

  integer(kind=4) function myHash2(A)
    implicit none
    integer(kind=4), dimension(0:), intent(IN) :: A
    
    myHash2 = A(2)+1

  end function myHash2

  
END PROGRAM

