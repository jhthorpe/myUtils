PROGRAM test
  USE myUtils
  IMPLICIT NONE
  integer(kind=4), dimension(:,:) , allocatable :: B
  logical, dimension(:),allocatable :: A,C 
  integer(kind=4), dimension(:), allocatable :: key
  logical :: val
  integer :: k,n,m


  n = 34
  m = 27
  val = .TRUE.
  call hash_qinit_2Dint4_bool(A,B,C,n,m)
  WRITE(*,*) SIZE(A(:))
  WRITE(*,*) SIZE(B(:,0))
  WRITE(*,*) SIZE(B(0,:))
  WRITE(*,*) SIZE(C(:))

  allocate(key(0:m-1))
  key(10) = 42
  WRITE(*,*) "key is..."
  WRITE(*,*) key

  call hash_qinsert_2Dint4_bool(A,B(:,:),C,key,val,0,n)
  key(11) = 24
  call hash_qinsert_2Dint4_bool(A,B(:,:),C,key,val,0,n)
  key(11) = 23
  call hash_qinsert_2Dint4_bool(A,B(:,:),C,key,val,0,n)

  write(*,*) B(0,:)

  key(12) = 44
  call hash_qsearch_2Dint4_bool(B,C,key,val,10,n)
  write(*,*) "key at at: ",val

  
END PROGRAM
