INTEGER FUNCTION hmm(x)
  INTEGER, INTENT(INOUT) :: x

  WRITE(*,*) "hmm called with x=",x
  x = x + 1

  hmm = x

END FUNCTION hmm
