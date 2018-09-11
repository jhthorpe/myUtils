!/////////////////////////////////////////////////////////////////////
!//             Module containing Monte Carlo Integration Subroutines
!//
!//                     James H Thorpe, in Group of John Stanton
!//                     The University of Florida
!//
!/////////////////////////////////////////////////////////////////////

! GENERAL STRUCTURE:
!       mc_algorithm_funcType_WType
!       example: calculate expectations value of a real8 function with real8 
!       distribution function using metropolis monte carlo
!               mc_metrop_real8_real8  
!
!       FuncType is the type of the function to be evaluated
!       WType is the type of the distribution function 
!       The monte carlo output will be of the same type as FuncType

! USER DEFINED  
!       user must define a function that is passed in for the args
!       This is referenced through the interface

MODULE monte_carlo 
  USE dynamic_arrays

  CONTAINS
!---------------------------------------------------------------------
!       mc_metrop_real8
!               James H Thorpe 
!               July 9, 2018
!       - calculates a real8 (64 byte) value of an integral via
!       Metropolis-Hastins Monte Carlo
!       - User defined functions that have integer and real8 input arrays
!---------------------------------------------------------------------
  ! Variables
  ! A           :       1D int4, input array for W
  REAL(KIND=8) FUNCTION mc_metrop_real8_real8()
    IMPLICIT NONE 

    !interface section
    INTERFACE
      REAL(KIND=8) FUNCTION W(A,B)
        INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: A
        REAL(KIND=8), DIMENSION(:), INTENT(IN) :: B
      END FUNCTION W
      REAL(KIND=8) FUNCTION F(A,B)
        INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: A
        REAL(KIND=8), DIMENSION(:), INTENT(IN) :: B
      END FUNCTION F
    END INTERFACE

    

  END FUNCTION mc_metrop_real8


  
END MODULE monte_carlo

