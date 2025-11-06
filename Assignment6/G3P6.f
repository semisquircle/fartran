      PROGRAM G3P6
c ----------------------------------------------
c Assignment 6: The Complex Equation Solver
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      INTEGER n, i, j
      COMPLEX :: A(10, 10), B(10)
      LOGICAL :: quit = .FALSE., correct = .FALSE., soln_exists = .TRUE.
      CHARACTER*1 response

      PRINT *, '---------------------------------------------'
      PRINT *, 'Gauss-Jordan Solver for Complex Equations'
      PRINT *, '---------------------------------------------'
      
c Keep prompting user for equations until done
      DO WHILE (.NOT. quit)
       PRINT *
       PRINT *, 'Enter the number of equations (maximum of 10):'
       READ(*, *) n
       DO WHILE (n .LT. 1 .OR. n .GT. 10)
        PRINT *, 'Error: n must be between 1 and 10. Please try again:'
        READ(*, *) n
       END DO

c Keep prompting user for data until verified
       DO WHILE (.NOT. correct)
        CALL input_data(A, B, n)
        CALL display_data(A, B, n)
        
        PRINT *
        PRINT *, 'Does the above data look correct? (Y/N)'
        READ (*, *) response
        IF (response .EQ. 'Y' .OR. response .EQ. 'y') THEN
         correct = .TRUE.
        END IF
       END DO

       CALL gauss_jordan(A, B, n, soln_exists)
       CALL display_soln(A, B, n, soln_exists)

       PRINT *
       PRINT *, 'Do you wish to continue solving equations? (Y/N)'
       READ (*, *) response
       IF (response .EQ. 'N' .OR. response .EQ. 'n') THEN
        quit = .TRUE.
       END IF
      END DO
      END PROGRAM

c Receive input from keyboard for coefficients and constants
      SUBROUTINE input_data(A, B, n)
       COMPLEX, INTENT(OUT) :: A(10, 10), B(10)
       INTEGER, INTENT(IN) :: n
       INTEGER i, j
       REAL re, im

       PRINT *
       PRINT *, 'Enter the coefficients and constants as pairs (R, I):'
       DO i = 1, n
        PRINT '(A,I0,A)', 'Equation ', i, ':'
        DO j = 1, n
         PRINT '(A,I0,A,I0,A)', 'A(', i, ', ', j, ') ='
         READ(*, *) re, im
         A(i, j) = CMPLX(re, im)
        END DO
        PRINT '(A,I0,A)', 'B(', i, ') ='
        READ(*, *) re, im
        B(i) = CMPLX(re, im)
       END DO
      END SUBROUTINE

c Verify the data to the user
      SUBROUTINE display_data(A, B, n)
       IMPLICIT NONE
       COMPLEX, INTENT(IN) :: A(10, 10), B(10)
       INTEGER, INTENT(IN) :: n
       INTEGER :: i, j

       PRINT *
       PRINT *, '---------------------------------------------'
       PRINT *, 'Entered Coefficient Matrix and Vector:'
       PRINT *, '---------------------------------------------'

c Fancy precision formatting (from Chapter 11)
  100  FORMAT('(', F6.3, SP, F7.3, 'i)X', SS, I0)
  200  FORMAT(' = (', F6.3, SP, F7.3, 'i)')

       DO i = 1, n
        WRITE(*,'(A,I0,A)', ADVANCE='NO') 'Eq ', i, ': '
        DO j = 1, n
         IF (j .GT. 1) THEN
          WRITE(*, '(A)', ADVANCE='NO') ' + '
         END IF
         WRITE(*, 100, ADVANCE='NO') REAL(A(i, j)), AIMAG(A(i, j)), j
        END DO
        WRITE(*, 200) REAL(B(i)), AIMAG(B(i))
       END DO
      END SUBROUTINE

c Gauss-Jordan elimination
      SUBROUTINE gauss_jordan(A, B, n, soln_exists)
       COMPLEX, INTENT(INOUT) :: A(10, 10), B(10)
       INTEGER, INTENT(IN) :: n
       LOGICAL, INTENT(OUT) :: soln_exists
       INTEGER i, j, pivot
       COMPLEX :: factor, pivval, tempB, rowA(10)
       REAL, PARAMETER :: eps = EPSILON(REAL(A(1, 1)))

       soln_exists = .TRUE.

c GJ implementation based (loosely) on Chapter 11
c In short, rows in the coef matrix can be swapped or factored based
c on a pivot value. If the pivot value is less than a very small
c amount (epsilon), then a solution doesn't exist. 
       DO i = 1, n
        pivot = i
        DO j = i + 1, n
         IF (ABS(A(j, i)) .GT. ABS(A(pivot, i))) pivot = j
        END DO

        IF (ABS(A(pivot, i)) .LT. eps) THEN
         soln_exists = .FALSE.
        ELSE
         IF (pivot .NE. i) THEN
          rowA(1:n) = A(i, 1:n)
          A(i, 1:n) = A(pivot, 1:n)
          A(pivot, 1:n) = rowA(1:n)

          tempB = B(i)
          B(i) = B(pivot)
          B(pivot) = tempB
         END IF

         pivval = A(i, i)
         A(i, 1:n) = A(i, 1:n) / pivval
         B(i) = B(i) / pivval

         DO j = 1, n
          IF (j .NE. i) THEN
           factor = A(j, i)
           A(j, 1:n) = A(j, 1:n) - factor * A(i, 1:n)
           B(j) = B(j) - factor * B(i)
          END IF
         END DO
        END IF
       END DO

c Double check each row for consistency
       IF (soln_exists) THEN
        DO i = 1, n
         IF (ABS(A(i, i) - CMPLX(1.0, 0.0)) .GT. eps) THEN
          soln_exists = .FALSE.
         END IF
        END DO
       END IF
      END SUBROUTINE

c Print the solution (or lack there of)
      SUBROUTINE display_soln(A, B, n, soln_exists)
       COMPLEX, INTENT(INOUT) :: A(10, 10), B(10)
       INTEGER, INTENT(IN) :: n
       LOGICAL, INTENT(OUT) :: soln_exists
       INTEGER i

       PRINT *
       PRINT *, '---------------------------------------------'
       PRINT *, 'Solution:'
       PRINT *, '---------------------------------------------'
       IF (soln_exists) THEN
        DO i = 1, n
  300    FORMAT('X', I0, ' = ', F6.3, SP, F7.3, 'i')
         PRINT 300, i, REAL(B(i)), AIMAG(B(i))
        END DO
       ELSE
        PRINT *, 'No unique solution exists.'
       END IF
      END SUBROUTINE
