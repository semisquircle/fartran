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
      
      DO WHILE (.NOT. quit)
       PRINT *
       PRINT *, 'Enter the number of equations (maximum of 10):'
       READ(*, *) n
       DO WHILE (n < 1 .OR. n > 10)
        PRINT *, 'Error: n must be between 1 and 10. Please try again:'
        READ(*, *) n
       END DO

       DO WHILE (.NOT. correct)
        CALL input_data(A, B, n)
        CALL gauss_jordan(A, B, n, soln_exists)
        CALL display_data(A, B, n)
        
        PRINT *
        PRINT *, 'Does the above data look correct? (Y/N)'
        READ (*, *) response
        IF (response .EQ. 'Y' .OR. response .EQ. 'y') THEN
         correct = .TRUE.
        END IF
       END DO

       PRINT *
       IF (soln_exists) THEN
        PRINT *, '---------------------------------------------'
        PRINT *, 'Solution:'
        PRINT *, '---------------------------------------------'
        DO i = 1, n
  100    FORMAT(A, I2, A, F10.5, A, F10.5, A)
         PRINT 100, 'x(', i, ') = ', REAL(B(i)), ' + ', AIMAG(B(i)), 'i'
        END DO
       ELSE
        PRINT *, 'No unique solution exists.'
       END IF

       PRINT *
       PRINT *, 'Do you wish to continue solving equations? (Y/N)'
       READ (*, *) response
       IF (response .EQ. 'N' .OR. response .EQ. 'n') THEN
        quit = .TRUE.
       END IF
      END DO
      END PROGRAM

      SUBROUTINE input_data(A, B, n)
       COMPLEX, INTENT(OUT) :: A(10, 10), B(10)
       INTEGER, INTENT(IN) :: n
       INTEGER i, j
       REAL re, im

       PRINT *
       PRINT *, 'Enter the coefficients and constants as pairs (R, I):'
       DO i = 1, n
        PRINT '(A,I2,A)', 'Equation ', i, ':'
        DO j = 1, n
         PRINT '(A,I2,A,I2,A)', 'A(', i, ',', j, ') ='
         READ(*, *) re, im
         A(i, j) = CMPLX(re, im)
        END DO
        PRINT '(A,I2,A)', 'B(', i, ') ='
        READ(*, *) re, im
        B(i) = CMPLX(re, im)
       END DO
      END SUBROUTINE

      SUBROUTINE display_data(A, B, n)
       IMPLICIT NONE
       COMPLEX, INTENT(IN) :: A(10, 10), B(10)
       INTEGER, INTENT(IN) :: n
       INTEGER :: i, j

       PRINT *
       PRINT *, '---------------------------------------------'
       PRINT *, 'Entered Coefficient Matrix and Vector:'
       PRINT *, '---------------------------------------------'

  200  FORMAT('(', F6.3, F7.3, 'i)X', I2)
  300  FORMAT(' = (', F6.3, F7.3, 'i)')

       DO i = 1, n
        WRITE(*,'(A,I2,A)', ADVANCE='NO') 'Eq ', i, ': '
        DO j = 1, n
         IF (j .GT. 1) THEN
          WRITE(*, '(A)', ADVANCE='NO') ' + '
         END IF
         WRITE(*, 200, ADVANCE='NO') REAL(A(i, j)), AIMAG(A(i, j)), j
        END DO
        WRITE(*, 300) REAL(B(i)), AIMAG(B(i))
       END DO
      END SUBROUTINE

      SUBROUTINE gauss_jordan(A, B, n, soln_exists)
       COMPLEX, INTENT(INOUT) :: A(10, 10), B(10)
       INTEGER, INTENT(IN) :: n
       LOGICAL, INTENT(OUT) :: soln_exists
       INTEGER i, j, k, pivot
       COMPLEX factor, temp, pivval

       soln_exists = .TRUE.

       DO i = 1, n
        pivot = i
        DO k = i + 1, n
         IF (abs(A(k, i)) .GT. abs(A(pivot, i))) THEN
          pivot = k
         END IF
        END DO

        IF (abs(A(pivot, i)) .LT. 0) THEN
         soln_exists = .FALSE.
        ELSE
         IF (pivot .NE. i) THEN
          A([i, pivot], :) = A([pivot, i], :)
          temp = B(i)
          B(i) = B(pivot)
          B(pivot) = temp
         END IF

         pivval = A(i, i)
         A(i, :) = A(i, :) / pivval
         B(i) = B(i) / pivval

         DO j = 1, n
          IF (j .NE. i) THEN
          factor = A(j, i)
          A(j, :) = A(j, :) - factor * A(i, :)
          B(j) = B(j) - factor * B(i)
          END IF
         END DO
        END IF
       END DO
      END SUBROUTINE
