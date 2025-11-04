      PROGRAM G3P5
c ----------------------------------------------
c Assignment 6: The Complex Equation Solver
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      INTEGER, PARAMETER :: dp = kind(1.0d0)
      INTEGER, PARAMETER :: nmax = 10
      INTEGER :: n, i, j
      COMPLEX(dp) :: A(nmax, nmax), B(nmax)
      LOGICAL :: ok
      CHARACTER*1 :: response

      PRINT *, "---------------------------------------------"
      PRINT *, " Gauss-Jordan Solver for Complex Systems"
      PRINT *, "---------------------------------------------"
      PRINT *, "Enter the number of equations (n ≤ 10):"
      READ(*,*) n
      IF (n < 1 .or. n > nmax) THEN
       PRINT *, "Error: n must be between 1 and 10."
       stop
      END IF

      CALL input_data(A, B, n)

      ok = .false.
      DO WHILE (.not. ok)
       CALL display_data(A, B, n)
       PRINT *, "Is the above data correct? (Y/N)"
       READ(*,*) response
       IF (response == 'Y' .or. response == 'y') THEN
        ok = .true.
       ELSE
        PRINT *, "Re-enter the data:"
        CALL input_data(A, B, n)
       END IF
      END DO

      CALL gauss_jordan(A, B, n, ok)

      IF (ok) THEN
       PRINT *, "---------------------------------------------"
       PRINT *, " Solution:"
       PRINT *, "---------------------------------------------"
       DO i = 1, n
        PRINT '(A,I2,A,F10.5,A,F10.5,A)', 'x(', i, ') = ', real(B(i)), ' + ', aimag(B(i)), 'i'
       END DO
      ELSE
       PRINT *, "No unique solution exists."
      END IF

      END PROGRAM

      SUBROUTINE input_data(A, B, n)
      implicit none
      INTEGER, intent(in) :: n
      INTEGER :: i, j
      COMPLEX(kind(1.0d0)), intent(out) :: A(:,:), B(:)
      real(kind(1.0d0)) :: re, im

      PRINT *, "Enter the coefficients and constants as pairs (Re, Im):"
      do i = 1, n
       PRINT *, "Equation ", i
       DO j = 1, n
        PRINT '(A,I2,A,I2,A)', "A(", i, ",", j, ") = (Re, Im): "
        READ(*,*) re, im
        A(i,j) = cmplx(re, im, kind(1.0d0))
       END DO
       PRINT '(A,I2,A)', "B(", i, ") = (Re, Im): "
       READ(*,*) re, im
       B(i) = cmplx(re, im, kind(1.0d0))
      END DO
      END SUBROUTINE input_data

      SUBROUTINE display_data(A, B, n)
      INTEGER, intent(in) :: n
      COMPLEX(kind(1.0d0)), intent(in) :: A(:,:), B(:)
      INTEGER :: i, j

      PRINT *, "---------------------------------------------"
      PRINT *, "Entered Coefficient Matrix and Vector:"
      PRINT *, "---------------------------------------------"
      DO i = 1, n
       WRITE(*,'(A,I2,A)', advance='no') 'Eq', i, ': '
       DO j = 1, n
        WRITE(*,'(F8.3,SP,F8.3,"i",1X)', advance='no') real(A(i,j)), aimag(A(i,j))
       END DO
       WRITE(*,'(A,F8.3,SP,F8.3,"i")') " | ", real(B(i)), aimag(B(i))
      END DO
      END SUBROUTINE

      SUBROUTINE gauss_jordan(A, B, n, success)
      implicit none
      INTEGER, intent(in) :: n
      COMPLEX(kind(1.0d0)), intent(inout) :: A(:,:), B(:)
      LOGICAL, intent(out) :: success
      INTEGER :: i, j, k, pivot
      COMPLEX(kind(1.0d0)) :: factor, temp, pivval

      success = .true.

      DO i = 1, n
       pivot = i
       DO k = i+1, n
        IF (abs(A(k,i)) > abs(A(pivot,i))) pivot = k
       END DO

       IF (abs(A(pivot,i)) < 1.0d-12) THEN
        success = .false.
        return
       END IF

       IF (pivot /= i) THEN
        A([i,pivot], :) = A([pivot,i], :)
        temp = B(i)
        B(i) = B(pivot)
        B(pivot) = temp
       END IF

       pivval = A(i,i)
       A(i,:) = A(i,:) / pivval
       B(i) = B(i) / pivval

       DO j = 1, n
        IF (j /= i) THEN
        factor = A(j,i)
        A(j,:) = A(j,:) - factor * A(i,:)
        B(j) = B(j) - factor * B(i)
        END IF
       END DO
      END DO
      END SUBROUTINE

      END PROGRAM
