      PROGRAM MATRIX_OPERATIONS
c ----------------------------------------------
c Assignment 4: The Matrix Calculator Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      INTEGER, PARAMETER :: max = 10
      REAL, DIMENSION(max, max) :: A, B, C
      INTEGER m1, n1, m2, n2, iopt
      LOGICAL :: done = .FALSE.

      DO WHILE (.NOT. done)
       CALL show_menu(iopt)

       IF (iopt .EQ. 1) THEN
        CALL read_matrix(A, m1, n1, 'A')
        CALL read_matrix(B, m2, n2, 'B')
        IF (m1 .EQ. m2 .AND. n1 .EQ. n2) THEN
         CALL add_matrix(A, B, C, m1, n1)
         CALL print_matrix(A, m1, n1, 'A')
         CALL print_matrix(B, m2, n2, 'B')
         CALL print_matrix(C, m1, n1, 'A + B')
        ELSE
         PRINT *, 'Error: Matrices must have the same dimensions.'
        ENDIF

       ELSE IF (iopt .EQ. 2) THEN
        CALL read_matrix(A, m1, n1, 'A')
        CALL read_matrix(B, m2, n2, 'B')
        IF (m1 .EQ. m2 .AND. n1 .EQ. n2) THEN
         CALL subtract_matrix(A, B, C, m1, n1)
         CALL print_matrix(A, m1, n1, 'A')
         CALL print_matrix(B, m2, n2, 'B')
         CALL print_matrix(C, m1, n1, 'A - B')
        ELSE
         PRINT *, 'Error: Matrices must have the same dimensions.'
        ENDIF

       ELSE IF (iopt .EQ. 3) THEN
        CALL read_matrix(A, m1, n1, 'A')
        CALL read_matrix(B, m2, n2, 'B')
        IF (n1 .EQ. m2) THEN
         CALL multiply_matrix(A, B, C, m1, n1, n2)
         CALL print_matrix(A, m1, n1, 'A')
         CALL print_matrix(B, m2, n2, 'B')
         CALL print_matrix(C, m1, n2, 'A * B')
        ELSE
         PRINT *, 'Error: Inner dimensions must match.'
        ENDIF

       ELSE IF (iopt .EQ. 4) THEN
        CALL read_matrix(A, m1, n1, 'A')
        CALL transpose_matrix(A, C, m1, n1)
        CALL print_matrix(A, m1, n1, 'A')
        CALL print_matrix(C, n1, m1, 'Transpose(A)')

       ELSE IF (iopt .EQ. 5) THEN
        done = .TRUE.

       ELSE
        PRINT *, 'Invalid option. Try again.'
       ENDIF
      END DO
      END PROGRAM

      SUBROUTINE show_menu(iopt)
       INTEGER iopt
       PRINT *
       PRINT *, '--------------------------------------'
       PRINT *, ' MATRIX OPERATIONS MENU'
       PRINT *, '--------------------------------------'
       PRINT *, '1. Matrix Addition'
       PRINT *, '2. Matrix Subtraction'
       PRINT *, '3. Matrix Multiplication (Dot Product)'
       PRINT *, '4. Matrix Transpose'
       PRINT *, '5. Quit'
       PRINT *, 'Enter your choice (1-5):'
       READ *, iopt
       RETURN
      END SUBROUTINE

      SUBROUTINE read_matrix(X, m, n, name)
       REAL, DIMENSION(10, 10) :: X
       CHARACTER*(*) name
       INTEGER i, j, m, n
       PRINT *
       PRINT *, 'Enter number of rows and columns for matrix ', name,':'
       READ *, m, n
       PRINT *, 'Enter elements of matrix ', name, ' (row by row):'
       DO i = 1, m
        DO j = 1, n
         READ *, X(i, j)
        END DO
       END DO
       RETURN
      END SUBROUTINE

      SUBROUTINE print_matrix(X, m, n, name)
       REAL, DIMENSION(10, 10) :: X
       CHARACTER*(*) name
       INTEGER i, j
       PRINT *
       PRINT *, 'Matrix ', name, ':'
       DO i = 1, m
        WRITE (*, '(10F10.3)') (X(i, j), j = 1, n)
       END DO
       RETURN
      END SUBROUTINE

      SUBROUTINE add_matrix(A, B, C, m, n)
       REAL, DIMENSION(10, 10) :: A, B, C
       INTEGER i, j
       DO i = 1, m
        DO j = 1, n
         C(i, j) = A(i, j) + B(i, j)
        END DO
       END DO
       RETURN
      END SUBROUTINE

      SUBROUTINE subtract_matrix(A, B, C, m, n)
       REAL, DIMENSION(10, 10) :: A, B, C
       INTEGER i, j
       DO i = 1, m
        DO j = 1, n
         C(i, j) = A(i, j) - B(i, j)
        END DO
       END DO
       RETURN
      END SUBROUTINE

      SUBROUTINE multiply_matrix(A, B, C, m, n, p)
       REAL, DIMENSION(10, 10) :: A, B, C
       INTEGER i, j, k, m, n, p
       DO i = 1, m
        DO j = 1, p
         C(i, j) = 0.0
         DO k = 1, n
          C(i, j) = C(i, j) + A(i, k) * B(k, j)
         END DO
        END DO
       END DO
       RETURN
      END SUBROUTINE

      SUBROUTINE transpose_matrix(A, C, m, n)
       REAL, DIMENSION(10, 10) :: A, C
       INTEGER i, j
       DO i = 1, m
        DO j = 1, n
         C(j, i) = A(i, j)
        END DO
       END DO
       RETURN
      END SUBROUTINE
