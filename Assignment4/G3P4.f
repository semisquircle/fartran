      PROGRAM G3P4
c ----------------------------------------------
c Assignment 4: The Matrix Calculator Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

c Variables
c Note: 'm' refers to columns and 'n' refers to rows
c (If we kept 'r' and 'c' we couldn't name the third matrix 'C')
      INTEGER, PARAMETER :: max = 10
      REAL, DIMENSION(max, max) :: A, B, C
      INTEGER m1, n1, m2, n2, choice
      LOGICAL :: quit = .FALSE.

c Main program loop, repeats until user quits
      DO WHILE (.NOT. quit)
       CALL menu(choice)
       SELECT CASE (choice)
      CASE (1)
       CALL read_matrix(A, m1, n1, 'A')
       CALL read_matrix(B, m2, n2, 'B')
         IF (m1 .EQ. m2 .AND. n1 .EQ. n2) THEN
          CALL add_matrices(A, B, C, m1, n1)
          CALL print_matrix(A, m1, n1, 'A')
          CALL print_matrix(B, m2, n2, 'B')
          CALL print_matrix(C, m1, n1, 'A + B')
         ELSE
          PRINT *, 'Error: Matrices must have the same dimensions.'
         ENDIF
        CASE (2)
         CALL read_matrix(A, m1, n1, 'A')
         CALL read_matrix(B, m2, n2, 'B')
         IF (m1 .EQ. m2 .AND. n1 .EQ. n2) THEN
          CALL subtract_matrices(A, B, C, m1, n1)
          CALL print_matrix(A, m1, n1, 'A')
          CALL print_matrix(B, m2, n2, 'B')
          CALL print_matrix(C, m1, n1, 'A - B')
         ELSE
          PRINT *, 'Error: Matrices must have the same dimensions.'
         ENDIF
        CASE (3)
         CALL read_matrix(A, m1, n1, 'A')
         CALL read_matrix(B, m2, n2, 'B')
         IF (n1 .EQ. m2) THEN
          CALL multiply_matrices(A, B, C, m1, n1, n2)
          CALL print_matrix(A, m1, n1, 'A')
          CALL print_matrix(B, m2, n2, 'B')
          CALL print_matrix(C, m1, n2, 'A * B')
         ELSE
          PRINT *, 'Error: Inner dimensions must match.'
         ENDIF
        CASE (4)
         CALL read_matrix(A, m1, n1, 'A')
         CALL transpose_matrix(A, C, m1, n1)
         CALL print_matrix(A, m1, n1, 'A')
         CALL print_matrix(C, n1, m1, 'Transpose(A)')
        CASE (5)
         PRINT *, 'Exiting the program...'
         PRINT *
         quit = .TRUE.
        CASE DEFAULT
         PRINT *, 'Invalid choice. Please try again.'
       END SELECT
      END DO
      END PROGRAM

c Prints a pretty operation menu, reads user choice
      SUBROUTINE menu(choice)
       INTEGER choice
       PRINT *
       PRINT *, '--------------------------------------'
       PRINT *, 'MATRIX OPERATIONS'
       PRINT *, '--------------------------------------'
       PRINT *, '1. Matrix Addition'
       PRINT *, '2. Matrix Subtraction'
       PRINT *, '3. Matrix Multiplication (Dot Product)'
       PRINT *, '4. Matrix Transpose'
       PRINT *, '5. Quit'
       PRINT *, 'Enter your choice (1-5):'
       READ *, choice
       RETURN
      END SUBROUTINE

c Reads data into one matrix
c Note: we added an allocatable string for the matrix name
c (This made the outputs significantly more clear)
      SUBROUTINE read_matrix(X, m, n, name)
       REAL, DIMENSION(10, 10) :: X
       CHARACTER*(*) name
       INTEGER i, j, m, n
       PRINT *
       PRINT *, 'Enter number of rows for matrix ', name, ':'
       READ *, m
       PRINT *, 'Enter number of columns for matrix ', name, ':'
       READ *, n
       PRINT *, 'Enter elements of matrix ', name, ' (one at a time):'
       DO i = 1, m
        DO j = 1, n
         READ *, X(i, j)
        END DO
       END DO
       RETURN
      END SUBROUTINE

c Prints one matrix row by row
c Note: we added an allocatable string for the matrix name
c (This made the outputs significantly more clear)
      SUBROUTINE print_matrix(X, m, n, name)
       REAL, DIMENSION(10, 10) :: X
       CHARACTER*(*) name
       INTEGER i, j, m, n
       PRINT *
  100  FORMAT(A, A, ' (', I2, ' x ', I2, '):')
       PRINT 100, 'Matrix ', name, m, n
       DO i = 1, m
        WRITE (*, '(10F10.3)') (X(i, j), j = 1, n)
       END DO
       RETURN
      END SUBROUTINE

c Adds two matrices, stores result in a third matrix
      SUBROUTINE add_matrices(A, B, C, m, n)
       REAL, DIMENSION(10, 10) :: A, B, C
       INTEGER i, j, m, n
       DO i = 1, m
        DO j = 1, n
         C(i, j) = A(i, j) + B(i, j)
        END DO
       END DO
       RETURN
      END SUBROUTINE

c Subtracts two matrices, stores result in a third matrix
      SUBROUTINE subtract_matrices(A, B, C, m, n)
       REAL, DIMENSION(10, 10) :: A, B, C
       INTEGER i, j, m, n
       DO i = 1, m
        DO j = 1, n
         C(i, j) = A(i, j) - B(i, j)
        END DO
       END DO
       RETURN
      END SUBROUTINE

c Multiplies two matrices (dot product), stores result in a third matrix
      SUBROUTINE multiply_matrices(A, B, C, m, n1, n2)
       REAL, DIMENSION(10, 10) :: A, B, C
       INTEGER i, j, k, m, n1, n2
       DO i = 1, m
        DO j = 1, n2
         C(i, j) = 0.0
         DO k = 1, n1
          C(i, j) = C(i, j) + A(i, k) * B(k, j)
         END DO
        END DO
       END DO
       RETURN
      END SUBROUTINE

c Transposes one matrix, stores result in a second matrix
      SUBROUTINE transpose_matrix(A, B, m, n)
       REAL, DIMENSION(10, 10) :: A, B
       INTEGER i, j, m, n
       DO i = 1, m
        DO j = 1, n
         B(j, i) = A(i, j)
        END DO
       END DO
       RETURN
      END SUBROUTINE
