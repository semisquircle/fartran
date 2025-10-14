      PROGRAM MATRIX_OPERATIONS
c ----------------------------------------------
c Assignment 4: The Matrix Calculator Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      INTEGER, PARAMETER :: max = 10
      REAL, DIMENSION(max, max) :: A, B, C
      INTEGER m1, n1, m2, n2, iopt
      INTERGER :: choice
      LOGICAL :: done = .FALSE.

      DO
            DO WHILE (.NOT. done)
            CALL show_menu(iopt)
             
             SELECT CASE (iopt)
                CASE (1)
                  CALL read_matrix(X, m, n, name)
                  CALL add_matrix(A, B, C, m, n)
                  CALL print_matrix(X, m, n, name)
                CASE (2)
                  CALL read_matrix(X, m, n, name)
                 CALL subtract_matrix(A, B, C, m, n)
                 CALL print_matrix(X, m, n, name)
                CASE (3)
                  CALL read_matrix(X, m, n, name)
                 CALL multiply_matrix(A, B, C, m, n, p)
                 CALL print_matrix(X, m, n, name)
                CASE (4)
                  CALL read_matrix(X, m, n, name)
                 CALL transpose_matrix(A, C, m, n)
                 CALL print_matrix(X, m, n, name)
                CASE (5)
                  PRINT *, 'Exiting the program.'
                 EXIT
                CASE DEFAULT
                 PRINT *, 'Invalid choice. Please try again.'
             END SELECT
        END DO
    
        CONTAINS

       
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
