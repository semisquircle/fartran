      PROGRAM G3P3
c ----------------------------------------------
c Assignment 3: The Matrix Add Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE
c Variable Declarations
      LOGICAL file_in_exists, file_out_exists
      CHARACTER(len=20) file_name_in, file_name_out
      INTEGER flag

      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      flag = 0

c User Input for File Names
      PRINT *, 'Please enter an input file name: '
      DO WHILE (.NOT. file_in_exists)
       READ(*, *) file_name_in
       INQUIRE(FILE=TRIM(file_name_in), EXIST=file_in_exists)
      END IF
       IF (.NOT. file_in_exists) THEN
       PRINT *, 'File does not exist, please try again: '
       END IF
      END DO

c Open input file    
      OPEN(UNIT=10, FILE=TRIM(file_name_in), STATUS='old', IOSTAT=flag)
      IF (flag .NE. 0) THEN
       PRINT *, 'Error opening input file.'
       STOP
      END IF

c Get output file name
      PRINT *, 'Please enter an output file name: '
      READ(*, *) file_name_out
      IF (.NOT. file_out_exists) THEN
       PRINT *, 'Output file not found, creating new file...'
       OPEN(UNIT=10, FILE=file_name_out, STATUS='NEW', ACTION='WRITE')
      END IF

c Open output file      
      OPEN(UNIT=10, FILE=TRIM(file_name_out), STATUS='old', IOSTAT=flag)
      IF (flag .NE. 0) THEN
       PRINT *, 'Error opening output file.'
       STOP
      END IF

c Read how many matrices there are from the input file
      INTEGER :: num_matrices
      READ(10, *, IOSTAT=flag) num_matrices
      IF (flag .NE. 0) THEN
       PRINT *, 'Error reading number of matrices from input file.'
       STOP
      END IF  

c Read # of rows and columns in each matrix
      INTEGER :: rows, cols
      READ(10, *, IOSTAT=flag) rows, cols
      IF (flag .NE. 0) THEN
       PRINT *, 'Error reading matrix dimensions from input file.'
       STOP
      END IF

c Read following set of rows of data as the first matrix completely into the program
      INTEGER, DIMENSION(rows, cols) :: matrix_a, matrix_b, matrix_sum
      INTEGER :: i, j, mat_count
      mat_count = 0
      DO mat_count = 1, num_matrices
       READ(10, *, IOSTAT=flag) ((matrix_a(i, j), j = 1, cols), i = 1, rows)
       IF (flag .NE. 0) THEN
        PRINT *, 'Error reading matrix from input file.'
        STOP
       END IF
       READ(10, *, IOSTAT=flag) ((matrix_b(i, j), j = 1, cols), i = 1, rows)
       IF (flag .NE. 0) THEN
        PRINT *, 'Error reading matrix from input file.'
        STOP
       END IF

c Add the matrix to the sum matrix
       matrix_sum = 0
       DO i = 1, rows
        DO j = 1, cols
         matrix_sum(i, j) = matrix_a(i, j) + matrix_b(i, j)
        END DO
       END DO

c Write the resulting sum matrix to the output file
       WRITE(20, *, IOSTAT=flag) ((matrix_sum(i, j), j = 1, cols), i = 1, rows)
       IF (flag .NE. 0) THEN
        PRINT *, 'Error writing matrix to output file.'
        STOP
       END IF       
      END DO

      END