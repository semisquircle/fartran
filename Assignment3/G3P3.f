      PROGRAM G3P3
c ----------------------------------------------
c Assignment 3: The Matrix Add Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

c     Declare variables
      INTEGER MAXROWS, MAXCOLS
      PARAMETER (MAXROWS=10, MAXCOLS=10)

      REAL matrix(MAXROWS, MAXCOLS)
      REAL matrix_sum(MAXROWS, MAXCOLS)

      INTEGER flag, num_matrices, rows, cols, i, j, mat_count
      LOGICAL file_in_exists, file_out_exists
      CHARACTER*20 file_name_in, file_name_out, response

c     Initialize
      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      flag = 0

c     Initialize sum matrix to zero
      DO i = 1, MAXROWS
       DO j = 1, MAXCOLS
        matrix_sum(i,j) = 0.0
       END DO
      END DO

c     Ask user for input file
      PRINT *, 'Please enter an input file name (or QUIT to exit):'
      DO WHILE (.NOT. file_in_exists)
      READ(*, *) file_name_in
      IF (file_name_in .EQ. 'QUIT') STOP
      INQUIRE(FILE=file_name_in, EXIST=file_in_exists)
      IF (.NOT. file_in_exists) THEN
       PRINT *, 'File does not exist, please try again.'
      END IF
      END DO
c     Open input file
      OPEN(UNIT=10, FILE=file_name_in, STATUS='OLD', IOSTAT=flag)
      IF (flag .NE. 0) THEN
       PRINT *, 'Error opening input file.'
       STOP
      END IF

c     Ask user for output file
      PRINT *, 'Please enter an output file name (or QUIT to exit):'
      DO WHILE (.NOT. file_out_exists)
      READ(*, *) file_name_out
      IF (file_name_out .EQ. 'QUIT') STOP
      INQUIRE(FILE=file_name_out, EXIST=file_out_exists)
      IF (.NOT. file_out_exists) THEN
       PRINT *, 'File does not exist, try again: '
      END IF
      END DO
      IF (file_out_exists) THEN
       PRINT *, 'File exists. Enter NEW name, OVERWRITE, or QUIT:'
       READ(*, *) response
       IF (response .EQ. 'QUIT') STOP
       IF (response .EQ. 'OVERWRITE') THEN
        OPEN(UNIT=20, FILE=file_name_out, STATUS='OLD', IOSTAT=flag)
       ELSE
        file_name_out = response
       END IF
      ELSE
       OPEN(UNIT=20, FILE=file_name_out, STATUS='NEW', IOSTAT=flag)
      END IF

      IF (flag .NE. 0) THEN
       PRINT *, 'Error opening output file.'
       STOP
      END IF

c     Read number of matrices
      READ(10, *, IOSTAT=flag) num_matrices
      IF (flag .NE. 0) THEN
       PRINT *, 'Error reading number of matrices.'
       STOP
      END IF
      IF (num_matrices .GT. 10) THEN
       PRINT *, 'Number of matrices exceeds maximum of 10.'
       STOP
      END IF

c     Read matrix dimensions
      READ(10, *, IOSTAT=flag) rows, cols
      IF (flag .NE. 0) THEN
       PRINT *, 'Error reading matrix dimensions.'
       STOP
      END IF
      IF (rows .GT. MAXROWS .OR. cols .GT. MAXCOLS) THEN
       PRINT *, 'Matrix dimensions exceed maximum allowed size.'
       STOP
      END IF

c     Loop over all matrices
      DO mat_count = 1, num_matrices
c      Read matrix
       DO i = 1, rows
        READ(10, *, IOSTAT=flag) (matrix(i,j), j=1,cols)
        IF (flag .NE. 0) THEN
         PRINT *, 'Error reading matrix from file.'
         STOP
        END IF
       END DO

c      Add to sum matrix
       DO i = 1, rows
        DO j = 1, cols
         matrix_sum(i,j) = matrix_sum(i,j) + matrix(i,j)
        END DO
       END DO

c      Print current matrix to output file
       WRITE(20, *) 'MATRIX ', mat_count
       DO i = 1, rows
        WRITE(20,'(10F6.1)') (matrix(i,j), j=1,cols)
       END DO
      END DO

c     Print sum matrix
      WRITE(20, *) 'SUM OF ALL ', num_matrices, ' MATRICES'
      DO i = 1, rows
       WRITE(20,'(10F6.1)') (matrix_sum(i,j), j=1,cols)
      END DO

      CLOSE(10)
      CLOSE(20)

      END
