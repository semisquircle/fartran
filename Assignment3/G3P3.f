      PROGRAM G3P3
c ----------------------------------------------
c Assignment 3: The Matrix Add Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

c     Declare variables
      INTEGER max_rows, max_cols
      PARAMETER (max_rows=10, max_cols=10)

      REAL, DIMENSION(max_rows, max_cols) :: matrix
      REAL, DIMENSION(max_rows, max_cols) :: matrix_sum

      INTEGER flag, num_matrices, rows, cols, i, j, mat_count
      LOGICAL file_in_exists, file_out_exists, quit
      CHARACTER*20 file_name_in, file_name_out, out_choice

c     Initialize file variables
      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      flag = 0
      quit = .FALSE.

c     Initialize sum matrix to zero
      DO i = 1, max_rows
       DO j = 1, max_cols
        matrix_sum(i,j) = 0.0
       END DO
      END DO

c     Prompt for input file (handle quitting + file not found)
      DO WHILE (.NOT. quit .AND. .NOT. file_in_exists)
       PRINT *, 'Please enter an input file name (or QUIT to exit):'
       READ(*, *) file_name_in
       IF (file_name_in .EQ. 'QUIT') THEN
        quit = .TRUE.
       ELSE
        INQUIRE(FILE=file_name_in, EXIST=file_in_exists)
        IF (file_in_exists) THEN
         PRINT *, 'Input file found! Reading data...'
         OPEN(UNIT=10, FILE=file_name_in, STATUS='OLD', IOSTAT=flag)
         PRINT *
        ELSE
         PRINT *, 'File does not exist...'
        END IF
       END IF
      END DO

c     Prompt for output file (handle all cases)
      IF (.NOT. quit) THEN
       PRINT *, 'Please enter an output file name (or QUIT to exit):'
       READ(*, *) file_name_out
       IF (file_name_out .EQ. 'QUIT') THEN
        quit = .TRUE.
       ELSE
        INQUIRE(FILE=file_name_out, EXIST=file_out_exists)
        IF (file_out_exists) THEN
         PRINT *, 'File already exists. Choose one of the following:'
         PRINT *, '- Enter a new filename to create a new file'
         PRINT *, '- Enter OVERWRITE to overwrite the existing file'
         PRINT *, '- Enter QUIT to exit'
         READ(*, *) out_choice
         SELECT CASE (out_choice)
         CASE ('OVERWRITE')
          PRINT *, 'Overwriting output data...'
          OPEN(UNIT=20, FILE=out_choice, STATUS='OLD', IOSTAT=flag)
         CASE ('QUIT')
          quit = .TRUE.
         CASE DEFAULT
          PRINT *, 'Creating new file...'
          file_name_out = out_choice
          OPEN(UNIT=20, FILE=file_name_out, STATUS='NEW', IOSTAT=flag)
         END SELECT
        ELSE
         OPEN(UNIT=20, FILE=file_name_out, STATUS='NEW', IOSTAT=flag)
        END IF
        file_out_exists = .TRUE.
        PRINT *
       END IF
      END IF

c     Read number of matrices
      IF (.NOT. quit) THEN
       READ(10, *, IOSTAT=flag) num_matrices
       IF (num_matrices .GT. 10) THEN
        PRINT *, 'Number of matrices exceeds maximum of 10.'
        quit = .TRUE.
       END IF
      END IF

c     Read matrix dimensions
      IF (.NOT. quit) THEN
       READ(10, *, IOSTAT=flag) rows, cols
       IF (rows .GT. max_rows .OR. cols .GT. max_cols) THEN
        PRINT *, 'Matrix dimensions exceed maximum allowed size.'
        quit = .TRUE.
       END IF
      END IF

c     Loop over all matrices
      IF (.NOT. quit) THEN
       DO mat_count = 1, num_matrices
c       Read matrix
        DO i = 1, rows
         READ(10, *, IOSTAT=flag) (matrix(i, j), j = 1, cols)
        END DO

c       Add to sum matrix
        DO i = 1, rows
         DO j = 1, cols
          matrix_sum(i, j) = matrix_sum(i, j) + matrix(i, j)
         END DO
        END DO

c       Write current matrix to output file
        WRITE(20, *) 'MATRIX ', mat_count
        DO i = 1, rows
         WRITE(20, '(10F6.1)') (matrix(i, j), j=1,cols)
        END DO
       END DO

c      Print sum matrix
       WRITE(20, *) 'SUM OF ALL ', num_matrices, ' MATRICES'
       DO i = 1, rows
        WRITE(20, '(10F6.1)') (matrix_sum(i, j), j = 1, cols)
       END DO

       PRINT *, '----------------------------------------------------'
       PRINT *, 'Sum calculated! See ', TRIM(file_name_out),
     1' for results.'
       PRINT *, '----------------------------------------------------'
      END IF

      CLOSE(10)
      CLOSE(20)
      PRINT *

      END
