      PROGRAM G3P3
c ----------------------------------------------
c Assignment 3: The Matrix Add Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      LOGICAL file_in_exists, file_out_exists
      CHARACTER(len=20) file_name_in, file_name_out
      INTEGER flag

      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      flag = 0

      PRINT *, 'Please enter an input file name: '
      DO WHILE (.NOT. file_in_exists)
       READ(*, *) file_name_in
       INQUIRE(FILE=TRIM(file_name_in), EXIST=file_in_exists)
       IF (.NOT. file_in_exists) THEN
        PRINT *, 'File does not exist, please try again: '
       END IF
      END DO
      
      OPEN(UNIT=10, FILE=TRIM(file_name_in), STATUS='old', IOSTAT=flag)
      IF (flag .NE. 0) THEN
       PRINT *, 'Error opening input file.'
       STOP
      END IF

      PRINT *, 'Please enter an output file name: '
      READ(*, *) file_name_out
      IF (.NOT. file_out_exists) THEN
       PRINT *, 'Output file not found, creating new file...'
       OPEN(UNIT=10, FILE=file_name_out, STATUS='NEW', ACTION='WRITE')
      END IF
      
c OPEN(UNIT=10, FILE=TRIM(file_name_out), STATUS='old', IOSTAT=flag)
c IF (flag .NE. 0) THEN
c  PRINT *, 'Error opening output file.'
c  STOP
c END IF

      END