      PROGRAM G3P3
c ----------------------------------------------
c Assignment 3: The Matrix Add Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      LOGICAL file_exists
	  CHARACTER(len=20) file_name_in, file_name_out
      INTEGER flag

      file_exists = .FALSE.
	  flag = 0

      PRINT *, 'Please enter input file name: '
      DO WHILE (.NOT. file_exists)
	   READ (*, *) file_name_in
       INQUIRE(FILE=TRIM(file_name_in), EXIST=file_exists)
	  END DO
	  
      OPEN (UNIT=10, FILE=TRIM(file_name_in), STATUS='old', IOSTAT=flag)
      IF (flag /= 0) THEN
         PRINT *, 'Error opening input file.'
         STOP
      END IF

      PRINT *, 'Please enter input file name: '
      DO WHILE (.NOT. file_exists)
	   READ (*, *) file_name_out
       INQUIRE(FILE=TRIM(file_name_out), EXIST=file_exists)
	  END DO
	  
      OPEN (UNIT=10, FILE=TRIM(file_name_out), STATUS='old', IOSTAT=flag)
      IF (flag /= 0) THEN
         PRINT *, 'Error opening output file.'
         STOP
      END IF

	  PRINT *, file_exists

      END