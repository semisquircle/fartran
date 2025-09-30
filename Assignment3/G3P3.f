      PROGRAM G3P3
c ----------------------------------------------
c Assignment 3: The Matrix Add Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      LOGICAL file_exists
	  CHARACTER(len=20) file_name
      INTEGER flag

      file_exists = .FALSE.
	  flag = 0

      PRINT *, 'Please enter input file name: '
      DO WHILE (.NOT. file_exists)
  100  FORMAT(A)
	   READ(*, 100), file_name
       INQUIRE(FILE=TRIM(file_name), EXIST=file_exists)
	  END DO
	  
	  PRINT *, file_exists

      END