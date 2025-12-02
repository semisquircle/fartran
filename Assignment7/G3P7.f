      PROGRAM G3P7
c ----------------------------------------------
c Assignment 7: Linked Lists
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      CHARACTER(LEN=100) file_in_name, file_out_name
      CHARACTER(LEN=200) records(25)
      INTEGER i, max_recs, flag
      LOGICAL file_in_exists, file_out_exists, quit

      max_recs = 25

c     Prompt for input file (handle quitting + file not found)
      DO WHILE (.NOT. quit .AND. .NOT. file_in_exists)
       PRINT *, 'Please enter an input file name (or QUIT to exit):'
       READ(*, *) file_in_name
       IF (file_in_name .EQ. 'QUIT') THEN
        quit = .TRUE.
       ELSE
        INQUIRE(FILE=file_in_name, EXIST=file_in_exists)
        IF (file_in_exists) THEN
         PRINT *, 'Input file found! Reading data...'
         OPEN(UNIT=10, FILE=file_in_name, STATUS='OLD', IOSTAT=flag)
         PRINT *
        ELSE
         PRINT *, 'File does not exist...'
        END IF
       END IF
      END DO

c     Prompt for output file (handle all cases)
      IF (.NOT. quit) THEN
       PRINT *, 'Please enter an output file name (or QUIT to exit):'
       READ(*, *) file_out_name
       IF (file_out_name .EQ. 'QUIT') THEN
        quit = .TRUE.
       ELSE
        INQUIRE(FILE=file_out_name, EXIST=file_out_exists)
        IF (file_out_exists) THEN
         PRINT *, 'File already exists. Choose one of the following:'
         PRINT *, '- Enter a new filename to create a new file'
         PRINT *, '- Enter OVERWRITE to overwrite the existing file'
         PRINT *, '- Enter QUIT to exit'
         READ(*, *) out_choice
         SELECT CASE (out_choice)
         CASE ('OVERWRITE')
          PRINT *, 'Overwriting output data...'
          OPEN(UNIT=20, FILE=file_out_name, STATUS='OLD', IOSTAT=flag)
         CASE ('QUIT')
          quit = .TRUE.
         CASE DEFAULT
          PRINT *, 'Creating new file...'
          file_out_name = out_choice
          OPEN(UNIT=20, FILE=file_out_name, STATUS='NEW', IOSTAT=flag)
         END SELECT
        ELSE
         OPEN(UNIT=20, FILE=file_out_name, STATUS='NEW', IOSTAT=flag)
        END IF
        file_out_exists = .TRUE.
        PRINT *
       END IF
      END IF

      TYPE :: Person
       CHARACTER(LEN=100) name
       INTEGER count
       TYPE(Person), POINTER :: prev => NULL()
       TYPE(Person), POINTER :: next => NULL()
      END TYPE

      TYPE(Person), POINTER :: head => NULL()
      TYPE(Person), POINTER :: tail => NULL()
      TYPE(Person), POINTER :: current => NULL()

      INTEGER name_line, count_line
      name_line = 0
      count_line = 0

      i = 0
      DO WHILE (i < max_recs)
       READ(10, '(A)', IOSTAT=flag) records(i + 1)
       IF (flag /= 0) EXIT
        IF (MOD(i, 2) == 0) THEN
         name_line = i + 1
        ELSE
         count_line = i + 1

         TYPE(Person), POINTER :: new_node
         ALLOCATE(new_node)
         new_node%name = TRIM(records(name_line))
         READ(records(count_line), *) new_node%count
         new_node%prev => tail
         new_node%next => NULL()

         IF (TAIL .NE. NULL()) THEN
          TAIL%next => new_node
         ELSE
          HEAD => new_node
         END IF
         TAIL => new_node
        END IF
       i = i + 1
      END DO

      INTEGER :: count_value
      current => head

      DO WHILE (current .NE. NULL())
       count_value = current%count
       PRINT *,'Current Name: ',TRIM(current%name),' Count: ',
     & count_value

       IF (count_value > 0) THEN
        DO i = 1, count_value
         IF (current%next .NE. NULL()) THEN
         current => current%next
         ELSE
          EXIT
         END IF
        END DO
       ELSE IF (count_value < 0) THEN
        DO i = 1, ABS(count_value)
         IF (current%prev .NE. NULL()) THEN
          current => current%prev
         ELSE
          EXIT
         END IF
        END DO
       ELSE
        EXIT
       END IF
      END DO

      current => head
      INTEGER :: elim_count
      elim_count = 1

      DO WHILE (head .NE. NULL())
       count_value = current%count
       PRINT *, 'Elimination Count: ', elim_count
       PRINT *,'Eliminating: ',TRIM(current%name),
     & ' with Count: ',count_value

       WRITE(20, '(A, I0)') TRIM(current%name), count_value

       IF (current%prev .NE. NULL()) THEN
        current%prev%next => current%next
       ELSE
        head => current%next
       END IF

       IF (current%next .NE. NULL()) THEN
        current%next%prev => current%prev
       ELSE
        tail => current%prev
       END IF

       TYPE(Person), POINTER :: temp_node
       temp_node => current

       IF (count_value > 0) THEN
        current => current%next
       ELSE IF (count_value < 0) THEN
        current => current%prev
       ELSE
        current => NULL()
       END IF

       DEALLOCATE(temp_node)
       elim_count = elim_count + 1

       IF (current .EQ. NULL()) THEN
        EXIT
       END IF
      END DO

      IF (head .NE. NULL()) THEN
       PRINT *, 'Survivor: ', TRIM(head%name)
      ELSE
       PRINT *, 'No survivor found.'
      END IF

      CLOSE(10)
      CLOSE(20)

      END
