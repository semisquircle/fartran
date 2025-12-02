      PROGRAM G3P7
c ----------------------------------------------
c Assignment 7: Linked Lists
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      CHARACTER*100 file_in_name, file_out_name, out_choice
      CHARACTER*200 records(25)
      INTEGER i, max_recs, flag
      INTEGER count_value, name_line, count_line, elim_count
      LOGICAL file_in_exists, file_out_exists, quit

      max_recs = 25
      name_line = 0
      count_line = 0
      elim_count = 1

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
       CHARACTER*100 name
       INTEGER count
       TYPE(Person), POINTER :: prev
       TYPE(Person), POINTER :: next
      END TYPE

      TYPE(Person), POINTER :: head, tail, current

      i = 0
      DO WHILE (i .LT. max_recs)
       READ(10, '(A)', IOSTAT=flag) records(i + 1)
       IF (flag .NE. 0) EXIT
        IF (MOD(i, 2) == 0) THEN
         name_line = i + 1
        ELSE
         count_line = i + 1

         TYPE(Person), POINTER :: new_node
         ALLOCATE(new_node)
         new_node%name = TRIM(records(name_line))
         READ(records(count_line), *) new_node%count
         new_node%prev => tail
         NULLIFY(new_node%next)

         IF (.NOT. ASSOCIATED(tail)) THEN
          tail%next => new_node
         ELSE
          head => new_node
         END IF
         tail => new_node
        END IF
       i = i + 1
      END DO

      current => head

      DO WHILE (.NOT. ASSOCIATED(current))
       count_value = current%count
       PRINT *, 'Current Name: ', TRIM(current%name), ' Count: ',
     & count_value

       IF (count_value > 0) THEN
        DO i = 1, count_value
         IF (current%next .NE. NULL()) THEN
         current => current%next
         ELSE
          EXIT
         END IF
        END DO
       ELSE IF (count_value .LT. 0) THEN
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

      DO WHILE (.NOT. ASSOCIATED(head))
       count_value = current%count
       PRINT *, 'Elimination Count: ', elim_count
       PRINT *,'Eliminating: ',TRIM(current%name),
     & ' with Count: ', count_value

       WRITE(20, '(A, I0)') TRIM(current%name), count_value

       IF (.NOT. ASSOCIATED(current%prev)) THEN
        current%prev%next => current%next
       ELSE
        head => current%next
       END IF

       IF (.NOT. ASSOCIATED(current%next)) THEN
        current%next%prev => current%prev
       ELSE
        tail => current%prev
       END IF

       TYPE(Person), POINTER :: temp_node
       temp_node => current

       IF (count_value > 0) THEN
        current => current%next
       ELSE IF (count_value .LT. 0) THEN
        current => current%prev
       ELSE
        NULLIFY(current)
       END IF

       DEALLOCATE(temp_node)
       elim_count = elim_count + 1

       IF (.NOT. ASSOCIATED(current)) THEN
        EXIT
       END IF
      END DO

      IF (.NOT. ASSOCIATED(head)) THEN
       PRINT *, 'Survivor: ', TRIM(head%name)
      ELSE
       PRINT *, 'No survivor found.'
      END IF

      CLOSE(10)
      CLOSE(20)

      END PROGRAM
