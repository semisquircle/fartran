      PROGRAM G3P7
c ----------------------------------------------
c Assignment 7: Linked Lists
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      CHARACTER*100 file_in_name, file_out_name, out_choice
      INTEGER i, max_people, flag
      INTEGER count_value, name_line, count_line, elim_count
      LOGICAL file_in_exists, file_out_exists, quit, eof

      TYPE :: Person
       CHARACTER*100 name
       INTEGER count
       TYPE(Person), POINTER :: prev
       TYPE(Person), POINTER :: next
      END TYPE

      TYPE(Person), POINTER :: head, tail, current
      TYPE(Person), POINTER :: new_person, temp_person

      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      quit = .FALSE.
      eof = .FALSE.
      max_people = 25
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

      NULLIFY(head)
      NULLIFY(tail)

c Read people into TYPEs from input file
      i = 1
      DO WHILE (.NOT. quit .AND. i .LE. max_people .AND. .NOT. eof)
       ALLOCATE(new_person)
       READ(10, '(A)', IOSTAT=flag) new_person%name
       IF (flag .EQ. 0) THEN
        READ(10, *, IOSTAT=flag) new_person%count
        IF (flag .EQ. 0) THEN
         NULLIFY(new_person%prev)
         NULLIFY(new_person%next)

         IF (.NOT. ASSOCIATED(head)) THEN
          head => new_person
         ELSE
          IF (ASSOCIATED(tail)) THEN
           tail%next => new_person
         END IF
         END IF
         tail => new_person
         i = i + 1
        ELSE
         DEALLOCATE(new_person)
         eof = .TRUE.
        END IF
       ELSE
        DEALLOCATE(new_person)
        eof = .TRUE.
       END IF
      END DO

      current => head

c Print out each person's name and count to confirm
      PRINT *, '-------------------------------------------------------'
      PRINT *, 'Unlucky souls:'
      DO WHILE (ASSOCIATED(current))
  100  FORMAT('Name: ', A, ', Count: ', I0)
       PRINT 100, TRIM(current%name), current%count
       current => current%next
      END DO
      PRINT *, '-------------------------------------------------------'

      CLOSE(10)
      CLOSE(20)

      END PROGRAM
