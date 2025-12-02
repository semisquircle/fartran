      PROGRAM G3P7
      IMPLICIT NONE

      CHARACTER*100 file_in_name, file_out_name, out_choice
      INTEGER i, max_people, flag, steps
      LOGICAL file_in_exists, file_out_exists, quit, eof, eol

      TYPE :: Person
       CHARACTER*100 name
       INTEGER count
       TYPE(Person), POINTER :: prev
       TYPE(Person), POINTER :: next
      END TYPE

      TYPE(Person), POINTER :: head, tail, current
      TYPE(Person), POINTER :: pers_to_add, pers_to_remove, next_start

      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      quit = .FALSE.
      eof = .FALSE.
      eol = .FALSE.
      max_people = 25

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

c Read people into linked list
      NULLIFY(head)
      NULLIFY(tail)
      i = 1
      DO WHILE (i .LE. max_people .AND. .NOT. eof)
       ALLOCATE(pers_to_add)
       READ(10, '(A)', IOSTAT=flag) pers_to_add%name
       IF (flag .EQ. 0) THEN
        READ(10, *, IOSTAT=flag) pers_to_add%count
        IF (flag .EQ. 0) THEN
         NULLIFY(pers_to_add%prev)
         NULLIFY(pers_to_add%next)

         IF (.NOT. ASSOCIATED(head)) THEN
          head => pers_to_add
          tail => pers_to_add
         ELSE
          tail%next => pers_to_add
          pers_to_add%prev => tail
          tail => pers_to_add
         END IF
         i = i + 1
        ELSE
         DEALLOCATE(pers_to_add)
         eof = .TRUE.
        END IF
       ELSE
        DEALLOCATE(pers_to_add)
        eof = .TRUE.
       END IF
      END DO

c Make list circular
      IF (ASSOCIATED(head) .AND. ASSOCIATED(tail)) THEN
       head%prev => tail
       tail%next => head
      END IF

      CLOSE(10)

c Print full players list (confirmation/testing purposes)
      PRINT *, '-------------------------------------------------------'
      PRINT *, 'PLAYERS'
      PRINT *, '-------------------------------------------------------'
      current => head
  100 FORMAT('Name: ', A, ', Count: ', I0)
      DO WHILE (.NOT. eol)
       PRINT 100, TRIM(current%name), current%count
       current => current%next
       eol = ASSOCIATED(current, head)
      END DO
      PRINT *

c Josephus elimination
      PRINT *, '-------------------------------------------------------'
      PRINT *, 'ELIMINATIONS'
      PRINT *, '-------------------------------------------------------'
      current => head
      DO WHILE (.NOT. ASSOCIATED(current%next, current))
       steps = current%count

c Identify the person to remove
       pers_to_remove => current
       IF (steps .GT. 0) THEN
        DO i = 1, steps
         pers_to_remove => pers_to_remove%next
        END DO
       ELSE IF (steps .LT. 0) THEN
        DO i = 1, -steps
         pers_to_remove => pers_to_remove%prev
        END DO
       END IF

c Determine the next starting person
       next_start => pers_to_remove%next

c Print eliminated person
       PRINT *, TRIM(pers_to_remove%name), ' has been eliminated!'
       WRITE(20, *) TRIM(pers_to_remove%name), ' has been eliminated!'

c Update pointers to remove the person
       IF (ASSOCIATED(pers_to_remove%prev)) THEN
        pers_to_remove%prev%next => pers_to_remove%next
       END IF
       IF (ASSOCIATED(pers_to_remove%next)) THEN
        pers_to_remove%next%prev => pers_to_remove%prev
       END IF

c Handle edge case: only two people left
       IF (ASSOCIATED(pers_to_remove%next, pers_to_remove)) THEN
        current => pers_to_remove%prev
        pers_to_remove%prev%next => pers_to_remove%prev
        pers_to_remove%prev%prev => pers_to_remove%prev
        NULLIFY(pers_to_remove%next)
        NULLIFY(pers_to_remove%prev)
        DEALLOCATE(pers_to_remove)
        EXIT
       END IF

c Nullify and deallocate the removed person
       NULLIFY(pers_to_remove%next)
       NULLIFY(pers_to_remove%prev)
       DEALLOCATE(pers_to_remove)

c Move to the next starting person
       current => next_start
      END DO
      PRINT *

c Survivor
      PRINT *, 'Survivor: ', TRIM(current%name)
      WRITE(20, *) 'Survivor: ', TRIM(current%name)

      CLOSE(20)

      END PROGRAM
