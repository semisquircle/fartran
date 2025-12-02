      PROGRAM G3P7
c ----------------------------------------------
c Assignment 7: Link List Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
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
      TYPE(Person), POINTER :: pers_to_add, next_start

      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      quit = .FALSE.
      eof = .FALSE.
      eol = .FALSE.
      max_people = 25

c     Prompt for input file (handle quitting + file not found)
      PRINT *
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

      IF (.NOT. quit) THEN
c Read people into linked list (handles EOF)
       NULLIFY(head)
       NULLIFY(tail)
       i = 1
       DO WHILE (i .LE. max_people .AND. .NOT. eof)
        ALLOCATE(pers_to_add)
        READ(10, '(A)', IOSTAT=flag) pers_to_add%name
        IF (flag .EQ. 0) THEN
         READ(10, *, IOSTAT=flag) pers_to_add%count
         IF (flag .EQ. 0) THEN
          IF (pers_to_add%count .EQ. 0) THEN
           DEALLOCATE(pers_to_add)
           eof = .TRUE.
           quit = .TRUE.
           PRINT *, 'Player has invalid count (0). Exiting program...'
          ELSE
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
          END IF
         ELSE
          DEALLOCATE(pers_to_add)
          eof = .TRUE.
          PRINT *, 'One or more players missing counts. Truncated data.'
          PRINT *
         END IF
        ELSE
         DEALLOCATE(pers_to_add)
         eof = .TRUE.
        END IF
       END DO
      END IF

      IF (.NOT. quit) THEN
c Make list circular (handles too few players)
       IF (ASSOCIATED(head) .AND. ASSOCIATED(tail)) THEN
        head%prev => tail
        tail%next => head

c Print full players list (confirmation/testing purposes)
        PRINT *, '-----------------------------------------------------'
        PRINT *, 'PLAYERS'
        PRINT *, '-----------------------------------------------------'
        current => head
  100   FORMAT(' Name: ', A, ', Count: ', I0)
        DO WHILE (.NOT. eol)
         PRINT 100, TRIM(current%name), current%count
         current => current%next
         eol = ASSOCIATED(current, head)
        END DO
        PRINT *

c "Josephus" elimination
        PRINT *, '-----------------------------------------------------'
        PRINT *, 'ELIMINATIONS'
        PRINT *, '-----------------------------------------------------'
        current => head
        steps = current%count
        DO WHILE (.NOT. ASSOCIATED(current%next, current))
c Move forward or backward depending on count (handles zero)
         IF (steps .GT. 0) THEN
          DO i = 1, steps
           current => current%next
          END DO
         ELSE IF (steps .LT. 0) THEN
          DO i = 1, -steps
           current => current%prev
          END DO
         END IF

         steps = current%count

c Select new "first" person (assumes a count of zero moves forward)
         IF (steps .GT. 0) THEN
          next_start => current%next
          steps = steps - 1
         ELSE IF (steps .LT. 0) THEN
          next_start => current%prev
          steps = steps + 1
         END IF

         PRINT *, TRIM(current%name), ' has been eliminated!'
         WRITE(20, *) TRIM(current%name), ' has been eliminated!'

c Restructure list and remove person from memory
         current%prev%next => current%next
         current%next%prev => current%prev
         DEALLOCATE(current)

         current => next_start
        END DO
        PRINT *

c Survivor
        PRINT *, '-----------------------------------------------------'
        PRINT *, 'SURVIVOR: ', TRIM(current%name)
        PRINT *, '-----------------------------------------------------'
        WRITE(20, *) '-------------------------------------------------'
        WRITE(20, *) 'SURVIVOR: ', TRIM(current%name)
        WRITE(20, *) '-------------------------------------------------'
       ELSE
        PRINT *, 'Error: Not enough players. Exiting program...'
       END IF
      END IF
      PRINT *

      CLOSE(10)
      CLOSE(20)

      END PROGRAM
