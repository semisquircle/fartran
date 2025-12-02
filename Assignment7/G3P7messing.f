      PROGRAM G3P7
c ----------------------------------------------
c Assignment 7: Linked Lists
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      CHARACTER*100 file_in_name, file_out_name, out_choice
      CHARACTER*200 records(25)
      INTEGER i, max_recs, flag
      INTEGER count_value, elim_count

      LOGICAL file_in_exists, file_out_exists, quit

c ===== DERIVED TYPE =====
      TYPE :: Person
         CHARACTER*100 name
         INTEGER count
         TYPE(Person), POINTER :: prev
         TYPE(Person), POINTER :: next
      END TYPE Person

c ===== POINTERS =====
      TYPE(Person), POINTER :: head => NULL(), tail => NULL()
      TYPE(Person), POINTER :: current => NULL()
      TYPE(Person), POINTER :: new_person => NULL()
      TYPE(Person), POINTER :: temp_person => NULL()

c ===== INITIAL VALUES =====
      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      quit = .FALSE.
      max_recs = 25
      elim_count = 1
      flag = 0

c -----------------------------------------------------
c OPEN INPUT FILE
c -----------------------------------------------------
      DO WHILE (.NOT. quit .AND. .NOT. file_in_exists)
         PRINT *, 'Enter input filename (or QUIT): '
         READ(*, '(A)') file_in_name

         IF (file_in_name .EQ. 'QUIT') THEN
            quit = .TRUE.
         ELSE
            INQUIRE(FILE=file_in_name, EXIST=file_in_exists)
            IF (file_in_exists) THEN
               OPEN(10, FILE=file_in_name, STATUS='OLD')
            ELSE
               PRINT *, 'File not found.'
            END IF
         END IF
      END DO
      IF (quit) STOP

c -----------------------------------------------------
c OPEN OUTPUT FILE
c -----------------------------------------------------
      PRINT *, 'Enter output filename (or QUIT): '
      READ(*, '(A)') file_out_name
      IF (file_out_name .EQ. 'QUIT') STOP

      OPEN(20, FILE=file_out_name, STATUS='REPLACE')

c -----------------------------------------------------
c READ NAME / COUNT PAIRS INTO LINKED LIST
c -----------------------------------------------------
      DO i = 1, max_recs
         READ(10, '(A)', IOSTAT=flag) records(1)
         IF (flag .NE. 0) EXIT   ! EOF

         ALLOCATE(new_person)
         new_person%name = TRIM(records(1))

         READ(10, '(A)', IOSTAT=flag) records(1)
         IF (flag .NE. 0) THEN
            PRINT *, 'Missing count for ', TRIM(new_person%name)
            DEALLOCATE(new_person)
            EXIT
         END IF
         READ(records(1), *, IOSTAT=flag) new_person%count
         IF (flag .NE. 0) THEN
            PRINT *, 'Invalid count for ', TRIM(new_person%name)
            DEALLOCATE(new_person)
            EXIT
         END IF

c ------ link node ------
         new_person%prev => tail
         new_person%next => NULL()

         IF (.NOT. ASSOCIATED(tail)) THEN
            head => new_person
         ELSE
            tail%next => new_person
         END IF
         tail => new_person
      END DO

c -----------------------------------------------------
c MAKE LIST CIRCULAR  (required for Josephus)
c -----------------------------------------------------
      IF (ASSOCIATED(head) .AND. ASSOCIATED(tail)) THEN
         head%prev => tail
         tail%next => head
      END IF

c -----------------------------------------------------
c JOSEPHUS ELIMINATION
c -----------------------------------------------------
      current => head

      DO WHILE (ASSOCIATED(current%next) .AND. current%next /= current)

         count_value = current%count

c ------ MOVE FORWARD OR BACKWARD ------
         IF (count_value > 0) THEN
            DO i = 1, count_value - 1
               current => current%next
            END DO
         ELSE IF (count_value < 0) THEN
            DO i = 1, ABS(count_value) - 1
               current => current%prev
            END DO
         END IF

c ------ ELIMINATE CURRENT ------
         PRINT *, "Eliminating:", TRIM(current%name), &
                  " Count:", count_value
         WRITE(20, '(A)') TRIM(current%name)

c unlink
         current%prev%next => current%next
         current%next%prev => current%prev

c next node becomes new current
         temp_person => current
         current => current%next
         DEALLOCATE(temp_person)

         elim_count = elim_count + 1
      END DO

c -----------------------------------------------------
c SURVIVOR
c -----------------------------------------------------
      PRINT *, "Survivor: ", TRIM(current%name)
      WRITE(20, '(A)') TRIM(current%name)

      CLOSE(10)
      CLOSE(20)

END PROGRAM