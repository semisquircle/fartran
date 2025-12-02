      PROGRAM G3P7
c ----------------------------------------------
c Assignment 7: Linked Lists
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      CHARACTER(LEN=100) :: file_in_name, file_out_name
      INTEGER :: i, max_recs, flag
      CHARACTER(LEN=200) :: records(25)

      max_recs = 25

      DO
       PRINT *, 'Enter the input file name (or QUIT to exit):'
       READ *, file_in_name

       IF (TRIM(file_in_name) .EQ. 'QUIT') THEN
        PRINT *, 'Exiting program.'
        STOP
       END IF

       OPEN(UNIT=10, FILE=file_in_name, STATUS='OLD', IOSTAT=flag)

       IF (flag == 0) THEN
        PRINT *, 'File opened successfully: ', file_in_name
        EXIT
       ELSE
        PRINT *, 'File does not exist: ', file_in_name
       END IF
      END DO

      DO
       PRINT *, 'Enter the output file name (or QUIT to exit):'
       READ *, file_out_name

       IF (TRIM(file_out_name) .EQ. 'QUIT') THEN
        PRINT *, 'Exiting program.'
        STOP
       END IF

       OPEN(UNIT=20, FILE=file_out_name, STATUS='OLD', IOSTAT=flag)

       IF (flag == 0) THEN
        PRINT *, 'File already exists: ', file_out_name
        PRINT *,'Enter new file, or type OVERWRITE to overwrite file:'
        READ *, file_out_name

        IF (TRIM(file_out_name) .EQ. 'OVERWRITE') THEN
         OPEN(UNIT=20, FILE=file_out_name, STATUS='REPLACE', IOSTAT=flag)
         IF (flag == 0) THEN
          PRINT *, 'File overwritten successfully: ', file_out_name
          EXIT
         ELSE
          PRINT *, 'Error overwriting file: ', file_out_name
         END IF
        END IF
       ELSE
        OPEN(UNIT=20, FILE=file_out_name, STATUS='NEW', IOSTAT=flag)
        IF (flag == 0) THEN
         PRINT *, 'File created successfully: ', file_out_name
         EXIT
        ELSE
         PRINT *, 'Error creating file: ', file_out_name
        END IF
       END IF
      END DO

      TYPE :: Node
      CHARACTER(LEN=100) :: name
      INTEGER :: count
      TYPE(Node), POINTER :: prev => NULL()
      TYPE(Node), POINTER :: next => NULL()
      END TYPE Node

      TYPE(Node), POINTER :: head => NULL()
      TYPE(Node), POINTER :: tail => NULL()
      TYPE(Node), POINTER :: current => NULL()

      INTEGER :: name_line, count_line
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

         TYPE(Node), POINTER :: new_node
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
       PRINT *,'Current Name: ',TRIM(current%name),' Count: ',count_value

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

       TYPE(Node), POINTER :: tempNode
       tempNode => current

       IF (count_value > 0) THEN
        current => current%next
       ELSE IF (count_value < 0) THEN
        current => current%prev
       ELSE
        current => NULL()
       END IF

       DEALLOCATE(tempNode)
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
