      PROGRAM G3P7
c ----------------------------------------------
c Assignment 7: Linked Lists
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

c Write a Structured FORTRAN program that will prompt for, test existence of, 
c and open an input file.
c If the file does not exist, re-prompt the user and accept another file name 
c or 'QUIT' to quit
c Read up to 25 records from the input file
        CHARACTER(LEN=100) :: INFILENAME
        INTEGER :: INUNIT, INIOSTAT
        INTEGER :: I, MAXRECS
        CHARACTER(LEN=200) :: RECORDS(25)
    
        INUNIT = 10
        MAXRECS = 25
    
        DO
            PRINT *, 'Enter the input file name (or QUIT to exit):'
            READ *, INFILENAME
    
            IF (TRIM(INFILENAME) .EQ. 'QUIT') THEN
                PRINT *, 'Exiting program.'
                STOP
            END IF
    
            OPEN(UNIT=INUNIT, FILE=INFILENAME, STATUS='OLD', IOSTAT=INIOSTAT)
    
            IF (INIOSTAT == 0) THEN
                PRINT *, 'File opened successfully: ', INFILENAME
                EXIT
            ELSE
                PRINT *, 'File does not exist: ', INFILENAME
            END IF
        END DO
c Prompt for, test existence of, and open an output file.
c If the file exists, prompt the user to enter a new file name, overwrite the existing file, or
c 'QUIT', otherwise open the new file
        CHARACTER(LEN=100) :: OUTFILENAME
        INTEGER :: OUTUNIT, OUTIOSTAT
    
        OUTUNIT = 20
    
        DO
            PRINT *, 'Enter the output file name (or QUIT to exit):'
            READ *, OUTFILENAME
    
            IF (TRIM(OUTFILENAME) .EQ. 'QUIT') THEN
                PRINT *, 'Exiting program.'
                STOP
            END IF
    
            OPEN(UNIT=OUTUNIT, FILE=OUTFILENAME, STATUS='OLD', IOSTAT=OUTIOSTAT)
    
            IF (OUTIOSTAT == 0) THEN
                PRINT *, 'File already exists: ', OUTFILENAME
                PRINT *, 'Enter a new file name, or type OVERWRITE to overwrite the existing file:'
                READ *, OUTFILENAME
    
                IF (TRIM(OUTFILENAME) .EQ. 'OVERWRITE') THEN
                    OPEN(UNIT=OUTUNIT, FILE=OUTFILENAME, STATUS='REPLACE', IOSTAT=OUTIOSTAT)
                    IF (OUTIOSTAT == 0) THEN
                        PRINT *, 'File overwritten successfully: ', OUTFILENAME
                        EXIT
                    ELSE
                        PRINT *, 'Error overwriting file: ', OUTFILENAME
                    END IF
                END IF
            ELSE
                OPEN(UNIT=OUTUNIT, FILE=OUTFILENAME, STATUS='NEW', IOSTAT=OUTIOSTAT)
                IF (OUTIOSTAT == 0) THEN
                    PRINT *, 'File created successfully: ', OUTFILENAME
                    EXIT
                ELSE
                    PRINT *, 'Error creating file: ', OUTFILENAME
                END IF
            END IF
        END DO
c A record will contain a string name and an integer count along with 
c appropriate data for a doubly linked list.
C The input file will contain a single string name on one line will be followed 
c by an integer count on the following line.
C The records will be stored in a doubly linked-list.
        TYPE :: Node
            CHARACTER(LEN=100) :: name
            INTEGER :: count
            TYPE(Node), POINTER :: prev => NULL()
            TYPE(Node), POINTER :: next => NULL()
        END TYPE Node
    
        TYPE(Node), POINTER :: head => NULL()
        TYPE(Node), POINTER :: tail => NULL()
        TYPE(Node), POINTER :: current => NULL()
    
        INTEGER :: nameLine, countLine
        nameLine = 0
        countLine = 0
    
        I = 0
        DO WHILE (I < MAXRECS)
            READ(INUNIT, '(A)', IOSTAT=INIOSTAT) RECORDS(I+1)
            IF (INIOSTAT /= 0) EXIT
            IF (MOD(I, 2) == 0) THEN
                nameLine = I + 1
            ELSE
                countLine = I + 1
                ! Create new node and add to linked list
                TYPE(Node), POINTER :: newNode
                ALLOCATE(newNode)
                newNode%name = TRIM(RECORDS(nameLine))
                READ(RECORDS(countLine), *) newNode%count
                newNode%prev => tail
                newNode%next => NULL()
    
                IF (TAIL .NE. NULL()) THEN
                    TAIL%next => newNode
                ELSE
                    HEAD => newNode
                END IF
                TAIL => newNode
            END IF
            I = I + 1
        END DO
c Processing will begin with the first record entered.
C The COUNT for this person will be used to count records from this starting record.
C A positive count will be forward in the linked-list and a negative count will be backward in the linked-list.
        INTEGER :: countValue
        current => head
    
        DO WHILE (current .NE. NULL())
            countValue = current%count
            PRINT *, 'Current Name: ', TRIM(current%name), ' Count: ', countValue
    
            IF (countValue > 0) THEN
                DO I = 1, countValue
                    IF (current%next .NE. NULL()) THEN
                        current => current%next
                    ELSE
                        EXIT
                    END IF
                END DO
            ELSE IF (countValue < 0) THEN
                DO I = 1, ABS(countValue)
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
c The first person counted from the start will have the count of one and the second will have the count of two, etc.
C When the last count is reached, the COUNT for this current person will be loaded and the record for this person will be removed from the linked-list.
c The person being eliminated and the count will be written to the file and on the screen.
C Before the person is eliminated the first person for the COUNT will be identified.
        current => head
        INTEGER :: elimCount
        elimCount = 1
    
        DO WHILE (head .NE. NULL())
            countValue = current%count
            PRINT *, 'Elimination Count: ', elimCount
            PRINT *, 'Eliminating: ', TRIM(current%name), ' with Count: ', countValue
    
            ! Write to output file
            WRITE(OUTUNIT, '(A, I0)') TRIM(current%name), countValue
    
            ! Remove current node from linked list
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
    
            ! Move to next node based on count value before deallocating
            IF (countValue > 0) THEN
                current => current%next
            ELSE IF (countValue < 0) THEN
                current => current%prev
            ELSE
                current => NULL()
            END IF
    
            DEALLOCATE(tempNode)
            elimCount = elimCount + 1
    
            IF (current .EQ. NULL()) THEN
                EXIT
            END IF
        END DO
c If the COUNT is positive the first person will be forward in the linked-list from the to be
c removed person, if the COUNT is negative the first person will be backward in the
c linked-list from the to be removed person.
C The program will stop when only one person is remaining.
C This person will be identified as the survivor.
C Do not keep a count of the number of people in the linked-list structure to identify the
c one person remaining, use the linked-list pointers for this identification. 
        IF (head .NE. NULL()) THEN
            PRINT *, 'Survivor: ', TRIM(head%name)
        ELSE
            PRINT *, 'No survivor found.'
        END IF
    
        CLOSE(INUNIT)
        CLOSE(OUTUNIT)
    
      END PROGRAM G3P7    
      