      PROGRAM G3P2
c ----------------------------------------------
c Assignment 2: The Averaging Program with Array
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

c Declare variables
      CHARACTER*30 name
      REAL grades(10)
      REAL sum, grade, average
      INTEGER i, num_grades

c Initialize variables
      sum = 0.0
      average = 0.0
      i = 1
      num_grades = 0

c Prompt name (allows spaces)
      PRINT *, 'Enter your name:'
  100 FORMAT(A)
      READ(*, 100), name

c Prompt grades, maximum of 10
      PRINT *
      PRINT *, 'You can enter up to 10 grades.'
      PRINT *, 'Enter an invalid grade to stop.'
      PRINT *

      DO WHILE (i .LE. 10)
       PRINT *, 'Enter grade number ', i, ':'
  200  FORMAT(F10.2)
       READ(*, 200), grade

c If grade is invalid, halt the entire loop (overload iterator)
       IF (grade .LT. 0.0 .OR. grade .GT. 100.0) THEN
        PRINT *, 'Grade outside valid range, stopping input.'
        i = 11
       ELSE
        grades(i) = grade
        sum = sum + grade
        num_grades = num_grades + 1
        i = i + 1
       END IF
      END DO

c Calculate average (avoid divide by 0 error)
      IF (num_grades .GT. 0) THEN
       average = sum / num_grades
      END IF

c Print results
  300 FORMAT(A, F6.2, A)

      PRINT *
      PRINT *, '----------------------------------------'
      PRINT *, 'Name: ', name
      PRINT *, 'Number of grades: ', num_grades

      PRINT *, 'Grades:'
      DO i = 1, num_grades
       PRINT 300, '  ', grades(i), '%'
      END DO

      PRINT *, 'Grade sum: ', sum
      PRINT *, 'Average: ', average
      PRINT *, '----------------------------------------'

      END