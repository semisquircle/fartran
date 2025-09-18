      PROGRAM G3P2
c ----------------------------------------------
c Assignment 2: The Averaging Program with Array
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

c Declare variables
      CHARACTER*30 name
      REAL grades(10)
      REAL sum, average, grade
      INTEGER i, num_grades

c Initialize variables
      sum = 0.0
      num_grades = 0

c Prompt name (allows spaces)
      PRINT *, 'Enter your name:'
      READ '(A)', name

c Prompt for grades, maximum of 10
      PRINT *
      PRINT *, 'You can enter up to 10 grades.'
      PRINT *, 'Enter an invalid grade to stop.'
      PRINT *
      DO i = 1, 10
         PRINT *, 'Enter grade number ', i, ':'
         READ *, grade

       IF (grade .LT. 0.0 .OR. grade .GT. 100.0) THEN
        PRINT *, 'Grade outside valid range, stopping input.'
       ENDIF

       num_grades = num_grades + 1
       grades(num_grades) = grade
       sum = sum + grade
      END DO

c Print results
      PRINT *
      PRINT *, '----------------------------------------'
      PRINT *, 'Name: ', name
      PRINT *, 'Number of grades: ', num_grades
      PRINT *, 'Grade sum: ', sum
      PRINT *, 'Average: ', sum / num_grades
      PRINT *, '----------------------------------------'

      END