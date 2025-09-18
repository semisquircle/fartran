      PROGRAM G3P1
      IMPLICIT NONE
* Declare variables
      CHARACTER name*50
      REAL sum, grade
      INTEGER i, num_grades

* Initialize grade sum
      sum = 0.0

* Prompt user for their name (allows for spaces)
      PRINT *, 'Enter your name:'
      READ '(A)', name

* Prompt user for number of grades (must be at least 1)
      num_grades = 0
      DO WHILE (num_grades < 1)
       PRINT *, 'Enter the number of grades (at least 1):'
       READ *, num_grades
       IF (num_grades < 1) THEN
        PRINT *, 'Must have at least 1 grade. Please try again.'
       END IF
      END DO

* Loop to read grades
      DO i = 1, num_grades
       PRINT *, 'Enter grade number ', i, ':'
       READ *, grade
       sum = sum + grade
      END DO

* Print name and average
      PRINT *, ''
      PRINT *, '----------------------------------------'
      PRINT *, 'Name: ', name
      PRINT *, 'Number of grades: ', num_grades
      PRINT *, 'Grade sum: ', sum
      PRINT *, 'Average: ', sum / num_grades
      PRINT *, '----------------------------------------'
      END