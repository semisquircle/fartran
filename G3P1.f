      PROGRAM G3P1
      IMPLICIT NONE
* Declare variables
      CHARACTER name*50
      REAL grade, total
      INTEGER i, n

* Initialize total to 0
      total = 0.0

* Prompt user for their name
      PRINT *, 'Enter your name:'
      READ *, name

* Prompt user for the number of grades
      PRINT *, 'Enter the number of grades:'
      READ *, n

* Loop to read grades
      DO i = 1, n
       PRINT *, 'Enter grade number ', i, ':'
       READ *, grade
       total = total + grade
      END DO

* ...

      END
