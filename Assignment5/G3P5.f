      PROGRAM G3P5
c ----------------------------------------------
c Assignment 5: The Graphing Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      REAL :: x(100), y(100)
      REAL xmin, xmax, ymin, ymax, dx, dy, tdx
      CHARACTER*50 file_in_name, file_out_name, out_choice
      INTEGER file_in_size, n, i, num_rows, num_cols, flag
      LOGICAL file_in_exists, file_out_exists, quit, stop_reading

c     Initialize variables
      num_cols = 67
      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      quit = .FALSE.
      flag = 0

      PRINT *

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

      IF (.NOT. quit .AND. flag .EQ. 0) THEN
c      Read number of points + the points themselves
c      (Handles too few or too much data)
       READ(10, *, IOSTAT=flag) n
       stop_reading = .FALSE.
       i = 1
       DO WHILE (.NOT. stop_reading .AND. flag .EQ. 0)
        READ(10, *, IOSTAT=flag) x(i), y(i)
        IF (i .EQ. n + 1 .AND. flag .EQ. 0) THEN
         PRINT *, 'Input file has more data than specified...'
         PRINT *, '(Likely just white space at the end of the file)'
         PRINT *, 'Program will only use the first ', i - 1, ' points.'
         stop_reading = .TRUE.
        ELSE IF (i .LE. n .AND. flag .LT. 0) THEN
         PRINT *, 'Input file has less data than specified...'
         PRINT *, 'Program will only use ', i - 1, ' points.'
         n = i - 1
        END IF
        i = i + 1
       END DO

c      Write the unsorted and unsorted points
       WRITE(20, *) 'DATA POINTS'
       DO i = 1, n
        WRITE(20, '(2F15.8)') x(i), y(i)
       END DO

       CALL sort_xy(x, y, n)
       WRITE(20, *) 'SORTED DATA'
       DO i = 1, n
        WRITE(20, '(2F15.8)') x(i), y(i)
       END DO

       xmin = MINVAL(X(1:n))
       xmax = MAXVAL(X(1:n))
       ymin = MINVAL(Y(1:n))
       ymax = MAXVAL(Y(1:n))

c      Find the smallest difference between successive X values
       dx = x(2) - x(1)
       DO i = 2, n - 1
        tdx = x(i + 1) - x(i)
        IF (tdx .NE. 0 .AND. tdx .LT. dx) THEN
         dx = tdx
        END IF
       END DO
       dy = (ymax - ymin) / (num_cols - 1)

       CALL plot(x, y, n, num_cols, xmin, xmax, ymin, ymax, dx, dy)
       PRINT *
       PRINT *, 'Plot has been drawn to ', TRIM(file_out_name), '!'
      END IF

      PRINT *

      CLOSE(10)
      CLOSE(20)
      END PROGRAM


      SUBROUTINE sort_xy(x, y, n)
       INTEGER n, i, j
       REAL :: x(n), y(n)
       REAL temp_x, temp_y

       DO i = 1, n - 1
        DO j = i + 1, n
         IF (x(j) .LT. x(i)) THEN
          temp_x = x(i)
          temp_y = y(i)
          x(i) = x(j)
          y(i) = y(j)
          x(j) = temp_x
          y(j) = temp_y
         END IF
        END DO
       END DO
      END SUBROUTINE

      SUBROUTINE plot(x, y, n, num_cols, xmin, xmax, ymin, ymax, dx, dy)
       INTEGER n, r, c, i, num_rows, num_cols, point_index
       REAL x(n), y(n), xmin, xmax, ymin, ymax, dx, dy, xval
       LOGICAL row_has_point, col_has_point, has_x_label, has_y_label
       CHARACTER*1, DIMENSION(num_cols) :: line

c      Scale rows/lines to between 20 and 200
       num_rows = (xmax - xmin) / dx + 1
       IF (num_rows .LT. 20) THEN
        num_rows = 20
        dx = (xmax - xmin) / 21
       ELSE IF (num_rows .GT. 200) THEN
        num_rows = 200
        dx = (xmax - xmin) / 201
       END IF

  100  FORMAT(A8, F12.6, A8, F12.6, A8, F12.6)
       WRITE(20, 100) 'YMIN ', ymin, ' YMAX ', ymax, ' YSTEP ', dy
       WRITE(20, 100) 'XMIN ', xmin, ' XMAX ', xmax, ' DX ', dx
       WRITE(20, '("LINES TO BE PLOTTED ", I12)') num_rows
       WRITE(20, *)

c      Print the Y-axis labels
       WRITE(20, '(7(X,F10.4))') (ymin + i*((ymax - ymin) / 6), i=0, 6)

       point_index = 0
       row_has_point = .FALSE.
       col_has_point = .FALSE.

       DO r = 1, num_rows
        line = ' '
        xval = xmin + (r - 1) * dx
       
c       Check whether row should plot a point
c       (This method seemed a little simpler than from Lesson 12)
        DO i = 1, n
         IF (ABS(x(i) - xval) .LT. dx / 2) THEN
          row_has_point = .TRUE.
          point_index = point_index + 1
         END IF
        END DO

c       X-axis labels
        has_x_label = MOD(r, 5) .EQ. 1
        IF (has_x_label) THEN
         WRITE(20, '(F10.4)', ADVANCE='no') xval
        ELSE
         WRITE(20, '(10X)', ADVANCE='no')
        END IF

        DO c = 1, num_cols
         yval = ymin + (c - 1) * dy

c        Y-axis labels
         has_y_label = MOD(c, 11) .EQ. 1
         col_has_point = ABS(y(point_index) - yval) .LT. dy / 2

c        Determine which character to print on line
         IF (row_has_point .AND. col_has_point) THEN
          line(c) = '*'
         ELSE IF (r .EQ. 1) THEN
          IF (has_y_label) THEN
           line(c) = '+'
          ELSE
           line(c) = '-'
          END IF
         ELSE IF (c .EQ. 1) THEN
          IF (has_x_label) THEN
           line(c) = '+'
          ELSE
           line(c) = '|'
          END IF
         END IF
        END DO

c       Print out full row/line
        DO i = 1, num_cols
         WRITE(20, '(A)', ADVANCE='no') line(i)
        END DO
        WRITE(20, *)

        row_has_point = .FALSE.
       END DO
      END SUBROUTINE
