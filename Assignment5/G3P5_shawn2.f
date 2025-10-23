      PROGRAM G3P5
c ----------------------------------------------
c Assignment 5: The Graphing Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE

      INTEGER, PARAMETER :: max_points = 100
      REAL :: x(max_points), y(max_points)
      REAL xmin, xmax, ymin, ymax, dx, dy, tdx
      CHARACTER*100 file_name_in, file_name_out, out_choice
      INTEGER n, i, num_lines, flag
      LOGICAL file_in_exists, file_out_exists, overwrite, quit

      file_in_exists = .FALSE.
      file_out_exists = .FALSE.
      quit = .FALSE.
      flag = 0

c Prompt for input file (handle quitting + file not found)
      DO WHILE (.NOT. quit .AND. .NOT. file_in_exists)
       PRINT *, 'Please enter an input file name (or QUIT to exit):'
       READ(*, *) file_name_in
       IF (file_name_in .EQ. 'QUIT') THEN
        quit = .TRUE.
       ELSE
        INQUIRE(FILE=file_name_in, EXIST=file_in_exists)
        IF (file_in_exists) THEN
         PRINT *, 'Input file found! Reading data...'
         OPEN(UNIT=10, FILE=file_name_in, STATUS='OLD', IOSTAT=flag)
         PRINT *
        ELSE
         PRINT *, 'File does not exist...'
        END IF
       END IF
      END DO

c Prompt for output file (handle all cases)
      IF (.NOT. quit) THEN
       PRINT *, 'Please enter an output file name (or QUIT to exit):'
       READ(*, *) file_name_out
       IF (file_name_out .EQ. 'QUIT') THEN
        quit = .TRUE.
       ELSE
        INQUIRE(FILE=file_name_out, EXIST=file_out_exists)
        IF (file_out_exists) THEN
         PRINT *, 'File already exists. Choose one of the following:'
         PRINT *, '- Enter a new filename to create a new file'
         PRINT *, '- Enter OVERWRITE to overwrite the existing file'
         PRINT *, '- Enter QUIT to exit'
         READ(*, *) out_choice
         SELECT CASE (out_choice)
         CASE ('OVERWRITE')
          PRINT *, 'Overwriting output data...'
          OPEN(UNIT=20, FILE=file_name_out, STATUS='OLD', IOSTAT=flag)
         CASE ('QUIT')
          quit = .TRUE.
         CASE DEFAULT
          PRINT *, 'Creating new file...'
          file_name_out = out_choice
          OPEN(UNIT=20, FILE=file_name_out, STATUS='NEW', IOSTAT=flag)
         END SELECT
        ELSE
         OPEN(UNIT=20, FILE=file_name_out, STATUS='NEW', IOSTAT=flag)
        END IF
        file_out_exists = .TRUE.
        PRINT *
       END IF
      END IF

c Read number of points + the points themselves
      IF (.NOT. quit) THEN
       READ(10, *, IOSTAT=flag) n
       DO i = 1, n
        READ(10, *, IOSTAT=flag) x(i), y(i)
       END DO

c Write the unsorted and unsorted points
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
       dy = (ymax - ymin) / 10.0
       CALL write_y_scale(ymin, ymax, dy)

       dx = x(2) - x(1)
       DO i = 2, n - 1
        tdx = x(i + 1) - x(i)
        IF (tdx .NE. 0 .AND. tdx .LT. dx) THEN
         dx = tdx
        END IF
       END DO

       num_lines = (xmax - xmin) / dx + 1
       IF (num_lines .LT. 20) THEN
        num_lines = 20
        dx = (xmax - xmin) / 21
       ELSE IF (num_lines .GT. 200) THEN
        num_lines = 20
        dx = (xmax - xmin) / 201
       END IF

       CALL plot(x, y, n, xmin, xmax, ymin, ymax, dx, dy)
      END IF

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

      SUBROUTINE write_y_scale(ymin, ymax, dy)
       REAL ymin, ymax, dy
       WRITE(20, '(6E10.4)') (ymin + (i - 1) * dy, i = 1, 6)
      END SUBROUTINE

      
      SUBROUTINE plot(x, y, n, xmin, xmax, ymin, ymax, dx, ystep)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: n
        REAL, INTENT(IN) :: x(:), y(:), xmin, xmax, ymin, ymax, dx, ystep
         REAL :: xp, yp
         INTEGER :: i, j, lines_to_plot
         LOGICAL :: printed

        lines_to_plot = MIN(200, MAX(20, INT((xmax - xmin) / dx)))

        WRITE(20, *) '--------------------------------------------'
        WRITE(20, *) 'PLOT PARAMETERS'
        WRITE(20, '(A, F12.6)') 'YMIN = ', ymin
        WRITE(20, '(A, F12.6)') 'YMAX = ', ymax
         WRITE(20, '(A, F12.6)') 'YSTEP = ', ystep
         WRITE(20, '(A, F12.6)') 'XMIN = ', xmin
         WRITE(20, '(A, F12.6)') 'XMAX = ', xmax
         WRITE(20, '(A, F12.6)') 'DX = ', dx
         WRITE(20, '(A, I6)')    'LINES TO BE PLOTTED = ', lines_to_plot
         WRITE(20, *) '--------------------------------------------'
         WRITE(20, *)

         DO yp = ymin, ymax, ystep
             WRITE(20, '(F10.4," |")', ADVANCE='NO') yp
            DO xp = xmin, xmax, (xmax - xmin) / 60.0
               printed = .FALSE.
               DO j = 1, n
                   IF (ABS(x(j) - xp) < dx / 2.0 .AND. ABS(y(j) - yp) < ystep / 2.0) THEN
                       WRITE(20, '(A)', ADVANCE='NO') '*'
                        printed = .TRUE.
                        EXIT
                    END IF
              END DO
                 IF (.NOT. printed) WRITE(20, '(A)', ADVANCE='NO') ' '
              END DO
             WRITE(20, *)
       END DO

       WRITE(20, *)
       WRITE(20, *) '--- End of Plot ---'
       WRITE(20, *)

      END SUBROUTINE plot