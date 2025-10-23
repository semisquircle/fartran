      PROGRAM G3P5
c ----------------------------------------------
c Assignment 5: The Graphing Program
c Group 3 (Shawn Gallagher, Lucas Giovannelli)
c ----------------------------------------------
      IMPLICIT NONE
c prompt for input file and test it exists
      CHARACTER(LEN=20) :: INFILE
      INTEGER :: IOSTAT, UNIT
      UNIT = 10
      PRINT *, 'Enter the name of the input file:'
      READ *, INFILE
      OPEN(UNIT=UNIT, FILE=INFILE, STATUS='old', IOSTAT=IOSTAT)
      IF (IOSTAT /= 0) THEN
          PRINT *, 'Error: Cannot open file ', INFILE
          STOP
      END IF

c If the file does not exist, re-prompt the user and accept another file name or 'QUIT' to quit
      DO WHILE (IOSTAT /= 0)
          PRINT *, 'File not found. Please enter another file name or type QUIT to exit:'
          READ *, INFILE
          IF (TRIM(INFILE) == 'QUIT') THEN
              PRINT *, 'Exiting program.'
              STOP
          END IF
          OPEN(UNIT=UNIT, FILE=INFILE, STATUS='old', IOSTAT=IOSTAT)
      END DO

c Prompt for, test existence of, and open an output file
      CHARACTER(LEN=20) :: OUTFILE
      UNIT = 20
      PRINT *, 'Enter the name of the output file:'
      READ *, OUTFILE
      OPEN(UNIT=UNIT, FILE=OUTFILE, STATUS='replace', IOSTAT=IOSTAT)
      IF (IOSTAT /= 0) THEN
          PRINT *, 'Error: Cannot open file ', OUTFILE
          STOP
      END IF

c If the file exists, prompt the user to enter a new file name, overwrite the existing file, or 'QUIT',
c otherwise open the new file
      DO WHILE (IOSTAT /= 0)
          PRINT *, 'Cannot open file. Please enter another file name, type OVERWRITE to overwrite, or type QUIT to exit:'
          READ *, OUTFILE
          IF (TRIM(OUTFILE) == 'QUIT') THEN
              PRINT *, 'Exiting program.'
              STOP
          ELSE IF (TRIM(OUTFILE) == 'OVERWRITE') THEN
              OPEN(UNIT=UNIT, FILE=OUTFILE, STATUS='replace', IOSTAT=IOSTAT)
          ELSE
              OPEN(UNIT=UNIT, FILE=OUTFILE, STATUS='replace', IOSTAT=IOSTAT)
          END IF
      END DO

c Once the files are open, read an integer, the only data on the line, which identifies the number of xy
c points included in the file. The maximum number of points is 100
      INTEGER :: NPOINTS, I
      REAL :: X(100), Y(100)
      READ(10, *, IOSTAT=IOSTAT) NPOINTS
      IF (IOSTAT /= 0 .OR. NPOINTS < 1 .OR. NPOINTS > 100) THEN
          PRINT *, 'Error: Invalid number of points.'
          STOP
      END IF

c Read the X, Y pairs of real data from the file. There will be one X, Y pair per line. The data will be
c separate by at least one space
      DO I = 1, NPOINTS
          READ(10, *, IOSTAT=IOSTAT) X(I), Y(I)
          IF (IOSTAT /= 0) THEN
              PRINT *, 'Error: Invalid data format.'
              STOP
          END IF
      END DO

c Write the data points to the output file
      WRITE(20, *) 'X Y Points:'
      DO I = 1, NPOINTS
          WRITE(20, '(F10.4, 1X, F10.4)') X(I), Y(I)
      END DO

c Write the minimum, maximum, and step values for the x and y data to the output file
      REAL :: XMIN, XMAX, YMIN, YMAX, XSTEP, YSTEP
      XMIN = MINVAL(X(1:NPOINTS))
      XMAX = MAXVAL(X(1:NPOINTS))
      YMIN = MINVAL(Y(1:NPOINTS))
      YMAX = MAXVAL(Y(1:NPOINTS))
      XSTEP = (XMAX - XMIN) / 10.0
      YSTEP = (YMAX - YMIN) / 10.0

      WRITE(20, *) 'X Minimum:', XMIN
      WRITE(20, *) 'X Maximum:', XMAX
      WRITE(20, *) 'X Step:', XSTEP
      WRITE(20, *) 'Y Minimum:', YMIN
      WRITE(20, *) 'Y Maximum:', YMAX
      WRITE(20, *) 'Y Step:', YSTEP

c Print the y scale to the output file, use 6 values with a 10.4 format each
      WRITE(20, *) 'Y Scale:'
      DO I = 0, 5
          WRITE(20, '(F10.4)', ADVANCE='NO') YMIN + I * YSTEP
      END DO
      WRITE(20, *) ''

c Print the y axis to the output file
      WRITE(20, *) 'Y Axis:'
      DO I = 0, 10
          WRITE(20, '(F10.4)', ADVANCE='NO') YMIN + I * YSTEP
      END DO
      WRITE(20, *) ''

c Plot the data including the x axis to the output file
c (The number of x lines printed will be determined by the smallest change in x and the domain of x. The
c minimum number of lines printed will be 20 and the maximum number of lines printed will be 200
c Print a x scale every five x lines
      INTEGER :: NXLINES, J
      REAL :: XRANGE, XMINLINE, XMAXLINE, XINCREMENT
      XRANGE = XMAX - XMIN
      NXLINES = MAX(20, MIN(200, INT(XRANGE / ((XMAX - XMIN) / 100.0))))
      XINCREMENT = XRANGE / REAL(NXLINES)

      WRITE(20, *) 'X Scale:'
      DO I = 0, NXLINES
          IF (MOD(I, 5) == 0) THEN
              WRITE(20, '(F10.4)', ADVANCE='NO') XMIN + I * XINCREMENT
          END IF
      END DO
      WRITE(20, *) ''

      WRITE(20, *) 'X Axis:'
      DO I = 0, NXLINES
          WRITE(20, '(F10.4)', ADVANCE='NO') XMIN + I * XINCREMENT
      END DO
      WRITE(20, *) ''

      WRITE(20, *) 'Data Plot:'
      DO I = 0, NXLINES
          REAL :: CURRENTX
          CURRENTX = XMIN + I * XINCREMENT
          WRITE(20, '(F10.4)', ADVANCE='NO') CURRENTX
          DO J = 1, NPOINTS
              IF (ABS(X(J) - CURRENTX) < (XINCREMENT / 2.0)) THEN
                  WRITE(20, '(1X, F10.4)', ADVANCE='NO') Y(J)
              END IF
          END DO
          WRITE(20, *)
      END DO     

      CLOSE(10)
      CLOSE(20)
      PRINT *, 'Graphing complete. Data written to ', OUTFILE
      END PROGRAM
