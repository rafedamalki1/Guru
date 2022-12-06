DEFINE VARIABLE lic-file  AS CHARACTER FORMAT "x(64)".
DEFINE VARIABLE i         AS INTEGER.

DEFINE VARIABLE Save_Date AS DATE LABEL "Date".
DEFINE VARIABLE sdate AS DATE LABEL "Enter Date Range".
DEFINE VARIABLE edate AS DATE LABEL " To" INIT TODAY.
DEFINE VARIABLE stime AS INTEGER LABEL "Enter Start Time (hour from 0 to 23)"
     INIT 0.
DEFINE VARIABLE endtime AS INTEGER LABEL "Enter Stop Time (hour from 0 to 24)"
     INIT 24.
DEFINE VARIABLE tdivision AS DECIMAL FORMAT ">9"
  LABEL "Enter time division (in hours, or 0 for complete range)" INIT 0.

DEFINE VARIABLE LicUsers AS DECIMAL FORMAT "->>>9" LABEL "# Lcn Users".
DEFINE VARIABLE MaxCon   AS DECIMAL FORMAT ">>>9" LABEL "MaxTot".
DEFINE VARIABLE Exception AS DECIMAL FORMAT ">>>9" LABEL "Excptns".
DEFINE VARIABLE MinCon   AS DECIMAL FORMAT ">>>9" LABEL "MinTot".
DEFINE VARIABLE AveCon   AS DECIMAL FORMAT ">9.<" LABEL "AveTot".
DEFINE VARIABLE MaxBatCon   AS DECIMAL FORMAT ">>>9" LABEL "MaxBat".
DEFINE VARIABLE MinBatCon   AS DECIMAL FORMAT ">>>9" LABEL "MinBat".
DEFINE VARIABLE AveBatCon   AS DECIMAL FORMAT ">9.<" LABEL "AveBat".
DEFINE VARIABLE BeginHr  AS INTEGER.
DEFINE VARIABLE EndHr    AS INTEGER.
DEFINE VARIABLE mydbname AS CHAR LABEL "Database Name" FORMAT "X(30)".
DEFINE VARIABLE Hour  AS INTEGER.

DEFINE FRAME out-frame WITH DOWN TITLE "Database Connection Counts" use-text SIZE 100 BY 20 view-as dialog-box.

UPDATE sdate edate stime endtime tdivision mydbname WITH SIDE-LABELS.

lic-file = mydbname + ".lic".

DEFINE TEMP-TABLE work-file
  FIELD CalDate   AS DATE
  FIELD Time-Per  AS CHARACTER FORMAT "x(8)" LABEL "Period"
  FIELD licUsrs   AS DECIMAL FORMAT "->>>9"  LABEL "LcnUsers"
  FIELD curconnect  AS DECIMAL FORMAT ">>>9"
  FIELD maxconnect  AS DECIMAL FORMAT ">>>9"
  FIELD minconnect  AS DECIMAL FORMAT ">>>9"
  FIELD curactconnect  AS DECIMAL FORMAT ">>>9"
  FIELD maxactconnect  AS DECIMAL FORMAT ">>>9"
  FIELD minactconnect  AS DECIMAL FORMAT ">>>9"
  FIELD curbatconnect  AS DECIMAL FORMAT ">>>9"
  FIELD maxbatconnect  AS DECIMAL FORMAT ">>>9"
  FIELD minbatconnect  AS DECIMAL FORMAT ">>>9".

INPUT FROM VALUE(lic-file) NO-ECHO.

REPEAT:
   CREATE work-file.
   IMPORT work-file.
END.

INPUT CLOSE.

/* Display Report */
FOR EACH work-file:

  Hour = INTEGER(SUBSTRING(work-file.Time-Per,1,2)).
  IF ( (tdivision <> 0) AND
       (((work-file.CalDate = sdate) AND 
         (Hour > stime)) OR (work-file.CalDate > sdate)) AND
       ((work-file.CalDate <  edate) OR 
        ((work-file.CalDate = edate) AND (Hour <= endtime))) ) OR

    ( (tdivision = 0) AND (work-file.CalDate >= sdate) AND
      (Hour > stime) AND (work-file.CalDate <= edate) AND (Hour <= endtime) ) OR

    ( (tdivision = 0) AND (work-file.CalDate <= (edate + 1)) AND (Hour = 0) AND (endtime = 24) )
  THEN DO:

     /* update min/max values */
     IF (work-file.maxbatconnect > MaxBatCon) THEN
        MaxBatCon = work-file.maxbatconnect.
     IF (work-file.minbatconnect < MinBatCon) THEN
        MinBatCon = work-file.minbatconnect.
     IF (work-file.maxconnect > MaxCon) THEN
        MaxCon = work-file.maxconnect.
     IF (work-file.minconnect < MinCon) THEN
        MinCon = work-file.minconnect.

     i = i + 1.
     /* if first test point, update Start Time value */
     IF (i = 1) THEN DO:

        IF (Hour = 0) THEN DO:
           BeginHr = 23.
	   Save_Date = work-file.CalDate - 1.
        END.
        ELSE IF (Hour = 1) THEN DO:
           BeginHr = 0.
	   Save_Date = work-file.CalDate.
        END.
        ELSE DO:
           BeginHr = Hour - 1.
           Save_Date = work-file.CalDate.
        END.

     END.  /* IF (i = 1) */

     /* if last test point for table entry */
     IF (i = tdivision) OR
        ((tdivision = 0) AND
         (work-file.CalDate <= edate) AND (Hour = endtime)) OR
        ((tdivision = 0) AND
         (work-file.CalDate <> edate) AND (Hour = 0) AND (endtime = 24)) THEN DO:

        /* fix time period for display */
        IF (Hour = 0) THEN DO:
           EndHr = 24.
        END.
        ELSE
           EndHr = Hour.
        work-file.Time-Per = STRING(BeginHr) + "-" + STRING(EndHr).

        DISPLAY Save_Date WITH FRAME out-frame.

        /* calculate exception */
        IF (work-file.licUsrs = -1) OR (MaxCon <= work-file.licUsrs) THEN
           Exception = 0.
        ELSE
           Exception = MaxCon - work-file.licUsrs.

        /* calculate average connections */
        AveCon = (MaxCon + MinCon) / 2.
        /* calculate average batch connections */
        AveBatCon = (MaxBatCon + MinBatCon) / 2.

        DISPLAY work-file.Time-Per work-file.licUsrs  MaxCon Exception MinCon  
          AveCon MaxBatCon MinBatCon AveBatCon WITH FRAME out-frame.
        i = 0.
        MaxCon = 0.
        MinCon = 9999.
        MaxBatCon = 0.
        MinBatCon = 9999.
     END.  /* IF (i = tdivision) THEN DO: */

  END.  /* IF ((work-file.CalDate >= sdate)  AND (work-file.CalDate <= edate)) 
      AND ((Hour > stime) AND (Hour <= endtime)) THEN DO: */

END. /* FOR */
Pause.
