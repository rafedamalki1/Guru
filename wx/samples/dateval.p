/************************************************************************************
	PROCEDURE: dateval.p

	PURPOSE:   A simple calendar is displayed, processed, and the
		   final date is returned

	SYNTAX:    "RUN samples/dateval.p"

	REMARKS:   This code displays a calendar with five buttons, four to
		   change the date and one to exit.
		   The final date is returned to the calling program of 
		   APPLHELP.p

	AUTHORS:   George Kassabgi/Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.         */

/*Code_Start*/

DEFINE temp-table tt                            /* Temporary table of      */
   FIELD h_WIDGET AS WIDGET-HANDLE              /* WIDGET HANDLEs          */
   FIELD h_num    AS INT.
   
DEFINE BUTTON btn_yr_last        LABEL "- Yr".
DEFINE BUTTON btn_yr_next        LABEL "+ Yr".
DEFINE BUTTON btn_prev           LABEL "- Mth".
DEFINE BUTTON btn_next           LABEL "+ Mth".

DEFINE VARIABLE cal_month        AS INT         NO-UNDO.
DEFINE VARIABLE cal_year         AS INT         NO-UNDO.
DEFINE VARIABLE grid_x           AS INT         NO-UNDO. /* Grid x-positions      */
DEFINE VARIABLE grid_y           AS INT         NO-UNDO. /* Grid y-positions      */
DEFINE VARIABLE start_day        AS INT         NO-UNDO.
DEFINE VARIABLE end_day          AS INT         NO-UNDO.
DEFINE VARIABLE the_day          AS INT INITIAL 1 NO-UNDO.

DEFINE VARIABLE dummy            AS DATE        NO-UNDO.

DEFINE VARIABLE month_string     AS CHAR EXTENT 12
    INITIAL ["January","February","March","April","May","June","July",
	     "August","September","October","November","December"] NO-UNDO.
DEFINE VARIABLE the_month        AS CHAR        NO-UNDO.
DEFINE VARIABLE the_year         AS CHAR        NO-UNDO.
DEFINE SHARED VAR s_date         AS DATE        NO-UNDO.

/* If the frame-VALUE looks like a date, use this date as the */
/* initial date of the calendar, otherwise use today */

dummy = s_date.

ASSIGN
   cal_month = MONTH(dummy)
   cal_year  = YEAR(dummy)
   start_day = WEEKDAY(DATE(cal_month, 1, cal_year))
   end_day   = IF cal_month = 12 THEN 
                    DAY(DATE(1, 1, cal_year + 1) - 1)
               ELSE DAY(DATE(cal_month + 1,1, cal_year) - 1)
   the_month = month_string[cal_month]
   the_year  = STRING(cal_year,"9999").

DISPLAY "Calendar - " AT 7 the_month NO-LABEL the_year NO-LABEL
	" Sun   Mon   Tue   Wed   Thu    Fri   Sat" AT ROW 1.7 COL 3
	SKIP(8) 
	btn_yr_last AT 9 btn_yr_next btn_prev btn_next 
	WITH FRAME calendar centered COL 4 Width 55 ROW 3 OVERLAY
	VIEW-AS DIALOG-BOX.
VIEW frame calendar.
	
ENABLE btn_yr_last btn_yr_next btn_prev btn_next WITH FRAME calendar.        
REPEAT grid_y = 40 TO 210 BY 37: /* Create rectangle WIDGETs dynamically */
  REPEAT grid_x = 20 TO 276 BY 37:
    DEFINE var grid AS WIDGET-HANDLE NO-UNDO.    
    CREATE FILL-IN grid
    ASSIGN
	x = grid_x
	y = grid_y
	HEIGHT-PIXELS = 35
	WIDTH-PIXELS =  35
	SCREEN-VALUE = 
	   IF TRUNC((grid_y - 10) / 37,0) * 7 + TRUNC(grid_x / 37,0) + 1
	      < start_day OR the_day > end_day THEN " " 
	   ELSE IF DAY(TODAY) = the_day THEN "(" + STRING(the_day) + ")"
	   ELSE STRING(the_day,"z9")
	FRAME = FRAME calendar:HANDLE
	SENSITIVE = yes
    TRIGGERS:
        ON ENTRY PERSISTENT RUN entry_set_day.
        ON MOUSE-SELECT-DBLCLICK PERSISTENT RUN set_day.
    END TRIGGERS.

    IF TRUNC((grid_y - 10) / 37,0) * 7 + TRUNC(grid_x / 37,0) + 1 
       >= start_day
       THEN the_day = the_day + 1.
	
    CREATE tt.          /* store all BUTTON WIDGETs in work table  */
    ASSIGN tt.h_WIDGET = grid
	   tt.h_num = TRUNC((grid_y - 10) / 37,0) * 7 + TRUNC(grid_x / 37,0) + 1.
    VIEW grid.          /* VIEW each BUTTON  */
  END.
END.

ON CHOOSE of btn_yr_last DO:
   cal_year = cal_year - 1.
   RUN update_calendar.
END.

ON CURSOR-RIGHT OF FRAME calendar DO:
   cal_year = cal_year + 1.
   RUN update_calendar.
END.

ON CHOOSE of btn_yr_next  DO:
   cal_year = cal_year + 1.
   RUN update_calendar.
END.

ON CHOOSE of btn_prev DO:
    ASSIGN
      cal_year =  IF cal_month > 1 THEN cal_year ELSE cal_year - 1
      cal_month = IF cal_month > 1 THEN cal_month - 1 ELSE 12.
    RUN update_calendar.
END.

ON CHOOSE of btn_next DO:
   ASSIGN
     cal_year =  IF cal_month < 12 THEN cal_year ELSE cal_year + 1
     cal_month = IF cal_month < 12 THEN cal_month + 1 ELSE 1.
   RUN update_calendar.
END.
  
UPDATE btn_next WITH FRAME calendar.
HIDE FRAME calendar NO-PAUSE.
RETURN.

PROCEDURE set_day.
   find tt where tt.h_widget = SELF NO-ERROR.
   the_day = tt.h_num - (WEEKDAY(DATE(cal_month,1,cal_year)) - 1).
   ASSIGN s_date = DATE(cal_month , the_day, cal_year).
   APPLY "GO" TO btn_next IN FRAME calendar.
END PROCEDURE.

PROCEDURE entry_set_day.
   find tt where tt.h_widget = SELF NO-ERROR.
   the_day = tt.h_num - (WEEKDAY(DATE(cal_month,1,cal_year)) - 1).
   ASSIGN s_date = DATE(cal_month , the_day, cal_year).
END PROCEDURE.

PROCEDURE update_calendar.
   DEFINE VARIABLE btn_nextmonth AS INT NO-UNDO.
   
   ASSIGN
      btn_nextmonth = IF cal_month = 12 THEN 1 ELSE cal_month + 1
      start_day     = WEEKDAY(DATE(STRING(cal_month) + "/01/" + 
		      STRING(cal_year)))
      end_day       = DAY(DATE(STRING(btn_nextmonth) + "/01/" + 
		      STRING(cal_year)) - 1)
      the_day       = 1
      the_month     = month_string[cal_month]
      the_year      = STRING(cal_year,"9999").
    DISPLAY the_month the_year WITH FRAME calendar.

    FOR EACH tt:
	tt.h_WIDGET:SCREEN-VALUE = 
	    IF tt.h_num < start_day OR the_day > end_day THEN " " 
	    ELSE IF DAY(TODAY) = the_day THEN "(" + STRING(the_day) + ")"
	    ELSE STRING(the_day,"z9").

	IF tt.h_num >= start_day THEN the_day = the_day + 1.
    END.        
END PROCEDURE. /* update_calendar */







