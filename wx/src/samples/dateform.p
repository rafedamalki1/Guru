/*********************************************************
***************************
        PROCEDURE: dateform.p

        PURPOSE:   Returns current date format

        SYNTAX:    "RUN samplesdateform.p"

        REMARKS:   This code displays the current date
format
                   (-d startup parameter)

        PARAMETERS:NONE

        AUTHORS:   Judy Rothermal
        DATE:      February 1993

        LAST INSPECTED:
        INSPECTED BY:

 *********************************************************
***************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 -
All Rights Reserved.            */
 
/*Co   de_Start*/

DEF VAR dt-format AS CHAR.
DEF BUTTON bOK LABEL "OK" AUTO-ENDKEY.

dt-format =
   ENTRY(LOOKUP(STRING(DATE(2,1,1903)) ,
        
"01/02/03,01/03/02,02/01/03,02/03/01,03/01/02,03/02/01") ,
         "dmy     ,dym     ,mdy     ,myd     ,ydm    
,ymd     ").
DISPLAY dt-format LABEL "Current Date Format"  SKIP(1) bOK
   WITH FRAME its-a-frame CENTERED ROW 5 VIEW-AS
DIALOG-BOX.
ENABLE ALL WITH FRAME its-a-frame.
WAIT-FOR CHOOSE OF bOK.
HIDE ALL NO-PAUSE.


