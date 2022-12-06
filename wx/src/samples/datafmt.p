/************************************************************************************
        PROCEDURE: datafmt.p

        PURPOSE:   Demonstrates formatting

        SYNTAX:    "RUN samples/datafmt.p"

        REMARKS:   This program illustrates data item formatting

        PARAMETERS:NONE

        AUTHORS:   Progress Consulting
        DATE:      March 1993

        LAST INSPECTED:
        INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.            */
 /*Code_Start*/

/* datafmt.p - Illustrate data item formatting */
DEFINE VARIABLE result AS CHAR FORMAT "x(25)".
DEFINE VARIABLE fmt    AS CHAR  FORMAT "x(23)" LABEL "Format".
DEFINE VARIABLE cval   AS CHAR FORMAT "x(15)" LABEL "Character Value".
DEFINE VARIABLE daval  AS DATE FORMAT "99/99/9999"   LABEL "Date Value".
DEFINE VARIABLE deval  AS DECIMAL FORMAT "->>>>>>>>>>9.9999999999"
                                                       LABEL "Decimal Value".
DEFINE VARIABLE ival   AS INTEGER FORMAT "->>>>>>>>>>" LABEL "Integer Value".
DEFINE VARIABLE lval   AS LOGICAL FORMAT "true/false"  LABEL "Logical Value".
DEFINE VARIABLE rval   AS RECID FORMAT ">>>>>>>>>>"    LABEL "Recid Value".

DEFINE BUTTON bOK LABEL "OK" AUTO-GO.
DEFINE BUTTON bQUIT LABEL "EXIT" AUTO-ENDKEY.

FORM "Character Value" AT 1 "Format" AT 20 "Formatted Result" AT 45 SKIP 
    cval NO-LABEL fmt NO-LABEL result AT 45 NO-LABEL 
    SKIP(1) SPACE(2) bOK SPACE(2) bQUIT
    WITH FRAME frm-cval  ROW 9
    VIEW-AS DIALOG-BOX TITLE "Character Formatting".
FORM "Decimal Value"         AT 1 "Format" AT 30 SKIP 
    deval NO-LABEL fmt NO-LABEL SKIP 
    "Formatted Result:" result NO-LABEL
    SKIP(1) SPACE(2) bOK SPACE(2) bQUIT
    WITH FRAME frm-deval ROW 9
    VIEW-AS DIALOG-BOX TITLE "Decimal Formatting".
FORM "Date Value"         AT 1 "Format" AT 20 "Formatted Result" AT 45 SKIP
    daval NO-LABEL fmt NO-LABEL result AT 45 NO-LABEL
    SKIP(1) SPACE(2) bOK SPACE(2) bQUIT
    WITH FRAME frm-daval ROW 9
    VIEW-AS DIALOG-BOX TITLE "Date Formatting".
FORM "Integer Value"         AT 1 "Format" AT 20 "Formatted Result" AT 45 SKIP
    ival NO-LABEL fmt NO-LABEL result AT 45 NO-LABEL 
    SKIP(1) SPACE(2) bOK SPACE(2) bQUIT
    WITH FRAME frm-ival ROW 9
    VIEW-AS DIALOG-BOX TITLE "Integer Formatting".
FORM "Logical Value"         AT 1 "Format" AT 20 "Formatted Result" AT 45 SKIP
    lval NO-LABEL fmt NO-LABEL  result AT 45 NO-LABEL
    SKIP(1) SPACE(2) bOK SPACE(2) bQUIT
    WITH FRAME frm-lval ROW 9
    VIEW-AS DIALOG-BOX TITLE "Logical Formatting".
FORM "Recid Value"         AT 1 "Format" AT 20 "Formatted Result" AT 45 SKIP
    rval NO-LABEL fmt NO-LABEL result AT 45 NO-LABEL 
    SKIP(1) SPACE(2) bOK SPACE(2) bQUIT
    WITH FRAME frm-rval ROW 9
    VIEW-AS DIALOG-BOX TITLE "Recid Formatting".

DEFINE VARIABLE dt AS CHARACTER INIT "c" VIEW-AS RADIO-SET
   RADIO-BUTTONS "Character", "C",
                 "Date", "da",
                 "Decimal", "de",
                 "Integer", "i",
                 "Logical", "l",
                 "RECID", "r".
DEFINE VAR no_more AS LOGICAL NO-UNDO.
no_more = NO.
FORM "What data type would you like to format?" 
      SKIP SPACE(3) dt NO-LABEL SPACE(5) bOK SPACE(2) bQUIT SKIP
      WITH FRAME frm-input ROW 2 NO-LABELS
      VIEW-AS DIALOG-BOX .

REPEAT:
 UPDATE dt bOK bQUIT WITH FRAME frm-input.
 fmt = "".
      IF dt EQ "c"  THEN {samples/datafmt.i &field = cval}
 ELSE IF dt EQ "da" THEN {samples/datafmt.i &field = daval}
 ELSE IF dt EQ "de" THEN {samples/datafmt.i &field = deval}
 ELSE IF dt EQ "i"  THEN {samples/datafmt.i &field = ival}
 ELSE IF dt EQ "l"  THEN {samples/datafmt.i &field = lval}
 ELSE IF dt EQ "r"  THEN {samples/datafmt.i &field = rval}
 ELSE MESSAGE "Data type entered is invalid - please retry.".
END.
HIDE FRAME frm-input NO-PAUSE.






