/************************************************************************************
	PROCEDURE: field.p

	PURPOSE:   Field Dictionary information is displayed with this
                   program

	SYNTAX:    "RUN samples/field.p"

	REMARKS:   This code reads the dictionary definition for a given
		   file.field and displays all information for that field.

	PARAMETERS: 

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/

DEFINE SHARED VARIABLE fnum       AS LOGICAL             NO-UNDO.
DEFINE SHARED VARIABLE fdb        AS CHAR FORMAT "x(12)" NO-UNDO.
DEFINE SHARED VARIABLE ffile      AS CHAR FORMAT "x(32)" NO-UNDO.
DEFINE SHARED VARIABLE ffield     AS CHAR FORMAT "x(32)" NO-UNDO.
DEFINE SHARED VARIABLE fformat    AS CHAR                NO-UNDO.
DEFINE SHARED VARIABLE flabel     AS CHAR                NO-UNDO.
DEFINE SHARED VARIABLE fdesc      AS CHAR                NO-UNDO.
DEFINE        VARIABLE dbt        AS CHAR LABEL "Type"   NO-UNDO.


FIND DICTDB._db WHERE _db-name =
      (IF DBTYPE(fdb) = "PROGRESS" THEN ? ELSE fdb) NO-ERROR.
IF NOT AVAILABLE dictdb._db   THEN DO:
   MESSAGE
      "This field is not defined in the dictionary (probably a local variable)".
   BELL.
END.
ELSE DO FOR DICTDB._field:
   FIND DICTDB._file OF _db WHERE _file-name = ffile .
   FIND DICTDB._field OF _file WHERE _field-name = ffield.
   dbt = DBTYPE(fdb).
   FORM
	 fdb            COLON 12 LABEL "Database" VIEW-AS TEXT
	 dbt            VIEW-AS TEXT
	_File-name      COLON 12 VIEW-AS TEXT
	_Field-Name     COLON 12 VIEW-AS TEXT
	_Data-Type      COLON 43 VIEW-AS TEXT
	_decimals       COLON 43 VIEW-AS TEXT
	_Format         COLON 12 VIEW-AS TEXT
	_Mandatory      COLON 43 VIEW-AS TEXT
	_Label          COLON 12 VIEW-AS TEXT
	_Col-label      COLON 12 VIEW-AS TEXT
	_Order          COLON 43 VIEW-AS TEXT
	_Initial        COLON 12 VIEW-AS TEXT
	_Extent         COLON 43 VIEW-AS TEXT
	_field._Valexp  COLON 8 FORMAT "x(62)" VIEW-AS TEXT
	_field._Valmsg  COLON 8 FORMAT "x(62)" VIEW-AS TEXT
	_Help           COLON 6 FORMAT "x(63)" VIEW-AS TEXT
	_Field._Desc    COLON 6 FORMAT "x(64)" VIEW-AS TEXT
        WITH 1 DOWN  SIDE-LABELS ROW 3 COLUMN 5 OVERLAY 
        TITLE "Dictionary-data".
        
   DISPLAY fdb dbt  _file-name _field-name _data-type _extent
           _Decimals  _Format  _Label   _Col-label  _Initial
	   _Mandatory   _Order   _Field._Valexp   _Field._Valmsg
	   _Help  _Field._Desc.

   PAUSE MESSAGE " Press any key to return.".
   HIDE  NO-PAUSE.
END.










