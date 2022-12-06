/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: XMYSQLTEST.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2004.04.06 12:21 ELPAO   
     Modified: 2004.04.06 12:56 ELPAO    
     Modified: 
*/

Def var ObjRecordSet as com-handle no-undo.
Def var ObjConnection as com-handle no-undo.
Def var ObjCommand as com-handle no-undo.
Def var ODBC-DSN as character no-undo.
Def var ODBC-SERVER as character no-undo.
Def var ODBC-USERID as CHARACTER no-undo.
Def var ODBC-PASSWD as character no-undo.
Def var ODBC-QUERY as character no-undo.
DEF VAR ODBC-Add AS CHARACTER NO-UNDO.
DEF VAR ODBC-Delete AS CHARACTER NO-UNDO.
Def var ODBC-STATUS as character no-undo.
Def var ODBC-RECCOUNT as integer no-undo.
Def var ODBC-NULL as character no-undo.
Def var ODBC-CURSOR as integer no-undo.


/* Create the connection object for the link to SQL */
Create "ADODB.Connection" ObjConnection.
/* Create a recordset object ready to return the data */
Create "ADODB.RecordSet" ObjRecordSet.
/* Create a command object for sending the SQL statement */
Create "ADODB.Command" ObjCommand.

/* Change the below values as necessary */
Assign ODBC-DSN = "bg" /* The ODBC DSN */
ODBC-SERVER = "localhost" /* The name of the server
hosting the SQL DB and DSN */
ODBC-USERID = "elpool" /* The user id for access to the
SQL Database */
ODBC-PASSWD = "kaggen". /* Password required by above
user-id */

ObjConnection:Open ( "data source=" + ODBC-DSN + ";server=" +
      ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0 ) NO-ERROR.
If ( error-status:num-messages > 0 ) THEN DO:
   MESSAGE "Error: Could not establish connection.".
END.
ELSE DO: 
   MESSAGE "Connected!".

/*Hämta poster*/
   ODBC-Add = "SELECT * FROM kund;".
   Assign
   ObjCommand:ActiveConnection = ObjConnection
   ObjCommand:CommandText = ODBC-Add
   ObjCommand:CommandType = 1 /* adCmdText */
   ObjConnection:CursorLocation = 3 /* adUseClient */
   ObjRecordSet:CursorType = 3 /* adOpenStatic*/
   ObjRecordSet = ObjCommand:Execute( OUTPUT ODBC-NULL, "", 32 ).

   DO WHILE NOT ObjRecordSet:eof:
      DISP ObjRecordSet:FIELDS("fornamn"):VALUE WITH 1 COL.
      DISP ObjRecordSet:FIELDS("efternamn"):VALUE WITH 1 COL.
      DISP ObjRecordSet:FIELDS("kundnr"):VALUE WITH 1 COL.
      ObjRecordSet:MoveNext.

   END.

/*Lägga till en post*/
/*    ODBC-Add = "INSERT INTO kund (fornamn,efternamn,kundnr) VALUES ('Bengt','Eriksson','114');". */
/*    Assign                                                                                       */
/*    ObjCommand:ActiveConnection = ObjConnection                                                  */
/*    ObjCommand:CommandText = ODBC-Add                                                            */
/*    ObjCommand:CommandType = 1 /* adCmdText */                                                   */
/*    ObjConnection:CursorLocation = 3 /* adUseClient */                                           */
/*    ObjRecordSet:CursorType = 3 /* adOpenStatic*/                                                */
/*    ObjRecordSet = ObjCommand:Execute( OUTPUT ODBC-NULL, "", 32 ).                               */
   
   
   /* Close the ADO connection */
   ObjConnection:Close NO-ERROR.

END.

/* Don't forget to release the memory!! */
Release object ObjConnection NO-ERROR.
Release object ObjCommand NO-ERROR.
Release object ObjRecordSet NO-ERROR.

Assign ObjConnection = ? ObjCommand = ? ObjRecordSet = ?.
