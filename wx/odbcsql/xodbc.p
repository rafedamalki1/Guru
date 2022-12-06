/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: XODBC.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2004.04.06 12:21 ELPAO   
     Modified: 2004.04.06 12:56 ELPAO    
     Modified: 2004.04.07 12:38 ELPAO    
     Modified: 
*/
DEFINE VARIABLE comrec AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE comcon AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE comcommand AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE comrec2 AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE comcommand2 AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE odbcdsn AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcserver AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcuserid AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcpassword AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcnull AS CHARACTER NO-UNDO.
DEFINE VARIABLE tabvar AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE sqltemp
   FIELD tint1 AS INTEGER
   FIELD tch1 AS CHARACTER
   FIELD tch2 AS CHARACTER
   FIELD tch3 AS CHARACTER
   FIELD tch4 AS CHARACTER
   FIELD tch5 AS CHARACTER
   FIELD tch6 AS CHARACTER
   FIELD tch7 AS CHARACTER
   FIELD tch8 AS CHARACTER
   FIELD tch9 AS CHARACTER
   FIELD tch10 AS CHARACTER
   FIELD tch11 AS CHARACTER
   FIELD tch12 AS CHARACTER
   FIELD tch13 AS CHARACTER
   FIELD tch14 AS CHARACTER
   FIELD tch15 AS CHARACTER
   INDEX tch1 AS PRIMARY tch1.

/* Create the connection object for the link to SQL */
CREATE "ADODB.Connection" comcon.
/* Create a recordset object ready to return the data */
CREATE "ADODB.RecordSet" comrec.
/* Create a command object for sending the SQL statement */
CREATE "ADODB.Command" comcommand.
/* Change the below values as necessary */
ASSIGN 
odbcdsn = "jalle" /* The ODBC DSN */
odbcserver = "pc009" /* The name of the server hosting the SQL DB and DSN */
odbcuserid = "jalle" /* The user id for access to the SQL Database */
odbcpassword = "jalle". /* Password required by above user-id */

comcon:Open ("data source=" + odbcdsn + ";server=" +
      odbcserver, odbcuserid, odbcpassword, 0 ) NO-ERROR.

If ( ERROR-STATUS:NUM-MESSAGES > 0 ) THEN DO:
   MESSAGE "Error: Could not establish connection.".
END.
ELSE DO:
    MESSAGE "Connected!".
    
/*Hämta poster*/
/*    odbcquery = "SELECT * FROM member;".                    */
/*    Assign                                                  */
/*    comcommand:ActiveConnection = comcon                    */
/*    comcommand:CommandText = odbcquery                      */
/*    comcommand:CommandType = 1 /* adCmdText */              */
/*    comcon:CursorLocation = 3 /* adUseClient */             */
/*    comrec:CursorType = 3 /* adOpenStatic*/                 */
/*    comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ). */
/*    DO WHILE NOT comrec:eof:                                */
/*       CREATE sqltemp.                                      */
/*       tint1 = comrec:FIELDS("member_id"):VALUE.            */
/*       tch1 = comrec:FIELDS("firstname"):VALUE.             */
/*       tch2 = comrec:FIELDS("lastname"):VALUE.              */
/*       tch3 = comrec:FIELDS("login"):VALUE.                 */
/*       tch4 = comrec:FIELDS("password"):VALUE.              */
/*       comrec:MoveNext.                                     */
/*    END.                                                    */
/*     FOR EACH sqltemp WHERE NO-LOCK:                        */
/*       DISP sqltemp.                                        */
/*    END.                                                    */
   
/*Lägga till en post*/
/*    odbcquery = "INSERT INTO member (firstname,lastname,login,password) VALUES ('Lena','Olsson','ll','ll');". */
/*    Assign                                                                                                    */
/*    comcommand:ActiveConnection = comcon                                                                      */
/*    comcommand:CommandText = odbcquery                                                                        */
/*    comcommand:CommandType = 1 /* adCmdText */                                                                */
/*    comcon:CursorLocation = 3 /* adUseClient */                                                               */
/*    comrec:CursorType = 3 /* adOpenStatic*/                                                                   */
/*    comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).                                                   */
   
/*Ta bort en post*/
/*    odbcquery = "DELETE from member WHERE firstname = 'Niklas' AND LOGIN = 'nn';". */
/*    Assign                                                                         */
/*    comcommand:ActiveConnection = comcon                                           */
/*    comcommand:CommandText = odbcquery                                             */
/*    comcommand:CommandType = 1 /* adCmdText */                                     */
/*    comcon:CursorLocation = 3 /* adUseClient */                                    */
/*    comrec:CursorType = 3 /* adOpenStatic*/                                        */
/*    comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).                        */

   /*Uppdatera fält i tabell*/
/*    odbcquery = "UPDATE member SET firstname = 'Niklas', lastname = 'Jonsson' , login = 'nn' , */
/*                 password = 'nn' WHERE firstname = 'Germund';".                                */
/*    Assign                                                                                     */
/*    comcommand:ActiveConnection = comcon                                                       */
/*    comcommand:CommandText = odbcquery                                                         */
/*    comcommand:CommandType = 1 /* adCmdText */                                                 */
/*    comcon:CursorLocation = 3 /* adUseClient */                                                */
/*    comrec:CursorType = 3 /* adOpenStatic*/                                                    */
/*    comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).                                    */
   
/*    odbcquery = "sp_help member". */
/*    odbcquery = "sp_columns @table_name=member". */

   odbcquery = "sp_tables".
   Assign
   comcommand:ActiveConnection = comcon
   comcommand:CommandText = odbcquery
   comcommand:CommandType = 1 /* adCmdText */
   comcon:CursorLocation = 3 /* adUseClient */
   comrec:CursorType = 3 /* adOpenStatic*/
   comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).
   DO WHILE NOT comrec:eof:
      IF comrec:FIELDS(3):VALUE = "TABLE" THEN do:
         tabvar = comrec:FIELDS(2):VALUE.         
      END.
      comrec:MoveNext.                          
   END.
   IF tabvar NE "" THEN DO:  
      ASSIGN 
      comrec = ?   
      comcommand = ?. 
      CREATE "ADODB.RecordSet" comrec.
      CREATE "ADODB.Command" comcommand.
      odbcquery = "sp_columns @table_name=" + tabvar.
      Assign
      comcommand:ActiveConnection = comcon
      comcommand:CommandText = odbcquery
      comcommand:CommandType = 1 /* adCmdText */
      comcon:CursorLocation = 3 /* adUseClient */
      comrec:CursorType = 3 /* adOpenStatic*/
      comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).
      DO WHILE NOT comrec:eof:
         CREATE sqltemp.
         tch1 = "TABLE_QUALIFIER: " + comrec:FIELDS("TABLE_QUALIFIER"):VALUE.
         tch2 = "TABLE_OWNER: " + comrec:FIELDS("TABLE_OWNER"):VALUE.
         tch3 = "TABLE_NAME: " + comrec:FIELDS("TABLE_NAME"):VALUE.
         tch4 = "COLUMN_NAME: " + comrec:FIELDS("COLUMN_NAME"):VALUE.
         tch5 = "DATA_TYPE: " + comrec:FIELDS("DATA_TYPE"):VALUE.
         tch6 = "TYPE_NAME: " + comrec:FIELDS("TYPE_NAME"):VALUE.
         tch7 = "PRECISION: " + comrec:FIELDS("PRECISION"):VALUE.
         tch8 = "LENGTH: " + comrec:FIELDS("LENGTH"):VALUE.
         tch9 = "ORDINAL_POSITION: " + comrec:FIELDS("ORDINAL_POSITION"):VALUE.        
         comrec:MoveNext.
      END.
      FOR EACH sqltemp WHERE NO-LOCK:
         DISP 
         sqltemp.tch1 FORMAT "X(30)"
         sqltemp.tch2 FORMAT "X(30)"       
         sqltemp.tch3 FORMAT "X(30)"
         sqltemp.tch4 FORMAT "X(30)"
         sqltemp.tch5 FORMAT "X(30)"
         sqltemp.tch6 FORMAT "X(30)"
         sqltemp.tch7 FORMAT "X(30)" 
         sqltemp.tch8 FORMAT "X(30)"
         sqltemp.tch9 FORMAT "X(30)" SKIP(2) WITH 1 COL. 
      END.
   END.               
  /* Close the ADO connection */
   comcon:Close NO-ERROR.

END.

/* Don't forget to release the memory!! */
RELEASE OBJECT comcon NO-ERROR.
RELEASE OBJECT comcommand NO-ERROR.
RELEASE OBJECT comrec NO-ERROR.

ASSIGN 
comcon = ? 
comcommand = ? 
comrec = ?.
