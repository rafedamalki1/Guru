/* STARTload_df0.p
prowin32.exe -rx -p STARTload_df.p

https://knowledgebase.progress.com/articles/Knowledge/P147079
COMPILE XCODE
xcode [ -k key ] -d directory [ files] [ - ]
*/


DEFINE VARIABLE filnamn AS CHARACTER FORMAT  "x(68)" NO-UNDO.
DEFINE VARIABLE dbnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
DEFINE VARIABLE provag AS CHARACTER NO-UNDO.
DEFINE VARIABLE app_server_info AS CHARACTER NO-UNDO.

{VALDBDEF.I}
{NAMNDB.I}
dbnamn = namndb().
IF NOT CONNECTED(dbnamn) OR dbnamn  = ? THEN DO : 
   {VALDBALL.I}   
   
   INPUT FROM OS-DIR(Guru.Konstanter:guruvar) NO-ECHO.
   REPEAT:
      SET filnamn ^ ^. 
      IF filnamn MATCHES "appserverinfo.*" THEN DO: 
         app_server_info = ENTRY(2, filnamn, "."). 
         LEAVE.                
      END.
   END.
   INPUT CLOSE.
   OS-DELETE VALUE(Guru.Konstanter:guruvar + filnamn).          
   FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = app_server_info NO-ERROR.
   IF AVAILABLE valdbtemp THEN DO:      
      koppla = valdbtemp.DBPLATS + valdbtemp.DBNAMN.      
      CONNECT VALUE(koppla) NO-ERROR.
   END.
   IF namndb() = ? THEN DO:
      MESSAGE "Inge bra!"     VIEW-AS ALERT-BOX.   
      QUIT.
   END.      
END.    
{VERALIAS.I}
   

RUN SETKUND.P.

provag = Guru.Konstanter:wtidvar + "guru.df".
MESSAGE "GURU.DF i " provag
VIEW-AS ALERT-BOX.
IF SEARCH(provag) NE ? THEN DO:
   RUN ladda\STARTload_df.p (INPUT provag).   
END.
{DELALIAS.I}
MESSAGE "Slut!"
VIEW-AS ALERT-BOX.
QUIT.
   

