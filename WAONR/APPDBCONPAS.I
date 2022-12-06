
/*------------------------------------------------------------------------
    File        : APPDBCONPAS.I
    Purpose     : 

    Syntax      :används ej

    Description : 

    Author(s)   : 
    Created     : Tue May 02 10:48:58 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/*Anders Olsson Elpool i Umeå AB  7 jun 2017 14:56:38 
första till Guru.Konstanter:appcon9 ej i Guru.Konstanter:appconpas ???

till   IF NOT CONNECTED("rt9") THEN DO: 
*/
koppla = valdbtemp.DBPLATS + valdbtemp.DBNAMN.
/*dbflex*/
lognamvar =  " -ld rt9".
kopplaut = koppla + " " + lognamvar.
koppla = koppla + " " + lognamvar + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).



IF pasoevar = TRUE THEN.
ELSE DO: 

   CONNECT VALUE(koppla) NO-ERROR.
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + filnamn) APPEND.
   PUT UNFORMATTED kopplaut + " " + STRING(CONNECTED("rt9")) + " första " + string(TIME,"hh:mm:ss") SKIP.
   DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT UNFORMATTED ERROR-STATUS:GET-NUMBER(ivar) " " TODAY " " string(time,"hh:mm:ss") SKIP.
      feltxt = ERROR-STATUS:GET-MESSAGE(ivar). 
      PUT UNFORMATTED feltxt SKIP.      
   END.     
   OUTPUT CLOSE.
END. 
IF NOT CONNECTED("rt9") THEN DO: 
   koppla = valdbtemp.DBCON.
   koppla = REPLACE(koppla,"www.guruonweb.se","webguru") NO-ERROR.
   koppla = REPLACE(koppla,"www2.guruonweb.se","webguru") NO-ERROR.
   
   kopplaut = koppla + " " + lognamvar.
   koppla = koppla + " " + lognamvar + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).  
   CONNECT VALUE(koppla) NO-ERROR. 
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + filnamn) APPEND.
   PUT UNFORMATTED kopplaut + " " + STRING(CONNECTED("rt9")) + " andra " + app_server_info SKIP.
   DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT UNFORMATTED ERROR-STATUS:GET-NUMBER(ivar) " " TODAY " " string(time,"hh:mm:ss") SKIP.
      feltxt = ERROR-STATUS:GET-MESSAGE(ivar). 
      PUT feltxt SKIP.      
   END.     
   OUTPUT CLOSE.
END.
IF NOT CONNECTED("rt9") THEN DO:
   ivar = 1.
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + filnamn) APPEND.
   PUT UNFORMATTED kopplaut + " " + STRING(CONNECTED("rt9")) + " " + app_server_info SKIP.
   DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT UNFORMATTED ERROR-STATUS:GET-NUMBER(ivar) " " TODAY " " string(time,"hh:mm:ss") SKIP.
      feltxt = ERROR-STATUS:GET-MESSAGE(ivar). 
      PUT feltxt SKIP.      
   END.      
     
   OUTPUT CLOSE.
END.   


IF CONNECTED("rt9") THEN DO:
   {VERALIAS.I}  
END.   
