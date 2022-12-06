
/*------------------------------------------------------------------------
    File        : AppSpringConDb.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue May 02 10:48:58 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
 
koppla = valdbtemp.DBPLATS + valdbtemp.DBNAMN.
/*dbflex*/

IF app_server_info[11] = "true" THEN lognamvar =  " -ld rt9".

kopplaut = koppla + " " + lognamvar.
koppla = koppla + " " + lognamvar + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).

/*
IF Guru.Konstanter:AppSpringSet[13] BEGINS "-URL" THEN.
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
*/
IF LDBNAME(1) = ? THEN DO: 
   koppla = valdbtemp.DBCON.
   koppla = REPLACE(koppla,"www.guruonweb.se","webguru") NO-ERROR.
   koppla = REPLACE(koppla,"www2.guruonweb.se","webguru") NO-ERROR.
   
   kopplaut = koppla + " " + lognamvar.
   koppla = koppla + " " + lognamvar + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).  
   CONNECT VALUE(koppla) NO-ERROR. 
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + filnamn) APPEND.
   PUT UNFORMATTED kopplaut + " " + STRING(CONNECTED("rt9")) + " andra " + app_server_info[1] SKIP.
   DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT UNFORMATTED ERROR-STATUS:GET-NUMBER(ivar) " " TODAY " " STRING(TIME,"hh:mm:ss") SKIP.
      feltxt = ERROR-STATUS:GET-MESSAGE(ivar). 
      PUT feltxt SKIP.      
   END.     
   OUTPUT CLOSE.
END.
IF LDBNAME(1) = ? THEN DO:
   ivar = 1.
   OUTPUT TO VALUE(Guru.Konstanter:guruvar + filnamn) APPEND.
   PUT UNFORMATTED kopplaut + " " + STRING(CONNECTED("rt9")) + " " + app_server_info[1] SKIP.
   DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT UNFORMATTED ERROR-STATUS:GET-NUMBER(ivar) " " TODAY " " string(time,"hh:mm:ss") SKIP.
      feltxt = ERROR-STATUS:GET-MESSAGE(ivar). 
      PUT feltxt SKIP.      
   END.      
     
   OUTPUT CLOSE.
END.   


IF CONNECTED(LDBNAME(1)) THEN DO:
   {VERALIAS.I}  


END.   

/*
AppConDarwinh = AppDarwinhand:CONNECT(valdbtemp.APPCON,{APPCON1.i},{APPCON2.i},valdbtemp.GFORETAG) NO-ERROR.   
appcongran = apphandgran:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gfore).
appconass = Guru.Konstanter:apphandass:CONNECT(conappvarass,{APPCON1.i},{APPCON2.i},soktemp.SOKCHAR[1]) NO-ERROR.

appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetagvar) NO-ERROR.
appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.
appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i}, 
, OUTPUT AppSpringFel) NO-ERROR.   
appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.   
appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.
appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.      
appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.
appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR. 
appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.
*/ 
