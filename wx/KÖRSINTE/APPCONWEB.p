/*APPCONWEB.p*/
/*för webservice med IFS körs ej nu*/
DEFINE INPUT PARAMETER app_server_info AS CHARACTER.
DEFINE OUTPUT PARAMETER okconn AS LOGICAL NO-UNDO.
DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.

{VALDBDEF.I}
{VALDBALL.I}
FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = app_server_info NO-ERROR.
IF NOT AVAILABLE valdbtemp THEN DO:
   OUTPUT TO felapp.txt APPEND.
   PUT app_server_info.
   OUTPUT CLOSE.   
   RETURN.
END.
koppla = valdbtemp.DBPLATS + valdbtemp.DBNAMN.
koppla = koppla + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).

CONNECT VALUE(koppla) NO-ERROR. 
IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO:   
   koppla = valdbtemp.DBCON.
   koppla = REPLACE(koppla,"www.guruonweb.se","webguru") NO-ERROR.
   koppla = REPLACE(koppla,"www.guruonweb.se","webguru") NO-ERROR.
   koppla = koppla + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).  
   CONNECT VALUE(koppla) NO-ERROR. 
END.
okconn = CONNECTED(valdbtemp.DBNAMN).
IF okconn = FALSE THEN RETURN.

{VERALIAS.I}

