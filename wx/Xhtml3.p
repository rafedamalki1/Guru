/*XHTML3.P*/
DEFINE OUTPUT PARAMETER htmlbody AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
FIND FIRST PERSONALTAB WHERE personaltab.personalkod = "ao" NO-LOCK NO-ERROR.
IF NOT AVAILABLE PERSONALTAB THEN RETURN. 
/*"Content-Type: text/html; charset=iso-8859-1".*/
RUN link_UI.
CREATE tidut.
ASSIGN
tidut.UT = "<html><head><meta http-equiv='Content-Type' content='text/html; charset=windows-1252'>
<meta name='GENERATOR' content='Microsoft FrontPage 4.0'>
<meta name='ProgId' content='FrontPage.Editor.Document'>
<title>Ny sida 1</title></head><body>".
CREATE tidut.
ASSIGN
tidut.UT = "p align='CENTER'>".
CREATE tidut.
ASSIGN
tidut.UT ="<img border='0' src='file:\\server3\d\delad\pro9\guru\Ctid\bilder\vasa2000.bmp'>".
CREATE tidut.
ASSIGN
tidut.UT ="</p'>".
CREATE tidut.
ASSIGN
tidut.UT = "<CENTER><FONT FACE='arial'><p>Andersson Olsson på plats 2971</p></CENTER>".
CREATE tidut.
ASSIGN
tidut.UT ="<FONT FACE='arial'>".
RUN poster_UI.
CREATE tidut.
ASSIGN
tidut.UT ="</font></p></body></HTML>".
CREATE tidut.
ASSIGN
tidut.UT = "<IMG src='file:\\server3\d\delad\PRO9\GURU\Ctid\BILDER\vasa2000.bmp'>".

FOR EACH tidut:
   htmlbody = htmlbody + tidut.UT + CHR(10).
END.

OUTPUT TO T.HTML. 
  
FOR EACH tidut:
   PUT UNFORMATTED tidut.UT SKIP.
END.
kommando = SEARCH("t.HTML").
IF kommando = ? THEN DO:          
   MESSAGE "Hittade inte filen" VIEW-AS ALERT-BOX.   
END.
/*ELSE RUN OPENDOC.P (kommando,"","",NO).   */
PAUSE 5.

PROCEDURE radbryt_UI:
   CREATE tidut.
   ASSIGN
   tidut.UT = "<BR>".
END PROCEDURE.
PROCEDURE linje_UI:
   CREATE tidut.
   ASSIGN
   tidut.UT = "<HR>".
END PROCEDURE.
PROCEDURE link_UI:
   CREATE tidut.
   ASSIGN
   tidut.UT = "<A HREF = 'http://www.elpool.se'>Elpool </A>".
   CREATE tidut.
   tidut.UT = "<img border='0' src='file:\\server3\d\delad\pro9\guru\Ctid\bilder\elpoollogga.gif'></p>".
END PROCEDURE.


PROCEDURE poster_UI:
   RUN linje_UI.   
   RUN radbryt_UI.  
   CREATE tidut.
   ASSIGN
   tidut.UT = PERSONALTAB.FORNAMN.
   RUN radbryt_UI.
   CREATE tidut.
   ASSIGN
   tidut.UT = PERSONALTAB.EFTERNAMN.
   RUN radbryt_UI.
   CREATE tidut.
   ASSIGN
   tidut.UT = "Datum   " + STRING(TODAY,"99999999").
   RUN radbryt_UI.
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   tidut.UT = "Kundnr  " + STRING(PERSONALTAB.PERSONALKOD).
   RUN radbryt_UI.
   RUN linje_UI.
END PROCEDURE.


  
