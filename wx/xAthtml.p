/*ATHTML.P*/
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
DEFINE SHARED VARIABLE epostut AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE Profile  AS CHARACTER.      /* Profile name for sender */
DEFINE VARIABLE Address  AS CHARACTER.      /* Email address of recipient */
DEFINE VARIABLE avsandare  AS CHARACTER NO-UNDO.
DEFINE VARIABLE Subject  AS CHARACTER.      /* Subject of email */
DEFINE VARIABLE Body     AS CHARACTER.      /* Body text */
DEFINE VARIABLE Filpath  AS CHARACTER.      /* Name of file to attach */
DEFINE VARIABLE Filnamn  AS CHARACTER.      /* Name only  */
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO. 
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE Ereslut  AS LOGICAL.        /* Email status  */
DEFINE VARIABLE Emailtxt AS CHARACTER.      /* Status txt  */



DEFINE SHARED TEMP-TABLE tidutE
   FIELD UT AS CHARACTER FORMAT "X(132)"
   FIELD BOTYP AS CHARACTER
   FIELD STADSDEL AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD HUSOBJEKT AS INTEGER
   INDEX UT IS PRIMARY BOTYP OMRADE STADSDEL.

DEFINE SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)". 
DEFINE SHARED TEMP-TABLE tidut2
   FIELD UT AS CHARACTER FORMAT "X(132)". 
DEFINE SHARED TEMP-TABLE tidut3
   FIELD UT AS CHARACTER FORMAT "X(132)". 
DEFINE SHARED TEMP-TABLE tidut4
   FIELD UT AS CHARACTER FORMAT "X(132)". 
DEFINE TEMP-TABLE tidut5
   FIELD UT AS CHARACTER FORMAT "X(132)".
RUN ZEEKPATH.P.   
CREATE tidut5.
ASSIGN
tidut5.UT = "<html><head><title>Visar utskrift</title></head><body bgcolor='F2F2FF'><p>&nbsp;</p>". 
/*namn adress*/
RUN radbryt_UI.
FIND FIRST tidut2 NO-ERROR.
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><B>" + tidut2.UT + "</B></CENTER>".
DELETE tidut2.
FOR EACH tidut2:
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<font size 26 face='Ariel'><B>" + tidut2.UT + "</B>".
   RUN radbryt_UI.
END.
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><P><B>Dessa objekt motsvarar dina sökkriterier.</B></P></CENTER>".
/*data VILLOR*/

FIND FIRST tidut4 NO-ERROR.
IF AVAILABLE tidut4 THEN DO:
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<P STYLE='( text-align: left; font-family: Ariel)'>
   <TABLE><COLGROUP SPAN='8' ></COLGROUP>
     <THEAD>
     <TR>
     <TH ID='N1'ALIGN=LEFT>" + SUBSTRING(tidut4.UT,utnr[1],utnr[4] - (utnr[1])) + "</TH>
     <TH ID='F2'ALIGN=LEFT>" + SUBSTRING(tidut4.UT,utnr[4],utnr[9] - (utnr[4])) + "</TH>
     <TH ID='E3'ALIGN=LEFT>" + SUBSTRING(tidut4.UT,utnr[9],utnr[10] - (utnr[9])) + "</TH>
     <TH ID='G5'ALIGN=LEFT>" + SUBSTRING(tidut4.UT,utnr[10],utnr[11] - (utnr[10])) + "</TH>
     <TH ID='M7'ALIGN=LEFT>" + SUBSTRING(tidut4.UT,utnr[11],utnr[12] - (utnr[11])) + "</TH>
     <TH ID='S7'ALIGN=LEFT>" + SUBSTRING(tidut4.UT,utnr[12],utnr[13] - (utnr[12])) + "</TH>
     <TH ID='H7'ALIGN=LEFT>" + SUBSTRING(tidut4.UT,utnr[13],utnr[14] - (utnr[13])) + "</TH>
     <TH ID='P8'ALIGN=LEFT>" + SUBSTRING(tidut4.UT,utnr[14]) + "</TH>                             
     </TR> 
   </THEAD>".
   CREATE tidut5.
   ASSIGN
   tidut5.UT = 
   "<P STYLE='( text-align: left; font-family: Ariel)'><TBODY>".
   FOR EACH tidutE WHERE tidutE.BOTYP = "VILLOR" :
      CREATE tidut5.
      ASSIGN
      tidut5.UT = 
      "<TR>
      <TH HEADERS='N1'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[1],utnr[4] - (utnr[1])) + "</TH>
      <TH HEADERS='F2'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[4],utnr[9] - (utnr[4])) + "</TH>
      <TH HEADERS='E3'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[9],utnr[10] - (utnr[9])) + "</TH>
      <TH HEADERS='G5'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[10],utnr[11] - (utnr[10])) + "</TH>
      <TH HEADERS='M7'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[11],utnr[12] - (utnr[11])) + "</TH>
      <TH HEADERS='S7'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[12],utnr[13] - (utnr[12])) + "</TH>
      <TH HEADERS='H7'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[13],utnr[14] - (utnr[13])) + "</TH>
      <TH HEADERS='P8'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[14]) + "</TH>                             
      </TR>".
      DELETE tidutE.
   END.
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "</TBODY> </TABLE>".   

END.
/*ÖVRIGA*/
FIND FIRST tidut3 NO-ERROR.
IF AVAILABLE tidut3 THEN DO:
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<P STYLE='( text-align: left; font-family: Ariel)'>
   <TABLE><COLGROUP SPAN='8' ></COLGROUP>
     <THEAD>
     <TR>
     <TH ID='N1'ALIGN=LEFT>" + SUBSTRING(tidut3.UT,utnr[1],utnr[4] - (utnr[1])) + "</TH>
     <TH ID='F2'ALIGN=LEFT>" + SUBSTRING(tidut3.UT,utnr[4],utnr[5] - (utnr[4])) + "</TH>
     <TH ID='E3'ALIGN=LEFT>" + SUBSTRING(tidut3.UT,utnr[5],utnr[6] - (utnr[5])) + "</TH>
     <TH ID='G5'ALIGN=LEFT>" + SUBSTRING(tidut3.UT,utnr[6],utnr[7] - (utnr[6])) + "</TH>
     <TH ID='M7'ALIGN=LEFT>" + SUBSTRING(tidut3.UT,utnr[7],utnr[8] - (utnr[7])) + "</TH>
     <TH ID='P8'ALIGN=LEFT>" + SUBSTRING(tidut3.UT,utnr[8]) + "</TH>                             
     </TR> 
   </THEAD>".
   CREATE tidut5.
   ASSIGN
   tidut5.UT = 
   "<P STYLE='( text-align: left; font-family: Ariel)'><TBODY>".
   FOR EACH tidutE:
      CREATE tidut5.
      ASSIGN
      tidut5.UT = 
      "<TR>
      <TH HEADERS='N1'ALIGN=LEFT>" + SUBSTRING(tidutE.UT,utnr[1],utnr[4] - (utnr[1])) + "</TH>
      <TH HEADERS='F2'ALIGN=LEFT>" + TRIM(SUBSTRING(tidutE.UT,utnr[4],utnr[5] - (utnr[4]))) + "</TH>
      <TH HEADERS='E3'ALIGN=LEFT>" + TRIM(SUBSTRING(tidutE.UT,utnr[5],utnr[6] - (utnr[5]))) + "</TH>
      <TH HEADERS='G5'ALIGN=LEFT>" + TRIM(SUBSTRING(tidutE.UT,utnr[6],utnr[7] - (utnr[6]))) + "</TH>
      <TH HEADERS='M7'ALIGN=LEFT>" + TRIM(SUBSTRING(tidutE.UT,utnr[7],utnr[8] - (utnr[7]))) + "</TH>                       
      <TH HEADERS='P8'ALIGN=LEFT>" + TRIM(SUBSTRING(tidutE.UT,utnr[8])) + "</TH>                      
      </TR>".
      DELETE tidutE.
   END.
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "</TBODY> </TABLE>".   
END.

/**********************BilObjekt******************/
CREATE tidut5.
ASSIGN
tidut5.UT = "<p align='CENTER'><img border='0' src='file:\\NTSERVER2\delad\pro9\guru\ctid\bilder\vasa2000.bmp'</p>".
CREATE tidut5.
RUN linje_UI. 
/*********************Slut*********************/


CREATE tidut5.
ASSIGN
tidut5.UT = "<font size 26 face='Ariel'><P><B>Är bostaden intressant för dig?</B></P>".
RUN radbryt_UI.
/*FORM*/

CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><form action='MAILTO:anders@elpool.se' method='post' enctype='text/plain'>Ja<input type='checkbox' name='Ja' unchecked><br><br>Nej<input type='checkbox' name='Nej' unchecked></CENTER>".
RUN radbryt_UI.
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><input type='submit' value='submit' name='B1'></FORM></CENTER>".
/*SLUT*/

CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><P>Kryssa i ditt svar och tryck på Skicka så mailas det automatiskt tillbaka.</P></CENTER>".
RUN radbryt_UI.
RUN poster_UI.
CREATE tidut5.
ASSIGN
tidut5.UT ="</font></p></body></HTML>".
kommando = SESSION:TEMP-DIRECTORY + pkod + ".HTML".
OUTPUT TO VALUE(kommando).
FOR EACH tidut5:
  PUT UNFORMATTED tidut5.UT SKIP.
END.
OUTPUT CLOSE.
kommando = SEARCH(kommando).
IF kommando = ? THEN DO:          
   MESSAGE "Hittade inte filen" VIEW-AS ALERT-BOX.   
END.
ELSE DO:
   IF epostut = FALSE THEN DO:  
      RUN OPENDOC.P (kommando,"","",NO).   
      PAUSE 2.
   END.
   ELSE DO:
   
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR. 
      ASSIGN
      Address = PERSONALTAB.E-POST
      Subject = "Söker du fortfarande nytt boende? Vi kanske kan hjälpa dig!"
      Body = "Vi på Erik Olsson Fastighetsförmedling har tidigare varit i kontakt med dig när du" + CHR(10) +
             "blev medlem i Bostadsguiden. Om det är så att du fortfarande letar efter en ny bostad," + CHR(10) +
             "så vill vi gärna presentera detta objekt för dig. Det passar in på de sökkriterier du" + CHR(10) +
             "angivit. Vi på Erik Olsson hoppas att vi kan vara behjälpliga så att du finner det " + CHR(10) +
             "boende du söker.".
      /*avsandare = "ulrica@erikolsson.se".       */
      Filpath = LC(kommando).
      Filnamn = LC(pkod + ".HTML").      
      RUN ZEPOSTN.P (INPUT  Profile,
                     INPUT  Address,
                     INPUT  avsandare,
                     INPUT  Subject,
                     INPUT  Body,
                     INPUT  Filpath,
                     INPUT  Filnamn,
                     INPUT  Guru.Konstanter:globanv,
                     INPUT  globforetag,
                     OUTPUT Ereslut,
                     OUTPUT Emailtxt). 

   END.
END.

PROCEDURE radbryt_UI:
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<BR>".
END PROCEDURE.

PROCEDURE linje_UI:
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<HR>".
END PROCEDURE.

PROCEDURE poster_UI:
   RUN radbryt_UI.  
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<p align left><font size 26 face='Ariel'><P>Med vänliga hälsningar</P>".
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<p align left><font size 26 face='Ariel'><P>Erik Olsson Fastighetsförmedling/Poolen</P>".
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<p align left><font size 26 face='Ariel'><P>Ulrica Aggestål, ulrica@erikolsson.se, 08-5555 1340</P>".
   RUN radbryt_UI.
   
   RUN radbryt_UI.
   RUN radbryt_UI.
   RUN linje_UI.
END PROCEDURE.

/*HUVUD*/
/*
<head>
		<meta http-equiv="content-type" content="text/html;charset=iso-8859-1">
	</head>
*/	

/*
CREATE tidut5.
ASSIGN
tidut5.UT = "<H2 align=center>Söker du fortfarande nytt boende? Vi kanske kan hjälpa dig!</H2>".
RUN radbryt_UI.
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><P><I>Vi på Erik Olsson Fastighetsförmedling har tidigare varit i kontakt med dig när</I> 
</P></CENTER>".
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><P><I> du blev medlem i Bostadsguiden. Om det är så att du fortfarande letar efter en</I> 
</P></CENTER>".
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><P><I> ny bostad, så vill vi gärna presentera detta objekt för dig. Det passar in på</I> 
</P></CENTER>".
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><P><I>de sökkriterier du angivit. Vi på Erik Olsson hoppas att vi kan vara behjälpliga</I> 
</P></CENTER>".
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><P><I>så att du finner det boende du söker.</I> 
</P></CENTER>".
RUN radbryt_UI.
*/

/*RUN link_UI.*/
/*CREATE tidut5.
   ASSIGN
   tidut5.UT = "Datum   " + STRING(TODAY,"99999999").*/
/*
PROCEDURE link_UI:
   CREATE tidut5.
   ASSIGN
   tidut5.UT = "<A HREF = 'http://www.elpool.se'>Elpool </A>".
   CREATE tidut5.
   tidut5.UT = "<img border='0' src='file:\\NTSERVER2\delad\pro9\guru\ctid\bilder\elpoollogga.gif'></p>".
END PROCEDURE. */
/*CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><font size 26 face='Ariel'><p>Anders Olsson på plats 2971</p></CENTER>".
CREATE tidut5.
ASSIGN
tidut5.UT ="<font face='Ariel'>".
RUN poster_UI. */
  
/*
CREATE tidut5.
ASSIGN
tidut5.UT = "<CENTER><input type="button" value="Skicka" name="B1"></CENTER>".*/


