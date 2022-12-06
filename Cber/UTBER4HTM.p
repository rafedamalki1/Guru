/*
     Filename: UTBER4HTM.P
*/
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{REGVAR.I}
{BILDBERTEMP.I}
{SOKDEF.I}
{pdf_StartInc.i}

{TIDUTTT.I}

DEFINE TEMP-TABLE htmlut
   FIELD UT AS CHARACTER FORMAT "X(132)".

DEFINE INPUT PARAMETER table FOR tidut.
DEFINE INPUT PARAMETER kompsida AS LOGICAL NO-UNDO.
                              
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE bryt AS LOGICAL NO-UNDO.

DEFINE VARIABLE typnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER  NO-UNDO.
DEFINE VARIABLE bodydoc AS CHARACTER INITIAL "Berlista4" LABEL "Berlista4" NO-UNDO.
DEFINE VARIABLE sokvag AS CHARACTER NO-UNDO.
DEFINE VARIABLE startpos AS INTEGER NO-UNDO.
DEFINE VARIABLE counter AS INTEGER NO-UNDO.
DEFINE VARIABLE logga AS CHARACTER NO-UNDO.
DEFINE VARIABLE boldblack AS LOGICAL NO-UNDO.
DEFINE VARIABLE boldblue AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrltable AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE utberapph AS HANDLE NO-UNDO.
DEFINE VARIABLE w AS INTEGER NO-UNDO.
DEFINE VARIABLE h AS INTEGER NO-UNDO.
DEFINE VARIABLE a AS DECIMAL NO-UNDO.
DEFINE VARIABLE com AS CHARACTER NO-UNDO. 
DEFINE VARIABLE numkopp AS INTEGER NO-UNDO.
DEFINE VARIABLE filvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bben_id1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE bben_id2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE spann AS LOGICAL NO-UNDO.
utfil = SESSION:TEMP-DIRECTORY.        
{SESSIONTEMPDIR.I}
{AMERICANEUROPEAN.I}
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN utfil = webclienttempdir.
utfil = utfil + STRING(TIME) + "temp.pdf".
IF Guru.Konstanter:appcon THEN DO:
   RUN UTBERAPP.P PERSISTENT SET utberapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
END.
ELSE DO:
   RUN UTBERAPP.P PERSISTENT SET utberapph.
END.
/* RUN idhmt_UI IN utberapph (INPUT idgrp, OUTPUT bben_id1, OUTPUT bben_id2). */
{AONRUTSID.I}
EMPTY TEMP-table bildbertemp NO-ERROR.
FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF SUBSTRING(tidut.UT,1,15) = "Materiel/Upplag" OR SUBSTRING(tidut.UT,1,13) = "Byggprotokoll" THEN DO: 
         RUN bildberhmt_UI IN utberapph (INPUT Guru.Konstanter:globanv,INPUT valaonr,INPUT valomrade,OUTPUT table bildbertemp).
         FIND FIRST bildbertemp NO-LOCK NO-ERROR.
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      IF SUBSTRING(tidut.UT,1,12) = "Satsläggning" OR SUBSTRING(tidut.UT,1,13) = "Byggprotokoll" THEN DO: 
         RUN bildberhmt_UI IN utberapph (INPUT Guru.Konstanter:globanv,INPUT valaonr,INPUT valomrade,OUTPUT table bildbertemp).
         FIND FIRST bildbertemp NO-LOCK NO-ERROR.
      END.
   END.
   ELSE DO:
      IF SUBSTRING(tidut.UT,1,15) = "Materiel/upplag" OR SUBSTRING(tidut.UT,1,13) = "Byggprotokoll" THEN DO: 
         RUN bildberhmt_UI IN utberapph (INPUT Guru.Konstanter:globanv,INPUT valaonr,INPUT valomrade,OUTPUT table bildbertemp).
         FIND FIRST bildbertemp NO-LOCK NO-ERROR.
      END.
   END.
END.
IF AVAILABLE bildbertemp THEN DO:
   MESSAGE "Vill Ni se kopplade dokument och bilder i beredningen?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar.         
   IF svar = FALSE THEN EMPTY TEMP-table bildbertemp NO-ERROR.
END.

EMPTY TEMP-table htmlut NO-ERROR. 
/* FOR EACH tidut:                          */
/*    tidut.UT = REPLACE(tidut.UT,":"," "). */
/* END.                                     */
ASSIGN
spann = FALSE
logga = ?.


/*Skapa body*/
CREATE htmlut.
ASSIGN htmlut.UT = "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.0 Transitional//EN'>".                  
CREATE htmlut.
ASSIGN htmlut.UT = "<html><head><title>" + bodydoc + "</title>
<meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'>".
CREATE htmlut.
ASSIGN htmlut.UT = "<style type='text/css'>
   <!--
   @media print " + string('~{') + "
   body" + STRING('~{') + "margin:0pt 0pt 0pt 0pt; text-align:left; font:normal 10pt Courier New;}
   pre" + STRING('~{') + "margin-top:0pt; margin-bottom:0pt; font:normal 10pt Courier New;} 
   hr" + string('~{') + "border:0; height:1pt; color:black;}   
   .tdnopre" + string('~{') + "font:normal 9pt Courier New;}
   .page" + STRING('~{') + "margin:0pt 0pt 0pt 30pt;}
   .header" + string('~{') + "text-align:LEFT; font:bold 15pt Courier New; color:black;}   
   .borderbg pre" + string('~{') + "background-color:#FDFDFF; border-bottom:solid 1pt black; border-top:solid 1pt black; font:normal 10pt Courier New; color:#0000CC;}   
   .boldblue pre" + string('~{') + "font:bold 10pt Courier New; color:#0000CC;}   
   .boldblack pre" + string('~{') + "font:bold 10pt Courier New; color:#000000;}   
   .boldbg pre" + string('~{') + "background-color:#FDFDFF; font:bold 10pt Courier New; color:#0000CC;}   
   .mtrltable pre" + string('~{') + "background-color:#FDFDFF; font:normal 10pt Courier New; color:#000000;}}  
   a " + string('~{') + "text-decoration : underline; color : #0000ff;}

   @media SCREEN " + string('~{') + "
   body" + STRING('~{') + "margin:0px 0px 0px 0px; text-align:left; font:normal 12px Courier New;}
   pre" + STRING('~{') + "margin:0px 0px 0px 0px; font:normal 12px Courier New;} 
   hr" + string('~{') + "border:0; height:2px; color:black; width:600px;}   
   .tdnopre" + string('~{') + "font:normal 11px Courier New;}
   .page" + STRING('~{') + "margin:0px 0px 0px 50px;}
   .header" + string('~{') + "text-align:LEFT; font:bold 16px Courier New; color:black;}   
   .borderbg pre" + string('~{') + "background-color:#FDFDFF; border-bottom:solid 1px black; border-top:solid 1px black; font:normal 12px Courier New; color:#0000CC;}   
   .boldblue pre" + string('~{') + "font:bold 12px Courier New; color:#0000CC;}   
   .boldblack pre" + string('~{') + "font:bold 12px Courier New; color:#000000;}   
   .boldbg pre" + string('~{') + "background-color:#FDFDFF; font:bold 12px Courier New; color:#0000CC;}   
   .mtrltable pre" + string('~{') + "background-color:#FDFDFF; font:normal 12px Courier New; color:#000000;}}   
    -->
   </style>
</head>
<body bgcolor='#F5F5FF'><div class='page'>". 
ASSIGN
counter = 1
numkopp = 0.
RUN logga_UI.


CREATE htmlut.
ASSIGN htmlut.UT = "<table width='600' border='0' cellspacing='0' cellpadding='0'>". 
FOR EACH tidut NO-LOCK: 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF SUBSTRING(tidut.UT,1,15) = "Materiel/Upplag" THEN DO: 
         boldblack = TRUE.      
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      IF SUBSTRING(tidut.UT,1,12) = "Satsläggning" THEN DO: 
         boldblack = TRUE.      
      END.
   END.
   ELSE DO:
      IF SUBSTRING(tidut.UT,1,15) = "Materiel/upplag" THEN DO: 
         boldblack = TRUE.      
      END.
   END.
   /*Rubrik*/
   IF SUBSTRING(tidut.UT,1,13) = "Byggprotokoll" THEN DO: 
      boldblack = TRUE.      
   END.
   ELSE IF SUBSTRING(tidut.UT,1,10) = "Upplag nr" THEN DO:  
      boldblack = FALSE.
   END.
   ELSE IF SUBSTRING(tidut.UT,1,16) = "Spann/kabellängd" THEN DO:
      spann = TRUE.
   END.
   IF tidut.UT = ? THEN NEXT.
   /*SIDA*/
   IF tidut.UT = "" THEN NEXT.
   /* KOMPSIDA*/
   IF SUBSTRING(tidut.UT,132) = "$" THEN DO:
      IF kompsida = TRUE THEN DO:
         IF SUBSTRING(tidut.UT,1,3) = "***" THEN DO:
            ASSIGN
            boldblue = FALSE
            boldblack = FALSE
            mtrltable = FALSE.         
            ASSIGN htmlut.UT = "</table><br><table width='600' border='0' cellspacing='0' cellpadding='0'><tr><td align='left'><hr></td></tr>".       
         END.
      END.
      ELSE DO:
         ASSIGN
         boldblue = FALSE
         boldblack = FALSE.
         RUN bryt_UI.
      END.
   END.
   
   ELSE IF SUBSTRING(tidut.UT,43,18) = "Fri ID           :" 
      OR SUBSTRING(tidut.UT,43,18) =   "Stolp nr         :" 
      OR SUBSTRING(tidut.UT,43,18) =   "Skåp nr          :"
      OR SUBSTRING(tidut.UT,43,18) =   "Lina/Alus nr     :"
      OR SUBSTRING(tidut.UT,43,18) =   "Kabelnr          :"
      OR SUBSTRING(tidut.UT,43,18) =   "Obj nr           :"
      OR SUBSTRING(tidut.UT,43,18) =   "Byggnr           :" THEN DO:
      IF spann = FALSE THEN DO:
         IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
            CREATE htmlut.
            ASSIGN htmlut.UT = "<tr><td class='boldblack'><pre>" + tidut.UT + "</pre></td></tr>".
         END.
         ELSE DO:
            CREATE htmlut.
            ASSIGN htmlut.UT = "<tr><td><pre>" + tidut.UT + "</pre></td></tr>".         
         END.
      END.
      ELSE DO:
         CREATE htmlut.
         ASSIGN htmlut.UT = "<tr><td><pre>" + tidut.UT + "</pre></td></tr>".         
      END.
   END.
   ELSE IF SUBSTRING(tidut.UT,1,28) = "Grupp nr                   :" 
      OR SUBSTRING(tidut.UT,1,28) =   "Sektion/Sträcka            :"
      OR SUBSTRING(tidut.UT,1,28) =   "Ledningsdel                :" THEN DO:
      IF spann = FALSE THEN DO:
         IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
            CREATE htmlut.
            ASSIGN htmlut.UT = "<tr><td class='boldblack'><pre>" + tidut.UT + "</pre></td></tr>".
         END.
         ELSE DO:
            CREATE htmlut.
            ASSIGN htmlut.UT = "<tr><td><pre>" + tidut.UT + "</pre></td></tr>".         
         END.
      END.
      ELSE DO:
         CREATE htmlut.
         ASSIGN htmlut.UT = "<tr><td><pre>" + tidut.UT + "</pre></td></tr>".         
      END.
   END.
   ELSE IF SUBSTRING(tidut.UT,1,19) = "Materiel ej kopplat" THEN DO:
      CREATE htmlut.
      ASSIGN htmlut.UT = "<tr><td class='boldbg'><pre>" + tidut.UT + "</pre></td></tr>". 
   END.
   ELSE IF SUBSTRING(tidut.UT,1,6) = "P3-KOD" OR SUBSTRING(tidut.UT,1,3) = SUBSTRING(Guru.Konstanter:genk,1,3) THEN DO:
      ASSIGN
      mtrltable = TRUE
      boldblue = FALSE
      boldblack = FALSE.
      CREATE htmlut.
      ASSIGN htmlut.UT = "</table><br><table width='600' border='0' cellspacing='0' cellpadding='0'>
      <tr><td class='borderbg'><pre>" + tidut.UT + "</pre></td></tr>".       
   END.
   ELSE IF tidut.UT = ? THEN DO:
      CREATE htmlut.
      ASSIGN htmlut.UT = "<tr><td><pre></pre></td></tr>". 
   END.
   /*
   ELSE IF tidut.UT = "" THEN DO:
   END.
   */
   ELSE DO:
      CREATE htmlut.
      IF SUBSTRING(tidut.UT,1,3) = "***" THEN DO:
         ASSIGN
         boldblue = FALSE
         boldblack = FALSE
         mtrltable = FALSE.         
         ASSIGN htmlut.UT = "</table><br><table width='600' border='0' cellspacing='0' cellpadding='0'><tr><td align='left'><hr></td></tr>".       
      END.
      ELSE IF SUBSTRING(tidut.UT,1,3) = "---" THEN DO:
         ASSIGN
         boldblack = FALSE
         boldblue = FALSE
         mtrltable = FALSE.
         ASSIGN htmlut.UT = "<tr><td><pre></pre></td></tr>".       
      END.
      ELSE IF SUBSTRING(tidut.UT,1,3) = "===" THEN DO:
         ASSIGN htmlut.UT = "<tr><td><pre></pre></td></tr>".       
      END.
      ELSE DO:
         IF boldblack = TRUE THEN DO:
            ASSIGN htmlut.UT = "<tr><td class='boldblack'><pre>" + tidut.UT + "</pre></td></tr>".
         END.
         ELSE IF boldblue = TRUE THEN DO:
            ASSIGN htmlut.UT = "<tr><td class='boldblue'><pre>" + tidut.UT + "</pre></td></tr>".
         END.
         ELSE IF mtrltable = TRUE THEN DO:
            ASSIGN htmlut.UT = "<tr><td class='mtrltable'><pre>" + tidut.UT + "</pre></td></tr>".
         END.
         ELSE ASSIGN htmlut.UT = "<tr><td><pre>" + tidut.UT + "</pre></td></tr>".
         
      END.
   END.
   IF SUBSTRING(tidut.UT,1,16) = "Spann/kabellängd" THEN spann = TRUE.
   ELSE spann = FALSE.
END.
CREATE htmlut.
ASSIGN htmlut.UT = "</table></div>".
IF AVAILABLE bildbertemp THEN DO:
   FOR EACH bildbertemp NO-LOCK:
      CREATE htmlut.
      ASSIGN htmlut.UT = "<div style='PAGE-BREAK-BEFORE=always'></div><div class='page'>".
      CREATE htmlut.
      ASSIGN htmlut.UT = "<br><hr><br>".
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      RUN htmkonval_UI IN utberapph (INPUT valaonr,INPUT valomrade,INPUT bildbertemp.NUM,OUTPUT TABLE valsoktemp).
      FIND FIRST valsoktemp NO-LOCK NO-ERROR.
      IF AVAILABLE valsoktemp THEN DO:
         IF bildbertemp.FILNAMN NE "" THEN DO:
            FILE-INFO:FILE-NAME = bildbertemp.FILNAMN.
            IF SEARCH(FILE-INFO:FULL-PATHNAME) = ? THEN DO:
               numkopp = numkopp + 1.
               NEXT.
            END. 
        /*Anders Olsson Elpool i Umeå AB  2 okt 2017 13:25:17 
        Blobbilder här 
        */
            /*Maska fram filnamnet*/
            bildbertemp.FILNAMN = REPLACE(bildbertemp.FILNAMN,"\","/").
            startpos = INDEX(bildbertemp.FILNAMN,".",LENGTH(bildbertemp.FILNAMN) - 5).
            DEFINE VARIABLE pos AS INTEGER NO-UNDO.
            DEFINE VARIABLE pos2 AS INTEGER NO-UNDO.
            
            pos = 0.
            REPEAT :
               pos2 = pos.
               pos = pos + 1.
               pos = INDEX(bildbertemp.FILNAMN,"/",pos).                                        
               IF pos = 0 THEN LEAVE.
            END.
            pos = pos2 + 1.
            IF pos GE 0 THEN filvar = SUBSTRING(bildbertemp.FILNAMN,pos,LENGTH(bildbertemp.FILNAMN)).
            ELSE filvar = bildbertemp.FILNAMN.
            /* */

            CREATE htmlut.
            ASSIGN htmlut.UT = "<table width='600' border='0' cellspacing='10' cellpadding='0'>".
            IF SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".gif" OR
            SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".jpg" OR
            SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".jpeg" OR
            SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".bmp" THEN DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>" + filvar + " (se bild nedan) är kopplad till:</pre></td></tr>".
            END.
            ELSE DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>" + filvar + " (se länk nedan) är kopplad till:</pre></td></tr>".
            END.
            IF valsoktemp.SOKCHAR[2] NE "" THEN DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>" + valsoktemp.SOKCHAR[6] + ": " + valsoktemp.SOKCHAR[2] + "</pre></td></tr>".
            END.
            IF valsoktemp.SOKCHAR[3] NE "" THEN DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>" + valsoktemp.SOKCHAR[7] + ": " + valsoktemp.SOKCHAR[3] + "</pre></td></tr>".
            END.
            IF valsoktemp.SOKCHAR[4] NE "" THEN DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>" + valsoktemp.SOKCHAR[8] + ": " + valsoktemp.SOKCHAR[4] + "</pre></td></tr>".
            END.
            IF valsoktemp.SOKINT[1] NE ? THEN DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>" + valsoktemp.SOKCHAR[9] + ": " + STRING(valsoktemp.SOKINT[1]) + "</pre></td></tr>".
            END.
            IF valsoktemp.SOKINT[2] NE ? THEN DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>" + valsoktemp.SOKCHAR[10] + ": " + STRING(valsoktemp.SOKINT[2]) + "</pre></td></tr>".
            END.
            IF valsoktemp.SOKCHAR[5] NE "" THEN DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>Fri ID: " + valsoktemp.SOKCHAR[5] + "</pre></td></tr>".
            END.
            IF valsoktemp.SOKCHAR[1] NE "" THEN DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td><pre>Konstruktion: " + valsoktemp.SOKCHAR[1] + "</pre></td></tr>".
            END.
            IF SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".gif" OR
            SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".jpg" OR
            SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".jpeg" OR
            SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".bmp" THEN DO:
               IF SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".jpg" OR
               SUBSTRING(bildbertemp.FILNAMN,startpos,LENGTH(bildbertemp.FILNAMN)) = ".jpeg" THEN DO:
                  RUN pdf_new IN h_PDFinc ("Spdf",utfil).
                  RUN pdf_load_image IN h_PDFinc ("Spdf","ProSysLogo",bildbertemp.FILNAMN).
                  h = pdf_ImageDim ("Spdf","ProSysLogo","HEIGHT").
                  w = pdf_ImageDim ("Spdf","ProSysLogo","WIDTH").
                  IF w > 620 THEN DO:
                     a = 620 / w.
                     h = a * h.
                     w = a * w.            
                  END.
                  com = "del " + utfil.                     
                  OS-COMMAND SILENT VALUE(com).  
                  CREATE htmlut.
                  ASSIGN htmlut.UT = "<tr><td><img src=""" + "file:" + bildbertemp.FILNAMN + """ alt=""" + bildbertemp.FILNAMN + """ width=""" + string(w) + """ height=""" + string(h) + """></td></tr>".
               END.
               ELSE DO:
                  CREATE htmlut.
                  ASSIGN htmlut.UT = "<tr><td><img src=""" + "file:" + bildbertemp.FILNAMN + """ alt=""" + bildbertemp.FILNAMN + """></td></tr>".
               END.             
            END.
            ELSE DO:
               CREATE htmlut.
               ASSIGN htmlut.UT = "<tr><td class='tdnopre'><a href=""" + bildbertemp.FILNAMN + """ target='_blank'>" + bildbertemp.FILNAMN + "</a></td></tr>".                  
            END.            
         END.
      END.
      CREATE htmlut.
      ASSIGN htmlut.UT = "</table></div>".
   END.
   CREATE htmlut.
   ASSIGN htmlut.UT = "</body></html>".
END.
ELSE DO:
   CREATE htmlut.
   ASSIGN htmlut.UT = "</body></html>".
END.
IF numkopp NE 0 THEN DO:
   IF numkopp = 1 THEN DO:
      MESSAGE numkopp "st kopplat dokument eller bild gick ej att visa!" VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      MESSAGE  numkopp  "st kopplade dokument eller bilder gick ej att visa!" VIEW-AS ALERT-BOX.
   END.                        
END.
utfil = SESSION:TEMP-DIRECTORY.        
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN utfil = webclienttempdir.
IF Guru.GlobalaVariabler:plusaonr = "" OR Guru.GlobalaVariabler:plusaonr = ? THEN DO:
   utfil = utfil + STRING(TIME) + "ber4.htm".
END.
ELSE DO:
   utfil = utfil + TRIM (Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + STRING(TIME) + "ber4.htm".
END.
OUTPUT TO VALUE(utfil).
FOR EACH htmlut:
   IF SUBSTRING(htmlut.UT,1,1) NE "?" THEN PUT UNFORMATTED htmlut.UT SKIP.
END.
OUTPUT CLOSE. 

RUN OPENDOC.P (utfil,"","IEXPLORE.EXE",NO).
/*
com = "del " + utfil.
OS-COMMAND SILENT VALUE(com).
*/
IF VALID-HANDLE(utberapph) THEN DELETE PROCEDURE utberapph NO-ERROR.
IF VALID-HANDLE(h_PDFinc) THEN DELETE PROCEDURE h_PDFinc NO-ERROR.
{EUROPEANAMERICAN.I}
PROCEDURE bryt_UI :
   counter = counter + 1.
   CREATE htmlut.
   ASSIGN htmlut.UT = "</table>".
   CREATE htmlut.
   ASSIGN htmlut.UT = "<div style='PAGE-BREAK-AFTER=always'></div>".
   RUN logga_UI.
   mtrltable = FALSE.
   CREATE htmlut.
   ASSIGN htmlut.UT = "<table width='600' border='0' cellspacing='0' cellpadding='0'>". 
END PROCEDURE.

PROCEDURE logga_UI.
   DEFINE VARIABLE namnvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hogvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE breddvar AS INTEGER NO-UNDO.
   /*FOREBILDER*/
   {LOGGORHTM.I}
   
   IF logga = ? THEN RETURN.
   IF counter = 1 THEN DO:
      CREATE htmlut.
      ASSIGN htmlut.UT = "<table width='600'><tr><td width='230'><img align='left' src=""" + logga + """ border='0' width='" + STRING(breddvar) + "' height='" + STRING(hogvar) + "' alt='" + namnvar + "'></td><td class='header' width='370'></td>".
   END.
   ELSE DO:
      CREATE htmlut.
      ASSIGN htmlut.UT = "<table width='600'><tr><td width='230'><img align='left' src=""" + logga + """ border='0' width='" + STRING(breddvar) + "' height='" + STRING(hogvar) + "' alt='" + namnvar + "'></td><td class='header' width='300'></td><td width='70'><pre>" + aoutvar + " sid " + STRING(counter) + "</pre></td>".
   END.
   CREATE htmlut.
   ASSIGN htmlut.UT = "</tr></table>".
   
END PROCEDURE.

