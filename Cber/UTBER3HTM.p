/*
     Filename: UTBER3HTM.P
*/
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{REGVAR.I}

{TIDUTTT.I}

DEFINE TEMP-TABLE htmlut
   FIELD UT AS CHARACTER FORMAT "X(132)".

DEFINE INPUT PARAMETER table FOR tidut.
DEFINE SHARED VARIABLE bryt AS LOGICAL NO-UNDO.
                              
DEFINE VARIABLE utfil AS CHARACTER  NO-UNDO.
DEFINE VARIABLE bodydoc AS CHARACTER INITIAL "Berlista3" LABEL "Berlista3" NO-UNDO.
DEFINE VARIABLE sokvag AS CHARACTER NO-UNDO.
DEFINE VARIABLE counter AS INTEGER NO-UNDO.
DEFINE VARIABLE logga AS CHARACTER NO-UNDO.
DEFINE VARIABLE boldblack AS LOGICAL NO-UNDO.
DEFINE VARIABLE boldblue AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrltable AS LOGICAL NO-UNDO.
{AMERICANEUROPEAN.I}
EMPTY TEMP-table htmlut NO-ERROR. 
FOR EACH tidut:
   tidut.UT = REPLACE(tidut.UT,":"," ").
END.
{AONRUTSID.I}
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
   body" + STRING('~{') + "margin:0pt 0pt 0pt 30pt; text-align:left; font:normal 10pt Courier New;}
   pre" + STRING('~{') + "margin-top:0pt; margin-bottom:0pt; font:normal 10pt Courier New;} 
   hr" + string('~{') + "border:0; height:2pt; color:black;}   
   .header" + string('~{') + "text-align:LEFT; font:bold 15pt Courier New; color:black;}   
   .borderbg pre" + string('~{') + "background-color:#FDFDFF; border-bottom:solid 1pt black; border-top:solid 1pt black; font:normal 10pt Courier New; color:#0000CC;}   
   .boldblue pre" + string('~{') + "font:bold 10pt Courier New; color:#0000CC;}   
   .boldblack pre" + string('~{') + "font:bold 10pt Courier New; color:#000000;}   
   .boldbg pre" + string('~{') + "background-color:#FDFDFF; font:bold 10pt Courier New; color:#0000CC;}   
   .mtrltable pre" + string('~{') + "background-color:#FDFDFF; font:normal 10pt Courier New; color:#000000;}}  

   @media SCREEN " + string('~{') + "
   body" + STRING('~{') + "margin:0px 0px 0px 50px; text-align:left; font:normal 12px Courier New;}
   pre" + STRING('~{') + "margin:0px 0px 0px 0px; font:normal 12px Courier New;} 
   hr" + string('~{') + "border:0; height:2px; color:black; width:600px;}   
   .header" + string('~{') + "text-align:LEFT; font:bold 16px Courier New; color:black;}   
   .borderbg pre" + string('~{') + "background-color:#FDFDFF; border-bottom:solid 1px black; border-top:solid 1px black; font:normal 12px Courier New; color:#0000CC;}   
   .boldblue pre" + string('~{') + "font:bold 12px Courier New; color:#0000CC;}   
   .boldblack pre" + string('~{') + "font:bold 12px Courier New; color:#000000;}   
   .boldbg pre" + string('~{') + "background-color:#FDFDFF; font:bold 12px Courier New; color:#0000CC;}   
   .mtrltable pre" + string('~{') + "background-color:#FDFDFF; font:normal 12px Courier New; color:#000000;}}   
    -->
   </style>
</head>
<body bgcolor='#F5F5FF'>". 
ASSIGN
counter = 1.
RUN logga_UI.
CREATE htmlut.
ASSIGN htmlut.UT = "<table width='600' border='0' cellspacing='0' cellpadding='0'>". 
FOR EACH tidut NO-LOCK: 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF SUBSTRING(tidut.UT,1,14) = "Lista Materiel" THEN DO: 
         boldblack = TRUE.      
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      IF SUBSTRING(tidut.UT,1,14) = "Lista Materiel" THEN DO: 
         boldblack = TRUE.      
      END.
   END.
   ELSE DO:
      IF SUBSTRING(tidut.UT,1,14) = "Lista Materiel" THEN DO: 
         boldblack = TRUE.      
      END.
   END.
   IF SUBSTRING(tidut.UT,1,10) = "Upplag nr" THEN DO:  
      boldblack = FALSE.
   END.
   IF SUBSTRING(tidut.UT,132) = "$" THEN DO:
      ASSIGN
      boldblue = FALSE
      boldblack = FALSE.
      RUN bryt_UI.
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
   ELSE IF INDEX(SUBSTRING(tidut.UT,1,5),"?",1) NE 0 THEN DO:
      CREATE htmlut.
      ASSIGN htmlut.UT = "<tr><td><pre></pre></td></tr>". 
   END.
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
         ASSIGN
         boldblack = FALSE
         boldblue = FALSE.
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
END.
CREATE htmlut.
ASSIGN htmlut.UT = "</table></body></html>".
utfil = SESSION:TEMP-DIRECTORY.        
{SESSIONTEMPDIR.I}
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN utfil = webclienttempdir.
IF Guru.GlobalaVariabler:plusaonr = "" OR Guru.GlobalaVariabler:plusaonr = ? THEN DO:
   utfil = utfil + STRING(TIME) + "ber3.htm".
END.
ELSE DO:
   utfil = utfil + TRIM (Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + STRING(TIME) + "ber3.htm".
END.

OUTPUT TO VALUE(utfil).
FOR EACH htmlut:
   IF SUBSTRING(htmlut.UT,1,1) NE "?" THEN PUT UNFORMATTED htmlut.UT SKIP.
END.
OUTPUT CLOSE. 
RUN OPENDOC.P (utfil,"","IEXPLORE.EXE",NO).
/*
DEFINE VARIABLE com AS CHARACTER NO-UNDO.
com = "del " + utfil.
OS-COMMAND SILENT VALUE(com).
*/
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
      ASSIGN htmlut.UT = "<table width='600'><tr><td width='230'><img align='left' src='" + logga + "' border='0' width='" + STRING(breddvar) + "' height='" + STRING(hogvar) + "' alt='" + namnvar + "'></td><td class='header' width='370'></td>".
   END.
   ELSE DO:
      CREATE htmlut.
      ASSIGN htmlut.UT = "<table width='600'><tr><td width='230'><img align='left' src='" + logga + "' border='0' width='" + STRING(breddvar) + "' height='" + STRING(hogvar) + "' alt='" + namnvar + "'></td><td class='header' width='300'></td><td width='70'><pre>" + aoutvar + " sid " + STRING(counter) + "</pre></td>".
   END.
   CREATE htmlut.
   ASSIGN htmlut.UT = "</tr></table>".
   
END PROCEDURE.

