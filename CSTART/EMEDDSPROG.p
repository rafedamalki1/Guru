/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: EMEDDSPROG.P
      Comment: En lista i .doc format som bifogad fil till e-post program
   Parameters:
         Uses:
      Used by:
      Created: 2008.09.04 11:24 ELPKL   
     Modified: 2008.09.19 14:07 ELPAO    
     Modified: 
*/

{ALLDEF.I}
&Scoped-define NEW

{GLOBVAR2DEL1.I}

{REGVAR.I}
{pdf_StartInc.i}

DEFINE VARIABLE tempnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE logga      AS CHARACTER NO-UNDO.
{TIDUTTTSHARED.I}
DEFINE VARIABLE Vitems     AS INTEGER NO-UNDO.
DEFINE VARIABLE vline     AS INTEGER NO-UNDO.
DEFINE VARIABLE h       AS INTEGER NO-UNDO.
DEFINE VARIABLE w       AS INTEGER NO-UNDO.
DEFINE VARIABLE a          AS DECIMAL NO-UNDO.
DEFINE VARIABLE forsta     AS LOGICAL NO-UNDO.
/* Sätter filnamn */
tempnamn = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
{SESSIONTEMPDIR.I}
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN tempnamn = webclienttempdir.
OS-CREATE-DIR VALUE(tempnamn) NO-ERROR.
tempnamn = tempnamn + Guru.Konstanter:globanv + ".pdf".

/*
tempnamn = tempnamn + globanv + ".doc".
/* Skapar .doc filen */
OUTPUT TO VALUE(tempnamn).
   FOR EACH tidut NO-LOCK:       
      PUT tidut.UT AT 1 SKIP.
   END.    
   OUTPUT CLOSE.
*/
/* Epost + bifoga */
IF Guru.GlobalaVariabler:globsidl = 0 THEN Guru.GlobalaVariabler:globsidl = 53.
IF Guru.GlobalaVariabler:globsids = 0 THEN Guru.GlobalaVariabler:globsids = 73.

/*FOREBILD*/
{BILDPDF.I}

IF logga NE ? THEN Guru.GlobalaVariabler:globsidl = Guru.GlobalaVariabler:globsidl - 3.
RUN startpdf_UI.
RUN pdf_close IN h_PDFinc ("Spdf").
IF VALID-HANDLE(h_PDFinc) THEN DELETE PROCEDURE h_PDFinc NO-ERROR. 
h_PDFinc = ?.
/*
MESSAGE "Skapad PDF-fil måste efter användning tas bort manuellt." + CHR(10) + "Den sparas under " + utfil VIEW-AS ALERT-BOX TITLE "Meddelande".
RUN OPENDOC.P (utfil,"","Acrobat.exe",NO).
*/


RUN epost.



PROCEDURE epost :
   file-info:file-name = ".".
   orgdir = file-info:full-pathname.
   
   RUN SPECIALMAPI.P (INPUT "", INPUT "", INPUT tempnamn).
   RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).
   


END PROCEDURE.
PROCEDURE startpdf_UI:
   FIND FIRST tidut NO-LOCK NO-ERROR.
   IF AVAILABLE tidut THEN DO: 
      RUN pdf_new IN h_PDFinc ("Spdf",tempnamn).
      RUN pdf_set_LeftMargin IN h_PDFinc ("Spdf",20).
      RUN pdf_set_TopMargin IN h_PDFinc ("Spdf",30).
      /* Dokument Information */ 
      RUN pdf_set_info("Spdf","Author","Elpool i Umeå AB").
      RUN pdf_set_info("Spdf","Subject","Kalkyl").
      RUN pdf_set_info("Spdf","Title","Kalkyl").
      RUN pdf_set_info("Spdf","Keywords","Kalkyl").
      RUN pdf_set_info("Spdf","Creator","PDFinclude").
      RUN pdf_set_info("Spdf","Producer","UTFKALPDF.P").
      /*Skapa ny sida*/
      RUN new_page.
      RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",9.0).
      RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
      FOR EACH tidut NO-LOCK:
         IF SUBSTRING(tidut.UT,1,10) = "          " AND forsta = FALSE THEN DO: 
            RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",9.0).
            RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
            forsta = TRUE.
         END.
         IF tidut.UT = ? THEN NEXT.
         vline = vline + 1.
         IF SUBSTRING(tidut.UT,132,1) = "$" THEN DO:
            RUN new_page.
         END.
         IF vline >= globsidl THEN DO:
            RUN new_page.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,4) = "====" THEN DO:
            RUN pdf_line IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf") , pdf_TextY("Spdf") + 8, pdf_PageWidth("Spdf") - 10 , pdf_TextY("Spdf") + 8, 1).
            RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",9.0).
            RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
         END.
         ELSE DO:
            RUN pdf_text IN h_PDFinc ("Spdf", tidut.UT).
            RUN pdf_skip IN h_PDFinc ("Spdf").
         END.
      END.
   END.  
END.

PROCEDURE new_page.
   RUN pdf_new_page2 IN h_PDFinc ("Spdf","Landscape").
   RUN pdf_set_LeftMargin IN h_PDFinc ("Spdf",20).
   RUN pdf_set_TopMargin IN h_PDFinc ("Spdf",30).
   /*Sätt font*/
   RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",9.0).
   RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
   vline = 0. 
   IF forsta = TRUE THEN DO:
      
      RUN pdf_skip IN h_PDFinc ("Spdf").
   END.
   /*Init. logga*/
   IF forsta = FALSE THEN DO:
      IF logga NE ? THEN DO:
         RUN pdf_load_image IN h_PDFinc ("Spdf","ProSysLogo",logga).
         h = pdf_ImageDim ("Spdf","ProSysLogo","HEIGHT").
         w = pdf_ImageDim ("Spdf","ProSysLogo","WIDTH").
         IF h > 30 THEN DO:
            a = 30 / h.
            h = a * h.
            w = a * w.            
         END.
         RUN pdf_place_image IN h_PDFinc ("Spdf","ProSysLogo",pdf_LeftMargin("Spdf"), pdf_TopMargin("Spdf") + 10,w,h).
         RUN pdf_skipn IN h_PDFinc ("Spdf",3).
         vline = 3.
      END.
      forsta = TRUE.
   END.
   ELSE DO:
      /*Placera ut logga överst på varje sida*/
      IF logga NE ? THEN DO:
         RUN pdf_place_image IN h_PDFinc ("Spdf","ProSysLogo",pdf_LeftMargin("Spdf"), pdf_TopMargin("Spdf") + 10,w,h).
         RUN pdf_skipn IN h_PDFinc ("Spdf",3).
         vline = 3.
      END.
      RUN pdf_text IN h_PDFinc ("Spdf", tidut.UT).
      RUN pdf_skip IN h_PDFinc ("Spdf").      
   END.

END PROCEDURE.



   


