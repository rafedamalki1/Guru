&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER typval AS CHARACTER.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EDITOR-TEXT BTN-SKRV BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-TEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-SKRV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-END-KEY 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE EDITOR-TEXT AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 60.5 BY 25.25 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     EDITOR-TEXT AT ROW 1.5 COL 2.5 NO-LABEL
     BTN-SKRV AT ROW 8 COL 64
     BTN_OK AT ROW 27.25 COL 64
     SPACE(1.24) SKIP(0.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Hjälptext"
         CANCEL-BUTTON BTN_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       EDITOR-TEXT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Hjälptext */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-SKRV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-SKRV Dialog-Frame
ON CHOOSE OF BTN-SKRV IN FRAME Dialog-Frame /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).         
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {muswait.i}
   RUN loadedit_UI.
   RUN enable_UI.
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY EDITOR-TEXT 
      WITH FRAME Dialog-Frame.
  ENABLE EDITOR-TEXT BTN-SKRV BTN_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadedit_UI Dialog-Frame 
PROCEDURE loadedit_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/           
   EDITOR-TEXT:READ-ONLY IN FRAME {&FRAME-NAME} = FALSE.
   CASE typval:
      WHEN "U10" THEN DO:
         EDITOR-TEXT = "Arb.timmar:" + CHR(10) + "-----------" + CHR(10) + 
"Totalt redovisade timmar på projektnumret förutom övertid och restid. Timmar ingår även för 
personal från annat område men registrerat tid på det sökta numret. Timmar för personal som 
ingår i omkostnaderna och alltså har timpriset 0 kr finns med i redovisningen. Timmar för 
tidskrivande entreprenörer finns med." + CHR(10) + CHR(10) + "Arbetskostnad:" + CHR(10) + 
"--------------" + CHR(10) + "Kostnaderna för de timmar som redovisats ovan." + CHR(10) + 
CHR(10) + "Över.timmar:" + CHR(10) + "------------" + CHR(10) + "Timmar utanför normal arbetstid." 
+ CHR(10) + CHR(10) + "Övertid kostnad:" + CHR(10) + "----------------" + CHR(10) + 
"Kostnaden för övertidstimmarna." + CHR(10) + CHR(10) + "Res.timmar:" + CHR(10) + "-----------" 
+ CHR(10) + "Restid utanför normal arbetstid." + CHR(10) + CHR(10) + "Trakt.kostnad:" + CHR(10) + 
"--------------" + CHR(10) + "Kostnad för alla redovisade traktamenten." + CHR(10) + 
CHR(10) + "Lönetill.kostnad:" + CHR(10) + "-----------------" + CHR(10) + 
"Kostnader för lönetillägg av olika slag. Ersättning för egen bil i tjänst etc.".
      END.               
      WHEN "U20" THEN DO:
         EDITOR-TEXT = "Listan redovisar kostnader etc. för valt projektnummer." + CHR(10) 
+ CHR(10) + "Listans innehåll:" + CHR(10) + "-----------------" + CHR(10) + 
"Överst visas vald period, projektnummer och benämning." + CHR(10) + CHR(10) + CHR(10) + "Raden TI:" 
+ CHR(10) + "---------" + CHR(10) + "*Arb.timmar:" + CHR(10) + "Totalt redovisade timmar på 
projektnumret förutom övertid och restid. Timmar ingår även för personal från annat område som 
redovisat tid på det sökta numret. Timmar för personal som ingår i omkostnaderna och alltså har 
timpriset 0 kr finns med i redovisningen." + CHR(10) + CHR(10) + "*Arbetskostnad:" + CHR(10) + 
"Kostnaderna för de timmar som redovisats ovan." + CHR(10) + CHR(10) + "*Över.timmar:" + CHR(10) + 
"Timmar utanför normal arbetstid." + CHR(10) + CHR(10) + "*Övertid kostnad:" + CHR(10) + 
"Kostnaden för övertidstimmarna" + CHR(10) + CHR(10) + "*Res.timmar:" + CHR(10) + 
"Restid utanför normal arbetstid." + CHR(10) + CHR(10) + "*Trakt.kostnad:" + CHR(10) + 
"Kostnad för alla redovisade traktamenten." + CHR(10) + CHR(10) + "*Lönetill.kostnad:" + CHR(10) + 
"Kostnader för lönetillägg av olika slag. Ersättning för egen bil i tjänst etc." + CHR(10) + CHR(10)
+ CHR(10) + "Raden KO:" + CHR(10) + "---------" + CHR(10) + "Här redovisas kostnader som lästs in 
från ekonomisystemet vad gäller externa tjänster." + CHR(10) + CHR(10) + "Främmande tjänster avser 
maskinentreprenörer etc. " + CHR(10) + "Materiel avser materielinköp." + CHR(10) + 
"Övrig kostnad gäller alltså annat än maskinentreprenörer och materiel." + CHR(10) + 
"Intäkt är vad som hittills fakturerats på projektnumret." + CHR(10) + CHR(10) + "Alla överlästa 
externa kostnader och intäkter redovisas i nedre delen av listan ." + CHR(10) + CHR(10) + CHR(10) + 
"Raden KA:" + CHR(10) + "---------" + CHR(10) + "Kalkylvärden normalt enligt EBR-P2.".
      END.
      WHEN "U30" THEN DO:
         EDITOR-TEXT = "Listans innehåll överensstämmer helt med den föregående. Skillnaden består 
i att här kan mer än ett projektnummer väljas.".
      END.
      WHEN "U40" THEN DO:
         EDITOR-TEXT = "U40. Text".
      END.
      WHEN "U50" THEN DO:
         EDITOR-TEXT = "Listan sammanställer tid etc. för sökt projektnummer, vidare redovisas 
tid och kostnad per person." + CHR(10) + CHR(10) + "Listans innehåll:" + CHR(10) + 
"-----------------" + CHR(10) + "I arb.timmar ingår all tidskrivning på projektnummer som tillhör 
det valda området oberoende av vilket område personalen kommer från. Övertid avser tid utanför 
ordinarie arbetstid registrerad på projektnummer som tillhör valt område oberoende av personalens 
hemvist. Restid avser tid utanför ordinarie arbetstid redovisad på projektnumret även här oberoende 
av personalens hemvist. Kostnaderna redovisas med motsvarande förutsättningar som gäller tid.".
      END.
      WHEN "U60" THEN DO:
         EDITOR-TEXT = "Listan redovisar kostnader etc. för valt projektnummer." + CHR(10) + CHR(10) + 
"Listan innehåller:" + CHR(10) + "------------------" + CHR(10) + "All tidregistrering under sökt 
period för all personal." + CHR(10) + CHR(10) + CHR(10) + "All registrerad tid visas i datumordning 
med start och slut samt antal timmar och vilket debiteringspris som använts. Registreringarnas 
godkännandevecka och datum för överläsning till ekonomisystemet visas.".
      END.
      WHEN "U70" THEN DO:
         EDITOR-TEXT = "Lista för kontroll av tid som registrerats på projektnummer sedan föregående 
körning till ekonomi-systemet. Här kan man alltså kontrollera vem som skrivit tid på de projektnummer 
som man ansvarar för innan registreringarna förs vidare." + CHR(10) + CHR(10) + CHR(10) + 
"Listans innehåll:" + CHR(10) + "-----------------" + CHR(10) + "Redovisning av tid sker på tre nivåer:" 
+ CHR(10) + CHR(10) + "     - Summa per projektnummer" + CHR(10) + "     - Summa per person och 
projektnummer" + CHR(10) + "     - Alla registreringar per person och projektnummer".
      END.
      WHEN "U80" THEN DO:
         EDITOR-TEXT = "Listan sammanställer upparbetade kostnader och fakturering för valda projektnummer."
+ CHR(10) + CHR(10) + CHR(10) + "Listans innehåll:" + CHR(10) + "-----------------" + CHR(10) + 
"Innehållet är en sammanfattning av upparbetade kostnader och genomförd fakturering för valda projektnummer.".
      END.
      WHEN "U90" THEN DO:
         EDITOR-TEXT = "Listan sammanställer fakturering på valda projektnummer och aktuella 
investerings-konton." + CHR(10) + CHR(10) + CHR(10) + "Listans innehåll:" + CHR(10) + "-----------------" 
+ CHR(10) + "Redovisar vilka konton som avses och den fakturering som skett för respektive projektnummer 
och konto. Total fakturering på projektnumren redovisas och kan avvika från redovisade konton om del av 
faktureringen avser konton som ej berör investering.".
      END.
      WHEN "U100" THEN DO:
         EDITOR-TEXT = "U100. Text".
      END.
      WHEN "U110" THEN DO:
         EDITOR-TEXT = "Listans utformning överensstämmer med den föregående och redovisar all tid 
och alla kostnader summerade för samtliga projektnummer inom den valda resultatenheten.".
      END.               
      WHEN "U120" THEN DO:
         EDITOR-TEXT = "Lista som redovisar alla projektnummer och summa för vald resultatenhet." 
+ CHR(10) + CHR(10) + "Projektnumren som ägs av aktuell resultatenhet redovisas samt gemensamma 
nummer - frånvaro etc." + CHR(10) + CHR(10) + "Här redovisas timmar och kostnader per projektnummer 
med samma urval och förutsättningar som de två tidigare listorna." + CHR(10) + CHR(10) + 
"I arb.timmar ingår all tidskrivning på projektnummer som tillhör den valda resultatenheten oberoende 
av vilket område personalen kommer från. Frånvaro ingår för personal som tillhör sökt område. 
Ej debiterbar tid ingår också för personal som tillhör det valda området. Med Ej debiterbar tid 
avses utbildning, möten av olika slag m.m." + CHR(10) + "Övertid avser tid utanför ordinarie arbetstid 
skriven på projektnummer som tillhör valt område oberoende av personalens hemvist. Restid avser tid 
utanför ordinarie arbetstid redovisad på projektnumret även här oberoende av personalens hemvist. 
Kostnaderna redovisas med motsvarande förutsättningar som gäller tid.".
      END.
      WHEN "U130" THEN DO:
         EDITOR-TEXT = "Här sker utsökningen för år eller period, resultatenhet och listan redovisar 
tid per person och summerat för hela enheten." + CHR(10) + CHR(10) + "Listans innehåll:" + CHR(10) + 
"-----------------" + CHR(10) + CHR(10) + "1  Möjlig arbetstid.(A):" + CHR(10) + 
"   Periodens totala arbetstid enligt arbetstidsschema." + CHR(10) + CHR(10) + "2  Närvaro timmar.(B):" 
+ CHR(10) + "   Total arbetstid minskat med frånvaro." + CHR(10) + CHR(10) + "3  Övertid timmar:" + CHR(10) + 
"   Periodens övertid." + CHR(10) + CHR(10) + "4  Restid timmar:" + CHR(10) + "   Restid utanför normal arbetstid." 
+ CHR(10) + CHR(10) + "4  Intern timmar:" + CHR(10) + "   Utbildning, fackliga möten etc." + CHR(10) + CHR(10) + 
"5  Deb timmar.(C):" + CHR(10) + "   Tid på arbeten som fakturerats alternativt skall faktureras." + CHR(10) 
+ CHR(10) + "6  Deb grad % närvaro:" + CHR(10) + "   Deb timmar(C)*100/Närvaro timmar(B)" + CHR(10) + CHR(10) + 
"7  Deb grad % möjlig:" + CHR(10) + "   Deb timmar(C)*100/Möjlig arbetstid(A)".
      END.
      WHEN "U140" THEN DO:
         EDITOR-TEXT = "Här sker utsökningen för år eller period samt resultatenhet/område." + CHR(10) 
+ CHR(10) + "I nedre högra hörnet sker valet om det skall gälla en lista som ger arbetsorder där intäkter 
förväntas alternativt kostnader." + CHR(10) + CHR(10) + CHR(10) + "Intäkter:" + CHR(10) + "---------" 
+ CHR(10) + "Visar alla projektnummer där vald resultatenhet utfört arbeten åt andra enheter. 
Listan visar nedlagda kostnader som alltså kommer att generera intäkter." + CHR(10) + CHR(10) + CHR(10) + 
"Kostnader:" + CHR(10) + "----------" + CHR(10) + "Visar alla projektnummer där andra enheter utfört 
arbeten åt den valda enheten, vilket kommer att generera utgifter.".
      END.
      WHEN "U150" THEN DO:
         EDITOR-TEXT = "Val görs enbart vad gäller period och resultatenhet." + CHR(10) + CHR(10) + 
"Här redovisas timmar och kostnader per pristyp." + CHR(10) + CHR(10) + 
"Timmar redovisas för all personal som tidskrivit på arbetsorder-nummer som ägs av det sökta området.".
      END.
      WHEN "U160" THEN DO:
         EDITOR-TEXT = "Listan ger möjlighet att redovisa vilka personer som erhållit lönetillägg 
alternativt omvänt vilket lönetillägg som utgått till vilka personer. Beroende på behörighet kan 
möjligheterna att hämta fram listan vara olika.".
      END.
      WHEN "U170" THEN DO:
         EDITOR-TEXT = "Listan visar kalkyl och utfall samt beräknar resultat baserat på EA-ekvivalenta 
arbetsmängder enligt EBR. Resultat beräknas för avslutade projektnummer" + CHR(10) + CHR(10) + CHR(10) + 
"Listans innehåll:" + CHR(10) + "-----------------" + CHR(10) + "1  För varje redovisat projektnummer 
finns två rader." + CHR(10) + "    -Rad ett(1) visar kalkylen som anges med Plan1 eller" + CHR(10) + 
"     Plan2 beroende på vilken EBR-kalkyl som finns" + CHR(10) + "     tillgänglig.     " + CHR(10) + 
"    -Rad två(2) visar utfallet i timmar och kronor." + CHR(10) + CHR(10) + "2  Benämning enligt registrering 
vid upplägg av " + CHR(10) + "   projektnumret." + CHR(10) + CHR(10) + "3  Projektnummer och 
eventuellt delnummer." + CHR(10) + CHR(10) + "4  Datum för avslut." + CHR(10) + CHR(10) + "5  EA-Ekvivalent 
arbetsmängd planerat och utfört." + CHR(10) + "   Beräknas som summan montörstid och omräknad maskintid" 
+ CHR(10) + "   enligt EBR-kostnadskatalog. Beredartid ingår ej." + CHR(10) + CHR(10) + "6  Resultat beräknas 
som EA planerat/EA utfört." + CHR(10) + CHR(10) + "7  Montörstimmar planerat och utfört. Här redovisas inte" 
+ CHR(10) + "   beredartid." + CHR(10) + CHR(10) + "8  Maskinkostnad planerat och utfört enligt redovisning" 
+ CHR(10) + "   från ekonomisystemet." + CHR(10) + CHR(10) + "9  Maskintimmar planerat och utfört." + CHR(10) 
+ CHR(10) + "10 Materielkostnad planerad och registrerat via" + CHR(10) + "   ekonomisystemet." + CHR(10) 
+ CHR(10) + "11 Lön avser kostnad för montörs- och beredartid." + CHR(10) + CHR(10) + 
"12 Övrig intäkt visar vad som fakturerats på" + CHR(10) + "   arbetsordernumret." + CHR(10) + CHR(10) + 
"13 Tot. redovisar summa planerad och redovisad kostnad." + CHR(10) + CHR(10) + "14 Ber tim visar planerad 
och utförda beredningstimmar." + CHR(10) + "   Dels för personer som har befattningen beredare och de" 
+ CHR(10) + "   som vid registrering av tid angett pristypen beredare." + CHR(10) + CHR(10) + "15 KOST RA:" 
+ CHR(10) + "   Kostnadsregistreringar från ekonomisystemet anges med" + CHR(10) + "   JA på utfallsraden." 
+ CHR(10) + CHR(10) + CHR(10) + "Övrigt:" + CHR(10) + "-------" + CHR(10) + "Nederst i listan redovisas de 
projektnummer som antingen saknar kalkyl eller saknar utfall.".
      END.
      WHEN "U180" THEN DO:
         EDITOR-TEXT = "U180. Text".
      END.
      WHEN "U190" THEN DO:
         EDITOR-TEXT = "U190. Text".
      END.
      WHEN "U200" THEN DO:
         EDITOR-TEXT = "U200. Text".
      END.
      WHEN "U210" THEN DO:
         EDITOR-TEXT = "U210. Text".
      END.
      WHEN "U220" THEN DO:
         EDITOR-TEXT = "U220. Text".
      END.
   END CASE.
   DISPLAY EDITOR-TEXT WITH FRAME {&FRAME-NAME}. 
   EDITOR-TEXT:READ-ONLY IN FRAME {&FRAME-NAME} = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI Dialog-Frame 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {PRINTSTAENDE.I}
   DISPLAY EDITOR-TEXT AT 8 VIEW-AS EDITOR SIZE 70 BY 8 NO-LABEL.
   OUTPUT CLOSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

