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
         TITLE "Hj�lptext"
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Hj�lptext */
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
"Totalt redovisade timmar p� projektnumret f�rutom �vertid och restid. Timmar ing�r �ven f�r 
personal fr�n annat omr�de men registrerat tid p� det s�kta numret. Timmar f�r personal som 
ing�r i omkostnaderna och allts� har timpriset 0 kr finns med i redovisningen. Timmar f�r 
tidskrivande entrepren�rer finns med." + CHR(10) + CHR(10) + "Arbetskostnad:" + CHR(10) + 
"--------------" + CHR(10) + "Kostnaderna f�r de timmar som redovisats ovan." + CHR(10) + 
CHR(10) + "�ver.timmar:" + CHR(10) + "------------" + CHR(10) + "Timmar utanf�r normal arbetstid." 
+ CHR(10) + CHR(10) + "�vertid kostnad:" + CHR(10) + "----------------" + CHR(10) + 
"Kostnaden f�r �vertidstimmarna." + CHR(10) + CHR(10) + "Res.timmar:" + CHR(10) + "-----------" 
+ CHR(10) + "Restid utanf�r normal arbetstid." + CHR(10) + CHR(10) + "Trakt.kostnad:" + CHR(10) + 
"--------------" + CHR(10) + "Kostnad f�r alla redovisade traktamenten." + CHR(10) + 
CHR(10) + "L�netill.kostnad:" + CHR(10) + "-----------------" + CHR(10) + 
"Kostnader f�r l�netill�gg av olika slag. Ers�ttning f�r egen bil i tj�nst etc.".
      END.               
      WHEN "U20" THEN DO:
         EDITOR-TEXT = "Listan redovisar kostnader etc. f�r valt projektnummer." + CHR(10) 
+ CHR(10) + "Listans inneh�ll:" + CHR(10) + "-----------------" + CHR(10) + 
"�verst visas vald period, projektnummer och ben�mning." + CHR(10) + CHR(10) + CHR(10) + "Raden TI:" 
+ CHR(10) + "---------" + CHR(10) + "*Arb.timmar:" + CHR(10) + "Totalt redovisade timmar p� 
projektnumret f�rutom �vertid och restid. Timmar ing�r �ven f�r personal fr�n annat omr�de som 
redovisat tid p� det s�kta numret. Timmar f�r personal som ing�r i omkostnaderna och allts� har 
timpriset 0 kr finns med i redovisningen." + CHR(10) + CHR(10) + "*Arbetskostnad:" + CHR(10) + 
"Kostnaderna f�r de timmar som redovisats ovan." + CHR(10) + CHR(10) + "*�ver.timmar:" + CHR(10) + 
"Timmar utanf�r normal arbetstid." + CHR(10) + CHR(10) + "*�vertid kostnad:" + CHR(10) + 
"Kostnaden f�r �vertidstimmarna" + CHR(10) + CHR(10) + "*Res.timmar:" + CHR(10) + 
"Restid utanf�r normal arbetstid." + CHR(10) + CHR(10) + "*Trakt.kostnad:" + CHR(10) + 
"Kostnad f�r alla redovisade traktamenten." + CHR(10) + CHR(10) + "*L�netill.kostnad:" + CHR(10) + 
"Kostnader f�r l�netill�gg av olika slag. Ers�ttning f�r egen bil i tj�nst etc." + CHR(10) + CHR(10)
+ CHR(10) + "Raden KO:" + CHR(10) + "---------" + CHR(10) + "H�r redovisas kostnader som l�sts in 
fr�n ekonomisystemet vad g�ller externa tj�nster." + CHR(10) + CHR(10) + "Fr�mmande tj�nster avser 
maskinentrepren�rer etc. " + CHR(10) + "Materiel avser materielink�p." + CHR(10) + 
"�vrig kostnad g�ller allts� annat �n maskinentrepren�rer och materiel." + CHR(10) + 
"Int�kt �r vad som hittills fakturerats p� projektnumret." + CHR(10) + CHR(10) + "Alla �verl�sta 
externa kostnader och int�kter redovisas i nedre delen av listan ." + CHR(10) + CHR(10) + CHR(10) + 
"Raden KA:" + CHR(10) + "---------" + CHR(10) + "Kalkylv�rden normalt enligt EBR-P2.".
      END.
      WHEN "U30" THEN DO:
         EDITOR-TEXT = "Listans inneh�ll �verensst�mmer helt med den f�reg�ende. Skillnaden best�r 
i att h�r kan mer �n ett projektnummer v�ljas.".
      END.
      WHEN "U40" THEN DO:
         EDITOR-TEXT = "U40. Text".
      END.
      WHEN "U50" THEN DO:
         EDITOR-TEXT = "Listan sammanst�ller tid etc. f�r s�kt projektnummer, vidare redovisas 
tid och kostnad per person." + CHR(10) + CHR(10) + "Listans inneh�ll:" + CHR(10) + 
"-----------------" + CHR(10) + "I arb.timmar ing�r all tidskrivning p� projektnummer som tillh�r 
det valda omr�det oberoende av vilket omr�de personalen kommer fr�n. �vertid avser tid utanf�r 
ordinarie arbetstid registrerad p� projektnummer som tillh�r valt omr�de oberoende av personalens 
hemvist. Restid avser tid utanf�r ordinarie arbetstid redovisad p� projektnumret �ven h�r oberoende 
av personalens hemvist. Kostnaderna redovisas med motsvarande f�ruts�ttningar som g�ller tid.".
      END.
      WHEN "U60" THEN DO:
         EDITOR-TEXT = "Listan redovisar kostnader etc. f�r valt projektnummer." + CHR(10) + CHR(10) + 
"Listan inneh�ller:" + CHR(10) + "------------------" + CHR(10) + "All tidregistrering under s�kt 
period f�r all personal." + CHR(10) + CHR(10) + CHR(10) + "All registrerad tid visas i datumordning 
med start och slut samt antal timmar och vilket debiteringspris som anv�nts. Registreringarnas 
godk�nnandevecka och datum f�r �verl�sning till ekonomisystemet visas.".
      END.
      WHEN "U70" THEN DO:
         EDITOR-TEXT = "Lista f�r kontroll av tid som registrerats p� projektnummer sedan f�reg�ende 
k�rning till ekonomi-systemet. H�r kan man allts� kontrollera vem som skrivit tid p� de projektnummer 
som man ansvarar f�r innan registreringarna f�rs vidare." + CHR(10) + CHR(10) + CHR(10) + 
"Listans inneh�ll:" + CHR(10) + "-----------------" + CHR(10) + "Redovisning av tid sker p� tre niv�er:" 
+ CHR(10) + CHR(10) + "     - Summa per projektnummer" + CHR(10) + "     - Summa per person och 
projektnummer" + CHR(10) + "     - Alla registreringar per person och projektnummer".
      END.
      WHEN "U80" THEN DO:
         EDITOR-TEXT = "Listan sammanst�ller upparbetade kostnader och fakturering f�r valda projektnummer."
+ CHR(10) + CHR(10) + CHR(10) + "Listans inneh�ll:" + CHR(10) + "-----------------" + CHR(10) + 
"Inneh�llet �r en sammanfattning av upparbetade kostnader och genomf�rd fakturering f�r valda projektnummer.".
      END.
      WHEN "U90" THEN DO:
         EDITOR-TEXT = "Listan sammanst�ller fakturering p� valda projektnummer och aktuella 
investerings-konton." + CHR(10) + CHR(10) + CHR(10) + "Listans inneh�ll:" + CHR(10) + "-----------------" 
+ CHR(10) + "Redovisar vilka konton som avses och den fakturering som skett f�r respektive projektnummer 
och konto. Total fakturering p� projektnumren redovisas och kan avvika fr�n redovisade konton om del av 
faktureringen avser konton som ej ber�r investering.".
      END.
      WHEN "U100" THEN DO:
         EDITOR-TEXT = "U100. Text".
      END.
      WHEN "U110" THEN DO:
         EDITOR-TEXT = "Listans utformning �verensst�mmer med den f�reg�ende och redovisar all tid 
och alla kostnader summerade f�r samtliga projektnummer inom den valda resultatenheten.".
      END.               
      WHEN "U120" THEN DO:
         EDITOR-TEXT = "Lista som redovisar alla projektnummer och summa f�r vald resultatenhet." 
+ CHR(10) + CHR(10) + "Projektnumren som �gs av aktuell resultatenhet redovisas samt gemensamma 
nummer - fr�nvaro etc." + CHR(10) + CHR(10) + "H�r redovisas timmar och kostnader per projektnummer 
med samma urval och f�ruts�ttningar som de tv� tidigare listorna." + CHR(10) + CHR(10) + 
"I arb.timmar ing�r all tidskrivning p� projektnummer som tillh�r den valda resultatenheten oberoende 
av vilket omr�de personalen kommer fr�n. Fr�nvaro ing�r f�r personal som tillh�r s�kt omr�de. 
Ej debiterbar tid ing�r ocks� f�r personal som tillh�r det valda omr�det. Med Ej debiterbar tid 
avses utbildning, m�ten av olika slag m.m." + CHR(10) + "�vertid avser tid utanf�r ordinarie arbetstid 
skriven p� projektnummer som tillh�r valt omr�de oberoende av personalens hemvist. Restid avser tid 
utanf�r ordinarie arbetstid redovisad p� projektnumret �ven h�r oberoende av personalens hemvist. 
Kostnaderna redovisas med motsvarande f�ruts�ttningar som g�ller tid.".
      END.
      WHEN "U130" THEN DO:
         EDITOR-TEXT = "H�r sker uts�kningen f�r �r eller period, resultatenhet och listan redovisar 
tid per person och summerat f�r hela enheten." + CHR(10) + CHR(10) + "Listans inneh�ll:" + CHR(10) + 
"-----------------" + CHR(10) + CHR(10) + "1  M�jlig arbetstid.(A):" + CHR(10) + 
"   Periodens totala arbetstid enligt arbetstidsschema." + CHR(10) + CHR(10) + "2  N�rvaro timmar.(B):" 
+ CHR(10) + "   Total arbetstid minskat med fr�nvaro." + CHR(10) + CHR(10) + "3  �vertid timmar:" + CHR(10) + 
"   Periodens �vertid." + CHR(10) + CHR(10) + "4  Restid timmar:" + CHR(10) + "   Restid utanf�r normal arbetstid." 
+ CHR(10) + CHR(10) + "4  Intern timmar:" + CHR(10) + "   Utbildning, fackliga m�ten etc." + CHR(10) + CHR(10) + 
"5  Deb timmar.(C):" + CHR(10) + "   Tid p� arbeten som fakturerats alternativt skall faktureras." + CHR(10) 
+ CHR(10) + "6  Deb grad % n�rvaro:" + CHR(10) + "   Deb timmar(C)*100/N�rvaro timmar(B)" + CHR(10) + CHR(10) + 
"7  Deb grad % m�jlig:" + CHR(10) + "   Deb timmar(C)*100/M�jlig arbetstid(A)".
      END.
      WHEN "U140" THEN DO:
         EDITOR-TEXT = "H�r sker uts�kningen f�r �r eller period samt resultatenhet/omr�de." + CHR(10) 
+ CHR(10) + "I nedre h�gra h�rnet sker valet om det skall g�lla en lista som ger arbetsorder d�r int�kter 
f�rv�ntas alternativt kostnader." + CHR(10) + CHR(10) + CHR(10) + "Int�kter:" + CHR(10) + "---------" 
+ CHR(10) + "Visar alla projektnummer d�r vald resultatenhet utf�rt arbeten �t andra enheter. 
Listan visar nedlagda kostnader som allts� kommer att generera int�kter." + CHR(10) + CHR(10) + CHR(10) + 
"Kostnader:" + CHR(10) + "----------" + CHR(10) + "Visar alla projektnummer d�r andra enheter utf�rt 
arbeten �t den valda enheten, vilket kommer att generera utgifter.".
      END.
      WHEN "U150" THEN DO:
         EDITOR-TEXT = "Val g�rs enbart vad g�ller period och resultatenhet." + CHR(10) + CHR(10) + 
"H�r redovisas timmar och kostnader per pristyp." + CHR(10) + CHR(10) + 
"Timmar redovisas f�r all personal som tidskrivit p� arbetsorder-nummer som �gs av det s�kta omr�det.".
      END.
      WHEN "U160" THEN DO:
         EDITOR-TEXT = "Listan ger m�jlighet att redovisa vilka personer som erh�llit l�netill�gg 
alternativt omv�nt vilket l�netill�gg som utg�tt till vilka personer. Beroende p� beh�righet kan 
m�jligheterna att h�mta fram listan vara olika.".
      END.
      WHEN "U170" THEN DO:
         EDITOR-TEXT = "Listan visar kalkyl och utfall samt ber�knar resultat baserat p� EA-ekvivalenta 
arbetsm�ngder enligt EBR. Resultat ber�knas f�r avslutade projektnummer" + CHR(10) + CHR(10) + CHR(10) + 
"Listans inneh�ll:" + CHR(10) + "-----------------" + CHR(10) + "1  F�r varje redovisat projektnummer 
finns tv� rader." + CHR(10) + "    -Rad ett(1) visar kalkylen som anges med Plan1 eller" + CHR(10) + 
"     Plan2 beroende p� vilken EBR-kalkyl som finns" + CHR(10) + "     tillg�nglig.     " + CHR(10) + 
"    -Rad tv�(2) visar utfallet i timmar och kronor." + CHR(10) + CHR(10) + "2  Ben�mning enligt registrering 
vid uppl�gg av " + CHR(10) + "   projektnumret." + CHR(10) + CHR(10) + "3  Projektnummer och 
eventuellt delnummer." + CHR(10) + CHR(10) + "4  Datum f�r avslut." + CHR(10) + CHR(10) + "5  EA-Ekvivalent 
arbetsm�ngd planerat och utf�rt." + CHR(10) + "   Ber�knas som summan mont�rstid och omr�knad maskintid" 
+ CHR(10) + "   enligt EBR-kostnadskatalog. Beredartid ing�r ej." + CHR(10) + CHR(10) + "6  Resultat ber�knas 
som EA planerat/EA utf�rt." + CHR(10) + CHR(10) + "7  Mont�rstimmar planerat och utf�rt. H�r redovisas inte" 
+ CHR(10) + "   beredartid." + CHR(10) + CHR(10) + "8  Maskinkostnad planerat och utf�rt enligt redovisning" 
+ CHR(10) + "   fr�n ekonomisystemet." + CHR(10) + CHR(10) + "9  Maskintimmar planerat och utf�rt." + CHR(10) 
+ CHR(10) + "10 Materielkostnad planerad och registrerat via" + CHR(10) + "   ekonomisystemet." + CHR(10) 
+ CHR(10) + "11 L�n avser kostnad f�r mont�rs- och beredartid." + CHR(10) + CHR(10) + 
"12 �vrig int�kt visar vad som fakturerats p�" + CHR(10) + "   arbetsordernumret." + CHR(10) + CHR(10) + 
"13 Tot. redovisar summa planerad och redovisad kostnad." + CHR(10) + CHR(10) + "14 Ber tim visar planerad 
och utf�rda beredningstimmar." + CHR(10) + "   Dels f�r personer som har befattningen beredare och de" 
+ CHR(10) + "   som vid registrering av tid angett pristypen beredare." + CHR(10) + CHR(10) + "15 KOST RA:" 
+ CHR(10) + "   Kostnadsregistreringar fr�n ekonomisystemet anges med" + CHR(10) + "   JA p� utfallsraden." 
+ CHR(10) + CHR(10) + CHR(10) + "�vrigt:" + CHR(10) + "-------" + CHR(10) + "Nederst i listan redovisas de 
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

