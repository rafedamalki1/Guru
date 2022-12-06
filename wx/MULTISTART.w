&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER nyprog AS LOGICAL NO-UNDO.
nyprog = FALSE.
DEFINE NEW SHARED VARIABLE hmtprog AS LOGICAL NO-UNDO.
hmtprog = nyprog.
SESSION:SUPPRESS-WARNINGS = TRUE. 

/* Local Variable Definitions ---                                       */

/*ALLA*/
/*menyaonr*/
{AONR.I} 
{KALKIN.I}
{FAKTIN.I}
{MARKVARDIN.I}
DEFINE VARIABLE ttbuffcopyh AS HANDLE NO-UNDO.

DEFINE NEW SHARED VARIABLE btnberh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnkalkh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.SharedVariable:btnfakth AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.SharedVariable:btnmarkh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnmtrlh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.SharedVariable:btndepah AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btntidh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.SharedVariable:btnupph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnpersh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.SharedVariable:btnsekh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.SharedVariable:btnregh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.SharedVariable:btnplanh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnstorh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/

DEFINE NEW SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.   /*lena satsinformation i mtrlmeny*/
DEFINE NEW SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE globanvbyt AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitriggh AS HANDLE NO-UNDO.
DEFINE VARIABLE Guru.SharedVariable:btnaonrh AS HANDLE NO-UNDO.
DEFINE VARIABLE btnguruh AS HANDLE NO-UNDO.
DEFINE VARIABLE startmh AS HANDLE NO-UNDO.

{WHANDLTEMP.I}
   
/*wstart*/
{WSTART.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{AVDELNINGTEMP.I}
/*BEREDNING*/
{BERVAR2.I}
 {HOPALLA.I}  
DEFINE NEW SHARED VARIABLE summeradberedning AS LOGICAL NO-UNDO.
/*kalkylering*/
&Scoped-define NEW NEW
{KALKALLTEMP.I}
DEFINE NEW SHARED TEMP-TABLE extravaldfasttemp NO-UNDO LIKE valdfasttemp.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
/*fakt*/
{FAKTPLANTEMP.I}   

DEFINE NEW SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE fbestapph AS HANDLE NO-UNDO.
/*mark*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MARKTEMP.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE mvalvardtemp NO-UNDO  LIKE valvardtemp.
   /*mtrl*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
{SPECMTRLTEMP.I}
DEFINE NEW SHARED TEMP-TABLE mspec_mtrlextra NO-UNDO LIKE spec_mtrl.
/*DEPÅ*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
DEFINE NEW SHARED TEMP-TABLE visa NO-UNDO
   FIELD UT AS CHARACTER    
   FIELD TYP AS CHARACTER       
   FIELD ORDNING AS INTEGER
   FIELD UPPFOLJVAL AS INTEGER
   FIELD KUURVAL AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   INDEX ORDNING IS PRIMARY ORDNING KUURVAL
   INDEX UT UT.
{DEPATEMP.I}
/*TID*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{ANSVARIGTEMP.I}
{GODKANNARTEMP.I}
DEFINE NEW SHARED TEMP-TABLE markpers    
   FIELD EFTERNAMN AS CHARACTER
   FIELD FORNAMN AS CHARACTER
   FIELD PERSONALKOD AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING. 
{PERSONALTEMP.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE pmpersonaltemp NO-UNDO LIKE personaltemp.
DEFINE NEW SHARED TEMP-TABLE vavdelningtemp    
   FIELD AVDELNINGNR AS INTEGER  
   FIELD AVDELNINGNAMN AS CHARACTER
   INDEX AVD IS PRIMARY AVDELNINGNR ASCENDING.
DEFINE NEW SHARED TEMP-TABLE vomrtemp
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY OMRADE
   INDEX OMRNAMN NAMN.
DEFINE {&NEW} {&SHARED} TEMP-TABLE egnaao NO-UNDO  LIKE utsokaonr.
/*UPPF*/
{BEFTEMP.I}
{VISUPPTMP.I}

/*pers*/
{VPERSONALTEMP.I}
/*SEK*/
{ANVTEMPS.I}
DEFINE NEW SHARED TEMP-TABLE berkalanvandartemp NO-UNDO LIKE anvandartemp.
{GURUSKEKMENY.I}
/*PLAN*/
&Scoped-define NEW NEW
{PLANNRTEMP.I}
&Scoped-define NEW NEW
/*REG*/
DEFINE NEW SHARED TEMP-TABLE rvisa NO-UNDO
   FIELD UT AS CHARACTER    
   FIELD TYP AS CHARACTER       
   FIELD ORDNING AS INTEGER
   FIELD UPPFOLJVAL AS INTEGER
   FIELD KUURVAL AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   INDEX ORDNING IS PRIMARY ORDNING KUURVAL
   INDEX UT UT.
/*STOR*/
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{STRTEMP.I}
DEFINE NEW SHARED VARIABLE strproch AS HANDLE NO-UNDO. /*Handle till STORHMT.P*/
DEFINE TEMP-TABLE copyvalberedningtemp NO-UNDO LIKE valberedningtemp.
DEFINE TEMP-TABLE copyvfaktplantemp    NO-UNDO LIKE vfaktplantemp.
DEFINE TEMP-TABLE copyvaldfasttemp     NO-UNDO LIKE valdfasttemp.
DEFINE TEMP-TABLE copymvalvardtemp      NO-UNDO LIKE mvalvardtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME AMULTISTART-FRAME
&Scoped-define BROWSE-NAME BRW_ANSV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ansvarigtemp anvandartemp utsokaonr ~
aotidslagtemp avdelningtemp depatemp godkannartemp mtrltemp markpers ~
xgurutemp mspec_mtrlextra omrtemp personaltemp pmpersonaltemp plannrtemp ~
rvisa faktplantemp utvaldfasttemp visaupp urberedningtemp urvardtemp ~
urstorntemp valdaaotemp vavdelningtemp valberedningtemp vfaktplantemp ~
valdfasttemp mvalvardtemp vomrtemp valperstemp valplantemp vstorntemp

/* Definitions for BROWSE BRW_ANSV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_ANSV ansvarigtemp.PERSONALKOD ~
ansvarigtemp.FORNAMN ansvarigtemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANSV 
&Scoped-define QUERY-STRING-BRW_ANSV FOR EACH ansvarigtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANSV OPEN QUERY BRW_ANSV FOR EACH ansvarigtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANSV ansvarigtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANSV ansvarigtemp


/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE ~
anvandartemp.AV-NAMN anvandartemp.AV-LEVEL anvandartemp.PERSONALKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV 
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH anvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH anvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV anvandartemp


/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT utsokaonr.BEREDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR 
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR utsokaonr


/* Definitions for BROWSE BRW_AOTID                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AOTID aotidslagtemp.AONR ~
aotidslagtemp.DELNR aotidslagtemp.TIDLAGE aotidslagtemp.AKTIVITET1 ~
aotidslagtemp.DAT1 aotidslagtemp.ANVANDARE1 aotidslagtemp.AKTIVITET2 ~
aotidslagtemp.DAT2 aotidslagtemp.ANVANDARE2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AOTID 
&Scoped-define QUERY-STRING-BRW_AOTID FOR EACH aotidslagtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AOTID OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AOTID aotidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AOTID aotidslagtemp


/* Definitions for BROWSE BRW_AVD                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_AVD avdelningtemp.AVDELNINGNR ~
avdelningtemp.AVDELNINGNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVD 
&Scoped-define QUERY-STRING-BRW_AVD FOR EACH avdelningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVD OPEN QUERY BRW_AVD FOR EACH avdelningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVD avdelningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVD avdelningtemp


/* Definitions for BROWSE BRW_DEPA                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_DEPA depatemp.Dep-Nr depatemp.Benamning 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEPA 
&Scoped-define QUERY-STRING-BRW_DEPA FOR EACH depatemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DEPA OPEN QUERY BRW_DEPA FOR EACH depatemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DEPA depatemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEPA depatemp


/* Definitions for BROWSE BRW_GODK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_GODK godkannartemp.PERSONALKOD ~
godkannartemp.FORNAMN godkannartemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GODK 
&Scoped-define QUERY-STRING-BRW_GODK FOR EACH godkannartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_GODK OPEN QUERY BRW_GODK FOR EACH godkannartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_GODK godkannartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GODK godkannartemp


/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MARK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MARK markpers.PERSONALKOD ~
markpers.FORNAMN markpers.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MARK 
&Scoped-define QUERY-STRING-BRW_MARK FOR EACH markpers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MARK OPEN QUERY BRW_MARK FOR EACH markpers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MARK markpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MARK markpers


/* Definitions for BROWSE BRW_MENY                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MENY xgurutemp.AV-LEVEL xgurutemp.MENY ~
xgurutemp.MENYOK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MENY 
&Scoped-define QUERY-STRING-BRW_MENY FOR EACH xgurutemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MENY OPEN QUERY BRW_MENY FOR EACH xgurutemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MENY xgurutemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MENY xgurutemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL mspec_mtrlextra.Enr ~
mspec_mtrlextra.BERKVANT mspec_mtrlextra.Enhet mspec_mtrlextra.LEVKOD ~
mspec_mtrlextra.NPRIS mspec_mtrlextra.Benamning 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL mspec_mtrlextra.BERKVANT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL mspec_mtrlextra
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL mspec_mtrlextra
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH mspec_mtrlextra NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH mspec_mtrlextra NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL mspec_mtrlextra
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL mspec_mtrlextra


/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR omrtemp.OMRADE omrtemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH omrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH omrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMR omrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR omrtemp


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.FORNAMN personaltemp.EFTERNAMN personaltemp.OMRADE ~
personaltemp.VECKOSCHEMA personaltemp.BEFATTNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for BROWSE BRW_PERS-10                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS-10 pmpersonaltemp.PERSONALKOD ~
pmpersonaltemp.AKTIV pmpersonaltemp.FORNAMN pmpersonaltemp.EFTERNAMN ~
pmpersonaltemp.TELEFON pmpersonaltemp.MOBILTEL pmpersonaltemp.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS-10 
&Scoped-define QUERY-STRING-BRW_PERS-10 FOR EACH pmpersonaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS-10 OPEN QUERY BRW_PERS-10 FOR EACH pmpersonaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS-10 pmpersonaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS-10 pmpersonaltemp


/* Definitions for BROWSE BRW_PLAN                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PLAN plannrtemp.OMRADE plannrtemp.PLANNR ~
plannrtemp.ARTAL plannrtemp.ORT plannrtemp.AONR plannrtemp.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PLAN 
&Scoped-define QUERY-STRING-BRW_PLAN FOR EACH plannrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PLAN OPEN QUERY BRW_PLAN FOR EACH plannrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PLAN plannrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PLAN plannrtemp


/* Definitions for BROWSE BRW_REG                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_REG rvisa.UT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_REG 
&Scoped-define QUERY-STRING-BRW_REG FOR EACH rvisa NO-LOCK
&Scoped-define OPEN-QUERY-BRW_REG OPEN QUERY BRW_REG FOR EACH rvisa NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_REG rvisa
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_REG rvisa


/* Definitions for BROWSE BRW_UFAKT                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_UFAKT faktplantemp.FAKTNR ~
faktplantemp.NAMN faktplantemp.VIBESTID faktplantemp.SENASTFAK ~
faktplantemp.PANVANDARE faktplantemp.ANVANDARE faktplantemp.HANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UFAKT 
&Scoped-define QUERY-STRING-BRW_UFAKT FOR EACH faktplantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UFAKT OPEN QUERY BRW_UFAKT FOR EACH faktplantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UFAKT faktplantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UFAKT faktplantemp


/* Definitions for BROWSE BRW_UKALK                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_UKALK utvaldfasttemp.OMRADE ~
utvaldfasttemp.KALKNR utvaldfasttemp.BENAMNING utvaldfasttemp.TYPCHAR ~
utvaldfasttemp.VIKATAR utvaldfasttemp.AONR utvaldfasttemp.DELNR ~
utvaldfasttemp.PLANNR utvaldfasttemp.ARTAL utvaldfasttemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UKALK 
&Scoped-define QUERY-STRING-BRW_UKALK FOR EACH utvaldfasttemp NO-LOCK ~
    BY utvaldfasttemp.OMRADE ~
       BY utvaldfasttemp.AONR ~
        BY utvaldfasttemp.DELNR
&Scoped-define OPEN-QUERY-BRW_UKALK OPEN QUERY BRW_UKALK FOR EACH utvaldfasttemp NO-LOCK ~
    BY utvaldfasttemp.OMRADE ~
       BY utvaldfasttemp.AONR ~
        BY utvaldfasttemp.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_UKALK utvaldfasttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UKALK utvaldfasttemp


/* Definitions for BROWSE BRW_UPP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UPP visaupp.UT visaupp.TYP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPP 
&Scoped-define QUERY-STRING-BRW_UPP FOR EACH visaupp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UPP OPEN QUERY BRW_UPP FOR EACH visaupp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UPP visaupp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPP visaupp


/* Definitions for BROWSE BRW_URBER                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_URBER urberedningtemp.OMRADE ~
urberedningtemp.BERNR urberedningtemp.BENAMNING urberedningtemp.AONR ~
urberedningtemp.DELNR urberedningtemp.AKTIV urberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_URBER 
&Scoped-define QUERY-STRING-BRW_URBER FOR EACH urberedningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_URBER OPEN QUERY BRW_URBER FOR EACH urberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_URBER urberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_URBER urberedningtemp


/* Definitions for BROWSE BRW_URMARK                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_URMARK urvardtemp.OMRADE ~
urvardtemp.VARDNR urvardtemp.BENAMNING urvardtemp.AONR urvardtemp.DELNR ~
urvardtemp.VARDANV urvardtemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_URMARK 
&Scoped-define QUERY-STRING-BRW_URMARK FOR EACH urvardtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_URMARK OPEN QUERY BRW_URMARK FOR EACH urvardtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_URMARK urvardtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_URMARK urvardtemp


/* Definitions for BROWSE BRW_URSTR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_URSTR urstorntemp.HDATUM ~
urstorntemp.HKLOCKAN urstorntemp.ANVANDARE urstorntemp.ANSVARIGPERS ~
urstorntemp.MERJOBB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_URSTR 
&Scoped-define QUERY-STRING-BRW_URSTR FOR EACH urstorntemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_URSTR OPEN QUERY BRW_URSTR FOR EACH urstorntemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_URSTR urstorntemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_URSTR urstorntemp


/* Definitions for BROWSE BRW_VAONR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VAONR valdaaotemp.AONR valdaaotemp.DELNR ~
valdaaotemp.ORT valdaaotemp.OMRADE valdaaotemp.BEREDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAONR 
&Scoped-define QUERY-STRING-BRW_VAONR FOR EACH valdaaotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAONR OPEN QUERY BRW_VAONR FOR EACH valdaaotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAONR valdaaotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAONR valdaaotemp


/* Definitions for BROWSE BRW_VAVD                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VAVD vavdelningtemp.AVDELNINGNR ~
vavdelningtemp.AVDELNINGNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAVD 
&Scoped-define QUERY-STRING-BRW_VAVD FOR EACH vavdelningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAVD OPEN QUERY BRW_VAVD FOR EACH vavdelningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAVD vavdelningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAVD vavdelningtemp


/* Definitions for BROWSE BRW_VBER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VBER valberedningtemp.OMRADE ~
valberedningtemp.BERNR valberedningtemp.BENAMNING valberedningtemp.AONR ~
valberedningtemp.DELNR valberedningtemp.AKTIV valberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VBER 
&Scoped-define QUERY-STRING-BRW_VBER FOR EACH valberedningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VBER OPEN QUERY BRW_VBER FOR EACH valberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VBER valberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VBER valberedningtemp


/* Definitions for BROWSE BRW_VFAKT                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VFAKT vfaktplantemp.FAKTNR ~
vfaktplantemp.NAMN vfaktplantemp.VIBESTID vfaktplantemp.SENASTFAK ~
vfaktplantemp.PANVANDARE vfaktplantemp.ANVANDARE vfaktplantemp.HANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VFAKT 
&Scoped-define QUERY-STRING-BRW_VFAKT FOR EACH vfaktplantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VFAKT OPEN QUERY BRW_VFAKT FOR EACH vfaktplantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VFAKT vfaktplantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VFAKT vfaktplantemp


/* Definitions for BROWSE BRW_VKALK                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VKALK valdfasttemp.KALKNR ~
valdfasttemp.BENAMNING valdfasttemp.TYPCHAR valdfasttemp.VIKATAR ~
valdfasttemp.AONR valdfasttemp.DELNR valdfasttemp.PLANNR valdfasttemp.ARTAL ~
valdfasttemp.OMRADE valdfasttemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VKALK 
&Scoped-define QUERY-STRING-BRW_VKALK FOR EACH valdfasttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VKALK OPEN QUERY BRW_VKALK FOR EACH valdfasttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VKALK valdfasttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VKALK valdfasttemp


/* Definitions for BROWSE BRW_VMARK                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VMARK mvalvardtemp.OMRADE ~
mvalvardtemp.VARDNR mvalvardtemp.BENAMNING mvalvardtemp.AONR ~
mvalvardtemp.DELNR mvalvardtemp.VARDANV mvalvardtemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VMARK 
&Scoped-define QUERY-STRING-BRW_VMARK FOR EACH mvalvardtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VMARK OPEN QUERY BRW_VMARK FOR EACH mvalvardtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VMARK mvalvardtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VMARK mvalvardtemp


/* Definitions for BROWSE BRW_VOMR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VOMR vomrtemp.OMRADE vomrtemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VOMR 
&Scoped-define QUERY-STRING-BRW_VOMR FOR EACH vomrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VOMR OPEN QUERY BRW_VOMR FOR EACH vomrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VOMR vomrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VOMR vomrtemp


/* Definitions for BROWSE BRW_VPERS                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VPERS valperstemp.PERSONALKOD ~
valperstemp.FORNAMN valperstemp.EFTERNAMN valperstemp.TELEFON ~
valperstemp.MOBILTEL valperstemp.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VPERS 
&Scoped-define QUERY-STRING-BRW_VPERS FOR EACH valperstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VPERS OPEN QUERY BRW_VPERS FOR EACH valperstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VPERS valperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VPERS valperstemp


/* Definitions for BROWSE BRW_VPLAN                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VPLAN valplantemp.OMRADE ~
valplantemp.PLANNR valplantemp.ARTAL valplantemp.ORT valplantemp.AONR ~
valplantemp.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VPLAN 
&Scoped-define QUERY-STRING-BRW_VPLAN FOR EACH valplantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VPLAN OPEN QUERY BRW_VPLAN FOR EACH valplantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VPLAN valplantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VPLAN valplantemp


/* Definitions for BROWSE BRW_VSTR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VSTR vstorntemp.HDATUM ~
vstorntemp.HKLOCKAN vstorntemp.ANVANDARE vstorntemp.ANSVARIGPERS ~
vstorntemp.MERJOBB vstorntemp.STORNUMMERID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VSTR 
&Scoped-define QUERY-STRING-BRW_VSTR FOR EACH vstorntemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VSTR OPEN QUERY BRW_VSTR FOR EACH vstorntemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VSTR vstorntemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VSTR vstorntemp


/* Definitions for FRAME FRAME-ANV                                      */

/* Definitions for FRAME FRAME-AONR                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-AONR ~
    ~{&OPEN-QUERY-BRW_AONR}~
    ~{&OPEN-QUERY-BRW_VAONR}

/* Definitions for FRAME FRAME-BEH                                      */

/* Definitions for FRAME FRAME-BER                                      */

/* Definitions for FRAME FRAME-DEPA                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-DEPA ~
    ~{&OPEN-QUERY-BRW_DEPA}

/* Definitions for FRAME FRAME-FAKT                                     */

/* Definitions for FRAME FRAME-KALK                                     */

/* Definitions for FRAME FRAME-MARK                                     */

/* Definitions for FRAME FRAME-MTRL                                     */

/* Definitions for FRAME FRAME-PERS                                     */

/* Definitions for FRAME FRAME-PLAN                                     */

/* Definitions for FRAME FRAME-REG                                      */

/* Definitions for FRAME FRAME-STATUS                                   */

/* Definitions for FRAME FRAME-STOR                                     */

/* Definitions for FRAME FRAME-TID                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TID ~
    ~{&OPEN-QUERY-BRW_AVD}~
    ~{&OPEN-QUERY-BRW_VAVD}~
    ~{&OPEN-QUERY-BRW_VOMR}

/* Definitions for FRAME FRAME-UPP                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-UPP ~
    ~{&OPEN-QUERY-BRW_UPP}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_MTRLM IMAGE-4 BTN_AONR RECT-MENY ~
BTN_TIDM BTN_REGM BTN_SEKM BTN_MARKM BTN_KALKM BTN_FAKTM BTN_BERM BTN_DEPA ~
BTN_FLEX BTN_PLAN BTN_SMS BTN_STOR BTN_GURU BTN_PERS BTN_UPPF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_HLEV 
       MENU-ITEM m_Visa_information LABEL "Visa information via vald Leverantör"
       MENU-ITEM m_Sats_information LABEL "Sats information"
       MENU-ITEM m_AvmarkeraHLEV LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_MTRL 
       MENU-ITEM m_Visa_information2 LABEL "Visa information via vald Leverantör"
       MENU-ITEM m_Sats_information2 LABEL "Sats information".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AONR 
     IMAGE-UP FILE "BILDER/xbtn_proj.gif":U NO-FOCUS
     LABEL "AONR" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_BERM 
     IMAGE-UP FILE "BILDER/xbtn_ber.gif":U NO-FOCUS
     LABEL "BEREDNING" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_DEPA 
     IMAGE-UP FILE "BILDER/xbtn_depa.gif":U NO-FOCUS
     LABEL "DEPÅ" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_FAKTM 
     IMAGE-UP FILE "BILDER/xbtn_fakt.gif":U NO-FOCUS
     LABEL "FAKTURERING" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_FLEX 
     IMAGE-UP FILE "BILDER/xbtn_flex.gif":U NO-FOCUS
     LABEL "FLEXTID" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_GURU 
     IMAGE-UP FILE "BILDER/xbtn_guru.gif":U NO-FOCUS
     LABEL "GURU" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_KALKM 
     IMAGE-UP FILE "BILDER/xbtn_kalk.gif":U NO-FOCUS
     LABEL "KALKYLERING" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_MARKM 
     IMAGE-UP FILE "BILDER/xbtn_mark.gif":U NO-FOCUS
     LABEL "MARKVÄRDERING" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_MTRLM 
     IMAGE-UP FILE "BILDER/xbtn_mtrl.gif":U NO-FOCUS
     LABEL "Materielhantering" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_PERS 
     IMAGE-UP FILE "BILDER/xbtn_pers.gif":U NO-FOCUS
     LABEL "PERSADM" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_PLAN 
     IMAGE-UP FILE "BILDER/xbtn_plan.gif":U NO-FOCUS
     LABEL "PLAN" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_REGM 
     IMAGE-UP FILE "BILDER/xbtn_reg.gif":U NO-FOCUS
     LABEL "REGISTER" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_SEKM 
     IMAGE-UP FILE "BILDER/xbtn_sek.gif":U NO-FOCUS
     LABEL "SEKRETESS" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_SMS 
     IMAGE-UP FILE "BILDER/xbtn_sms.gif":U NO-FOCUS
     LABEL "SMS ADM" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_STOR 
     IMAGE-UP FILE "BILDER/xbtn_stor.gif":U NO-FOCUS
     LABEL "AVBROTT/STÖRNING" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_TIDM 
     IMAGE-UP FILE "BILDER/xbtn_tid.gif":U NO-FOCUS
     LABEL "TIDREDOVISNING" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_UPPF 
     IMAGE-UP FILE "BILDER/xbtn_upp.gif":U NO-FOCUS
     LABEL "UPPFÖLJNING" 
     SIZE 7.25 BY 2.33.

DEFINE IMAGE IMAGE-4
     FILENAME "BILDER/xbgmenu.gif":U TRANSPARENT
     SIZE 5.5 BY 2.

DEFINE RECTANGLE RECT-MENY
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY .08.

DEFINE BUTTON BTN_BORT-11 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY-11 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP-11 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISA-11 
     LABEL "Visa":L 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_SANVANDARE-11 AS CHARACTER FORMAT "x(12)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 13.75 BY .83.

DEFINE VARIABLE FILL-IN_SNAMN-11 AS CHARACTER FORMAT "x(40)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 16.88 BY .83.

DEFINE VARIABLE FILL-IN_TEXTANV AS CHARACTER FORMAT "X(256)":U INITIAL "Sekretess" 
      VIEW-AS TEXT 
     SIZE 70 BY 1.21
     FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-15
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66.25 BY 1.21
     BGCOLOR 8 .

DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AOF 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVROP 
     LABEL "Best-Avrop":L 
     SIZE 14 BY 1 TOOLTIP "Besställning/Avrop".

DEFINE BUTTON BTN_AVSAONR 
     LABEL "Avsluta aonr":L 
     SIZE 14 BY 1 TOOLTIP "Avsluta aonr:et. Ingen tidskrivning skall kan registreras mera".

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BER 
     LABEL "Bereda":L 
     SIZE 14 BY 1 TOOLTIP "Beredningsmodul".

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1 TOOLTIP "Ta bort ett nytt aonr".

DEFINE BUTTON BTN_BYTPNR 
     LABEL "Byt aonr":L 
     SIZE 14 BY 1 TOOLTIP "Byt ett aonr till ett annat och flytta gjorda kopplingar".

DEFINE BUTTON BTN_FAKT 
     IMAGE-UP FILE "BILDER\xbtn_faktu.gif":U
     LABEL "Fakturera":L 
     SIZE 14 BY 1 TOOLTIP "Fakturarutinen".

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_HAOF 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALK 
     LABEL "Kalkylera":L 
     SIZE 14 BY 1 TOOLTIP "Kalkylera olika typer av kalkyler".

DEFINE BUTTON BTN_KOPI 
     LABEL "Kopiera":L 
     SIZE 14 BY 1 TOOLTIP "Kopiera ett aonr".

DEFINE BUTTON BTN_KOST 
     LABEL "Kostnadsreg":L 
     SIZE 14 BY 1 TOOLTIP "Registrera externa kostnader eller visa kostnader från andra system.".

DEFINE BUTTON BTN_MARK 
     LABEL "Markvärdera":L 
     SIZE 14 BY 1 TOOLTIP "Markvärderingsmodul".

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1 TOOLTIP "Skapa ett nytt aonr.".

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_RAPP 
     LABEL "Rapporter":L 
     SIZE 14 BY 1 TOOLTIP "Rapport sammanstälning".

DEFINE BUTTON BTN_STATUS 
     LABEL "Status":L 
     SIZE 14 BY 1 TOOLTIP "Tidlägen - Milstolpar".

DEFINE BUTTON BTN_TIDPLAN 
     LABEL "Tidplan" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UNDER 
     LABEL "Underindela":L 
     SIZE 14 BY 1 TOOLTIP "Underindela dvs skapa nya delnr".

DEFINE BUTTON BTN_UPP 
     LABEL "Aonr huvud":L 
     SIZE 14 BY 1 TOOLTIP "Här kan du ändra allt som rör aonr:et".

DEFINE BUTTON BTN_VISAO 
     LABEL "Visa":L 
     SIZE 14 BY 1 TOOLTIP "Visa/skriv ut allt som rör aonr:et".

DEFINE VARIABLE CMB_ANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetsansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_ARBART AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetsart" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BERE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beredare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BESORG AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beställare/Kund" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FAK AS CHARACTER FORMAT "X(35)":U 
     LABEL "Fakturakategori" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PRIO AS CHARACTER FORMAT "X(35)":U 
     LABEL "Prioritet" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 23 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_PROJ AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av arbetsorder" 
      VIEW-AS TEXT 
     SIZE 34.25 BY 1.08
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta:" 
      VIEW-AS TEXT 
     SIZE 6.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSLUTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSTARTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-K1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "KONTOK1" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Det går att använda ~"*~" men endast som ex ~"32*~"." NO-UNDO.

DEFINE VARIABLE FILL-IN-K2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "KONTO" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Det går att använda ~"*~" men endast som ex ~"32*~"." NO-UNDO.

DEFINE VARIABLE FILL-IN-KTO AS CHARACTER FORMAT "X(256)":U INITIAL "Urval konto" 
      VIEW-AS TEXT 
     SIZE 12.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-MELL AS CHARACTER FORMAT "X(6)":U 
      VIEW-AS TEXT 
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH AS CHARACTER FORMAT "X(3)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-REF AS CHARACTER FORMAT "X(256)" 
     LABEL "Ref.nr beställare" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .83 TOOLTIP "Sök på Ref.nr Beställare".

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(256)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83 TOOLTIP "Sök på benämning" NO-UNDO.

DEFINE VARIABLE FILL-IN_SAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Sök på Aonr"
     FGCOLOR 2  NO-UNDO.

DEFINE IMAGE IMAGE-6
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_PAAV AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pågående", 1,
"Avslutade", 2
     SIZE 20 BY 1 TOOLTIP "Utsökningen ska gälla antingen pågående eller avslutade eller både och." NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 1.21.

DEFINE VARIABLE TOG_AONY AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 1.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_AVSLUTADE AS LOGICAL INITIAL no 
     LABEL "Avslutade" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 TOOLTIP "Utsökningen ska gälla avslutade mellan angivna datum." NO-UNDO.

DEFINE VARIABLE TOG_FASTA AS LOGICAL INITIAL no 
     LABEL "Fasta" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_HUVNR AS LOGICAL INITIAL no 
     LABEL "Endast huvudnummer" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_PAGA AS LOGICAL INITIAL no 
     LABEL "Pågående" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 TOOLTIP "Utsökningen ska gälla pågående." NO-UNDO.

DEFINE VARIABLE TOG_TILLF AS LOGICAL INITIAL no 
     LABEL "Tillfälliga" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 NO-UNDO.

DEFINE BUTTON BTN_BORT-112 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY-112 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_AV-LEVEL-FRAN AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Från nivå" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_AV-LEVEL-NY AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Ny nivå" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_TEXTBEH AS CHARACTER FORMAT "X(256)":U INITIAL "Sekretess" 
      VIEW-AS TEXT 
     SIZE 70 BY 1.21
     FONT 17 NO-UNDO.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70.25 BY 2.42
     BGCOLOR 8 .

DEFINE VARIABLE SEL_MENY AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 26 BY 17.29
     FONT 4 NO-UNDO.

DEFINE BUTTON BTN_ADM 
     LABEL "Administration" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ALLBACK-2 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-2 
     IMAGE-UP FILE "BILDER\forwrd-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AOF-2 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ATG 
     LABEL "Åtgärder" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ATT 
     LABEL "Attestera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB-2 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BA 
     LABEL "Begär attest" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-2 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT-2 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_EXP 
     LABEL "Export" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HAMT-2 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_HAOF-2 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IMP 
     LABEL "Import" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INAKTIV 
     LABEL "Aktiv/Inaktiv":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INK 
     LABEL "Inköp" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALK-2 
     IMAGE-UP FILE "BILDER\xbtn_bered.gif":U
     LABEL "Bereda":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOPI-2 
     LABEL "Kopiera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LAS 
     LABEL "Låsta beredningar" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LIST 
     LABEL "Listor" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY-2 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER-2 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_UPP-2 
     LABEL "Beredningshuvud":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UTF-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT-B AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av beredning" 
      VIEW-AS TEXT 
     SIZE 26.5 BY 1.21
     FGCOLOR 5 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     LABEL "Beredningsnr" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY .83
     FGCOLOR 5  NO-UNDO.

DEFINE VARIABLE FILL-IN-HBERNR AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka beredningar."
     FGCOLOR 5 .

DEFINE VARIABLE FILL-IN-HTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta beredning:" 
      VIEW-AS TEXT 
     SIZE 16.75 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(8)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR-2 AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EAONR-2 AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE IMAGE IMAGE-7
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 2.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 1.21.

DEFINE VARIABLE TOG_AKT-2 AS LOGICAL INITIAL yes 
     LABEL "Aktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 10.75 BY .71 NO-UNDO.

DEFINE VARIABLE TOG_BERAO AS LOGICAL INITIAL no 
     LABEL "Endast ej kopplade" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY .75 NO-UNDO.

DEFINE VARIABLE TOG_BERNY AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 1.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_INAKT-2 AS LOGICAL INITIAL no 
     LABEL "Inaktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 11.63 BY .71 NO-UNDO.

DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB-7 AUTO-GO 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_best 
     LABEL "Beställningar":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT-7 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BSTAT 
     LABEL "Best.status":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_inventering 
     LABEL "Inventering":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_lager 
     LABEL "Lager":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LAS-2 
     LABEL "Lås upp" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LEV 
     LABEL "Leveransstatus":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LEVE 
     LABEL "Leverantör" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MTRL-7 
     LABEL "Materiel Admin" 
     SIZE 14 BY 1 TOOLTIP "Uppdatera kataloger mm.".

DEFINE BUTTON BTN_MTRLPRIS 
     LABEL "Materiel Priser" 
     SIZE 14 BY 1 TOOLTIP "Visning materielpriser mm.".

DEFINE BUTTON BTN_NY-7 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_RAPP-7 
     LABEL "Rapporter":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SEK 
     LABEL "Sekretess":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UT 
     LABEL "Uttag/Returer":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA-7 
     LABEL "vISA":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visa allt", 3,
"Visning per år", 1,
"Visning per period", 2
     SIZE 14 BY 1 TOOLTIP "Vising av kostnader och intäkter." NO-UNDO.

DEFINE BUTTON BTN_ADM-3 
     LABEL "Faktura adm":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ALLBACK-4 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla personer i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda personer tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-4 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla personer i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla personer väljs"
     FONT 11.

DEFINE BUTTON BTN_AOF-4 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AONRM 
     LABEL "Aonr med Faktp.nr" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AONRU 
     LABEL "Aonr utan Faktp.nr" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB-4 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-4 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT-4 
     LABEL "Ta Bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FAK 
     IMAGE-UP FILE "BILDER\xbtn_faktu.gif":U
     LABEL "Fakturera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FLISTA 
     LABEL "Fakturerat kr":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HAMT-4 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Hämta och visa urval utifrån sökkriterierna ovan".

DEFINE BUTTON BTN_HAOF-4 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOPP 
     LABEL "Koppla":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_Kred 
     LABEL "Kreditfakt." 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY-4 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER-4 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_PRELB 
     LABEL "Ta bort prel.fakt.":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP-4 
     LABEL "Fakturaplan":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VFAK 
     LABEL "Visa fakturor" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISAO-2 
     LABEL "Visa fakturaplan":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_BESORG-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beställare/Kund" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FAK-4 AS CHARACTER FORMAT "X(35)":U 
     LABEL "Fakturakategori" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR-4 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HAMT AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta:" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Slutfakt. från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR-4 AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EAONR-4 AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EFAKTNR AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Faktp.nr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFAKUNR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fakturanr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PROJEKTKOD AS CHARACTER FORMAT "X(8)" 
     LABEL "Projektkod" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_SFAKTNR AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Faktp.nr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83.

DEFINE VARIABLE FILL-IN_SNAMN AS CHARACTER FORMAT "X(20)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 12.88 BY .83.

DEFINE IMAGE IMAGE-8
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 2.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE TOG_ANSV AS LOGICAL INITIAL yes 
     LABEL "Ansvarig" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_GAMLA AS LOGICAL INITIAL no 
     LABEL "Visa avs. fakturap.nr" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .83 NO-UNDO.

DEFINE VARIABLE TOG_GOD AS LOGICAL INITIAL no 
     LABEL "Godk. prel.fakt." 
     VIEW-AS TOGGLE-BOX
     SIZE 30.88 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_HUVA AS LOGICAL INITIAL no 
     LABEL "Huvudansvarig" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_PREL AS LOGICAL INITIAL no 
     LABEL "Endast prel.fakt." 
     VIEW-AS TOGGLE-BOX
     SIZE 19.63 BY .79 NO-UNDO.

DEFINE BUTTON BTN_ADM-2 
     LABEL "Administration":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ALLBACK-3 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.25 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-3 
     IMAGE-UP FILE "BILDER\forwrd-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.25 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AOF-3 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB-3 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-3 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.25 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT-3 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_EXP-2 
     LABEL "Export" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HAMT-3 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_HAOF-3 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IMP-2 
     LABEL "Import" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INAKTIV-2 
     LABEL "Aktiv/Inaktiv":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALK-3 
     IMAGE-UP FILE "BILDER\xbtn_kalkyl.gif":U
     LABEL "Kalkylera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KONV 
     LABEL "Konvertera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOPI-3 
     LABEL "Kopiera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY-3 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER-3 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.25 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_UPP-3 
     LABEL "Kalkyl huvud":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISKAL 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD-3 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BESORG-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Best./Kund" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP-3 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kalkylansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KTYP-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kalkyl typ" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR-3 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UTF-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KATEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av kalkyler" 
      VIEW-AS TEXT 
     SIZE 37.13 BY 1.21
     FGCOLOR 1 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-KPLANNR AS CHARACTER FORMAT "X(6)" 
     LABEL "Plannr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE FILL-IN-VALK AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta kalkyl nr:" 
      VIEW-AS TEXT 
     SIZE 11.75 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR-3 AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EAONR-3 AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EKALNR AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka kalkyler."
     FGCOLOR 1 .

DEFINE VARIABLE FILL-IN_KALKB AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_KALKYL AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Kalkyl nr" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .83
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FILL-IN_KAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83
     FGCOLOR 2  NO-UNDO.

DEFINE IMAGE IMAGE-9
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 2.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE TOG_AKT-3 AS LOGICAL INITIAL no 
     LABEL "Aktiva" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.25 BY .75 TOOLTIP "Utsökningen ska gälla aktiva kalkyler." NO-UNDO.

DEFINE VARIABLE TOG_INAK-3 AS LOGICAL INITIAL no 
     LABEL "Inaktiva" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.63 BY .75 TOOLTIP "Utsökningen ska gälla inaktiva kalkyler." NO-UNDO.

DEFINE VARIABLE TOG_KALKAO AS LOGICAL INITIAL no 
     LABEL "Endast ej kopplade" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.5 BY 1 NO-UNDO.

DEFINE BUTTON BTN_ADM-4 
     LABEL "Admin. markvärd." 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ALLBACK-5 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-5 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AOF-5 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB-5 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-5 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT-5 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_EXPM 
     LABEL "Export" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FASTIGHET 
     LABEL "Fastigh./Markägare" 
     SIZE 14 BY 1 TOOLTIP "Registrera/ändra fastigheter och markägare, skapa kopplingar".

DEFINE BUTTON BTN_HAMT-5 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_HAOF-5 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IMPM 
     LABEL "Import" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INAKTIV-3 
     LABEL "Aktiv/Inaktiv":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY-5 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OMRAKNA 
     LABEL "Omräkna m.v. KPI" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OVER-5 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_UPP-5 
     LABEL "Markv. huvud":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VARD 
     IMAGE-UP FILE "BILDER\xbtn_markvard.gif":U
     LABEL "Markvärdering":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISKAL-2 
     LABEL "Visa markvärdering":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISMARK 
     LABEL "Visa markägare":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ANSV-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Värderingsansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR-5 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UTF-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av markvärdering" 
      VIEW-AS TEXT 
     SIZE 29.63 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-HTEXT-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta markv.:" 
      VIEW-AS TEXT 
     SIZE 11.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR-5 AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN_BEN-5 AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR-5 AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EAONR-5 AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EVARD AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     LABEL "Värdering nr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_SVARD AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     LABEL "Värdering nr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_VARDANV AS CHARACTER FORMAT "x(40)" 
     LABEL "Värderingsman" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 2.33.

DEFINE VARIABLE TOG_AKT-5 AS LOGICAL INITIAL yes 
     LABEL "Aktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_INAKT-5 AS LOGICAL INITIAL no 
     LABEL "Inaktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 12.5 BY 1 NO-UNDO.

DEFINE BUTTON BTN_AVB-6 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back-6 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_IEXC 
     LABEL "Import Exel" 
     SIZE 14 BY 1 TOOLTIP "Enr första kolumnen, antal i andra (blank = 1), lev i tredje (blank = vald leverantör till vänster i bilden),ev benämning i fjärde."
     FGCOLOR 1 .

DEFINE BUTTON BTN_LEVH 
     LABEL "Åter huvudleverantör" 
     SIZE 18 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_MTRL 
     LABEL "Materiel Admin" 
     SIZE 14 BY 1 TOOLTIP "Uppdatera kataloger mm.".

DEFINE BUTTON BTN_OFF 
     LABEL "Off. Onninen" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON btn_over-6 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(30)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>>9":U INITIAL 1 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 11.88 BY .83 TOOLTIP "Tryck ~"Enter~" för att uppdatera" NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN-6 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR2 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 32.5 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 3.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 3.5
     BGCOLOR 8 .

DEFINE BUTTON BTN_BLOBADM 
     LABEL "Upplägg av BLOB:ar" 
     SIZE 43 BY 2.

DEFINE BUTTON BTN_EJAUTO 
     LABEL "Manuell start av tidautomatkörning":L 
     SIZE 43 BY 2.

DEFINE BUTTON BTN_KOSTS 
     LABEL "Manuell inläsning av kostnadregistreringar":L 
     SIZE 43 BY 2.

DEFINE BUTTON BTN_OBEORD 
     LABEL "Manuell utskick av epost övertidsbeordrare":L 
     SIZE 43 BY 2.

DEFINE BUTTON BTN_SUPPORT 
     LABEL "Support ärenden" 
     SIZE 43 BY 2.

DEFINE VARIABLE FILL-IN_TEXTOVR AS CHARACTER FORMAT "X(256)":U INITIAL "Sekretess" 
      VIEW-AS TEXT 
     SIZE 70 BY 1.21
     FONT 17 NO-UNDO.

DEFINE BUTTON BTN_ALLBACK-10 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla personer i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda personer tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-10 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla personer i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla personer väljs"
     FONT 11.

DEFINE BUTTON BTN_AOF-10 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ARB-10 
     LABEL "Arbetstider":L 
     SIZE 14 BY 1 TOOLTIP "Byte av veckoschema".

DEFINE BUTTON BTN_AVB-10 AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-10 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT-10 
     LABEL "Ta bort":L 
     SIZE 12 BY 1 TOOLTIP "Ta bort personal".

DEFINE BUTTON BTN_DEBPR 
     LABEL "Debiteringspriser":L 
     SIZE 14 BY 1 TOOLTIP "Ändring av debiteringspriser".

DEFINE BUTTON BTN_HAMT-10 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Hämta och visa urval utifrån sökkriterierna ovan".

DEFINE BUTTON BTN_HAOF-10 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY-10 
     LABEL "Ny":L 
     SIZE 12 BY 1 TOOLTIP "Skapa ny personal".

DEFINE BUTTON BTN_OT 
     LABEL "Övertidsbeord" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OVER-10 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_SCH 
     LABEL "Scheman":L 
     SIZE 14 BY 1 TOOLTIP "Uppläggning av scheman".

DEFINE BUTTON BTN_SEK-10 
     LABEL "Sekretess":L 
     SIZE 14 BY 1 TOOLTIP "Ändring av personal".

DEFINE BUTTON BTN_TELEFONLISTA 
     LABEL "Telefon" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP-10 
     LABEL "Personal":L 
     SIZE 14 BY 1 TOOLTIP "Ändring av personal".

DEFINE BUTTON BTN_VISA-10 
     LABEL "Visa arbetstider" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISAP 
     LABEL "Visa":L 
     SIZE 14 BY 1 TOOLTIP "Visa personaluppgifter".

DEFINE VARIABLE CMB_AKTIV-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Aktiv eller Inaktiv" 
     LABEL "Pers. med tidskrivning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Aktiv eller Inaktiv","Aktiv","Inaktiv" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_ANST-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Anställningsform" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_AVD-10 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BEFATTNING-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Befattning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BER-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beredskapsalternativ" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_DELTID-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Ja eller Nej" 
     LABEL "Deltid" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ja eller Nej","Ja","Nej" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FLEX-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Ja eller Nej" 
     LABEL "Flex" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ja eller Nej","Ja","Nej" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP-10 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_OMR-10 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Visa personal." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_SEMFOR-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Ja eller Nej" 
     LABEL "Förskottssem" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ja eller Nej","Ja","Nej" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TIDSGODK-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Godkänner tid" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TRA-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Traktamentsavtal" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VECKO-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Veckoschema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PERSONAL AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av personal" 
      VIEW-AS TEXT 
     SIZE 23.5 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EPERSONALKOD-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_SEFTERNAMN-10 AS CHARACTER FORMAT "x(256)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN-10 AS CHARACTER FORMAT "x(256)" 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD-10 AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .83.

DEFINE IMAGE IMAGE-11
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 2.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 1.21
     BGCOLOR 8 .

DEFINE BUTTON BTN_ALLBACK-13 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda plannr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-13 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla plannr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla plannr väljs"
     FONT 11.

DEFINE BUTTON BTN_AOF-13 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB-13 AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVSAONR-2 
     LABEL "Avsluta plannr":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-13 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT-13 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BUD 
     LABEL "Budget":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BUNDER 
     LABEL "Ta bort uppdel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE-133 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-134 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_HAMT-6 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1.

DEFINE BUTTON BTN_HAOF-13 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALK-4 
     LABEL "Kalkylera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NVE-133 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-134 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY-13 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER-13 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_PTIDPLAN 
     LABEL "Tidplan":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_RAPP-2 
     LABEL "Rapporter":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UNDER-2 
     LABEL "Årsuppdela":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP-6 
     LABEL "Plannummerhuvud":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISAO-3 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ANSV-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_ARBART-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetsart" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_ARTAL-13 AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "år" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BESORG-13 AS CHARACTER FORMAT "X(16)":U 
     LABEL "Best/Kund" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FRAN-13 AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "från" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_OMR-13 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TILL-13 AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "till" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSLUTD-13 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSTARTD-13 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-K1-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "KONTOK1" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Det går att använda ~"*~" men endast som ex ~"32*~"." NO-UNDO.

DEFINE VARIABLE FILL-IN-K2-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "KONTO" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Det går att använda ~"*~" men endast som ex ~"32*~"." NO-UNDO.

DEFINE VARIABLE FILL-IN-KTO-13 AS CHARACTER FORMAT "X(256)":U INITIAL "Urval konto" 
      VIEW-AS TEXT 
     SIZE 12.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-MELL-13 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH-13 AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VAL AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta plannr:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_ARTAL-13 AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .83
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE FILL-IN_EPLANNR-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE FILL-IN_ORT-13 AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PLANNRVAL AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av plannummer" 
      VIEW-AS TEXT 
     SIZE 29.5 BY 1.21
     FGCOLOR 4 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_SPLANNR-13 AS CHARACTER FORMAT "X(6)" 
     LABEL "Plannummer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .83
     FGCOLOR 4  NO-UNDO.

DEFINE IMAGE IMAGE-12
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD-13 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visa år", 1,
"Visa period", 2
     SIZE 14 BY 2.46 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 1.21.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 2.33.

DEFINE VARIABLE TOG_AVSLUTADE-13 AS LOGICAL INITIAL no 
     LABEL "Avslutade" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 TOOLTIP "Utsökningen ska gälla avslutade mellan angivna datum." NO-UNDO.

DEFINE VARIABLE TOG_FASTA-13 AS LOGICAL INITIAL no 
     LABEL "Fasta" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_PAGA-13 AS LOGICAL INITIAL no 
     LABEL "Pågående" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY 1 TOOLTIP "Utsökningen ska gälla pågående." NO-UNDO.

DEFINE VARIABLE TOG_TILLF-13 AS LOGICAL INITIAL no 
     LABEL "Tillfälliga" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE BUTTON BTN_AVB-12 AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOR 
     LABEL "Kör funktion" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Register" 
      VIEW-AS TEXT 
     SIZE 15.75 BY 1.21
     FONT 17 NO-UNDO.

DEFINE BUTTON BTN_AVB-11 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKR-2 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV-2 
     LABEL "Skriv ut alla" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRTEST 
     LABEL "Skrivar test" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VIALLA 
     LABEL "Visa alla" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VPERS 
     LABEL "Visa tid och pers.sek" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_ANV 
     LABEL "Användare" 
     SIZE 7.5 BY 1.92.

DEFINE BUTTON MBTN_BEH 
     LABEL "Behörighet" 
     SIZE 7.5 BY 1.92.

DEFINE BUTTON MBTN_OVR 
     LABEL "Övrigt" 
     SIZE 7.5 BY 1.92.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Användare", 1,
"Behörighet", 2,
"Övrigt", 3
     SIZE 61 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-MENY-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY .08.

DEFINE BUTTON BTN_AVBGURU AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_BYT 
     LABEL "Byt användare" 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_BYTW 
     LABEL "Byt fönsterstorlek" 
     SIZE 19 BY 1.5 TOOLTIP "Anpassa Guru till din skärmupplösning mm.".

DEFINE BUTTON BTN_MEDD 
     LABEL "Skapa meddelande till Guruanvändare" 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_UPPDAT 
     LABEL "Uppdatera program" 
     SIZE 19 BY 1.5 TOOLTIP "Uppdatera din Guru applikation med den senaste versionen.".

DEFINE VARIABLE EDD_FUNK AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 35 BY 7
     BGCOLOR 15 FGCOLOR 9 FONT 5 NO-UNDO.

DEFINE VARIABLE ED_WWW AS CHARACTER INITIAL "Nyheter på www.elpool.se." 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 28.5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ELPOOL AS CHARACTER FORMAT "X(256)":U INITIAL "Nyheter på www.elpool.se." 
      VIEW-AS TEXT 
     SIZE 28.5 BY .75
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FILL-IN-GURU AS CHARACTER FORMAT "X(256)":U INITIAL "Välkommen till" 
      VIEW-AS TEXT 
     SIZE 50.5 BY 1.21
     BGCOLOR 15 FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "adeicon/blank":U CONVERT-3D-COLORS
     SIZE 8 BY 1.

DEFINE IMAGE IMAGE-5
     FILENAME "BILDER/xstartguru.gif":U
     STRETCH-TO-FIT
     SIZE 125 BY 25.75.

DEFINE BUTTON BTN_ATER 
     LABEL "Åter":L 
     SIZE 14 BY 1 TOOLTIP "Tidlägen - Milstolpar".

DEFINE BUTTON BTN_ADM-14 
     LABEL "Admin. störning" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ALLBACK-14 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-14 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AND-14 
     LABEL "Störningshuvud" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AOF-14 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB-14 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BACK-14 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT-14 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_HAMT-14 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_HAOF-14 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LAS-14 
     LABEL "Import/Export" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY-14 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER-14 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_RAPP-14 
     LABEL "Rapporter" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VIS-14 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ANL-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Anläggning, -del" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_AR-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BEL-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Belägenhet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FEL-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Felorsak" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FOR-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Företag" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 28.75 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Distrikt" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 28.75 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_SYS-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Frånkopplingsställe" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_SYS2-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Felställe" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT-14 AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av störning" 
      VIEW-AS TEXT 
     SIZE 26.5 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN-14 AS CHARACTER FORMAT "x(25)" 
     LABEL "Upprättad av" 
     VIEW-AS FILL-IN 
     SIZE 17.75 BY .83.

DEFINE VARIABLE FILL-IN-BEN-142 AS CHARACTER FORMAT "x(25)" 
     LABEL "Ansvarig" 
     VIEW-AS FILL-IN 
     SIZE 17.75 BY .83.

DEFINE VARIABLE FILL-IN-STOR-14 AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9.75 BY .83.

DEFINE VARIABLE FILL-IN-STRNR-14 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Störnings-ID" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-13
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_VAL-14 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Driftstörning", 1,
"Planerat avbrott", 2
     SIZE 36.5 BY 1.04 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 2.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 1.21
     BGCOLOR 8 .

DEFINE BUTTON BTN_ALLBACK-8 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla områden i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda områden tas bort"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-8 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla områden i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla områden väljs"
     FONT 11.

DEFINE BUTTON BTN_ANDGOD 
     LABEL "Ändra godkänd tid":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ANDT 
     LABEL "Ändra tid.reg." 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ANOV 
     LABEL "Övriga listor" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AOF-8 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ARB 
     LABEL "Arbetstider" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ARES 
     LABEL "Ändra tjänsteresa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB-8 AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVVIK 
     LABEL "Avvikelse" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-8 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort".

DEFINE BUTTON BTN_BER-8 
     LABEL "Beredskap":L 
     SIZE 14 BY 1 TOOLTIP "Registrera/ta bort beredskapsperiod".

DEFINE BUTTON BTN_FDA 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FLKORN 
     LABEL "Flexkörning" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FRAMAN 
     LABEL "Flexrapport/Månad" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FRAPP 
     LABEL "Flexrapport m saldo" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_GOD 
     LABEL "Godkänna tidsedel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_GRAN 
     LABEL "Granska tillägg":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HAOF-8 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KONT 
     LABEL "Kontroll av tid":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KONTROLL 
     LABEL "Flexkontroll" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LGOD 
     LABEL "Lista tidsedlar":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LON 
     LABEL "Lönetillägg":L 
     SIZE 14 BY 1 TOOLTIP "Registrera/ta bort lönetillägg/avdrag".

DEFINE BUTTON BTN_LONA 
     LABEL "Löneadmin. tid":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MANSAL 
     LABEL "Uppdat. saldo" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NDA 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OVERT 
     LABEL "Övertidstillägg":L 
     SIZE 14 BY 1 TOOLTIP "Titta på övertidstillägg tolkade av registrerad tid ".

DEFINE BUTTON BTN_PALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla områden i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda personer tas bort"
     FONT 11.

DEFINE BUTTON BTN_PALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla PERSONER i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla personer väljs"
     FONT 11.

DEFINE BUTTON BTN_PBACK 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort".

DEFINE BUTTON BTN_PERIOD 
     LABEL "Frånvaroperiod" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PVVOVER 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_REG 
     LABEL "Reg. beredskap":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SALDO 
     LABEL "Aktuellt saldo" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SALMAN 
     LABEL "Saldo per månad" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_TBRES 
     LABEL "Radera tjänsteresa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_TID 
     LABEL "Tidregistrering":L 
     SIZE 14 BY 1 TOOLTIP "Registrera/ta bort tidregistreringar, tjänsteresor".

DEFINE BUTTON BTN_TRA 
     LABEL "Traktamente":L 
     SIZE 14 BY 1 TOOLTIP "Titta på traktamente som kommer från tjänstereseregistrering. Borttag görs i tjänstereserutinen".

DEFINE BUTTON BTN_UTRES 
     LABEL "Reg. utlandsresa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VBACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort".

DEFINE BUTTON BTN_VECK 
     LABEL "Eko-löne-sammans.":L 
     SIZE 14 BY 1 TOOLTIP "Kör samtliga godkända tidsedlar t.o.m angivet datum till Eko.- och lönesystem.".

DEFINE BUTTON BTN_VISA-8 
     LABEL "Visa senaste":L 
     SIZE 14 BY 1 TOOLTIP "Visa senaste tidregistreringen".

DEFINE BUTTON BTN_VOVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_VTID 
     LABEL "Visa tidsedel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VTIDV 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VVOVER-8 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON MBTN_FLEXADM 
     IMAGE-UP FILE "BILDER/xbtn_flexadm.gif":U
     LABEL "Flexadm" 
     SIZE 7.5 BY 1.92.

DEFINE BUTTON MBTN_STANSA 
     LABEL "Stansa upp" 
     SIZE 7.5 BY 1.92.

DEFINE BUTTON MBTN_TIDADM 
     IMAGE-UP FILE "BILDER/xbtn_tidadm.gif":U
     LABEL "Tidadm" 
     SIZE 7.5 BY 1.92.

DEFINE BUTTON MBTN_TIDSEDEL 
     IMAGE-UP FILE "BILDER/xbtn_tidsedel.gif":U
     LABEL "Visa tidsedel" 
     SIZE 7.5 BY 1.92.

DEFINE BUTTON MBTN_TILLAGG 
     IMAGE-UP FILE "BILDER/xbtn_tillagg.gif":U
     LABEL "Tid och tillägg" 
     SIZE 7.5 BY 1.92.

DEFINE BUTTON MBTN_TJANSTERESA 
     IMAGE-UP FILE "BILDER/xbtn_resor.gif":U
     LABEL "Tjänsteresor" 
     SIZE 7.5 BY 1.92.

DEFINE VARIABLE CMB_AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_MANAD AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december" 
     DROP-DOWN-LIST
     SIZE 12 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_OMR-8 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_TAR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_TARSL AS INTEGER FORMAT "9999":U INITIAL ? 
     LABEL "Slut" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_TARST AS INTEGER FORMAT "9999":U INITIAL ? 
     LABEL "Start" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_TMANAD AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 13
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december","hela året" 
     DROP-DOWN-LIST
     SIZE 12 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_TMANADSL AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december" 
     DROP-DOWN-LIST
     SIZE 12 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_TMANADST AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december" 
     DROP-DOWN-LIST
     SIZE 12 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-HAMTA-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta person:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TDAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TDAG-2 AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TDATUM AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "T.o.m" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TDATUM-2 AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "F.o.m" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-VP AS CHARACTER FORMAT "X(256)":U INITIAL "Visa personal för:" 
      VIEW-AS TEXT 
     SIZE 22.5 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9.13 BY 1.04 NO-UNDO.

DEFINE VARIABLE FILL-IN_EPERSONALKOD-8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_SEFTERNAMN-8 AS CHARACTER FORMAT "x(25)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN-8 AS CHARACTER FORMAT "x(15)" 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD-8 AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .83.

DEFINE IMAGE IMAGE-14
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLTID AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 3 BY .67 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          " ", 0
     SIZE 74.88 BY .58 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_TIDSVAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla tidsedlar", 1,
"Godkända", 2,
"Ej godkända", 3
     SIZE 20 BY 2 NO-UNDO.

DEFINE RECTANGLE RECT-H
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 2.33.

DEFINE RECTANGLE RECT-MENY-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY .08.

DEFINE RECTANGLE RECT-S
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53.5 BY 1.21.

DEFINE VARIABLE TOG_MAN AS LOGICAL INITIAL no 
     LABEL "Månadssammanst." 
     VIEW-AS TOGGLE-BOX
     SIZE 18.5 BY 1 NO-UNDO.

DEFINE BUTTON BTN_AVB-9 AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA-9 
     LABEL "Visa" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANSV FOR 
      ansvarigtemp SCROLLING.

DEFINE QUERY BRW_ANV FOR 
      anvandartemp SCROLLING.

DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_AOTID FOR 
      aotidslagtemp SCROLLING.

DEFINE QUERY BRW_AVD FOR 
      avdelningtemp SCROLLING.

DEFINE QUERY BRW_DEPA FOR 
      depatemp SCROLLING.

DEFINE QUERY BRW_GODK FOR 
      godkannartemp SCROLLING.

DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_MARK FOR 
      markpers SCROLLING.

DEFINE QUERY BRW_MENY FOR 
      xgurutemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      mspec_mtrlextra SCROLLING.

DEFINE QUERY BRW_OMR FOR 
      omrtemp SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.

DEFINE QUERY BRW_PERS-10 FOR 
      pmpersonaltemp SCROLLING.

DEFINE QUERY BRW_PLAN FOR 
      plannrtemp SCROLLING.

DEFINE QUERY BRW_REG FOR 
      rvisa SCROLLING.

DEFINE QUERY BRW_UFAKT FOR 
      faktplantemp SCROLLING.

DEFINE QUERY BRW_UKALK FOR 
      utvaldfasttemp SCROLLING.

DEFINE QUERY BRW_UPP FOR 
      visaupp SCROLLING.

DEFINE QUERY BRW_URBER FOR 
      urberedningtemp SCROLLING.

DEFINE QUERY BRW_URMARK FOR 
      urvardtemp SCROLLING.

DEFINE QUERY BRW_URSTR FOR 
      urstorntemp SCROLLING.

DEFINE QUERY BRW_VAONR FOR 
      valdaaotemp SCROLLING.

DEFINE QUERY BRW_VAVD FOR 
      vavdelningtemp SCROLLING.

DEFINE QUERY BRW_VBER FOR 
      valberedningtemp SCROLLING.

DEFINE QUERY BRW_VFAKT FOR 
      vfaktplantemp SCROLLING.

DEFINE QUERY BRW_VKALK FOR 
      valdfasttemp SCROLLING.

DEFINE QUERY BRW_VMARK FOR 
      mvalvardtemp SCROLLING.

DEFINE QUERY BRW_VOMR FOR 
      vomrtemp SCROLLING.

DEFINE QUERY BRW_VPERS FOR 
      valperstemp SCROLLING.

DEFINE QUERY BRW_VPLAN FOR 
      valplantemp SCROLLING.

DEFINE QUERY BRW_VSTR FOR 
      vstorntemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANSV C-Win _STRUCTURED
  QUERY BRW_ANSV NO-LOCK DISPLAY
      ansvarigtemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      ansvarigtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 20
      ansvarigtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 13.92
         FONT 4
         TITLE "Ansvariga tidredovisare".

DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV C-Win _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(256)":U
            WIDTH 12
      anvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(256)":U
            WIDTH 40
      anvandartemp.AV-LEVEL COLUMN-LABEL "Anv.!Nivå" FORMAT ">>>9":U
      anvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 66.25 BY 17.96
         FONT 4
         TITLE "Användare".

DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR C-Win _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(256)":U
            WIDTH 18.5
      utsokaonr.BEREDARE FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 13.04
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_AOTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AOTID C-Win _STRUCTURED
  QUERY BRW_AOTID NO-LOCK DISPLAY
      aotidslagtemp.AONR FORMAT "X(6)":U
      aotidslagtemp.DELNR COLUMN-LABEL "cDELNR" FORMAT "999":U
      aotidslagtemp.TIDLAGE COLUMN-LABEL "Tidläge" FORMAT "X(256)":U
            WIDTH 23
      aotidslagtemp.AKTIVITET1 COLUMN-LABEL "Aktivitet" FORMAT "X(256)":U
            WIDTH 18 COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      aotidslagtemp.DAT1 FORMAT "99/99/99":U COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      aotidslagtemp.ANVANDARE1 COLUMN-LABEL "Ändrad av" FORMAT "x(12)":U
            WIDTH 10 COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      aotidslagtemp.AKTIVITET2 COLUMN-LABEL "Aktivitet" FORMAT "X(256)":U
            WIDTH 18 COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      aotidslagtemp.DAT2 FORMAT "99/99/99":U COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      aotidslagtemp.ANVANDARE2 COLUMN-LABEL "Ändrad av" FORMAT "x(12)":U
            WIDTH 10 COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 109 BY 23.75
         TITLE "Tidlägen".

DEFINE BROWSE BRW_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVD C-Win _STRUCTURED
  QUERY BRW_AVD NO-LOCK DISPLAY
      avdelningtemp.AVDELNINGNR COLUMN-LABEL "Avdelning" FORMAT ">>>>>>9":U
      avdelningtemp.AVDELNINGNAMN COLUMN-LABEL "Namn" FORMAT "x(16)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 28.5 BY 8.25
         FONT 4
         TITLE "Avdelningar".

DEFINE BROWSE BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DEPA C-Win _STRUCTURED
  QUERY BRW_DEPA NO-LOCK DISPLAY
      depatemp.Dep-Nr FORMAT ">>>":U
      depatemp.Benamning FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 50.5 BY 20.21.

DEFINE BROWSE BRW_GODK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GODK C-Win _STRUCTURED
  QUERY BRW_GODK NO-LOCK DISPLAY
      godkannartemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      godkannartemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 20
      godkannartemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 13.92
         FONT 4
         TITLE "Godkänner tidsedlar".

DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV C-Win _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 30
      mtrltemp.Enhet COLUMN-LABEL "Enh" FORMAT "x(3)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 50 BY 19
         FONT 4
         TITLE "Materiellista huvudleverantör.".

DEFINE BROWSE BRW_MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MARK C-Win _STRUCTURED
  QUERY BRW_MARK NO-LOCK DISPLAY
      markpers.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(5)":U
            WIDTH 7
      markpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U
            WIDTH 15
      markpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 13.92
         FONT 4
         TITLE "Valda personer".

DEFINE BROWSE BRW_MAXMALL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MAXMALL C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 80 BY 20
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_MENY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MENY C-Win _STRUCTURED
  QUERY BRW_MENY NO-LOCK DISPLAY
      xgurutemp.AV-LEVEL COLUMN-LABEL "Nivå" FORMAT ">>>9":U COLUMN-FONT 4
            LABEL-FONT 4
      xgurutemp.MENY COLUMN-LABEL "Meny" FORMAT "x(256)":U WIDTH 25
            COLUMN-FONT 4 LABEL-FONT 4
      xgurutemp.MENYOK COLUMN-LABEL "Behörig" FORMAT "Ja/Nej":U
            COLUMN-FONT 4 LABEL-FONT 4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 39.88 BY 17.29
         FONT 4
         TITLE "Behörighetslista".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL C-Win _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      mspec_mtrlextra.Enr FORMAT "X(11)":U
      mspec_mtrlextra.BERKVANT FORMAT "->>,>>9.99":U
      mspec_mtrlextra.Enhet COLUMN-LABEL "Enh" FORMAT "x(3)":U
      mspec_mtrlextra.LEVKOD COLUMN-LABEL "Lev" FORMAT "X(5)":U
            WIDTH 3
      mspec_mtrlextra.NPRIS FORMAT ">>>>99.99":U
      mspec_mtrlextra.Benamning FORMAT "x(256)":U WIDTH 5
  ENABLE
      mspec_mtrlextra.BERKVANT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 54 BY 19
         FONT 4
         TITLE "Vald materiel".

DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR C-Win _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      omrtemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 28.5 BY 9
         FONT 4
         TITLE "Område".

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 12
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 25
      personaltemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      personaltemp.VECKOSCHEMA COLUMN-LABEL "Vecko!schema" FORMAT "99":U
      personaltemp.BEFATTNING COLUMN-LABEL "Befatting" FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 13.92
         FONT 4
         TITLE "Personal".

DEFINE BROWSE BRW_PERS-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS-10 C-Win _STRUCTURED
  QUERY BRW_PERS-10 NO-LOCK DISPLAY
      pmpersonaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      pmpersonaltemp.AKTIV COLUMN-LABEL "Tid!skrivning" FORMAT "Aktiv/Inaktiv":U
      pmpersonaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 13
      pmpersonaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 25
      pmpersonaltemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "x(11)":U
      pmpersonaltemp.MOBILTEL COLUMN-LABEL "Mobiltele" FORMAT "x(11)":U
      pmpersonaltemp.OMRADE FORMAT "x(6)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 13.58
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_PLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PLAN C-Win _STRUCTURED
  QUERY BRW_PLAN NO-LOCK DISPLAY
      plannrtemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      plannrtemp.PLANNR COLUMN-LABEL "Plannr" FORMAT "X(8)":U COLUMN-FGCOLOR 4
            LABEL-FGCOLOR 4
      plannrtemp.ARTAL COLUMN-LABEL "Årtal" FORMAT "9999":U COLUMN-FGCOLOR 4
            LABEL-FGCOLOR 4
      plannrtemp.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(25)":U
      plannrtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(8)":U WIDTH 7
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      plannrtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 13.25
         FONT 4
         TITLE "Urvalsresutat".

DEFINE BROWSE BRW_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_REG C-Win _STRUCTURED
  QUERY BRW_REG NO-LOCK DISPLAY
      rvisa.UT COLUMN-LABEL "Register" FORMAT "X(256)":U WIDTH 45.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING NO-SCROLLBAR-VERTICAL SIZE 48.25 BY 14.

DEFINE BROWSE BRW_UFAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UFAKT C-Win _STRUCTURED
  QUERY BRW_UFAKT NO-LOCK DISPLAY
      faktplantemp.FAKTNR COLUMN-LABEL "Faktp.!nr" FORMAT ">>>>>>9":U
            WIDTH 4
      faktplantemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U WIDTH 20
      faktplantemp.VIBESTID COLUMN-LABEL "Best./!Kund" FORMAT "X(6)":U
            WIDTH 9
      faktplantemp.SENASTFAK COLUMN-LABEL "Senast!fakt." FORMAT "99/99/99":U
      faktplantemp.PANVANDARE COLUMN-LABEL "Senast!ändrad av" FORMAT "x(12)":U
      faktplantemp.ANVANDARE COLUMN-LABEL "Ansvarig" FORMAT "x(12)":U
      faktplantemp.HANVANDARE COLUMN-LABEL "Huvud-!ansvarig" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 49.5 BY 14.38
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_UKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UKALK C-Win _STRUCTURED
  QUERY BRW_UKALK NO-LOCK DISPLAY
      utvaldfasttemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      utvaldfasttemp.KALKNR COLUMN-LABEL "Kalkyl nr" FORMAT ">>>>>>9":U
            WIDTH 5.5 COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      utvaldfasttemp.BENAMNING COLUMN-LABEL "Ort/!Benämning" FORMAT "x(20)":U
      utvaldfasttemp.TYPCHAR COLUMN-LABEL "Typ" FORMAT "X(3)":U
      utvaldfasttemp.VIKATAR COLUMN-LABEL "Kat.!år" FORMAT "9999":U
      utvaldfasttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
      utvaldfasttemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      utvaldfasttemp.PLANNR COLUMN-LABEL "Plannr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      utvaldfasttemp.ARTAL FORMAT "9999":U COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      utvaldfasttemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 14.04
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPP C-Win _STRUCTURED
  QUERY BRW_UPP NO-LOCK DISPLAY
      visaupp.UT COLUMN-LABEL "Lista" FORMAT "X(50)":U
      visaupp.TYP COLUMN-LABEL "Typ" FORMAT "X(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 48.25 BY 23.

DEFINE BROWSE BRW_URBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_URBER C-Win _STRUCTURED
  QUERY BRW_URBER NO-LOCK DISPLAY
      urberedningtemp.OMRADE FORMAT "x(6)":U
      urberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 5.5 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      urberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 16.5
      urberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      urberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      urberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      urberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 49.5 BY 15.46
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_URMARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_URMARK C-Win _STRUCTURED
  QUERY BRW_URMARK NO-LOCK DISPLAY
      urvardtemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      urvardtemp.VARDNR COLUMN-LABEL "Värdering!nr" FORMAT "->,>>>,>>9":U
            WIDTH 9
      urvardtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(21)":U
      urvardtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      urvardtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
      urvardtemp.VARDANV COLUMN-LABEL "Värderings!ansvarig" FORMAT "X(8)":U
      urvardtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 15.71
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_URSTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_URSTR C-Win _STRUCTURED
  QUERY BRW_URSTR NO-LOCK DISPLAY
      urstorntemp.HDATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      urstorntemp.HKLOCKAN COLUMN-LABEL "Klockan" FORMAT "->>,>>9.99":U
      urstorntemp.ANVANDARE COLUMN-LABEL "Upprättad av" FORMAT "X(8)":U
      urstorntemp.ANSVARIGPERS COLUMN-LABEL "Ansvarig" FORMAT "X(8)":U
      urstorntemp.MERJOBB COLUMN-LABEL "Återstående" FORMAT "Ja/Nej":U
            WIDTH 11.75
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 14.88
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAONR C-Win _STRUCTURED
  QUERY BRW_VAONR NO-LOCK DISPLAY
      valdaaotemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      valdaaotemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      valdaaotemp.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(256)":U
            WIDTH 24.5
      valdaaotemp.OMRADE FORMAT "x(6)":U
      valdaaotemp.BEREDARE FORMAT "x(5)":U WIDTH 7
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 13.04
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_VAVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAVD C-Win _STRUCTURED
  QUERY BRW_VAVD NO-LOCK DISPLAY
      vavdelningtemp.AVDELNINGNR COLUMN-LABEL "Avdelning" FORMAT ">>>9":U
      vavdelningtemp.AVDELNINGNAMN COLUMN-LABEL "Namn" FORMAT "x(16)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 28.5 BY 8.25
         FONT 4
         TITLE "Valda avdelningar".

DEFINE BROWSE BRW_VBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VBER C-Win _STRUCTURED
  QUERY BRW_VBER NO-LOCK DISPLAY
      valberedningtemp.OMRADE FORMAT "x(6)":U
      valberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 7 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      valberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 19
      valberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      valberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 53.5 BY 15.46
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_VFAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VFAKT C-Win _STRUCTURED
  QUERY BRW_VFAKT NO-LOCK DISPLAY
      vfaktplantemp.FAKTNR COLUMN-LABEL "Faktp.!nr" FORMAT ">>>>>>9":U
            WIDTH 4.5
      vfaktplantemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U
            WIDTH 20
      vfaktplantemp.VIBESTID COLUMN-LABEL "Best./!Kund" FORMAT "X(6)":U
            WIDTH 9
      vfaktplantemp.SENASTFAK COLUMN-LABEL "Senast!fakt." FORMAT "99/99/99":U
      vfaktplantemp.PANVANDARE COLUMN-LABEL "Senast!ändrad av" FORMAT "x(12)":U
      vfaktplantemp.ANVANDARE COLUMN-LABEL "Ansvarig" FORMAT "x(12)":U
      vfaktplantemp.HANVANDARE COLUMN-LABEL "Huvud-!ansvarig" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 53.5 BY 14.38
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_VKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VKALK C-Win _STRUCTURED
  QUERY BRW_VKALK NO-LOCK DISPLAY
      valdfasttemp.KALKNR COLUMN-LABEL "Kalkyl nr" FORMAT ">>>>>>9":U
            WIDTH 6 COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      valdfasttemp.BENAMNING COLUMN-LABEL "Ort/!Benämning" FORMAT "x(20)":U
            WIDTH 2
      valdfasttemp.TYPCHAR COLUMN-LABEL "Typ" FORMAT "X(3)":U
      valdfasttemp.VIKATAR COLUMN-LABEL "Kat.!år" FORMAT "9999":U
      valdfasttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
      valdfasttemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
      valdfasttemp.PLANNR COLUMN-LABEL "Planr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      valdfasttemp.ARTAL FORMAT "9999":U COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      valdfasttemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      valdfasttemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
            WIDTH 4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 14.04
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_VMARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VMARK C-Win _STRUCTURED
  QUERY BRW_VMARK NO-LOCK DISPLAY
      mvalvardtemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      mvalvardtemp.VARDNR COLUMN-LABEL "Värdering!nr" FORMAT "->,>>>,>>9":U
            WIDTH 9
      mvalvardtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(21)":U
      mvalvardtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      mvalvardtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
      mvalvardtemp.VARDANV COLUMN-LABEL "Värderings!ansvarig" FORMAT "X(8)":U
      mvalvardtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 15.71
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_VOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VOMR C-Win _STRUCTURED
  QUERY BRW_VOMR NO-LOCK DISPLAY
      vomrtemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      vomrtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(19)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 28.5 BY 9
         FONT 4
         TITLE "Valda områden".

DEFINE BROWSE BRW_VPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VPERS C-Win _STRUCTURED
  QUERY BRW_VPERS NO-LOCK DISPLAY
      valperstemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      valperstemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 13
      valperstemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 25
      valperstemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "x(11)":U
      valperstemp.MOBILTEL COLUMN-LABEL "Mobiltele" FORMAT "x(11)":U
      valperstemp.OMRADE FORMAT "x(6)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 13.58
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_VPLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VPLAN C-Win _STRUCTURED
  QUERY BRW_VPLAN NO-LOCK DISPLAY
      valplantemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      valplantemp.PLANNR COLUMN-LABEL "Plannr" FORMAT "X(8)":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      valplantemp.ARTAL COLUMN-LABEL "Årtal" FORMAT "9999":U COLUMN-FGCOLOR 4
            LABEL-FGCOLOR 4
      valplantemp.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(25)":U
      valplantemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(8)":U WIDTH 6.5
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valplantemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 13.25
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_VSTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VSTR C-Win _STRUCTURED
  QUERY BRW_VSTR NO-LOCK DISPLAY
      vstorntemp.HDATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      vstorntemp.HKLOCKAN COLUMN-LABEL "Klockan" FORMAT "->>,>>9.99":U
      vstorntemp.ANVANDARE COLUMN-LABEL "Upprättad av" FORMAT "X(8)":U
      vstorntemp.ANSVARIGPERS COLUMN-LABEL "Ansvarig" FORMAT "X(8)":U
      vstorntemp.MERJOBB COLUMN-LABEL "Återstående" FORMAT "Ja/Nej":U
      vstorntemp.STORNUMMERID COLUMN-LABEL "Störnings id" FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 14.88
         FONT 4
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME AMULTISTART-FRAME
     BTN_MTRLM AT ROW 1 COL 22.38
     BTN_AONR AT ROW 1 COL 8.25
     BTN_TIDM AT ROW 1 COL 44.88
     BTN_REGM AT ROW 1 COL 82.38
     BTN_SEKM AT ROW 1 COL 75
     BTN_MARKM AT ROW 1 COL 104.5
     BTN_KALKM AT ROW 1 COL 37.25
     BTN_FAKTM AT ROW 1 COL 89.75
     BTN_BERM AT ROW 1 COL 15.38
     BTN_DEPA AT ROW 1 COL 29.75
     BTN_FLEX AT ROW 1 COL 52.5
     BTN_PLAN AT ROW 1 COL 97.13
     BTN_SMS AT ROW 1 COL 111.5
     BTN_STOR AT ROW 1 COL 118.38
     BTN_GURU AT ROW 1 COL 1
     BTN_PERS AT ROW 1 COL 67.63
     BTN_UPPF AT ROW 1 COL 60.25
     IMAGE-4 AT ROW 1 COL 120.5
     RECT-MENY AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.

DEFINE FRAME FRAME-AONR
     BTN_OVER AT ROW 16.75 COL 51.75
     BTN_ALLOVER AT ROW 14.67 COL 51.75
     BTN_NVE-3 AT ROW 1.79 COL 55.13
     BTN_NVE-4 AT ROW 1.79 COL 72.5
     FILL-IN-AVSTARTD AT ROW 2 COL 43.13 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSLUTD AT ROW 2 COL 60.5 COLON-ALIGNED NO-LABEL
     TOG_PAGA AT ROW 2.17 COL 3
     TOG_AVSLUTADE AT ROW 2.17 COL 23
     RAD_FAST AT ROW 2.25 COL 99.75 NO-LABEL
     BTN_FVE-3 AT ROW 2.58 COL 55.13
     BTN_FVE-4 AT ROW 2.58 COL 72.5
     TOG_TILLF AT ROW 2.92 COL 3
     TOG_FASTA AT ROW 2.92 COL 23
     FILL-IN-AR AT ROW 3.38 COL 107.38 COLON-ALIGNED NO-LABEL
     TOG_HUVNR AT ROW 3.67 COL 3
     RAD_PAAV AT ROW 3.75 COL 103.38 NO-LABEL
     CMB_JURP AT ROW 4.5 COL 8.38
     CMB_PROJ AT ROW 4.54 COL 56.75 COLON-ALIGNED
     CMB_AVD AT ROW 5.67 COL 7.38
     CMB_BERE AT ROW 5.67 COL 56.75 COLON-ALIGNED
     FILL-IN-K1 AT ROW 5.75 COL 96.13 COLON-ALIGNED
     CMB_ANSV AT ROW 6.79 COL 56.75 COLON-ALIGNED
     CMB_OMR AT ROW 6.83 COL 10.38
     FILL-IN-K2 AT ROW 6.96 COL 96.13 COLON-ALIGNED
     CMB_ARBART AT ROW 7.92 COL 56.75 COLON-ALIGNED
     CMB_BESORG AT ROW 8 COL 16.38 COLON-ALIGNED
     TOG_AONY AT ROW 8 COL 109.38
     CMB_PRIO AT ROW 9.08 COL 16.38 COLON-ALIGNED
     CMB_FAK AT ROW 9.08 COL 56.75 COLON-ALIGNED
     BTN_HAMT AT ROW 10.21 COL 18.5
     BRW_AONR AT ROW 11.42 COL 1.5
     BRW_VAONR AT ROW 11.42 COL 56.5
     BTN_AOF AT ROW 12.29 COL 111
     BTN_HAOF AT ROW 13.38 COL 111
     BTN_BYTPNR AT ROW 15.33 COL 111
     BTN_UPP AT ROW 16.29 COL 111
     BTN_MARK AT ROW 17.29 COL 111
     BTN_TIDPLAN AT ROW 18.08 COL 111
     BTN_BER AT ROW 18.88 COL 111
     BTN_UNDER AT ROW 19.25 COL 111
     BTN_FAKT AT ROW 19.79 COL 111
     BTN_KALK AT ROW 21.13 COL 111
     BTN_RAPP AT ROW 21.75 COL 111
     BTN_AVSAONR AT ROW 21.75 COL 111
     BTN_KOST AT ROW 22 COL 111
     BTN_VISAO AT ROW 22 COL 111
     BTN_STATUS AT ROW 22.54 COL 111
     BTN_KOPI AT ROW 23.5 COL 111
     BTN_AVROP AT ROW 24.42 COL 111
     BTN_NY AT ROW 24.54 COL 68
     BTN_BORT AT ROW 24.54 COL 85
     FILL-IN_SAONR AT ROW 25.83 COL 11.75 COLON-ALIGNED
     FILL-IN_ORT AT ROW 25.83 COL 36 COLON-ALIGNED
     FILL-IN_EAONR AT ROW 25.83 COL 65.88 COLON-ALIGNED AUTO-RETURN 
     BTN_ALLBACK AT ROW 20.92 COL 51.75
     FILL-IN_DELNR AT ROW 25.83 COL 75.25 COLON-ALIGNED NO-LABEL
     FILL-IN-REF AT ROW 25.83 COL 97.25 COLON-ALIGNED AUTO-RETURN 
     BTN_AVB AT ROW 25.83 COL 111
     BTN_BACK AT ROW 18.83 COL 51.75
     FILL-IN-AOTEXT AT ROW 1 COL 2.25 NO-LABEL
     FILL-IN-MELL AT ROW 2.13 COL 36 COLON-ALIGNED NO-LABEL
     FILL-IN-OCH AT ROW 2.13 COL 56.5 COLON-ALIGNED NO-LABEL
     FILL-IN-KTO AT ROW 4.75 COL 92.63 COLON-ALIGNED NO-LABEL
     FILL-IN-AOTEXT-3 AT ROW 25.83 COL 56.88 NO-LABEL
     IMAGE-6 AT ROW 25.88 COL 2.25
     RECT-50 AT ROW 25.63 COL 1.5
     RECT-60 AT ROW 25.63 COL 56.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-MTRL
     BRW_HLEV AT ROW 2.79 COL 1.5
     BRW_MTRL AT ROW 2.79 COL 56.25
     btn_over-6 AT ROW 7.29 COL 51.75
     BTN_MTRL AT ROW 8.5 COL 111
     BTN_VISA AT ROW 10.75 COL 111
     btn_back-6 AT ROW 10.79 COL 51.75
     BTN_OFF AT ROW 11.25 COL 111
     BTN_IEXC AT ROW 13.5 COL 111
     BTN_SKRIV AT ROW 17.25 COL 111
     CMB_LEV AT ROW 22 COL 22.38 COLON-ALIGNED
     BTN_LEVH AT ROW 22 COL 24.63
     FILL-IN-ENR AT ROW 23.54 COL 19.25 COLON-ALIGNED
     BTN_UP AT ROW 24 COL 106
     FILL-IN-BEN-6 AT ROW 24.54 COL 19.25 COLON-ALIGNED
     FILL-IN-ENR2 AT ROW 24.54 COL 62.63 COLON-ALIGNED
     FILL-IN-ANTAL AT ROW 24.54 COL 90.63 COLON-ALIGNED
     BTN_MIN AT ROW 25 COL 106
     RAD_SOK AT ROW 25.71 COL 17.5 NO-LABEL
     BTN_AVB-6 AT ROW 25.83 COL 111
     FILL-IN-SOKALT AT ROW 25.71 COL 3 NO-LABEL
     "Hämta och ange antal:" VIEW-AS TEXT
          SIZE 37.63 BY .67 AT ROW 23.71 COL 57.38
     "Materiel" VIEW-AS TEXT
          SIZE 23.5 BY 1.21 AT ROW 1 COL 2.5
          FONT 17
     IMAGE-1 AT ROW 23.54 COL 2.25
     RECT-2 AT ROW 23.33 COL 1.5
     RECT-4 AT ROW 23.33 COL 56.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-BER
     BTN_ALLOVER-2 AT ROW 12.5 COL 51.5
     BTN_ALLBACK-2 AT ROW 19.42 COL 51.5
     TOG_AKT-2 AT ROW 2.17 COL 3
     TOG_INAKT-2 AT ROW 2.17 COL 16.25
     TOG_BERAO AT ROW 2.17 COL 28.13
     CMB_JURP-2 AT ROW 3 COL 3.25
     CMB_AVD-2 AT ROW 4.08 COL 2.25
     CMB_OMR-2 AT ROW 5.17 COL 11.25 COLON-ALIGNED
     CMB_UTF-2 AT ROW 6.29 COL 11.25 COLON-ALIGNED
     BTN_HAMT-2 AT ROW 7.58 COL 18.5
     BTN_KALK-2 AT ROW 7.96 COL 111
     BTN_UPP-2 AT ROW 8.5 COL 111
     BRW_URBER AT ROW 8.79 COL 1.5
     BRW_VBER AT ROW 8.79 COL 56.13
     TOG_BERNY AT ROW 9.08 COL 109.38
     BTN_LIST AT ROW 9.21 COL 111
     BTN_INK AT ROW 10.71 COL 111
     BTN_AOF-2 AT ROW 11 COL 111
     BTN_HAOF-2 AT ROW 11 COL 111
     BTN_ATG AT ROW 12.21 COL 111
     BTN_KOPI-2 AT ROW 13.46 COL 111
     BTN_INAKTIV AT ROW 14.71 COL 111
     BTN_ADM AT ROW 16.21 COL 111
     BTN_LAS AT ROW 17.46 COL 111
     BTN_EXP AT ROW 18.46 COL 111
     BTN_IMP AT ROW 19.71 COL 111
     BTN_BACK-2 AT ROW 17.08 COL 51.5
     BTN_BA AT ROW 20.96 COL 111
     BTN_ATT AT ROW 22.21 COL 111
     BTN_NY-2 AT ROW 24.42 COL 67.63
     BTN_BORT-2 AT ROW 24.42 COL 84.63
     FILL-IN-BERNR AT ROW 24.71 COL 21.88 COLON-ALIGNED
     FILL-IN_AONR AT ROW 24.71 COL 39.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 25.83 COL 21.88 COLON-ALIGNED
     FILL-IN-HBERNR AT ROW 25.83 COL 73.63 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN_EAONR-2 AT ROW 25.83 COL 91.63 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR-2 AT ROW 25.83 COL 102.13 COLON-ALIGNED NO-LABEL
     BTN_OVER-2 AT ROW 14.79 COL 51.5
     BTN_AVB-2 AT ROW 25.83 COL 111
     FILL-IN-AOTEXT-B AT ROW 1 COL 2.25 NO-LABEL
     FILL-IN-HTEXT AT ROW 25.83 COL 55 COLON-ALIGNED NO-LABEL
     IMAGE-7 AT ROW 24.75 COL 2.25
     RECT-22 AT ROW 24.5 COL 1.5
     RECT-62 AT ROW 25.63 COL 56.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-REG
     BRW_REG AT ROW 3 COL 39.38
     BTN_KOR AT ROW 8 COL 111
     BTN_AVB-12 AT ROW 25.83 COL 111
     FILL-IN_TEXT AT ROW 1 COL 2.25 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-START
     EDD_FUNK AT ROW 9 COL 20.5 NO-LABEL
     BTN_MEDD AT ROW 13.25 COL 106.5
     BTN_BYT AT ROW 15.96 COL 106.5
     BTN_BYTW AT ROW 18.67 COL 106.5
     BTN_UPPDAT AT ROW 21.33 COL 106.5
     BTN_AVBGURU AT ROW 25.17 COL 106.5
     ED_WWW AT ROW 25.96 COL 19 NO-LABEL
     FILL-IN-GURU AT ROW 2.5 COL 23.75 NO-LABEL
     FILL-IN-ELPOOL AT ROW 25.96 COL 19 NO-LABEL
     IMAGE-3 AT ROW 2.5 COL 72
     IMAGE-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-DEPA
     BRW_DEPA AT ROW 3 COL 27.63
     BTN_best AT ROW 7.25 COL 111
     BTN_lager AT ROW 8.25 COL 111
     BTN_RAPP-7 AT ROW 9.25 COL 111
     BTN_BSTAT AT ROW 10.5 COL 111
     BTN_LEV AT ROW 11.25 COL 111
     BTN_inventering AT ROW 12.75 COL 111
     BTN_UT AT ROW 14.5 COL 111
     RAD_PERIOD AT ROW 15.25 COL 85.5 NO-LABEL
     BTN_LEVE AT ROW 16 COL 111
     BTN_MTRL-7 AT ROW 17.5 COL 111
     BTN_MTRLPRIS AT ROW 19.5 COL 111
     BTN_SEK AT ROW 21.25 COL 111
     BTN_VISA-7 AT ROW 23 COL 111
     BTN_NY-7 AT ROW 23.92 COL 30.5
     BTN_AND AT ROW 23.92 COL 47
     BTN_BORT-7 AT ROW 23.92 COL 63.38
     BTN_LAS-2 AT ROW 24.5 COL 111
     BTN_AVB-7 AT ROW 25.83 COL 111
     "Depå" VIEW-AS TEXT
          SIZE 20.5 BY 1.21 AT ROW 1 COL 2.25
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-STOR
     RAD_VAL-14 AT ROW 2.92 COL 3 NO-LABEL
     CMB_ANL-14 AT ROW 2.96 COL 57.5
     CMB_FOR-14 AT ROW 4.25 COL 11.75 COLON-ALIGNED
     CMB_SYS-14 AT ROW 4.25 COL 73.5 COLON-ALIGNED
     CMB_OMR-14 AT ROW 5.54 COL 11.75 COLON-ALIGNED
     CMB_AR-14 AT ROW 5.54 COL 41.25 COLON-ALIGNED NO-LABEL
     CMB_SYS2-14 AT ROW 5.54 COL 73.5 COLON-ALIGNED
     CMB_BEL-14 AT ROW 6.83 COL 73.5 COLON-ALIGNED
     CMB_FEL-14 AT ROW 8.08 COL 65.5
     BTN_HAMT-14 AT ROW 8.17 COL 18.5
     BRW_URSTR AT ROW 9.38 COL 1.5
     BRW_VSTR AT ROW 9.38 COL 56.5
     BTN_AOF-14 AT ROW 11 COL 111
     BTN_HAOF-14 AT ROW 12.04 COL 111
     BTN_ALLOVER-14 AT ROW 13.17 COL 51.75
     BTN_OVER-14 AT ROW 15.33 COL 51.75
     BTN_AND-14 AT ROW 16.5 COL 111
     BTN_BACK-14 AT ROW 17.54 COL 51.75
     BTN_RAPP-14 AT ROW 18.5 COL 111
     BTN_ALLBACK-14 AT ROW 19.75 COL 51.75
     BTN_ADM-14 AT ROW 19.75 COL 111
     BTN_LAS-14 AT ROW 21.75 COL 111
     BTN_VIS-14 AT ROW 23.25 COL 111
     BTN_NY-14 AT ROW 24.5 COL 68
     BTN_BORT-14 AT ROW 24.5 COL 85
     FILL-IN-BEN-14 AT ROW 24.71 COL 29.38 COLON-ALIGNED
     FILL-IN-STOR-14 AT ROW 25.83 COL 8 COLON-ALIGNED
     FILL-IN-BEN-142 AT ROW 25.83 COL 29.38 COLON-ALIGNED
     FILL-IN-STRNR-14 AT ROW 25.83 COL 89.38 COLON-ALIGNED
     BTN_AVB-14 AT ROW 25.83 COL 111
     FILL-IN-AOTEXT-14 AT ROW 1 COL 2.25 NO-LABEL
     "Sök i databas:" VIEW-AS TEXT
          SIZE 15.25 BY .83 AT ROW 25.83 COL 57.25
     IMAGE-13 AT ROW 24.71 COL 2.25
     RECT-29 AT ROW 24.5 COL 1.5
     RECT-33 AT ROW 25.63 COL 56.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-STATUS
     BRW_AOTID AT ROW 1.5 COL 1.5
     BTN_ATER AT ROW 12 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-UPP
     BRW_UPP AT ROW 3 COL 39.38
     BTN_VISA-9 AT ROW 8 COL 111
     BTN_AVB-9 AT ROW 25.83 COL 111
     "Uppföljning" VIEW-AS TEXT
          SIZE 17.63 BY 1.21 AT ROW 1 COL 2.25
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-FAKT
     CMB_FAK-4 AT ROW 2.17 COL 19.38 COLON-ALIGNED
     FILL-IN_PROJEKTKOD AT ROW 2.17 COL 61.38 COLON-ALIGNED
     CMB_OMR-4 AT ROW 3.33 COL 19.38 COLON-ALIGNED
     CMB_BESORG-4 AT ROW 4.46 COL 19.38 COLON-ALIGNED
     TOG_ANSV AT ROW 5.54 COL 21.38
     TOG_HUVA AT ROW 5.54 COL 34.5
     TOG_PREL AT ROW 5.54 COL 54
     TOG_GOD AT ROW 5.54 COL 78.25
     TOG_GAMLA AT ROW 6.5 COL 21.38
     FILL-IN-STARTDAT AT ROW 6.5 COL 61.38 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 6.5 COL 82.63 COLON-ALIGNED
     BTN_HAMT-4 AT ROW 7.67 COL 18.5
     BTN_KOPP AT ROW 8 COL 111
     BRW_UFAKT AT ROW 8.88 COL 1.5
     BRW_VFAKT AT ROW 8.88 COL 56.5
     BTN_Kred AT ROW 10.5 COL 111
     BTN_HAOF-4 AT ROW 11 COL 111
     BTN_PRELB AT ROW 11.75 COL 111
     BTN_ALLOVER-4 AT ROW 12.04 COL 51.75
     BTN_UPP-4 AT ROW 12.25 COL 111
     BTN_VISAO-2 AT ROW 12.25 COL 111
     BTN_VFAK AT ROW 12.25 COL 111
     BTN_FAK AT ROW 12.25 COL 111
     BTN_ADM-3 AT ROW 13.25 COL 111
     BTN_FLISTA AT ROW 13.25 COL 111
     BTN_AONRM AT ROW 13.25 COL 111
     BTN_OVER-4 AT ROW 14.21 COL 51.75
     BTN_AONRU AT ROW 14.75 COL 111
     BTN_AOF-4 AT ROW 16.25 COL 111
     BTN_BACK-4 AT ROW 16.46 COL 51.75
     BTN_ALLBACK-4 AT ROW 18.63 COL 51.75
     BTN_NY-4 AT ROW 23.38 COL 68
     BTN_BORT-4 AT ROW 23.38 COL 85
     FILL-IN_EFAKTNR AT ROW 24.75 COL 73.25 COLON-ALIGNED
     FILL-IN-HAMT AT ROW 24.79 COL 55.25 COLON-ALIGNED NO-LABEL
     FILL-IN_EFAKUNR AT ROW 24.79 COL 93 COLON-ALIGNED
     FILL-IN_SFAKTNR AT ROW 25.79 COL 20.75 COLON-ALIGNED
     FILL-IN_SNAMN AT ROW 25.79 COL 35.38 COLON-ALIGNED
     FILL-IN_EAONR-4 AT ROW 25.79 COL 73.25 COLON-ALIGNED
     FILL-IN_DELNR-4 AT ROW 25.79 COL 93 COLON-ALIGNED NO-LABEL
     BTN_AVB-4 AT ROW 25.83 COL 111
     "Visa fakturaplaner för" VIEW-AS TEXT
          SIZE 24.88 BY 1.08 AT ROW 1 COL 2.25
          FONT 17
     IMAGE-8 AT ROW 25.88 COL 2.25
     RECT-20 AT ROW 24.5 COL 56.5
     RECT-21 AT ROW 25.63 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-MARK
     TOG_AKT-5 AT ROW 2.17 COL 3
     TOG_INAKT-5 AT ROW 2.17 COL 17.88
     CMB_OMR-5 AT ROW 3.38 COL 19.5 COLON-ALIGNED
     CMB_UTF-5 AT ROW 4.54 COL 19.5 COLON-ALIGNED
     CMB_ANSV-5 AT ROW 5.75 COL 1.5
     BTN_HAMT-5 AT ROW 7.29 COL 18.5
     BRW_URMARK AT ROW 8.5 COL 1.5
     BRW_VMARK AT ROW 8.5 COL 56.5
     BTN_UPP-5 AT ROW 9.46 COL 111
     BTN_AOF-5 AT ROW 11 COL 111
     BTN_FASTIGHET AT ROW 11.5 COL 111
     BTN_HAOF-5 AT ROW 11.83 COL 111
     BTN_ALLOVER-5 AT ROW 12.75 COL 51.75
     BTN_VARD AT ROW 12.75 COL 111
     BTN_VISKAL-2 AT ROW 13.75 COL 111
     BTN_OVER-5 AT ROW 15 COL 51.75
     BTN_VISMARK AT ROW 15.75 COL 111
     BTN_OMRAKNA AT ROW 17 COL 111
     BTN_BACK-5 AT ROW 17.25 COL 51.75
     BTN_INAKTIV-3 AT ROW 18 COL 111
     BTN_ADM-4 AT ROW 19 COL 111
     BTN_ALLBACK-5 AT ROW 19.46 COL 51.75
     BTN_EXPM AT ROW 20.13 COL 111
     BTN_IMPM AT ROW 21.25 COL 111
     BTN_NY-5 AT ROW 24.5 COL 68
     BTN_BORT-5 AT ROW 24.5 COL 85.38
     FILL-IN_VARDANV AT ROW 24.67 COL 39 COLON-ALIGNED
     FILL-IN_SVARD AT ROW 24.71 COL 16.25 COLON-ALIGNED
     FILL-IN_AONR-5 AT ROW 25.79 COL 16.25 COLON-ALIGNED
     FILL-IN_BEN-5 AT ROW 25.79 COL 33.5 COLON-ALIGNED
     FILL-IN_EVARD AT ROW 25.79 COL 77.63 COLON-ALIGNED
     FILL-IN_EAONR-5 AT ROW 25.79 COL 94.88 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR-5 AT ROW 25.79 COL 103.75 COLON-ALIGNED NO-LABEL
     BTN_AVB-5 AT ROW 25.83 COL 111
     FILL-IN-AOTEXT-5 AT ROW 1 COL 2.25 NO-LABEL
     FILL-IN-HTEXT-5 AT ROW 25.83 COL 55 COLON-ALIGNED NO-LABEL
     IMAGE-10 AT ROW 24.79 COL 2.25
     RECT-23 AT ROW 25.63 COL 56.5
     RECT-52 AT ROW 24.5 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-SEK
     MBTN_ANV AT ROW 1 COL 1
     MBTN_BEH AT ROW 1 COL 13
     MBTN_OVR AT ROW 1 COL 25
     RAD_VAL AT ROW 3.13 COL 22.63 NO-LABEL
     BTN_VIALLA AT ROW 8.25 COL 111
     BTN_SKR-2 AT ROW 8.25 COL 111
     BTN_VPERS AT ROW 10.25 COL 111
     BTN_SKRIV-2 AT ROW 11.25 COL 111
     BTN_SKRTEST AT ROW 13 COL 111
     BTN_AVB-11 AT ROW 25.83 COL 111
     RECT-MENY-2 AT ROW 2.92 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-ANV
     BRW_ANV AT ROW 2.75 COL 25.25
     BTN_NY-11 AT ROW 21 COL 36.25
     BTN_UPP-11 AT ROW 21 COL 48.88
     BTN_BORT-11 AT ROW 21 COL 61.5
     BTN_VISA-11 AT ROW 21 COL 74.13
     FILL-IN_SANVANDARE-11 AT ROW 23.04 COL 46.5 COLON-ALIGNED
     FILL-IN_SNAMN-11 AT ROW 23.04 COL 68.75 COLON-ALIGNED
     FILL-IN_TEXTANV AT ROW 1.33 COL 2.75 NO-LABEL
     IMAGE-15 AT ROW 23.04 COL 26.88
     RECT-26 AT ROW 22.83 COL 25.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3
         SIZE 106.25 BY 23.88.

DEFINE FRAME FRAME-BEH
     SEL_MENY AT ROW 3 COL 19.63 NO-LABEL
     BRW_MENY AT ROW 3 COL 47.38
     BTN_NY-112 AT ROW 20.63 COL 56.25
     BTN_BORT-112 AT ROW 20.63 COL 68.88
     FILL-IN_AV-LEVEL-FRAN AT ROW 23.25 COL 41.13 COLON-ALIGNED
     FILL-IN_AV-LEVEL-NY AT ROW 23.25 COL 59.63 COLON-ALIGNED
     FILL-IN_TEXTBEH AT ROW 1.33 COL 2.75 NO-LABEL
     "Om du inte vill kopiera någon nivå lämna ~"Kopiera från fältet ~" = 0." VIEW-AS TEXT
          SIZE 68.75 BY .63 AT ROW 22.29 COL 20.63
     RECT-41 AT ROW 22.04 COL 19.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3
         SIZE 106.25 BY 23.88.

DEFINE FRAME FRAME-OVR
     BTN_SUPPORT AT ROW 3.79 COL 35.75
     BTN_BLOBADM AT ROW 6.63 COL 35.75
     BTN_EJAUTO AT ROW 9.46 COL 35.75
     BTN_KOSTS AT ROW 12.29 COL 35.75
     BTN_OBEORD AT ROW 15.13 COL 35.75
     FILL-IN_TEXTOVR AT ROW 1.33 COL 2.75 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.25
         SIZE 106.25 BY 18.04.

DEFINE FRAME FRAME-TID
     MBTN_TILLAGG AT ROW 1 COL 1
     MBTN_TIDSEDEL AT ROW 1 COL 15
     MBTN_TJANSTERESA AT ROW 1 COL 29
     MBTN_TIDADM AT ROW 1 COL 57
     MBTN_FLEXADM AT ROW 1 COL 73
     MBTN_STANSA AT ROW 1 COL 94.5
     BTN_PALLBACK AT ROW 19.75 COL 51.75
     BTN_PALLOVER AT ROW 13.17 COL 51.75
     BTN_PBACK AT ROW 17.58 COL 51.75
     RAD_ALLTID AT ROW 1.08 COL 122 NO-LABEL
     BTN_TID AT ROW 1.96 COL 111
     BTN_TBRES AT ROW 3.21 COL 111
     BTN_UTRES AT ROW 3.21 COL 111
     CMB_TARST AT ROW 3.25 COL 2
     CMB_TMANADST AT ROW 3.25 COL 15 COLON-ALIGNED NO-LABEL
     CMB_TARSL AT ROW 3.25 COL 30.5
     CMB_TMANADSL AT ROW 3.25 COL 42.5 COLON-ALIGNED NO-LABEL
     RAD_TIDSVAL AT ROW 3.25 COL 57 NO-LABEL
     BTN_REG AT ROW 3.38 COL 111
     BTN_ARES AT ROW 3.5 COL 111
     BTN_NDA AT ROW 3.79 COL 37.75
     BTN_LON AT ROW 3.83 COL 111
     CMB_AR AT ROW 4.08 COL 26.5 NO-LABEL
     CMB_MANAD AT ROW 4.08 COL 32 COLON-ALIGNED NO-LABEL
     FILL-IN_DATUM AT ROW 4.21 COL 25.5 COLON-ALIGNED
     FILL-IN-TDATUM AT ROW 4.58 COL 103 COLON-ALIGNED
     TOG_MAN AT ROW 4.63 COL 5.5
     CMB_TAR AT ROW 4.63 COL 26 NO-LABEL
     CMB_TMANAD AT ROW 4.63 COL 32 COLON-ALIGNED NO-LABEL
     BTN_FDA AT ROW 4.67 COL 37.75
     BTN_BER-8 AT ROW 4.79 COL 111
     BTN_GOD AT ROW 5.96 COL 111
     RAD_ALLVAL AT ROW 6 COL 2.5 NO-LABEL
     BTN_GRAN AT ROW 6 COL 111
     BTN_TRA AT ROW 6.42 COL 111
     BTN_KONT AT ROW 6.58 COL 111
     BRW_AVD AT ROW 6.75 COL 25.38
     BRW_VAVD AT ROW 6.75 COL 65.38
     BTN_OVERT AT ROW 7.46 COL 111
     CMB_OMR-8 AT ROW 7.67 COL 8.5 COLON-ALIGNED NO-LABEL
     BTN_VISA-8 AT ROW 8.46 COL 111
     BRW_PERS AT ROW 9.17 COL 1.5
     BRW_ANSV AT ROW 9.17 COL 1.5
     BRW_GODK AT ROW 9.17 COL 1.5
     BRW_MARK AT ROW 9.17 COL 56.5
     BTN_ARB AT ROW 9.5 COL 111
     BTN_VOVER AT ROW 9.54 COL 57.13
     BTN_ANDGOD AT ROW 10.5 COL 111
     BTN_VTIDV AT ROW 10.54 COL 111
     BTN_HAOF-8 AT ROW 11 COL 111
     BTN_LGOD AT ROW 11.5 COL 111
     BTN_SKR AT ROW 11.58 COL 111
     BTN_VBACK AT ROW 11.75 COL 57.13
     BTN_VECK AT ROW 12.75 COL 111
     BTN_LONA AT ROW 13.5 COL 111
     BTN_VTID AT ROW 13.67 COL 111
     BTN_AOF-8 AT ROW 15.25 COL 111
     BTN_ANDT AT ROW 15.5 COL 111
     BRW_OMR AT ROW 15.75 COL 25.38
     BRW_VOMR AT ROW 15.75 COL 65.38
     BTN_PERIOD AT ROW 16.5 COL 111
     BTN_FRAMAN AT ROW 17.5 COL 111
     BTN_ALLOVER-8 AT ROW 17.83 COL 57.38
     BTN_FRAPP AT ROW 18.5 COL 111
     BTN_AVVIK AT ROW 19.5 COL 111
     BTN_VVOVER-8 AT ROW 19.67 COL 57.38
     BTN_ANOV AT ROW 20.5 COL 111
     BTN_BACK-8 AT ROW 21.5 COL 57.38
     BTN_SALDO AT ROW 21.5 COL 111
     BTN_MANSAL AT ROW 22.5 COL 111
     BTN_ALLBACK-8 AT ROW 23.29 COL 57.38
     BTN_KONTROLL AT ROW 23.5 COL 111
     BTN_FLKORN AT ROW 24.25 COL 111
     BTN_SALMAN AT ROW 24.5 COL 111
     FILL-IN_SPERSONALKOD-8 AT ROW 24.79 COL 15.75 COLON-ALIGNED
     FILL-IN_SFORNAMN-8 AT ROW 24.79 COL 29.13 COLON-ALIGNED
     FILL-IN_SEFTERNAMN-8 AT ROW 25.79 COL 29.13 COLON-ALIGNED
     FILL-IN-TDAG-2 AT ROW 25.79 COL 77.25 COLON-ALIGNED NO-LABEL
     FILL-IN-TDATUM-2 AT ROW 25.79 COL 81.25 COLON-ALIGNED
     FILL-IN_EPERSONALKOD-8 AT ROW 25.79 COL 83.38 COLON-ALIGNED
     FILL-IN-TDAG AT ROW 25.79 COL 85.25 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-TID
     FILL-IN-HAMTA-8 AT ROW 25.83 COL 55.13 COLON-ALIGNED NO-LABEL
     BTN_PVVOVER AT ROW 15.33 COL 51.75
     BTN_AVB-8 AT ROW 25.83 COL 111
     FILL-IN-VP AT ROW 6.83 COL 8.5 COLON-ALIGNED NO-LABEL
     RECT-H AT ROW 24.5 COL 1.5
     IMAGE-14 AT ROW 24.88 COL 2.25
     RECT-S AT ROW 25.63 COL 56.5
     RECT-MENY-3 AT ROW 2.92 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-PERS
     CMB_JURP-10 AT ROW 2.25 COL 7
     CMB_AKTIV-10 AT ROW 2.25 COL 56.25 COLON-ALIGNED
     CMB_SEMFOR-10 AT ROW 2.25 COL 91.88 COLON-ALIGNED
     CMB_AVD-10 AT ROW 3.46 COL 6
     CMB_VECKO-10 AT ROW 3.46 COL 56.25 COLON-ALIGNED
     CMB_DELTID-10 AT ROW 3.46 COL 91.88 COLON-ALIGNED
     CMB_OMR-10 AT ROW 4.67 COL 15 COLON-ALIGNED
     CMB_BEFATTNING-10 AT ROW 4.67 COL 56.25 COLON-ALIGNED
     CMB_FLEX-10 AT ROW 4.67 COL 91.88 COLON-ALIGNED
     CMB_TIDSGODK-10 AT ROW 5.88 COL 15 COLON-ALIGNED
     CMB_TRA-10 AT ROW 5.88 COL 56.25 COLON-ALIGNED
     FILL-IN_FORNAMN-10 AT ROW 7.08 COL 15 COLON-ALIGNED
     CMB_BER-10 AT ROW 7.08 COL 56.25 COLON-ALIGNED
     FILL-IN_EFTERNAMN-10 AT ROW 8.29 COL 15 COLON-ALIGNED
     CMB_ANST-10 AT ROW 8.29 COL 56.25 COLON-ALIGNED
     BTN_HAMT-10 AT ROW 9.46 COL 17.63
     BTN_UPP-10 AT ROW 10 COL 111
     BRW_PERS-10 AT ROW 10.67 COL 1.5
     BRW_VPERS AT ROW 10.67 COL 56.5
     BTN_AOF-10 AT ROW 11 COL 111
     BTN_SCH AT ROW 11.5 COL 111
     BTN_HAOF-10 AT ROW 11.88 COL 111
     BTN_ARB-10 AT ROW 12.5 COL 111
     BTN_ALLOVER-10 AT ROW 13.5 COL 51.88
     BTN_VISA-10 AT ROW 13.5 COL 111
     BTN_DEBPR AT ROW 14.75 COL 111
     BTN_OVER-10 AT ROW 15.67 COL 51.88
     BTN_VISAP AT ROW 16 COL 111
     BTN_SEK-10 AT ROW 17.5 COL 111
     BTN_BACK-10 AT ROW 17.92 COL 51.88
     BTN_OT AT ROW 18.75 COL 111
     BTN_ALLBACK-10 AT ROW 20.08 COL 51.88
     BTN_TELEFONLISTA AT ROW 20.5 COL 111
     BTN_NY-10 AT ROW 24.46 COL 68
     BTN_BORT-10 AT ROW 24.46 COL 85
     FILL-IN_SFORNAMN-10 AT ROW 24.75 COL 30.25 COLON-ALIGNED
     FILL-IN_SPERSONALKOD-10 AT ROW 25.83 COL 11.63 COLON-ALIGNED
     FILL-IN_SEFTERNAMN-10 AT ROW 25.83 COL 30.25 COLON-ALIGNED
     FILL-IN_EPERSONALKOD-10 AT ROW 25.83 COL 87.75 COLON-ALIGNED
     BTN_AVB-10 AT ROW 25.88 COL 111
     FILL-IN-PERSONAL AT ROW 1 COL 2 NO-LABEL
     "Hämta person:" VIEW-AS TEXT
          SIZE 12.75 BY .83 AT ROW 25.79 COL 57.38
     IMAGE-11 AT ROW 24.75 COL 1.75
     RECT-24 AT ROW 24.46 COL 1.5
     RECT-25 AT ROW 25.67 COL 56.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-KALK
     BTN_ALLOVER-3 AT ROW 13 COL 51.75
     BTN_ALLBACK-3 AT ROW 20.33 COL 51.75
     TOG_KALKAO AT ROW 2.04 COL 30.5
     TOG_AKT-3 AT ROW 2.17 COL 3
     TOG_INAK-3 AT ROW 2.17 COL 17.88
     BRW_MAXMALL AT ROW 3 COL 10
     CMB_JURP-3 AT ROW 3 COL 6
     CMB_AVD-3 AT ROW 4.17 COL 5
     CMB_OMR-3 AT ROW 5.33 COL 8
     CMB_UTF-3 AT ROW 5.33 COL 58.5 COLON-ALIGNED
     CMB_BESORG-3 AT ROW 6.5 COL 14 COLON-ALIGNED
     CMB_KANSV AT ROW 6.5 COL 58.5 COLON-ALIGNED
     CMB_KTYP-3 AT ROW 7.67 COL 14 COLON-ALIGNED
     BTN_HAMT-3 AT ROW 9 COL 18.5
     BTN_UPP-3 AT ROW 9.25 COL 111
     BRW_UKALK AT ROW 10.21 COL 1.5
     BRW_VKALK AT ROW 10.21 COL 56.5
     BTN_AOF-3 AT ROW 11 COL 111
     BTN_HAOF-3 AT ROW 11 COL 111
     BTN_KALK-3 AT ROW 12.25 COL 111
     BTN_VISKAL AT ROW 13.5 COL 111
     BTN_KOPI-3 AT ROW 14.75 COL 111
     BTN_KONV AT ROW 15.75 COL 111
     BTN_INAKTIV-2 AT ROW 17.25 COL 111
     BTN_ADM-2 AT ROW 19 COL 111
     BTN_EXP-2 AT ROW 21.25 COL 111
     BTN_IMP-2 AT ROW 23.25 COL 111
     BTN_NY-3 AT ROW 24.5 COL 68
     BTN_BORT-3 AT ROW 24.5 COL 85
     FILL-IN_KAONR AT ROW 24.71 COL 14.5 COLON-ALIGNED
     FILL-IN_KALKYL AT ROW 24.71 COL 35.25 COLON-ALIGNED
     BTN_BACK-3 AT ROW 17.92 COL 51.75
     FILL-IN-KPLANNR AT ROW 25.79 COL 14.5 COLON-ALIGNED
     FILL-IN_KALKB AT ROW 25.79 COL 35.25 COLON-ALIGNED
     FILL-IN_EKALNR AT ROW 25.79 COL 72.75 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN_EAONR-3 AT ROW 25.79 COL 92.75 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR-3 AT ROW 25.79 COL 103 COLON-ALIGNED NO-LABEL
     BTN_AVB-3 AT ROW 25.83 COL 111
     BTN_OVER-3 AT ROW 15.46 COL 51.75
     FILL-IN-KATEXT AT ROW 1 COL 2.25 NO-LABEL
     FILL-IN-VALK AT ROW 25.83 COL 55.38 COLON-ALIGNED NO-LABEL
     IMAGE-9 AT ROW 24.79 COL 2.25
     RECT-51 AT ROW 24.5 COL 1.5
     RECT-53 AT ROW 25.63 COL 56.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.

DEFINE FRAME FRAME-PLAN
     BTN_NVE-133 AT ROW 1.92 COL 70.25
     BTN_NVE-134 AT ROW 1.92 COL 87.63
     TOG_FASTA-13 AT ROW 2.17 COL 4.75
     TOG_AVSLUTADE-13 AT ROW 2.17 COL 31.25
     FILL-IN-MELL-13 AT ROW 2.17 COL 51.25 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSTARTD-13 AT ROW 2.17 COL 58.38 COLON-ALIGNED NO-LABEL
     FILL-IN-OCH-13 AT ROW 2.17 COL 71.75 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSLUTD-13 AT ROW 2.17 COL 75.75 COLON-ALIGNED NO-LABEL
     BTN_FVE-133 AT ROW 2.79 COL 70.25
     BTN_FVE-134 AT ROW 2.79 COL 87.63
     TOG_TILLF-13 AT ROW 3.42 COL 4.75
     TOG_PAGA-13 AT ROW 3.42 COL 31.25
     CMB_OMR-13 AT ROW 4.58 COL 10.75
     RAD_PERIOD-13 AT ROW 5.79 COL 66 NO-LABEL
     CMB_BESORG-13 AT ROW 5.83 COL 16.75 COLON-ALIGNED
     FILL-IN-K1-13 AT ROW 5.83 COL 50.25 COLON-ALIGNED
     CMB_ARTAL-13 AT ROW 5.83 COL 82.5
     FILL-IN-K2-13 AT ROW 7.04 COL 50.25 COLON-ALIGNED
     CMB_ANSV-13 AT ROW 7.08 COL 16.75 COLON-ALIGNED
     CMB_FRAN-13 AT ROW 7.08 COL 80.5
     CMB_TILL-13 AT ROW 7.08 COL 96
     CMB_ARBART-13 AT ROW 8.33 COL 16.75 COLON-ALIGNED
     BTN_HAMT-6 AT ROW 9.79 COL 18.5
     BRW_PLAN AT ROW 11 COL 1.5
     BRW_VPLAN AT ROW 11 COL 56.5
     BTN_UPP-6 AT ROW 11 COL 111
     BTN_HAOF-13 AT ROW 11 COL 111
     BTN_ALLOVER-13 AT ROW 13.25 COL 51.5
     BTN_PTIDPLAN AT ROW 13.75 COL 111
     BTN_UNDER-2 AT ROW 15.25 COL 111
     BTN_OVER-13 AT ROW 15.42 COL 51.5
     BTN_BUNDER AT ROW 16.75 COL 111
     BTN_BACK-13 AT ROW 17.63 COL 51.5
     BTN_BUD AT ROW 18.25 COL 111
     BTN_KALK-4 AT ROW 19.25 COL 111
     BTN_ALLBACK-13 AT ROW 19.83 COL 51.5
     BTN_RAPP-2 AT ROW 20.75 COL 111
     BTN_AVSAONR-2 AT ROW 22 COL 111
     BTN_VISAO-3 AT ROW 23.25 COL 111
     BTN_AOF-13 AT ROW 23.75 COL 111
     BTN_NY-13 AT ROW 24.5 COL 68
     BTN_BORT-13 AT ROW 24.5 COL 85
     FILL-IN_SPLANNR-13 AT ROW 24.75 COL 22.5 COLON-ALIGNED
     FILL-IN_ORT-13 AT ROW 25.83 COL 22.5 COLON-ALIGNED
     FILL-IN_EPLANNR-13 AT ROW 25.83 COL 71.88 COLON-ALIGNED NO-LABEL
     FILL-IN_ARTAL-13 AT ROW 25.83 COL 95.38
     BTN_AVB-13 AT ROW 25.83 COL 111
     FILL-IN_PLANNRVAL AT ROW 1 COL 2.25 NO-LABEL
     FILL-IN-KTO-13 AT ROW 4.83 COL 41.75 COLON-ALIGNED NO-LABEL
     FILL-IN-VAL AT ROW 25.83 COL 55.13 COLON-ALIGNED NO-LABEL
     "Årtalen gäller både för urval och rapporter" VIEW-AS TEXT
          SIZE 44 BY 1.21 AT ROW 4.5 COL 66.5
          FONT 17
     IMAGE-12 AT ROW 24.79 COL 2.25
     RECT-27 AT ROW 25.63 COL 56.5
     RECT-28 AT ROW 24.5 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ansvarigtemp T "?" NO-UNDO temp-db ansvarigtemp
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: avdelningtemp T "?" NO-UNDO temp-db avdelningtemp
      TABLE: depatemp T "?" NO-UNDO temp-db depatemp
      TABLE: faktplantemp T "?" NO-UNDO temp-db faktplantemp
      TABLE: godkannartemp T "?" NO-UNDO temp-db godkannartemp
      TABLE: jurperstemp T "?" NO-UNDO temp-db jurperstemp
      TABLE: markpers T "?" NO-UNDO temp-db markpers
      TABLE: mspec_mtrlextra T "?" NO-UNDO temp-db mspec_mtrlextra
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: mvalvardtemp T "?" NO-UNDO temp-db mvalvardtemp
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: plannrtemp T "?" NO-UNDO temp-db plannrtemp
      TABLE: pmpersonaltemp T "?" NO-UNDO temp-db pmpersonaltemp
      TABLE: rvisa T "?" NO-UNDO temp-db rvisa
      TABLE: urberedningtemp T "?" NO-UNDO temp-db urberedningtemp
      TABLE: urstorntemp T "?" NO-UNDO temp-db urstorntemp
      TABLE: urvardtemp T "?" NO-UNDO temp-db urvardtemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: utvaldfasttemp T "?" NO-UNDO temp-db utvaldfasttemp
      TABLE: valberedningtemp T "?" NO-UNDO temp-db valberedningtemp
      TABLE: valdaaotemp T "?" NO-UNDO temp-db valdaaotemp
      TABLE: valdfasttemp T "?" NO-UNDO temp-db valdfasttemp
      TABLE: valperstemp T "?" NO-UNDO temp-db valperstemp
      TABLE: valplantemp T "?" NO-UNDO temp-db valplantemp
      TABLE: vavdelningtemp T "?" NO-UNDO temp-db vavdelningtemp
      TABLE: vfaktplantemp T "?" NO-UNDO temp-db vfaktplantemp
      TABLE: visaupp T "?" NO-UNDO temp-db visaupp
      TABLE: vomrtemp T "?" NO-UNDO temp-db vomrtemp
      TABLE: vstorntemp T "?" NO-UNDO temp-db vstorntemp
      TABLE: xgurutemp T "?" NO-UNDO temp-db xgurutemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ANV:FRAME = FRAME FRAME-SEK:HANDLE
       FRAME FRAME-AONR:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-BEH:FRAME = FRAME FRAME-SEK:HANDLE
       FRAME FRAME-BER:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-DEPA:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-FAKT:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-KALK:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-MARK:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-MTRL:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-OVR:FRAME = FRAME FRAME-SEK:HANDLE
       FRAME FRAME-PERS:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-PLAN:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-REG:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-SEK:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-START:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-STATUS:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-STOR:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-TID:FRAME = FRAME AMULTISTART-FRAME:HANDLE
       FRAME FRAME-UPP:FRAME = FRAME AMULTISTART-FRAME:HANDLE.

/* SETTINGS FOR FRAME AMULTISTART-FRAME
                                                                        */
ASSIGN 
       BTN_AONR:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_BERM:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_DEPA:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_FAKTM:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_FLEX:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_KALKM:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_MARKM:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_MTRLM:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_PERS:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_PLAN:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_REGM:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_SEKM:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_SMS:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_STOR:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_TIDM:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

ASSIGN 
       BTN_UPPF:HIDDEN IN FRAME AMULTISTART-FRAME           = TRUE.

/* SETTINGS FOR FRAME FRAME-ANV
                                                                        */
/* BROWSE-TAB BRW_ANV RECT-26 FRAME-ANV */
ASSIGN 
       BRW_ANV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-ANV = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_TEXTANV IN FRAME FRAME-ANV
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-AONR
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AONR BTN_HAMT FRAME-AONR */
/* BROWSE-TAB BRW_VAONR BRW_AONR FRAME-AONR */
ASSIGN 
       BRW_AONR:MAX-DATA-GUESS IN FRAME FRAME-AONR         = 1000
       BRW_AONR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-AONR = TRUE
       BRW_AONR:COLUMN-RESIZABLE IN FRAME FRAME-AONR       = TRUE.

ASSIGN 
       BRW_VAONR:MAX-DATA-GUESS IN FRAME FRAME-AONR         = 1000
       BRW_VAONR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-AONR = TRUE
       BRW_VAONR:COLUMN-RESIZABLE IN FRAME FRAME-AONR       = TRUE.

ASSIGN 
       BTN_AVROP:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_AVSAONR:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_BER:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_BYTPNR:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_FAKT:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_FVE-4:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_KALK:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_KOPI:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_KOST:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_MARK:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_NVE-4:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_RAPP:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_STATUS:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_TIDPLAN:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_UNDER:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_UPP:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       BTN_VISAO:HIDDEN IN FRAME FRAME-AONR           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARBART IN FRAME FRAME-AONR
   NO-DISPLAY                                                           */
ASSIGN 
       CMB_ARBART:HIDDEN IN FRAME FRAME-AONR           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME FRAME-AONR
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME FRAME-AONR
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME FRAME-AONR
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME FRAME-AONR
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT-3 IN FRAME FRAME-AONR
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-AR IN FRAME FRAME-AONR
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       FILL-IN-AVSLUTD:HIDDEN IN FRAME FRAME-AONR           = TRUE.

ASSIGN 
       FILL-IN-AVSTARTD:HIDDEN IN FRAME FRAME-AONR           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KTO IN FRAME FRAME-AONR
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-MELL IN FRAME FRAME-AONR
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MELL:READ-ONLY IN FRAME FRAME-AONR        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OCH IN FRAME FRAME-AONR
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME FRAME-AONR
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME FRAME-AONR           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PAAV IN FRAME FRAME-AONR
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_PAAV:HIDDEN IN FRAME FRAME-AONR           = TRUE.

/* SETTINGS FOR FRAME FRAME-BEH
                                                                        */
/* BROWSE-TAB BRW_MENY SEL_MENY FRAME-BEH */
ASSIGN 
       BRW_MENY:MAX-DATA-GUESS IN FRAME FRAME-BEH         = 500
       BRW_MENY:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BEH = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_TEXTBEH IN FRAME FRAME-BEH
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-BER
                                                                        */
/* BROWSE-TAB BRW_URBER BTN_UPP-2 FRAME-BER */
/* BROWSE-TAB BRW_VBER BRW_URBER FRAME-BER */
ASSIGN 
       BRW_URBER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BER = TRUE
       BRW_URBER:COLUMN-RESIZABLE IN FRAME FRAME-BER       = TRUE.

ASSIGN 
       BRW_VBER:HIDDEN  IN FRAME FRAME-BER                = TRUE
       BRW_VBER:MAX-DATA-GUESS IN FRAME FRAME-BER         = 1000
       BRW_VBER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BER = TRUE
       BRW_VBER:COLUMN-RESIZABLE IN FRAME FRAME-BER       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD-2 IN FRAME FRAME-BER
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP-2 IN FRAME FRAME-BER
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT-B IN FRAME FRAME-BER
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-HTEXT IN FRAME FRAME-BER
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-DEPA
                                                                        */
/* BROWSE-TAB BRW_DEPA TEXT-7 FRAME-DEPA */
ASSIGN 
       BRW_DEPA:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-DEPA = TRUE.

ASSIGN 
       BTN_best:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

ASSIGN 
       BTN_BSTAT:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

ASSIGN 
       BTN_inventering:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

ASSIGN 
       BTN_lager:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

ASSIGN 
       BTN_LEV:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

ASSIGN 
       BTN_RAPP-7:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

ASSIGN 
       BTN_SEK:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

ASSIGN 
       BTN_UT:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

ASSIGN 
       BTN_VISA-7:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME FRAME-DEPA
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_PERIOD:HIDDEN IN FRAME FRAME-DEPA           = TRUE.

/* SETTINGS FOR FRAME FRAME-FAKT
                                                                        */
/* BROWSE-TAB BRW_UFAKT BTN_KOPP FRAME-FAKT */
/* BROWSE-TAB BRW_VFAKT BRW_UFAKT FRAME-FAKT */
ASSIGN 
       BRW_UFAKT:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-FAKT = TRUE
       BRW_UFAKT:COLUMN-RESIZABLE IN FRAME FRAME-FAKT       = TRUE.

ASSIGN 
       BRW_VFAKT:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-FAKT = TRUE
       BRW_VFAKT:COLUMN-RESIZABLE IN FRAME FRAME-FAKT       = TRUE.

ASSIGN 
       BTN_ADM-3:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

ASSIGN 
       BTN_AONRM:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

ASSIGN 
       BTN_FLISTA:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

ASSIGN 
       BTN_PRELB:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

ASSIGN 
       CMB_OMR-4:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-HAMT IN FRAME FRAME-FAKT
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-HAMT:READ-ONLY IN FRAME FRAME-FAKT        = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

/* SETTINGS FOR FRAME FRAME-KALK
                                                                        */
/* BROWSE-TAB BRW_MAXMALL TOG_INAK-3 FRAME-KALK */
/* BROWSE-TAB BRW_UKALK BTN_UPP-3 FRAME-KALK */
/* BROWSE-TAB BRW_VKALK BRW_UKALK FRAME-KALK */
ASSIGN 
       BRW_MAXMALL:MAX-DATA-GUESS IN FRAME FRAME-KALK         = 1000.

ASSIGN 
       BRW_UKALK:MAX-DATA-GUESS IN FRAME FRAME-KALK         = 1000.

ASSIGN 
       BRW_VKALK:MAX-DATA-GUESS IN FRAME FRAME-KALK         = 1000.

ASSIGN 
       BTN_ADM-2:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_BORT-3:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_EXP-2:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_IMP-2:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_INAKTIV-2:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_KALK-3:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_KONV:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_KOPI-3:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_NY-3:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_UPP-3:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_VISKAL:HIDDEN IN FRAME FRAME-KALK           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD-3 IN FRAME FRAME-KALK
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP-3 IN FRAME FRAME-KALK
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR-3 IN FRAME FRAME-KALK
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-KATEXT IN FRAME FRAME-KALK
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-VALK IN FRAME FRAME-KALK
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-MARK
                                                                        */
/* BROWSE-TAB BRW_URMARK BTN_HAMT-5 FRAME-MARK */
/* BROWSE-TAB BRW_VMARK BRW_URMARK FRAME-MARK */
/* SETTINGS FOR COMBO-BOX CMB_ANSV-5 IN FRAME FRAME-MARK
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT-5 IN FRAME FRAME-MARK
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-HTEXT-5 IN FRAME FRAME-MARK
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-MTRL
                                                                        */
/* BROWSE-TAB BRW_HLEV RECT-4 FRAME-MTRL */
/* BROWSE-TAB BRW_MTRL BRW_HLEV FRAME-MTRL */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_HLEV:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_HLEV:HANDLE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 40000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE.

ASSIGN 
       BRW_MTRL:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_MTRL:HANDLE
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 40000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-MTRL       = TRUE.

ASSIGN 
       BTN_LEVH:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_MIN:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_UP:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-ANTAL:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-BEN-6:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-ENR2:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-MTRL
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-MTRL        = TRUE.

ASSIGN 
       RECT-4:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR FRAME FRAME-OVR
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN_TEXTOVR IN FRAME FRAME-OVR
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-PERS
                                                                        */
/* BROWSE-TAB BRW_PERS-10 BTN_UPP-10 FRAME-PERS */
/* BROWSE-TAB BRW_VPERS BRW_PERS-10 FRAME-PERS */
ASSIGN 
       BRW_PERS-10:MAX-DATA-GUESS IN FRAME FRAME-PERS         = 1000
       BRW_PERS-10:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-PERS = TRUE
       BRW_PERS-10:COLUMN-RESIZABLE IN FRAME FRAME-PERS       = TRUE.

ASSIGN 
       BRW_VPERS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-PERS = TRUE
       BRW_VPERS:COLUMN-RESIZABLE IN FRAME FRAME-PERS       = TRUE.

ASSIGN 
       BTN_ARB-10:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_BORT-10:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_DEBPR:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_NY-10:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_OT:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_SCH:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_SEK-10:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_TELEFONLISTA:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_UPP-10:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_VISA-10:HIDDEN IN FRAME FRAME-PERS           = TRUE.

ASSIGN 
       BTN_VISAP:HIDDEN IN FRAME FRAME-PERS           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD-10 IN FRAME FRAME-PERS
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP-10 IN FRAME FRAME-PERS
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR-10 IN FRAME FRAME-PERS
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-PERSONAL IN FRAME FRAME-PERS
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-PLAN
                                                                        */
/* BROWSE-TAB BRW_PLAN BTN_HAMT-6 FRAME-PLAN */
/* BROWSE-TAB BRW_VPLAN BRW_PLAN FRAME-PLAN */
ASSIGN 
       BRW_PLAN:MAX-DATA-GUESS IN FRAME FRAME-PLAN         = 1000.

ASSIGN 
       BRW_VPLAN:MAX-DATA-GUESS IN FRAME FRAME-PLAN         = 1000.

ASSIGN 
       BTN_AVSAONR-2:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_BORT-13:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_BUD:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_BUNDER:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_FVE-133:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_FVE-134:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_KALK-4:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_NVE-133:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_NVE-134:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_NY-13:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_PTIDPLAN:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_RAPP-2:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_UNDER-2:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_UPP-6:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       BTN_VISAO-3:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARBART-13 IN FRAME FRAME-PLAN
   NO-DISPLAY                                                           */
ASSIGN 
       CMB_ARBART-13:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL-13 IN FRAME FRAME-PLAN
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_FRAN-13 IN FRAME FRAME-PLAN
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR-13 IN FRAME FRAME-PLAN
   SHARED ALIGN-L                                                       */
/* SETTINGS FOR COMBO-BOX CMB_TILL-13 IN FRAME FRAME-PLAN
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-AVSLUTD-13:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       FILL-IN-AVSTARTD-13:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KTO-13 IN FRAME FRAME-PLAN
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-MELL-13:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

ASSIGN 
       FILL-IN-OCH-13:HIDDEN IN FRAME FRAME-PLAN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VAL IN FRAME FRAME-PLAN
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ARTAL-13 IN FRAME FRAME-PLAN
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_PLANNRVAL IN FRAME FRAME-PLAN
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RADIO-SET RAD_PERIOD-13 IN FRAME FRAME-PLAN
   SHARED                                                               */
/* SETTINGS FOR FRAME FRAME-REG
                                                                        */
/* BROWSE-TAB BRW_REG 1 FRAME-REG */
/* SETTINGS FOR FILL-IN FILL-IN_TEXT IN FRAME FRAME-REG
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-SEK
                                                                        */
/* SETTINGS FOR FRAME FRAME-START
   NOT-VISIBLE                                                          */
ASSIGN 
       BTN_UPPDAT:HIDDEN IN FRAME FRAME-START           = TRUE.

ASSIGN 
       EDD_FUNK:READ-ONLY IN FRAME FRAME-START        = TRUE.

ASSIGN 
       ED_WWW:READ-ONLY IN FRAME FRAME-START        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ELPOOL IN FRAME FRAME-START
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-ELPOOL:HIDDEN IN FRAME FRAME-START           = TRUE
       FILL-IN-ELPOOL:READ-ONLY IN FRAME FRAME-START        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-GURU IN FRAME FRAME-START
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME FRAME-START
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-3:HIDDEN IN FRAME FRAME-START           = TRUE.

/* SETTINGS FOR FRAME FRAME-STATUS
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AOTID 1 FRAME-STATUS */
ASSIGN 
       BRW_AOTID:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-STATUS = TRUE
       BRW_AOTID:COLUMN-RESIZABLE IN FRAME FRAME-STATUS       = TRUE.

ASSIGN 
       BTN_ATER:HIDDEN IN FRAME FRAME-STATUS           = TRUE.

/* SETTINGS FOR FRAME FRAME-STOR
                                                                        */
/* BROWSE-TAB BRW_URSTR BTN_HAMT-14 FRAME-STOR */
/* BROWSE-TAB BRW_VSTR BRW_URSTR FRAME-STOR */
/* SETTINGS FOR COMBO-BOX CMB_ANL-14 IN FRAME FRAME-STOR
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_FEL-14 IN FRAME FRAME-STOR
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT-14 IN FRAME FRAME-STOR
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME FRAME-TID
                                                                        */
/* BROWSE-TAB BRW_AVD BTN_KONT FRAME-TID */
/* BROWSE-TAB BRW_VAVD BRW_AVD FRAME-TID */
/* BROWSE-TAB BRW_PERS BTN_VISA-8 FRAME-TID */
/* BROWSE-TAB BRW_ANSV BRW_PERS FRAME-TID */
/* BROWSE-TAB BRW_GODK BRW_ANSV FRAME-TID */
/* BROWSE-TAB BRW_MARK BRW_GODK FRAME-TID */
/* BROWSE-TAB BRW_OMR BTN_ANDT FRAME-TID */
/* BROWSE-TAB BRW_VOMR BRW_OMR FRAME-TID */
ASSIGN 
       BRW_ANSV:HIDDEN  IN FRAME FRAME-TID                = TRUE
       BRW_ANSV:MAX-DATA-GUESS IN FRAME FRAME-TID         = 300.

ASSIGN 
       BRW_AVD:HIDDEN  IN FRAME FRAME-TID                = TRUE.

ASSIGN 
       BRW_GODK:HIDDEN  IN FRAME FRAME-TID                = TRUE
       BRW_GODK:MAX-DATA-GUESS IN FRAME FRAME-TID         = 300.

ASSIGN 
       BRW_MARK:HIDDEN  IN FRAME FRAME-TID                = TRUE
       BRW_MARK:MAX-DATA-GUESS IN FRAME FRAME-TID         = 300.

ASSIGN 
       BRW_OMR:HIDDEN  IN FRAME FRAME-TID                = TRUE.

ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME FRAME-TID                = TRUE
       BRW_PERS:MAX-DATA-GUESS IN FRAME FRAME-TID         = 300.

ASSIGN 
       BRW_VAVD:HIDDEN  IN FRAME FRAME-TID                = TRUE
       BRW_VAVD:MAX-DATA-GUESS IN FRAME FRAME-TID         = 10000.

ASSIGN 
       BRW_VOMR:HIDDEN  IN FRAME FRAME-TID                = TRUE
       BRW_VOMR:MAX-DATA-GUESS IN FRAME FRAME-TID         = 10000.

ASSIGN 
       BTN_ALLBACK-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_ALLOVER-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_ANDT:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_ANOV:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_ARB:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_ARES:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_AVVIK:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_BACK-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_BER-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_FDA:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_FLKORN:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_FRAMAN:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_FRAPP:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_GOD:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_GRAN:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_KONT:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_KONTROLL:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_LGOD:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_LON:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_LONA:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_MANSAL:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_NDA:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_OVERT:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_PALLBACK:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_PALLOVER:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_PBACK:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_PERIOD:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_PVVOVER:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_REG:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_SALDO:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_SALMAN:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_SKR:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_TBRES:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_TID:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_TRA:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_UTRES:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_VBACK:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_VECK:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_VISA-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_VOVER:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_VTID:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_VTIDV:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       BTN_VVOVER-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AR IN FRAME FRAME-TID
   NO-DISPLAY ALIGN-L                                                   */
/* SETTINGS FOR COMBO-BOX CMB_MANAD IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_TAR IN FRAME FRAME-TID
   NO-DISPLAY SHARED ALIGN-L                                            */
/* SETTINGS FOR COMBO-BOX CMB_TARSL IN FRAME FRAME-TID
   NO-DISPLAY ALIGN-L                                                   */
/* SETTINGS FOR COMBO-BOX CMB_TARST IN FRAME FRAME-TID
   NO-DISPLAY ALIGN-L                                                   */
/* SETTINGS FOR COMBO-BOX CMB_TMANAD IN FRAME FRAME-TID
   NO-DISPLAY SHARED                                                    */
/* SETTINGS FOR COMBO-BOX CMB_TMANADSL IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_TMANADST IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-HAMTA-8 IN FRAME FRAME-TID
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-HAMTA-8:HIDDEN IN FRAME FRAME-TID           = TRUE
       FILL-IN-HAMTA-8:READ-ONLY IN FRAME FRAME-TID        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TDAG IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-TDAG:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TDAG-2 IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-TDAG-2:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TDATUM IN FRAME FRAME-TID
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-TDATUM:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TDATUM-2 IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-TDATUM-2:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VP IN FRAME FRAME-TID
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VP:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       FILL-IN_DATUM:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_EPERSONALKOD-8 IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN_SEFTERNAMN-8 IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_SEFTERNAMN-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SFORNAMN-8 IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_SFORNAMN-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SPERSONALKOD-8 IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_SPERSONALKOD-8:HIDDEN IN FRAME FRAME-TID           = TRUE.

ASSIGN 
       MBTN_STANSA:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLTID IN FRAME FRAME-TID
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLTID:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME FRAME-TID
   SHARED                                                               */
/* SETTINGS FOR RADIO-SET RAD_TIDSVAL IN FRAME FRAME-TID
   NO-DISPLAY SHARED                                                    */
ASSIGN 
       RAD_TIDSVAL:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-H IN FRAME FRAME-TID
   NO-ENABLE                                                            */
ASSIGN 
       RECT-H:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_MAN IN FRAME FRAME-TID
   NO-DISPLAY                                                           */
ASSIGN 
       TOG_MAN:HIDDEN IN FRAME FRAME-TID           = TRUE.

/* SETTINGS FOR FRAME FRAME-UPP
                                                                        */
/* BROWSE-TAB BRW_UPP TEXT-5 FRAME-UPP */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANSV
/* Query rebuild information for BROWSE BRW_ANSV
     _TblList          = "Temp-Tables.ansvarigtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ansvarigtemp.PERSONALKOD
"ansvarigtemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.ansvarigtemp.FORNAMN
"ansvarigtemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.ansvarigtemp.EFTERNAMN
"ansvarigtemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANSV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Användare" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Användarnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.anvandartemp.AV-LEVEL
"anvandartemp.AV-LEVEL" "Anv.!Nivå" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.anvandartemp.PERSONALKOD
"anvandartemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.utsokaonr"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.utsokaonr.OMRADE|yes,Temp-Tables.utsokaonr.AONR|yes,Temp-Tables.utsokaonr.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utsokaonr.OMRADE
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "18.5" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.utsokaonr.BEREDARE
"utsokaonr.BEREDARE" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AOTID
/* Query rebuild information for BROWSE BRW_AOTID
     _TblList          = "Temp-Tables.aotidslagtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.aotidslagtemp.AONR
     _FldNameList[2]   > Temp-Tables.aotidslagtemp.DELNR
"aotidslagtemp.DELNR" "cDELNR" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.aotidslagtemp.TIDLAGE
"aotidslagtemp.TIDLAGE" "Tidläge" "X(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.aotidslagtemp.AKTIVITET1
"aotidslagtemp.AKTIVITET1" "Aktivitet" "X(256)" "character" ? 4 ? ? 4 ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.aotidslagtemp.DAT1
"aotidslagtemp.DAT1" ? ? "date" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.aotidslagtemp.ANVANDARE1
"aotidslagtemp.ANVANDARE1" "Ändrad av" ? "character" ? 4 ? ? 4 ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.aotidslagtemp.AKTIVITET2
"aotidslagtemp.AKTIVITET2" "Aktivitet" "X(256)" "character" ? 3 ? ? 3 ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.aotidslagtemp.DAT2
"aotidslagtemp.DAT2" ? ? "date" ? 3 ? ? 3 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.aotidslagtemp.ANVANDARE2
"aotidslagtemp.ANVANDARE2" "Ändrad av" ? "character" ? 3 ? ? 3 ? no ? no no "10" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AOTID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVD
/* Query rebuild information for BROWSE BRW_AVD
     _TblList          = "Temp-Tables.avdelningtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avdelningtemp.AVDELNINGNR
"avdelningtemp.AVDELNINGNR" "Avdelning" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.avdelningtemp.AVDELNINGNAMN
"avdelningtemp.AVDELNINGNAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_AVD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DEPA
/* Query rebuild information for BROWSE BRW_DEPA
     _TblList          = "Temp-Tables.depatemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.depatemp.Dep-Nr
     _FldNameList[2]   = Temp-Tables.depatemp.Benamning
     _Query            is OPENED
*/  /* BROWSE BRW_DEPA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GODK
/* Query rebuild information for BROWSE BRW_GODK
     _TblList          = "Temp-Tables.godkannartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.godkannartemp.PERSONALKOD
"godkannartemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.godkannartemp.FORNAMN
"godkannartemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.godkannartemp.EFTERNAMN
"godkannartemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_GODK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = vald_kundlev
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" "Enh" "x(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MARK
/* Query rebuild information for BROWSE BRW_MARK
     _TblList          = "Temp-Tables.markpers"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.markpers.PERSONALKOD
"markpers.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.markpers.FORNAMN
"markpers.FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.markpers.EFTERNAMN
"markpers.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MARK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MAXMALL
/* Query rebuild information for BROWSE BRW_MAXMALL
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MAXMALL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MENY
/* Query rebuild information for BROWSE BRW_MENY
     _TblList          = "Temp-Tables.xgurutemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.xgurutemp.AV-LEVEL
"xgurutemp.AV-LEVEL" "Nivå" ">>>9" "integer" ? ? 4 ? ? 4 no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.xgurutemp.MENY
"xgurutemp.MENY" "Meny" "x(256)" "character" ? ? 4 ? ? 4 no ? no no "25" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.xgurutemp.MENYOK
"xgurutemp.MENYOK" "Behörig" "Ja/Nej" "logical" ? ? 4 ? ? 4 no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MENY */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.mspec_mtrlextra"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mspec_mtrlextra.Enr
     _FldNameList[2]   > Temp-Tables.mspec_mtrlextra.BERKVANT
"mspec_mtrlextra.BERKVANT" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.mspec_mtrlextra.Enhet
"mspec_mtrlextra.Enhet" "Enh" "x(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.mspec_mtrlextra.LEVKOD
"mspec_mtrlextra.LEVKOD" "Lev" "X(5)" "character" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" ""
     _FldNameList[5]   = Temp-Tables.mspec_mtrlextra.NPRIS
     _FldNameList[6]   > Temp-Tables.mspec_mtrlextra.Benamning
"mspec_mtrlextra.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "Temp-Tables.omrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omrtemp.OMRADE
"omrtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp.NAMN
"omrtemp.NAMN" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.personaltemp.OMRADE
"personaltemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.personaltemp.VECKOSCHEMA
"personaltemp.VECKOSCHEMA" "Vecko!schema" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.personaltemp.BEFATTNING
"personaltemp.BEFATTNING" "Befatting" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS-10
/* Query rebuild information for BROWSE BRW_PERS-10
     _TblList          = "Temp-Tables.pmpersonaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.pmpersonaltemp.PERSONALKOD
"pmpersonaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.pmpersonaltemp.AKTIV
"pmpersonaltemp.AKTIV" "Tid!skrivning" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.pmpersonaltemp.FORNAMN
"pmpersonaltemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.pmpersonaltemp.EFTERNAMN
"pmpersonaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.pmpersonaltemp.TELEFON
"pmpersonaltemp.TELEFON" "Telefon" ? "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.pmpersonaltemp.MOBILTEL
"pmpersonaltemp.MOBILTEL" "Mobiltele" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.pmpersonaltemp.OMRADE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS-10 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PLAN
/* Query rebuild information for BROWSE BRW_PLAN
     _TblList          = "Temp-Tables.plannrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.plannrtemp.OMRADE
"plannrtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.plannrtemp.PLANNR
"plannrtemp.PLANNR" "Plannr" "X(8)" "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.plannrtemp.ARTAL
"plannrtemp.ARTAL" "Årtal" ? "integer" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.plannrtemp.ORT
"plannrtemp.ORT" "Ort/Benämning" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.plannrtemp.AONR
"plannrtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.plannrtemp.DELNR
"plannrtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PLAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_REG
/* Query rebuild information for BROWSE BRW_REG
     _TblList          = "Temp-Tables.rvisa"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.rvisa.UT
"rvisa.UT" "Register" "X(256)" "character" ? ? ? ? ? ? no ? no no "45.13" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_REG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UFAKT
/* Query rebuild information for BROWSE BRW_UFAKT
     _TblList          = "Temp-Tables.faktplantemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.faktplantemp.FAKTNR
"faktplantemp.FAKTNR" "Faktp.!nr" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktplantemp.NAMN
"faktplantemp.NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktplantemp.VIBESTID
"faktplantemp.VIBESTID" "Best./!Kund" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktplantemp.SENASTFAK
"faktplantemp.SENASTFAK" "Senast!fakt." ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktplantemp.PANVANDARE
"faktplantemp.PANVANDARE" "Senast!ändrad av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.faktplantemp.ANVANDARE
"faktplantemp.ANVANDARE" "Ansvarig" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.faktplantemp.HANVANDARE
"faktplantemp.HANVANDARE" "Huvud-!ansvarig" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UFAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UKALK
/* Query rebuild information for BROWSE BRW_UKALK
     _TblList          = "Temp-Tables.utvaldfasttemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.utvaldfasttemp.OMRADE|yes,Temp-Tables.utvaldfasttemp.AONR|yes,Temp-Tables.utvaldfasttemp.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utvaldfasttemp.OMRADE
"utvaldfasttemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utvaldfasttemp.KALKNR
"utvaldfasttemp.KALKNR" "Kalkyl nr" ">>>>>>9" "integer" ? 1 ? ? 1 ? no ? no no "5.5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utvaldfasttemp.BENAMNING
"utvaldfasttemp.BENAMNING" "Ort/!Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utvaldfasttemp.TYPCHAR
"utvaldfasttemp.TYPCHAR" "Typ" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.utvaldfasttemp.VIKATAR
"utvaldfasttemp.VIKATAR" "Kat.!år" "9999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.utvaldfasttemp.AONR
"utvaldfasttemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.utvaldfasttemp.DELNR
"utvaldfasttemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.utvaldfasttemp.PLANNR
"utvaldfasttemp.PLANNR" "Plannr" ? "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.utvaldfasttemp.ARTAL
"utvaldfasttemp.ARTAL" ? ? "integer" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.utvaldfasttemp.AKTIV
"utvaldfasttemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UKALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPP
/* Query rebuild information for BROWSE BRW_UPP
     _TblList          = "Temp-Tables.visaupp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.visaupp.UT
"visaupp.UT" "Lista" "X(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.visaupp.TYP
"visaupp.TYP" "Typ" "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_URBER
/* Query rebuild information for BROWSE BRW_URBER
     _TblList          = "Temp-Tables.urberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   = Temp-Tables.urberedningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.urberedningtemp.BERNR
"urberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "5.5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.urberedningtemp.BENAMNING
"urberedningtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "16.5" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.urberedningtemp.AONR
"urberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.urberedningtemp.DELNR
"urberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.urberedningtemp.AKTIV
"urberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.urberedningtemp.ANVANDARE
"urberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_URBER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_URMARK
/* Query rebuild information for BROWSE BRW_URMARK
     _TblList          = "Temp-Tables.urvardtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.urvardtemp.OMRADE
"urvardtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.urvardtemp.VARDNR
"urvardtemp.VARDNR" "Värdering!nr" ? "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.urvardtemp.BENAMNING
"urvardtemp.BENAMNING" "Benämning" "X(21)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.urvardtemp.AONR
"urvardtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.urvardtemp.DELNR
"urvardtemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.urvardtemp.VARDANV
"urvardtemp.VARDANV" "Värderings!ansvarig" "X(8)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.urvardtemp.AKTIV
"urvardtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_URMARK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_URSTR
/* Query rebuild information for BROWSE BRW_URSTR
     _TblList          = "Temp-Tables.urstorntemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.urstorntemp.HDATUM
"urstorntemp.HDATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.urstorntemp.HKLOCKAN
"urstorntemp.HKLOCKAN" "Klockan" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.urstorntemp.ANVANDARE
"urstorntemp.ANVANDARE" "Upprättad av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.urstorntemp.ANSVARIGPERS
"urstorntemp.ANSVARIGPERS" "Ansvarig" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.urstorntemp.MERJOBB
"urstorntemp.MERJOBB" "Återstående" ? "logical" ? ? ? ? ? ? no ? no no "11.75" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_URSTR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAONR
/* Query rebuild information for BROWSE BRW_VAONR
     _TblList          = "Temp-Tables.valdaaotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valdaaotemp.AONR
"valdaaotemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valdaaotemp.DELNR
"valdaaotemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valdaaotemp.ORT
"valdaaotemp.ORT" "Ort/Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "24.5" yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.valdaaotemp.OMRADE
     _FldNameList[5]   > Temp-Tables.valdaaotemp.BEREDARE
"valdaaotemp.BEREDARE" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VAONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAVD
/* Query rebuild information for BROWSE BRW_VAVD
     _TblList          = "Temp-Tables.vavdelningtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.vavdelningtemp.AVDELNINGNR
"vavdelningtemp.AVDELNINGNR" "Avdelning" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.vavdelningtemp.AVDELNINGNAMN
"vavdelningtemp.AVDELNINGNAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VAVD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VBER
/* Query rebuild information for BROWSE BRW_VBER
     _TblList          = "Temp-Tables.valberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   = Temp-Tables.valberedningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.valberedningtemp.BERNR
"valberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valberedningtemp.BENAMNING
"valberedningtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valberedningtemp.AONR
"valberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.valberedningtemp.DELNR
"valberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.valberedningtemp.AKTIV
"valberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.valberedningtemp.ANVANDARE
"valberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VBER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VFAKT
/* Query rebuild information for BROWSE BRW_VFAKT
     _TblList          = "Temp-Tables.vfaktplantemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.vfaktplantemp.FAKTNR
"vfaktplantemp.FAKTNR" "Faktp.!nr" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no "4.5" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.vfaktplantemp.NAMN
"vfaktplantemp.NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.vfaktplantemp.VIBESTID
"vfaktplantemp.VIBESTID" "Best./!Kund" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.vfaktplantemp.SENASTFAK
"vfaktplantemp.SENASTFAK" "Senast!fakt." ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.vfaktplantemp.PANVANDARE
"vfaktplantemp.PANVANDARE" "Senast!ändrad av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.vfaktplantemp.ANVANDARE
"vfaktplantemp.ANVANDARE" "Ansvarig" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.vfaktplantemp.HANVANDARE
"vfaktplantemp.HANVANDARE" "Huvud-!ansvarig" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VFAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VKALK
/* Query rebuild information for BROWSE BRW_VKALK
     _TblList          = "Temp-Tables.valdfasttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valdfasttemp.KALKNR
"valdfasttemp.KALKNR" "Kalkyl nr" ">>>>>>9" "integer" ? 1 ? ? 1 ? no ? no no "6" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valdfasttemp.BENAMNING
"valdfasttemp.BENAMNING" "Ort/!Benämning" ? "character" ? ? ? ? ? ? no ? no no "2" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valdfasttemp.TYPCHAR
"valdfasttemp.TYPCHAR" "Typ" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valdfasttemp.VIKATAR
"valdfasttemp.VIKATAR" "Kat.!år" "9999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.valdfasttemp.AONR
"valdfasttemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.valdfasttemp.DELNR
"valdfasttemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.valdfasttemp.PLANNR
"valdfasttemp.PLANNR" "Planr" ? "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.valdfasttemp.ARTAL
"valdfasttemp.ARTAL" ? ? "integer" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.valdfasttemp.OMRADE
"valdfasttemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.valdfasttemp.AKTIV
"valdfasttemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VKALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VMARK
/* Query rebuild information for BROWSE BRW_VMARK
     _TblList          = "Temp-Tables.mvalvardtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.mvalvardtemp.OMRADE
"mvalvardtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.mvalvardtemp.VARDNR
"mvalvardtemp.VARDNR" "Värdering!nr" ? "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.mvalvardtemp.BENAMNING
"mvalvardtemp.BENAMNING" "Benämning" "X(21)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.mvalvardtemp.AONR
"mvalvardtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.mvalvardtemp.DELNR
"mvalvardtemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.mvalvardtemp.VARDANV
"mvalvardtemp.VARDANV" "Värderings!ansvarig" "X(8)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.mvalvardtemp.AKTIV
"mvalvardtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VMARK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VOMR
/* Query rebuild information for BROWSE BRW_VOMR
     _TblList          = "Temp-Tables.vomrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.vomrtemp.OMRADE
"vomrtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.vomrtemp.NAMN
"vomrtemp.NAMN" "Benämning" "X(19)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VOMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VPERS
/* Query rebuild information for BROWSE BRW_VPERS
     _TblList          = "Temp-Tables.valperstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valperstemp.PERSONALKOD
"valperstemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valperstemp.FORNAMN
"valperstemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valperstemp.EFTERNAMN
"valperstemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valperstemp.TELEFON
"valperstemp.TELEFON" "Telefon" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.valperstemp.MOBILTEL
"valperstemp.MOBILTEL" "Mobiltele" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   = Temp-Tables.valperstemp.OMRADE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VPERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VPLAN
/* Query rebuild information for BROWSE BRW_VPLAN
     _TblList          = "Temp-Tables.valplantemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valplantemp.OMRADE
"valplantemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valplantemp.PLANNR
"valplantemp.PLANNR" "Plannr" "X(8)" "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valplantemp.ARTAL
"valplantemp.ARTAL" "Årtal" ? "integer" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valplantemp.ORT
"valplantemp.ORT" "Ort/Benämning" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.valplantemp.AONR
"valplantemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no "6.5" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.valplantemp.DELNR
"valplantemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VPLAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VSTR
/* Query rebuild information for BROWSE BRW_VSTR
     _TblList          = "Temp-Tables.vstorntemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.vstorntemp.HDATUM
"vstorntemp.HDATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.vstorntemp.HKLOCKAN
"vstorntemp.HKLOCKAN" "Klockan" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.vstorntemp.ANVANDARE
"vstorntemp.ANVANDARE" "Upprättad av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.vstorntemp.ANSVARIGPERS
"vstorntemp.ANSVARIGPERS" "Ansvarig" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.vstorntemp.MERJOBB
"vstorntemp.MERJOBB" "Återstående" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.vstorntemp.STORNUMMERID
"vstorntemp.STORNUMMERID" "Störnings id" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VSTR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-AONR
/* Query rebuild information for FRAME FRAME-AONR
     _Query            is NOT OPENED
*/  /* FRAME FRAME-AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BER
/* Query rebuild information for FRAME FRAME-BER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-DEPA
/* Query rebuild information for FRAME FRAME-DEPA
     _Query            is NOT OPENED
*/  /* FRAME FRAME-DEPA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-FAKT
/* Query rebuild information for FRAME FRAME-FAKT
     _Query            is NOT OPENED
*/  /* FRAME FRAME-FAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KALK
/* Query rebuild information for FRAME FRAME-KALK
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-MARK
/* Query rebuild information for FRAME FRAME-MARK
     _Query            is NOT OPENED
*/  /* FRAME FRAME-MARK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-MTRL
/* Query rebuild information for FRAME FRAME-MTRL
     _Query            is NOT OPENED
*/  /* FRAME FRAME-MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-PERS
/* Query rebuild information for FRAME FRAME-PERS
     _Query            is NOT OPENED
*/  /* FRAME FRAME-PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-PLAN
/* Query rebuild information for FRAME FRAME-PLAN
     _Query            is NOT OPENED
*/  /* FRAME FRAME-PLAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-REG
/* Query rebuild information for FRAME FRAME-REG
     _Query            is NOT OPENED
*/  /* FRAME FRAME-REG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-SEK
/* Query rebuild information for FRAME FRAME-SEK
     _Query            is NOT OPENED
*/  /* FRAME FRAME-SEK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-START
/* Query rebuild information for FRAME FRAME-START
     _Query            is NOT OPENED
*/  /* FRAME FRAME-START */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-STATUS
/* Query rebuild information for FRAME FRAME-STATUS
     _Query            is NOT OPENED
*/  /* FRAME FRAME-STATUS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-STOR
/* Query rebuild information for FRAME FRAME-STOR
     _Query            is NOT OPENED
*/  /* FRAME FRAME-STOR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TID
/* Query rebuild information for FRAME FRAME-TID
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-UPP
/* Query rebuild information for FRAME FRAME-UPP
     _Query            is NOT OPENED
*/  /* FRAME FRAME-UPP */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME BRW_ANSV

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */

ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:   
   IF BTN_GURU:HIDDEN IN FRAME AMULTISTART-FRAME  = TRUE THEN DO:
      
   END.
   ELSE DO:
       RUN avb_UI.     
       APPLY "CLOSE":U TO THIS-PROCEDURE.
       RETURN NO-APPLY.
   END.
     
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:   
   RETURN NO-APPLY.
   /*
   RUN avb_UI.     
   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
   */
END.


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
   {STARTWIN2.I}                   
   IF varforetypval[25] = 22 THEN IMAGE-5:LOAD-IMAGE("BILDER/xstartguruxp.gif")  NO-ERROR.
   IF Guru.Konstanter:gaok BEGINS "s" THEN BTN_AONR:LOAD-IMAGE ("BILDER\xbtn_sonummer.gif") NO-ERROR.
   IF Guru.Konstanter:gaok BEGINS "A" THEN BTN_AONR:LOAD-IMAGE ("BILDER\xbtn_aonummer.gif") NO-ERROR.
   IF Guru.Konstanter:gaok BEGINS "k" THEN BTN_AONR:LOAD-IMAGE ("BILDER\xbtn_kknummer.gif") NO-ERROR.
   RUN huvud1_UI.
   IF musz = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      APPLY "CLOSE" TO THIS-PROCEDURE. 
      LEAVE MAIN-BLOCK.    
   END.      
   RUN huvud2_UI.
   
   IF musz = FALSE THEN RUN enable_UI.    
   BRW_MAXMALL:HIDDEN = TRUE.
   {FRMSIZEF.I}          
      
   RUN btnguru_UI.
   RUN insatt_UI.
   RUN huvud3_UI.
   RUN frame_UI (INPUT "START").
   IF Guru.Konstanter:hoppsekvar[2] = TRUE THEN DO:
      IF Guru.Konstanter:mtrlsekvar[5] = TRUE THEN DO:
         RUN btnberstart_UI (INPUT 0, INPUT "", INPUT 0, INPUT TABLE berintemp).                  
      END.      
   END.        
   IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO:  
      RUN btnkalkstart_UI (INPUT 0, INPUT 0, INPUT 0, INPUT TABLE kalkinmtemp).   
   END.    
   IF Guru.Konstanter:hoppsekvar[10] = TRUE THEN DO:          
      RUN btnfaktstart_UI (INPUT 0, INPUT "", INPUT 0, INPUT TABLE faktintemp).   
   END.
   IF Guru.Konstanter:hoppsekvar[12] = TRUE THEN DO:
      RUN btnmarkstart_UI (INPUT 0, INPUT "", INPUT 0, INPUT TABLE markintemp).   
   END.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:      
      ASSIGN
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE
      mspec_mtrlextra.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE.
   END.   
   IF musz = TRUE THEN LEAVE MAIN-BLOCK.  
   
   RUN frame_UI (INPUT "START").
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.                  
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?.   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      alltidmax = extradatatemp.SOKLOG[10].      
   END.
   /*
   IF extradatatemp.SOKCHAR[1] = "" THEN extradatatemp.SOKCHAR[1] = "AMERICAN".
   SESSION:NUMERIC-FORMAT = extradatatemp.SOKCHAR[1].
   */
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?. 
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {WIN_M_SLUT.I}
   FOR EACH widgettemp WHERE widgettemp.WIDGETTYP = "FILL-IN":
      IF widgettemp.WIDGETHAND:READ-ONLY = FALSE THEN widgettemp.WIDGETHAND:BGCOLOR = 15.
      ELSE widgettemp.WIDGETHAND:BGCOLOR = ?.     
   END.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allman_UI C-Win 
PROCEDURE allman_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER prog AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vad AS  CHARACTER NO-UNDO.
   IF prog = "RADTIDSVAL" THEN DO:
      RAD_TIDSVAL = INTEGER(vad).
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI C-Win 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE franfil  AS CHARACTER NO-UNDO.   
   IF avslut = TRUE THEN RETURN.
   avslut = TRUE.
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   IF Guru.Konstanter:globanv = "ELPAO" AND Guru.Konstanter:apphand NE ? THEN DO:
      Guru.Konstanter:appcon = TRUE.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.                  
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?.   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      IF VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
         IF extradatatemp.SOKLOG[1] = TRUE THEN RUN autofavo_UI IN Guru.SharedVariable:btnaonrh.           
      END.         
      IF VALID-HANDLE(btnberh) THEN DO:
         IF extradatatemp.SOKLOG[2] = TRUE THEN RUN autofavo_UI IN btnberh.       
      END.
      IF VALID-HANDLE(btnkalkh) THEN DO:
         IF extradatatemp.SOKLOG[3] = TRUE THEN RUN autofavo_UI IN btnkalkh.       
      END.     
      IF VALID-HANDLE(Guru.SharedVariable:btnmarkh) THEN DO:
         IF extradatatemp.SOKLOG[4] = TRUE THEN RUN autofavo_UI IN Guru.SharedVariable:btnmarkh.       
      END.      
      IF VALID-HANDLE(btnpersh) THEN DO:
         IF extradatatemp.SOKLOG[5] = TRUE THEN RUN autofavo_UI IN btnpersh.     
      END.
      IF VALID-HANDLE(btntidh) THEN DO:
         IF extradatatemp.SOKLOG[6] = TRUE THEN RUN autofavo_UI IN btntidh.
      END.
      IF VALID-HANDLE(Guru.SharedVariable:btnfakth) THEN DO:
         IF extradatatemp.SOKLOG[7] = TRUE THEN RUN autofavo_UI IN Guru.SharedVariable:btnfakth.
        
      END.
      IF VALID-HANDLE(Guru.SharedVariable:btnplanh) THEN DO:
         IF extradatatemp.SOKLOG[8] = TRUE THEN RUN autofavo_UI IN Guru.SharedVariable:btnplanh.    
      END.
      IF VALID-HANDLE(btnstorh) THEN DO:
         IF extradatatemp.SOKLOG[9] = TRUE THEN RUN autofavo_UI IN btnstorh.        
      END.     
   END.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?. 
   /*ut*/
   EMPTY TEMP-TABLE xsektemp NO-ERROR. 
   EMPTY TEMP-TABLE utsokaonr NO-ERROR. 
   EMPTY TEMP-TABLE aotidslagtemp NO-ERROR. 
   EMPTY TEMP-TABLE valdaaotemp NO-ERROR. 
   EMPTY TEMP-TABLE sparaladdatemp NO-ERROR. 
   EMPTY TEMP-TABLE uppvaltemp NO-ERROR. 
   EMPTY TEMP-TABLE bestkundallt NO-ERROR. 
   EMPTY TEMP-TABLE omrtemp NO-ERROR. 
   EMPTY TEMP-TABLE jurperstemp NO-ERROR. 
   IF Guru.Konstanter:appcon THEN DO:
      anvdator = "".
      ASSIGN
      SUBSTRING(anvdator,1,20) = Guru.Konstanter:globanv
      SUBSTRING(anvdator,25,20) = datornamn
      SUBSTRING(anvdator,50,20) = Guru.Konstanter:globanvnt
      SUBSTRING(anvdator,75,20) = SESSION:CLIENT-TYPE.
      RUN INLOGRAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT anvdator,INPUT FALSE).
   END. 
   IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
      RUN feldum_UI NO-ERROR.
   END.
   IF VALID-HANDLE(btnguruh) THEN DO:
      RUN borthand_UI IN btnguruh.
      DELETE PROCEDURE btnguruh NO-ERROR.      
   END.
   IF VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btnaonrh.
      DELETE PROCEDURE Guru.SharedVariable:btnaonrh NO-ERROR.      
   END.         
   IF VALID-HANDLE(btnberh) THEN DO:
      RUN borthand_UI IN btnberh.
      DELETE PROCEDURE btnberh NO-ERROR.      
   END.
   IF VALID-HANDLE(btnkalkh) THEN DO:
      RUN borthand_UI IN btnkalkh.
      DELETE PROCEDURE btnkalkh NO-ERROR.      
   END.
   IF VALID-HANDLE(Guru.SharedVariable:btnfakth) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btnfakth.
      DELETE PROCEDURE Guru.SharedVariable:btnfakth NO-ERROR.      
   END.
   IF VALID-HANDLE(Guru.SharedVariable:btnmarkh) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btnmarkh.
      DELETE PROCEDURE Guru.SharedVariable:btnmarkh NO-ERROR.      
   END.
   IF VALID-HANDLE(btnmtrlh) THEN DO:
      RUN borthand_UI IN btnmtrlh.
      DELETE PROCEDURE btnmtrlh NO-ERROR.      
   END.
   IF VALID-HANDLE(Guru.SharedVariable:btndepah) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btndepah.
      DELETE PROCEDURE Guru.SharedVariable:btndepah NO-ERROR.      
   END.
   IF VALID-HANDLE(btntidh) THEN DO:
      RUN borthand_UI IN btntidh.
      DELETE PROCEDURE btntidh NO-ERROR.      
   END.
   IF VALID-HANDLE(Guru.SharedVariable:btnupph) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btnupph.
      DELETE PROCEDURE Guru.SharedVariable:btnupph NO-ERROR.      
   END.
   IF VALID-HANDLE(btnpersh) THEN DO:
      RUN borthand_UI IN btnpersh.
      DELETE PROCEDURE btnpersh NO-ERROR.      
   END.
   IF VALID-HANDLE(Guru.SharedVariable:btnsekh) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btnsekh.
      DELETE PROCEDURE Guru.SharedVariable:btnsekh NO-ERROR.      
   END.
   IF VALID-HANDLE(btnstorh) THEN DO:
      RUN borthand_UI IN btnstorh.
      DELETE PROCEDURE btnstorh NO-ERROR.      
   END.
   IF VALID-HANDLE(Guru.SharedVariable:btnregh) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btnregh.
      DELETE PROCEDURE Guru.SharedVariable:btnregh NO-ERROR.      
   END.
   IF VALID-HANDLE(Guru.SharedVariable:btnplanh) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btnplanh.
      DELETE PROCEDURE Guru.SharedVariable:btnplanh NO-ERROR.      
   END.
   
   
   IF PROGRESS = "FULL" THEN DEFAULT-WINDOW:HIDDEN = FALSE.
   franfil = "DEL " + SESSION:TEMP-DIR + "*.TXT". 
   /*OS-DELETE VALUE(franfil).*/
   OS-COMMAND SILENT VALUE(franfil).
   franfil = "DEL " + SESSION:TEMP-DIR + "*.pdf". 
   /*OS-DELETE VALUE(franfil).*/
   OS-COMMAND SILENT VALUE(franfil).
   franfil = "DEL " + SESSION:TEMP-DIR + "*.htm". 
   /*OS-DELETE VALUE(franfil).*/
   OS-COMMAND SILENT VALUE(franfil).
   franfil = "DEL " + SESSION:TEMP-DIR + "*.html". 
   /*OS-DELETE VALUE(franfil).*/
   OS-COMMAND SILENT VALUE(franfil).
   IF Guru.Konstanter:globanv = "DEMO" THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      QUIT.
   END.
   IF VALID-HANDLE(startmh) THEN DO:
      DELETE PROCEDURE startmh NO-ERROR.      
   END.
   IF VALID-HANDLE(multitriggh) THEN DO:
      DELETE PROCEDURE multitriggh NO-ERROR.      
   END.
   nyprog = hmtprog.
   
   APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnaonr_UI C-Win 
PROCEDURE btnaonr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     
   HIDE FRAME FRAME-START.
   IF FRAME FRAME-BER:HIDDEN = FALSE THEN RUN avbe_UI IN btnberh.
   ELSE IF FRAME FRAME-FAKT:HIDDEN = FALSE THEN RUN avbe_UI IN Guru.SharedVariable:btnfakth.
   ELSE IF FRAME FRAME-KALK:HIDDEN = FALSE THEN RUN avbe_UI IN btnkalkh.
   ELSE IF FRAME FRAME-MARK:HIDDEN = FALSE THEN RUN avbe_UI IN Guru.SharedVariable:btnmarkh.
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   IF NOT VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
      EMPTY TEMP-TABLE whandltemp NO-ERROR. 
      CREATE whandltemp.
      ordningnr = 0.
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_OMR:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_JURP:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_AVD:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_BESORG:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN_EAONR:HANDLE IN FRAME FRAME-AONR).              
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN_SAONR:HANDLE IN FRAME FRAME-AONR).              
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN_DELNR:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, TOG_HUVNR:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, TOG_PAGA:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, TOG_AVSLUTADE:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, TOG_TILLF:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, TOG_FASTA:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-MELL:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-OCH:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_ARBART:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BRW_AONR:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BRW_VAONR:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BRW_AOTID:HANDLE IN FRAME FRAME-STATUS). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_FAK:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_TIDPLAN:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-K1:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-K2:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-KTO:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_OVER:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_ALLOVER:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_ALLBACK:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_BACK:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN_ORT:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_ANSV:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_PROJ:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_BERE:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-AOTEXT:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_AVSAONR:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.               
      RUN whandle_UI (INPUT ordningnr, BTN_BORT:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_HAMT:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_NY:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_UNDER:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_KOPI:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_VISAO:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_UPP:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-REF:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_BYTPNR:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_RAPP:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_KOST:HANDLE IN FRAME FRAME-AONR).     
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_BER:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_KALK:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_MARK:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_FAKT:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_AVROP:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_STATUS:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-AVSLUTD:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-AVSTARTD:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_FVE-3:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_FVE-4:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_NVE-3:HANDLE IN FRAME FRAME-AONR).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_NVE-4:HANDLE IN FRAME FRAME-AONR).              
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_AVB:HANDLE IN FRAME FRAME-AONR).              
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_AOF:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_HAOF:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, TOG_AONY:HANDLE IN FRAME FRAME-AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, CMB_PRIO:HANDLE IN FRAME FRAME-AONR). 
      RUN AONRMENY.P PERSISTENT SET Guru.SharedVariable:btnaonrh (INPUT THIS-PROCEDURE,INPUT btnguruh,INPUT framesizeh,INPUT TABLE whandltemp).   
   END.
   
   RUN frame_UI (INPUT "AONR").
   RUN brwfix_UI IN Guru.SharedVariable:btnaonrh.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnber0_UI C-Win 
PROCEDURE btnber0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnberstart_UI (INPUT 1, INPUT "", INPUT 0, INPUT TABLE berintemp).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnberstart_UI C-Win 
PROCEDURE btnberstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER beraonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER berdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR berintemp.     
   HIDE FRAME FRAME-START.   
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN goma_UI (INPUT TRUE,INPUT ""). 
   IF NOT VALID-HANDLE(btnberh) THEN DO:
      RUN btnber_UI  (INPUT franvart, INPUT beraonr, INPUT berdelnr, INPUT TABLE berintemp).       
   END.  
   ELSE RUN franstart_UI IN btnberh (INPUT franvart, INPUT beraonr, INPUT berdelnr, INPUT TABLE berintemp). 
   RUN frame_UI (INPUT "BER").
   /*direkt här*/
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN repo_UI IN btnberh. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnber_UI C-Win 
PROCEDURE btnber_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER beraonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER berdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR berintemp.
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_AKT-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_INAKT-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_JURP-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_AVD-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_OMR-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BACK-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_UTF-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_UPP-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_HAMT-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_OVER-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ALLOVER-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ALLBACK-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KALK-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_URBER:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_VBER:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_LIST:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_INK:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ATG:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KOPI-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_INAKTIV:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ADM:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_LAS:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_EXP:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_IMP:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BA:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ATT:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-BERNR:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_AONR:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_NY-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BORT-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-BEN:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-HBERNR:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_EAONR-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_DELNR-2:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-AOTEXT-B:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,IMAGE-7:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-HTEXT:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-22:HANDLE IN FRAME FRAME-BER).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-62:HANDLE IN FRAME FRAME-BER).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AOF-2:HANDLE IN FRAME FRAME-BER). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BER:HANDLE). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAOF-2:HANDLE IN FRAME FRAME-BER). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FRAME AMULTISTART-FRAME:HANDLE). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_BERNY:HANDLE IN FRAME FRAME-BER). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_MAXMALL:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_BERAO:HANDLE IN FRAME FRAME-BER).
   RUN BERMENY.P PERSISTENT SET btnberh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,
                                         INPUT franvart,INPUT beraonr,INPUT berdelnr,INPUT TABLE berintemp).   
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btndep0_UI C-Win 
PROCEDURE btndep0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btndepstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btndepstart_UI C-Win 
PROCEDURE btndepstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.
   IF NOT VALID-HANDLE(Guru.SharedVariable:btndepah) THEN DO:
      RUN btndep_UI.
   END.  
   ELSE RUN franstart_UI IN Guru.SharedVariable:btndepah. 
   RUN frame_UI (INPUT "DEPA").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btndep_UI C-Win 
PROCEDURE btndep_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_DEPA:HANDLE IN FRAME FRAME-DEPA). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_best:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_lager:HANDLE IN FRAME FRAME-DEPA).     
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_RAPP-7:HANDLE IN FRAME FRAME-DEPA).    
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BSTAT:HANDLE IN FRAME FRAME-DEPA).    
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_LEV:HANDLE IN FRAME FRAME-DEPA).     
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_inventering:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_UT:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RAD_PERIOD:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_LEVE:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_MTRL-7:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_MTRLPRIS:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_SEK:HANDLE IN FRAME FRAME-DEPA).     
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VISA-7:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_NY-7:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AND:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BORT-7:HANDLE IN FRAME FRAME-DEPA).   
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_LAS-2:HANDLE IN FRAME FRAME-DEPA).     
    ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-7:HANDLE IN FRAME FRAME-DEPA).       
   RUN DEPMENY.P PERSISTENT SET Guru.SharedVariable:btndepah (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfakt0_UI C-Win 
PROCEDURE btnfakt0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnfaktstart_UI (INPUT 1, INPUT "", INPUT 0, INPUT TABLE faktintemp).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfaktstart_UI C-Win 
PROCEDURE btnfaktstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER faonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER fdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR faktintemp.     
   HIDE FRAME FRAME-START.  
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN goma_UI (INPUT TRUE,INPUT ""). 
   IF NOT VALID-HANDLE(Guru.SharedVariable:btnfakth) THEN DO:
      RUN btnfakt_UI  (INPUT franvart, INPUT faonr, INPUT fdelnr, INPUT TABLE faktintemp).       
   END.  
   ELSE RUN franstart_UI IN Guru.SharedVariable:btnfakth (INPUT franvart, INPUT faonr, INPUT fdelnr, INPUT TABLE faktintemp). 
   RUN frame_UI (INPUT "FAKT").
   /*direkt här*/
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN repo_UI IN Guru.SharedVariable:btnfakth. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfakt_UI C-Win 
PROCEDURE btnfakt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER faonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER fdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR faktintemp.     
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).

   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, CMB_FAK-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_PROJEKTKOD:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, CMB_OMR-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, CMB_BESORG-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, TOG_ANSV:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, TOG_HUVA:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, TOG_PREL:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, TOG_GOD:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, TOG_GAMLA:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN-STARTDAT:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN-STOPPDAT:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAMT-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_KOPP:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BRW_UFAKT:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BRW_VFAKT:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_ALLOVER-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_FAK:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_PRELB:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_Kred:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_VISAO-2:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_UPP-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_VFAK:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_FLISTA:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AONRM:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_ADM-3:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AONRU:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_OVER-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_BACK-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_ALLBACK-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_NY-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_BORT-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_SFAKTNR:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_SNAMN:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, IMAGE-8:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_EFAKTNR:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_EFAKUNR:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN-HAMT:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_EAONR-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_DELNR-4:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AVB-4:HANDLE IN FRAME FRAME-FAKT). 
        ordningnr = ordningnr + 1.     
   RUN whandle_UI (INPUT ordningnr, RECT-21:HANDLE IN FRAME FRAME-FAKT). 
     ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, RECT-20:HANDLE IN FRAME FRAME-FAKT). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AOF-4:HANDLE IN FRAME FRAME-FAKT). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAOF-4:HANDLE IN FRAME FRAME-FAKT). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_MAXMALL:HANDLE IN FRAME FRAME-KALK).
   RUN FAKTMENY.P PERSISTENT SET Guru.SharedVariable:btnfakth (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,
                                         INPUT franvart,INPUT faonr,INPUT fdelnr,INPUT TABLE faktintemp).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnguru_UI C-Win 
PROCEDURE btnguru_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   IF NOT VALID-HANDLE(btnguruh) THEN DO:
      EMPTY TEMP-TABLE whandltemp NO-ERROR. 
      CREATE whandltemp.
      ordningnr = 0.   
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_AONR:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_BERM:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_DEPA:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_FAKTM:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_FLEX:HANDLE IN FRAME AMULTISTART-FRAME).              
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_GURU:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_KALKM:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_MARKM:HANDLE IN FRAME AMULTISTART-FRAME). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_MTRLM:HANDLE IN FRAME AMULTISTART-FRAME). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_PERS:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_PLAN:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_REGM:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_SEKM:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_SMS:HANDLE IN FRAME AMULTISTART-FRAME).              
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_STOR:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_TIDM:HANDLE IN FRAME AMULTISTART-FRAME).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_UPPF:HANDLE IN FRAME AMULTISTART-FRAME). 
      ordningnr = ordningnr + 1.   
      RUN whandle_UI (INPUT ordningnr, EDD_FUNK:HANDLE IN FRAME FRAME-START).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_MEDD:HANDLE IN FRAME FRAME-START).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_BYT:HANDLE IN FRAME FRAME-START).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_BYTW:HANDLE IN FRAME FRAME-START).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_UPPDAT:HANDLE IN FRAME FRAME-START).              
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, BTN_AVBGURU:HANDLE IN FRAME FRAME-START).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, ED_WWW:HANDLE IN FRAME FRAME-START).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-GURU:HANDLE IN FRAME FRAME-START). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, FILL-IN-ELPOOL:HANDLE IN FRAME FRAME-START). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, utsokaonr.OMRADE:HANDLE IN BROWSE BRW_AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, utsokaonr.BEREDARE:HANDLE IN BROWSE BRW_AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valdaaotemp.OMRADE:HANDLE IN BROWSE BRW_VAONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valdaaotemp.BEREDARE:HANDLE IN BROWSE BRW_VAONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, urberedningtemp.OMRADE:HANDLE IN BROWSE BRW_URBER). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, urberedningtemp.ANVANDARE:HANDLE IN BROWSE BRW_URBER). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valberedningtemp.OMRADE:HANDLE IN BROWSE BRW_VBER). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valberedningtemp.ANVANDARE:HANDLE IN BROWSE BRW_VBER). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, utvaldfasttemp.OMRADE:HANDLE IN BROWSE BRW_UKALK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, utvaldfasttemp.PLANNR:HANDLE IN BROWSE BRW_UKALK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, utvaldfasttemp.ARTAL:HANDLE IN BROWSE BRW_UKALK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valdfasttemp.OMRADE:HANDLE IN BROWSE BRW_VKALK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valdfasttemp.PLANNR:HANDLE IN BROWSE BRW_VKALK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valdfasttemp.ARTAL:HANDLE IN BROWSE BRW_VKALK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, urvardtemp.OMRADE:HANDLE IN BROWSE BRW_URMARK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, urvardtemp.VARDANV:HANDLE IN BROWSE BRW_URMARK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, mvalvardtemp.OMRADE:HANDLE IN BROWSE BRW_VMARK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, mvalvardtemp.VARDANV:HANDLE IN BROWSE BRW_VMARK).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, utsokaonr.ORT:HANDLE IN BROWSE BRW_AONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valdaaotemp.ORT:HANDLE IN BROWSE BRW_VAONR). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, urberedningtemp.BENAMNING:HANDLE IN BROWSE BRW_URBER). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valberedningtemp.BENAMNING:HANDLE IN BROWSE BRW_VBER). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, utvaldfasttemp.BENAMNING:HANDLE IN BROWSE BRW_UKALK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, valdfasttemp.BENAMNING:HANDLE IN BROWSE BRW_VKALK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, urvardtemp.BENAMNING:HANDLE IN BROWSE BRW_URMARK). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr, mvalvardtemp.BENAMNING:HANDLE IN BROWSE BRW_VMARK). 
      RUN GURUSTART.P PERSISTENT SET btnguruh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT multitriggh,INPUT TABLE whandltemp,INPUT-OUTPUT nyprog).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkalk0_UI C-Win 
PROCEDURE btnkalk0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnkalkstart_UI (INPUT 1, INPUT 0, INPUT 0, INPUT TABLE kalkinmtemp).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkalkstart_UI C-Win 
PROCEDURE btnkalkstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kalknrao AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kalktypvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR kalkinmtemp.   
   HIDE FRAME FRAME-START.
   
   
      /*assign
   utvaldfasttemp.KATAR:VISIBLE IN BROWSE BRW_UKALK = FALSE 
   valdfasttemp.KATAR:VISIBLE IN BROWSE BRW_VKALK = FALSE. 
       */  
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN goma_UI (INPUT TRUE,INPUT ""). 
   IF NOT VALID-HANDLE(btnkalkh) THEN DO:
      RUN btnkalk_UI (INPUT franvart, INPUT kalknrao, INPUT kalktypvar, INPUT TABLE kalkinmtemp).       
   END.  
   ELSE RUN franstart_UI IN btnkalkh (INPUT franvart, INPUT kalknrao, INPUT kalktypvar, INPUT TABLE kalkinmtemp). 
   RUN frame_UI (INPUT "KALK").
   /*direkt här*/
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN repo_UI IN btnkalkh. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkalk_UI C-Win 
PROCEDURE btnkalk_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kalknrao AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kalktypvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR kalkinmtemp.   
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_AKT-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_INAK-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_JURP-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_AVD-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_OMR-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BACK-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_UTF-3:HANDLE IN FRAME FRAME-KALK).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_BESORG-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_KANSV:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_KTYP-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_UPP-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_HAMT-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_OVER-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ALLOVER-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ALLBACK-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KALK-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_VKALK:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_UKALK:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KOPI-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_INAKTIV-2:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ADM-2:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VISKAL:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_EXP-2:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_IMP-2:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KONV:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_KALKB:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_NY-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BORT-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_KAONR:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_EAONR-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-KPLANNR:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN-KATEXT :HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_EKALNR:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_DELNR-3:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-VALK:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-51:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-53:HANDLE IN FRAME FRAME-KALK).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AOF-3:HANDLE IN FRAME FRAME-KALK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_KALKYL:HANDLE IN FRAME FRAME-KALK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,IMAGE-9:HANDLE IN FRAME FRAME-KALK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAOF-3:HANDLE IN FRAME FRAME-KALK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FRAME FRAME-AONR:HANDLE). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FRAME FRAME-PLAN:HANDLE).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_MAXMALL:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_KALKAO:HANDLE IN FRAME FRAME-KALK).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,valdfasttemp.BENAMNING:HANDLE IN BROWSE BRW_VKALK).

   RUN KALKMENY.P PERSISTENT SET btnkalkh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,
                                         INPUT franvart,INPUT kalknrao,INPUT kalktypvar,INPUT TABLE kalkinmtemp).   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnmark0_UI C-Win 
PROCEDURE btnmark0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnmarkstart_UI (INPUT 1, INPUT "", INPUT 0, INPUT TABLE markintemp).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnmarkstart_UI C-Win 
PROCEDURE btnmarkstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER faonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER fdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR markintemp.     
   HIDE FRAME FRAME-START.
   
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN goma_UI (INPUT TRUE,INPUT "").   
   IF NOT VALID-HANDLE(Guru.SharedVariable:btnmarkh) THEN DO:
      RUN btnmark_UI  (INPUT franvart, INPUT faonr, INPUT fdelnr, INPUT TABLE markintemp).       
   END.  
   ELSE RUN franstart_UI IN Guru.SharedVariable:btnmarkh (INPUT franvart, INPUT faonr, INPUT fdelnr, INPUT TABLE markintemp). 
   RUN frame_UI (INPUT "MARK").
   /*direkt här*/
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN repo_UI IN Guru.SharedVariable:btnmarkh. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnmark_UI C-Win 
PROCEDURE btnmark_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER faonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER fdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR markintemp.     
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, TOG_AKT-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, TOG_INAKT-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, CMB_OMR-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, CMB_UTF-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, CMB_ANSV-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAMT-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BRW_URMARK:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BRW_VMARK:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_UPP-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AOF-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_FASTIGHET:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_ALLOVER-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_VARD:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_VISKAL-2:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_OVER-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_VISMARK:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_OMRAKNA:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_BACK-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_INAKTIV-3:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_ADM-4:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_ALLBACK-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_SVARD:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_VARDANV:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_NY-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_BORT-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_AONR-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_BEN-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_EVARD:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_EAONR-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_DELNR-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AVB-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN-AOTEXT-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, IMAGE-10:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN-HTEXT-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, RECT-52:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, RECT-23:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAOF-5:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_EXPM:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_IMPM:HANDLE IN FRAME FRAME-MARK). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_MAXMALL:HANDLE IN FRAME FRAME-KALK).
   RUN MARKMENY.P PERSISTENT SET Guru.SharedVariable:btnmarkh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,
                                         INPUT franvart,INPUT faonr,INPUT fdelnr,INPUT TABLE markintemp).   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnmtrl0_UI C-Win 
PROCEDURE btnmtrl0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnmtrlstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnmtrlstart_UI C-Win 
PROCEDURE btnmtrlstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.
   IF NOT VALID-HANDLE(btnmtrlh) THEN DO:
      RUN btnmtrl_UI.
   END.  
   ELSE RUN franstart_UI IN btnmtrlh. 
   RUN frame_UI (INPUT "MTRL").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnmtrl_UI C-Win 
PROCEDURE btnmtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_HLEV:HANDLE IN FRAME FRAME-MTRL). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_MTRL:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,btn_over-6:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_MTRL:HANDLE IN FRAME FRAME-MTRL).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VISA:HANDLE IN FRAME FRAME-MTRL).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,btn_back-6:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_OFF :HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_SKRIV:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_LEV:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_LEVH:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-ENR:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_UP:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-BEN-6:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-ENR2 :HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-ANTAL:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_MIN:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RAD_SOK:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-6:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-SOKALT:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-4:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,IMAGE-1:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-2:HANDLE IN FRAME FRAME-MTRL).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_information:HANDLE IN MENU POPUP-MENU-BRW_HLEV).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_information2:HANDLE IN MENU POPUP-MENU-BRW_MTRL).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_IEXC:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,mspec_mtrlextra.BERKVANT:HANDLE IN BROWSE BRW_MTRL).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_AvmarkeraHLEV:HANDLE IN MENU POPUP-MENU-BRW_HLEV).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Sats_information:HANDLE IN MENU POPUP-MENU-BRW_HLEV).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Sats_information2:HANDLE IN MENU POPUP-MENU-BRW_MTRL).     
   RUN MTRLMENY.P PERSISTENT SET btnmtrlh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnpers0_UI C-Win 
PROCEDURE btnpers0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnpersstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnpersstart_UI C-Win 
PROCEDURE btnpersstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.
  
   IF NOT VALID-HANDLE(btnpersh) THEN DO:
      RUN btnpers_UI.
   END.  
   ELSE RUN franstart_UI IN btnpersh. 
   RUN frame_UI (INPUT "PERS").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnpers_UI C-Win 
PROCEDURE btnpers_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_JURP-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_AKTIV-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_SEMFOR-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_AVD-10 :HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_VECKO-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_DELTID-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_OMR-10 :HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_BEFATTNING-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_FLEX-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_TIDSGODK-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_TRA-10 :HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_FORNAMN-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_BER-10 :HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_EFTERNAMN-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_ANST-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_HAMT-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_UPP-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_PERS-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_VPERS:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_SCH:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ARB-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ALLOVER-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VISA-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_DEBPR :HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_OVER-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VISAP:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_SEK-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BACK-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_OT:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ALLBACK-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_NY-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BORT-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_SPERSONALKOD-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_SFORNAMN-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_SEFTERNAMN-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_EPERSONALKOD-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-10 :HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-PERSONAL:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-24:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-25:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AOF-10:HANDLE IN FRAME FRAME-PERS).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAOF-10:HANDLE IN FRAME FRAME-PERS). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_TELEFONLISTA:HANDLE IN FRAME FRAME-PERS). 
   RUN PERSMENY.P PERSISTENT SET btnpersh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnplan0_UI C-Win 
PROCEDURE btnplan0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnplanstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnplanstart_UI C-Win 
PROCEDURE btnplanstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.
   IF NOT VALID-HANDLE(Guru.SharedVariable:btnplanh) THEN DO:
      RUN btnplan_UI.
   END.  
   ELSE RUN franstart_UI IN Guru.SharedVariable:btnplanh. 
   RUN frame_UI (INPUT "PLAN").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnplan_UI C-Win 
PROCEDURE btnplan_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).         
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_NVE-133:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_NVE-134:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_FASTA-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_AVSLUTADE-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-MELL-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-AVSTARTD-13:HANDLE IN FRAME FRAME-PLAN).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-OCH-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-AVSLUTD-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_FVE-133:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_FVE-134:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_TILLF-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,TOG_PAGA-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_OMR-13 :HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RAD_PERIOD-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_BESORG-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-K1-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_ARTAL-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-K2-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_ANSV-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_FRAN-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_TILL-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_ARBART-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_HAMT-6 :HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_PLAN:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_VPLAN:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_UPP-6:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ALLOVER-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_UNDER-2:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_OVER-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BUNDER:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BACK-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BUD:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KALK-4:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_ALLBACK-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_RAPP-2:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVSAONR-2:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VISAO-3:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AOF-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_NY-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BORT-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_SPLANNR-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-13 :HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_ORT-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_EPLANNR-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_ARTAL-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_PLANNRVAL:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-KTO-13:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,IMAGE-12:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN-VAL:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-27:HANDLE IN FRAME FRAME-PLAN).    
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-28:HANDLE IN FRAME FRAME-PLAN).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAOF-13:HANDLE IN FRAME FRAME-PLAN).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_PTIDPLAN:HANDLE IN FRAME FRAME-PLAN).

   RUN PLANMENY.P PERSISTENT SET Guru.SharedVariable:btnplanh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg0_UI C-Win 
PROCEDURE btnreg0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnregstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnregstart_UI C-Win 
PROCEDURE btnregstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.
   
   IF NOT VALID-HANDLE(Guru.SharedVariable:btnregh) THEN DO:
      RUN btnreg_UI.
   END.  
   ELSE RUN franstart_UI IN Guru.SharedVariable:btnregh. 
   RUN frame_UI (INPUT "REG").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg_UI C-Win 
PROCEDURE btnreg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_REG:HANDLE IN FRAME FRAME-REG).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KOR:HANDLE IN FRAME FRAME-REG).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-12:HANDLE IN FRAME FRAME-REG).
   RUN REGMENY.P PERSISTENT SET Guru.SharedVariable:btnregh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnsek0_UI C-Win 
PROCEDURE btnsek0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnsekstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnsekstart_UI C-Win 
PROCEDURE btnsekstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE helpsek AS LOGICAL NO-UNDO.
   HIDE FRAME FRAME-START.
   IF NOT VALID-HANDLE(Guru.SharedVariable:btnsekh) THEN DO:
      RUN btnsek_UI.
      helpsek = TRUE.
   END.  
   ELSE RUN franstart_UI IN Guru.SharedVariable:btnsekh. 
   RUN frame_UI (INPUT "SEK").
   IF helpsek = TRUE THEN DO:
      helpsek = FALSE.
      RUN frameval_UI IN Guru.SharedVariable:btnsekh (INPUT 1).
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnsek_UI C-Win 
PROCEDURE btnsek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).         
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,MBTN_ANV:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RAD_VAL:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,MBTN_BEH:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,MBTN_OVR:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VIALLA:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_SKR-2:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VPERS:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_SKRIV-2:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_SKRTEST:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-11:HANDLE IN FRAME FRAME-SEK).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BLOBADM:HANDLE IN FRAME FRAME-OVR).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_EJAUTO:HANDLE IN FRAME FRAME-OVR).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KOSTS:HANDLE IN FRAME FRAME-OVR).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_OBEORD:HANDLE IN FRAME FRAME-OVR).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,SEL_MENY:HANDLE IN FRAME FRAME-BEH).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_MENY:HANDLE IN FRAME FRAME-BEH).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_NY-112:HANDLE IN FRAME FRAME-BEH).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BORT-112:HANDLE IN FRAME FRAME-BEH).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_AV-LEVEL-FRAN:HANDLE IN FRAME FRAME-BEH).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_AV-LEVEL-NY:HANDLE IN FRAME FRAME-BEH).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-41:HANDLE IN FRAME FRAME-BEH).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_ANV:HANDLE IN FRAME FRAME-ANV).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_NY-11:HANDLE IN FRAME FRAME-ANV).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_UPP-11:HANDLE IN FRAME FRAME-ANV).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_BORT-11:HANDLE IN FRAME FRAME-ANV).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_VISA-11:HANDLE IN FRAME FRAME-ANV).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_SANVANDARE-11:HANDLE IN FRAME FRAME-ANV).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_SNAMN-11:HANDLE IN FRAME FRAME-ANV).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,RECT-26:HANDLE IN FRAME FRAME-ANV).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-ANV:HANDLE).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BEH:HANDLE).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-OVR:HANDLE).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_SUPPORT:HANDLE IN FRAME FRAME-OVR).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_TEXTANV:HANDLE IN FRAME FRAME-ANV).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_TEXTBEH:HANDLE IN FRAME FRAME-BEH).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,FILL-IN_TEXTOVR:HANDLE IN FRAME FRAME-OVR).   
   

   RUN SEKMENY.P PERSISTENT SET Guru.SharedVariable:btnsekh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnstatus_UI C-Win 
PROCEDURE btnstatus_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {muswait.i}
   RUN btnstatus_UI IN Guru.SharedVariable:btnaonrh.
   {musarrow.i}   
   RUN frame_UI (INPUT "STATUS").  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnstor0_UI C-Win 
PROCEDURE btnstor0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnstorstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnstorstart_UI C-Win 
PROCEDURE btnstorstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.
   IF NOT VALID-HANDLE(btnstorh) THEN DO:
      RUN btnstor_UI.
   END.  
   ELSE RUN franstart_UI IN btnstorh. 
   RUN frame_UI (INPUT "STOR").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnstor_UI C-Win 
PROCEDURE btnstor_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).         
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,RAD_VAL-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,CMB_ANL-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,CMB_FOR-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,CMB_SYS-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,CMB_OMR-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,CMB_AR-14 :HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,CMB_SYS2-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,CMB_BEL-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,CMB_FEL-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_HAMT-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BRW_URSTR:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BRW_VSTR:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_ALLOVER-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_OVER-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_AND-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_BACK-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_RAPP-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_ALLBACK-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_ADM-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_LAS-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_VIS-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_NY-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_BORT-14 :HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,FILL-IN-BEN-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,FILL-IN-STOR-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,FILL-IN-BEN-142:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,FILL-IN-STRNR-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_AVB-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,FILL-IN-AOTEXT-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,RECT-29:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,RECT-33:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr,BTN_AOF-14:HANDLE IN FRAME FRAME-STOR).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAOF-14:HANDLE IN FRAME FRAME-STOR). 
   RUN STORMENY.P PERSISTENT SET btnstorh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,INPUT 1).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btntid0_UI C-Win 
PROCEDURE btntid0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btntidstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btntidstart_UI C-Win 
PROCEDURE btntidstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.
   IF NOT VALID-HANDLE(btntidh) THEN DO:
      RUN btntid_UI.
   END.  
   ELSE RUN franstart_UI IN btntidh. 
   RUN frame_UI (INPUT "TID").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btntid_UI C-Win 
PROCEDURE btntid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_PALLBACK:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_PALLOVER:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_PBACK:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, RAD_ALLTID:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_TARST:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_PVVOVER:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_TMANADST:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_TARSL:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_TMANADSL:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, MBTN_FLEXADM:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, RAD_TIDSVAL:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_NDA:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_TID:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_REG:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_AR:HANDLE IN FRAME FRAME-TID).         
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_MANAD:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN_DATUM:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN-TDATUM:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, MBTN_STANSA:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, TOG_MAN:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_TAR:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_TMANAD:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_FDA:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_LON:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_ARES:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_BER-8:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_TBRES:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, MBTN_TIDADM:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, RAD_ALLVAL:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_TRA:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_UTRES:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_AVD:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_VAVD:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, MBTN_TJANSTERESA:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_OVERT:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_KONT:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, CMB_OMR-8:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_PERS:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_VISA-8:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_GRAN:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_ANSV:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_GODK:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_MARK:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_ARB:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_GOD:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, MBTN_TIDSEDEL:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_VOVER:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_ANDGOD:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_VTIDV:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_LGOD:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_SKR:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_VBACK:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_VECK:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_VTID:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, MBTN_TILLAGG:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_LONA:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_ANDT:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_OMR:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BRW_VOMR:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_AOF-8:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_PERIOD:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_FRAMAN:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_ALLOVER-8:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_FRAPP:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_AVVIK:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_VVOVER-8:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_ANOV:HANDLE IN FRAME FRAME-TID).   
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_BACK-8:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_SALDO:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_MANSAL:HANDLE IN FRAME FRAME-TID).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_ALLBACK-8:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_KONTROLL:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_FLKORN:HANDLE IN FRAME FRAME-TID).       
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_SALMAN:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN_SPERSONALKOD-8:HANDLE IN FRAME FRAME-TID).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, FILL-IN_SFORNAMN-8:HANDLE IN FRAME FRAME-TID).    
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, IMAGE-14:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BTN_AVB-8:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN-TDAG-2:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN-TDATUM-2:HANDLE IN FRAME FRAME-TID).     
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN-TDAG:HANDLE IN FRAME FRAME-TID).         
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN-HAMTA-8:HANDLE IN FRAME FRAME-TID).       
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN_EPERSONALKOD-8:HANDLE IN FRAME FRAME-TID).
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN_SEFTERNAMN-8:HANDLE IN FRAME FRAME-TID). 
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, FILL-IN-VP:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, RECT-S:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, RECT-H:HANDLE IN FRAME FRAME-TID).      
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_HAOF-8:HANDLE IN FRAME FRAME-TID). 
   RUN TIDMENY.P PERSISTENT SET btntidh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnupp0_UI C-Win 
PROCEDURE btnupp0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   RUN btnuppstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnuppstart_UI C-Win 
PROCEDURE btnuppstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.
   IF NOT VALID-HANDLE(Guru.SharedVariable:btnupph) THEN DO:
      RUN btnupp_UI.
   END.  
   ELSE RUN franstart_UI IN Guru.SharedVariable:btnupph. 
   IF FRAME FRAME-FAKT:HIDDEN = FALSE THEN RUN uppladd_UI IN Guru.SharedVariable:btnupph (INPUT "FAK").
   ELSE IF FRAME FRAME-PLAN:HIDDEN = FALSE THEN RUN uppladd_UI IN Guru.SharedVariable:btnupph (INPUT "PLAN").
   ELSE RUN uppladd_UI IN Guru.SharedVariable:btnupph (INPUT "AO").
   RUN frame_UI (INPUT "UPP").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnupp_UI C-Win 
PROCEDURE btnupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_UPP:HANDLE IN FRAME FRAME-UPP).  
   ordningnr = ordningnr + 1.    
   RUN whandle_UI (INPUT ordningnr, BTN_VISA-9:HANDLE IN FRAME FRAME-UPP).  
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AVB-9 :HANDLE IN FRAME FRAME-UPP).   
   ordningnr = ordningnr + 1.
   RUN UPPMENY.P PERSISTENT SET Guru.SharedVariable:btnupph (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eddstart_UI C-Win 
PROCEDURE eddstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EDD_FUNK:HIDDEN IN FRAME FRAME-START = TRUE.
    RETURN.
   ASSIGN      
   {&WINDOW-NAME}:WIDTH-CHARS = 128.
   {&WINDOW-NAME}:HEIGHT-CHARS = {&WINDOW-NAME}:HEIGHT-CHARS + 3.
   FRAME FRAME-START:HEIGHT-CHARS = FRAME FRAME-START:HEIGHT-CHARS + 3.
   FRAME FRAME-START:WIDTH-CHARS = {&WINDOW-NAME}:WIDTH-CHARS - 3.
   /*FRAME FRAME-PERS:HEIGHT-CHARS = {&WINDOW-NAME}:HEIGHT-CHARS - 1.*/
   
   ASSIGN
   EDD_FUNK:FONT = 5
   EDD_FUNK:WIDTH-CHARS = EDD_FUNK:WIDTH-CHARS - 9
   EDD_FUNK:HEIGHT-CHARS = EDD_FUNK:HEIGHT-CHARS + 11.5
   EDD_FUNK:COLUMN = 69 
   EDD_FUNK:ROW = 2. 
   ASSIGN
   IMAGE-3:WIDTH-CHARS = 20
   IMAGE-3:HEIGHT-CHARS = 3.2
   IMAGE-3:COLUMN = 50 
   IMAGE-3:ROW = 2. 
   IMAGE-3:LOAD-IMAGE("BILDER\elpool_guru.gif").
   IMAGE-3:HIDDEN = FALSE.
   ASSIGN
   ED_WWW:ROW = ED_WWW:ROW + 3
   BTN_AVBGURU:ROW =     BTN_AVBGURU:ROW + 3   
   BTN_BYT:ROW =     BTN_BYT:ROW + 3
   BTN_BYTW:ROW =    BTN_BYTW:ROW + 3  
   BTN_MEDD:ROW =    BTN_MEDD:ROW + 3  
   BTN_UPPDAT:ROW =  BTN_UPPDAT:ROW + 3
   EDD_FUNK:ROW =    EDD_FUNK:ROW + 3.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE BTN_MTRLM IMAGE-4 BTN_AONR RECT-MENY BTN_TIDM BTN_REGM BTN_SEKM 
         BTN_MARKM BTN_KALKM BTN_FAKTM BTN_BERM BTN_DEPA BTN_FLEX BTN_PLAN 
         BTN_SMS BTN_STOR BTN_GURU BTN_PERS BTN_UPPF 
      WITH FRAME AMULTISTART-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-AMULTISTART-FRAME}
  DISPLAY FILL-IN_SANVANDARE-11 FILL-IN_SNAMN-11 FILL-IN_TEXTANV 
      WITH FRAME FRAME-ANV IN WINDOW C-Win.
  ENABLE IMAGE-15 RECT-26 BRW_ANV BTN_NY-11 BTN_UPP-11 BTN_BORT-11 BTN_VISA-11 
         FILL-IN_SANVANDARE-11 FILL-IN_SNAMN-11 FILL-IN_TEXTANV 
      WITH FRAME FRAME-ANV IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANV}
  DISPLAY SEL_MENY FILL-IN_AV-LEVEL-FRAN FILL-IN_AV-LEVEL-NY FILL-IN_TEXTBEH 
      WITH FRAME FRAME-BEH IN WINDOW C-Win.
  ENABLE RECT-41 SEL_MENY BRW_MENY BTN_NY-112 BTN_BORT-112 
         FILL-IN_AV-LEVEL-FRAN FILL-IN_AV-LEVEL-NY FILL-IN_TEXTBEH 
      WITH FRAME FRAME-BEH IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BEH}
  DISPLAY FILL-IN_TEXTOVR 
      WITH FRAME FRAME-OVR IN WINDOW C-Win.
  ENABLE BTN_SUPPORT BTN_BLOBADM BTN_EJAUTO BTN_KOSTS BTN_OBEORD 
         FILL-IN_TEXTOVR 
      WITH FRAME FRAME-OVR IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-OVR}
  DISPLAY FILL-IN-AVSTARTD FILL-IN-AVSLUTD TOG_PAGA TOG_AVSLUTADE TOG_TILLF 
          TOG_FASTA TOG_HUVNR CMB_JURP CMB_PROJ CMB_AVD CMB_BERE FILL-IN-K1 
          CMB_ANSV CMB_OMR FILL-IN-K2 CMB_BESORG TOG_AONY CMB_PRIO CMB_FAK 
          FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR FILL-IN-REF 
          FILL-IN-AOTEXT FILL-IN-KTO FILL-IN-AOTEXT-3 
      WITH FRAME FRAME-AONR IN WINDOW C-Win.
  ENABLE BTN_OVER IMAGE-6 BTN_ALLOVER RECT-50 RECT-60 BTN_NVE-3 BTN_NVE-4 
         FILL-IN-AVSTARTD FILL-IN-AVSLUTD TOG_PAGA TOG_AVSLUTADE BTN_FVE-3 
         BTN_FVE-4 TOG_TILLF TOG_FASTA TOG_HUVNR CMB_JURP CMB_PROJ CMB_AVD 
         CMB_BERE FILL-IN-K1 CMB_ANSV CMB_OMR FILL-IN-K2 CMB_ARBART CMB_BESORG 
         TOG_AONY CMB_PRIO CMB_FAK BTN_HAMT BRW_AONR BRW_VAONR BTN_AOF BTN_HAOF 
         BTN_BYTPNR BTN_UPP BTN_MARK BTN_TIDPLAN BTN_BER BTN_UNDER BTN_FAKT 
         BTN_KALK BTN_RAPP BTN_AVSAONR BTN_KOST BTN_VISAO BTN_STATUS BTN_KOPI 
         BTN_AVROP BTN_NY BTN_BORT FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR 
         BTN_ALLBACK FILL-IN_DELNR FILL-IN-REF BTN_AVB BTN_BACK 
      WITH FRAME FRAME-AONR IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-AONR}
  DISPLAY TOG_AKT-2 TOG_INAKT-2 TOG_BERAO CMB_JURP-2 CMB_AVD-2 CMB_OMR-2 
          CMB_UTF-2 TOG_BERNY FILL-IN-BERNR FILL-IN_AONR FILL-IN-BEN 
          FILL-IN-HBERNR FILL-IN_EAONR-2 FILL-IN_DELNR-2 FILL-IN-AOTEXT-B 
          FILL-IN-HTEXT 
      WITH FRAME FRAME-BER IN WINDOW C-Win.
  ENABLE BTN_ALLOVER-2 IMAGE-7 BTN_ALLBACK-2 RECT-22 RECT-62 TOG_AKT-2 
         TOG_INAKT-2 TOG_BERAO CMB_JURP-2 CMB_AVD-2 CMB_OMR-2 CMB_UTF-2 
         BTN_HAMT-2 BTN_KALK-2 BTN_UPP-2 BRW_URBER BRW_VBER TOG_BERNY BTN_LIST 
         BTN_INK BTN_AOF-2 BTN_HAOF-2 BTN_ATG BTN_KOPI-2 BTN_INAKTIV BTN_ADM 
         BTN_LAS BTN_EXP BTN_IMP BTN_BACK-2 BTN_BA BTN_ATT BTN_NY-2 BTN_BORT-2 
         FILL-IN-BERNR FILL-IN_AONR FILL-IN-BEN FILL-IN-HBERNR FILL-IN_EAONR-2 
         FILL-IN_DELNR-2 BTN_OVER-2 BTN_AVB-2 
      WITH FRAME FRAME-BER IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BER}
  ENABLE BRW_DEPA BTN_best BTN_lager BTN_RAPP-7 BTN_BSTAT BTN_LEV 
         BTN_inventering BTN_UT BTN_LEVE BTN_MTRL-7 BTN_MTRLPRIS BTN_SEK 
         BTN_VISA-7 BTN_NY-7 BTN_AND BTN_BORT-7 BTN_LAS-2 BTN_AVB-7 
      WITH FRAME FRAME-DEPA IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-DEPA}
  DISPLAY CMB_FAK-4 FILL-IN_PROJEKTKOD CMB_OMR-4 CMB_BESORG-4 TOG_ANSV TOG_HUVA 
          TOG_PREL TOG_GOD TOG_GAMLA FILL-IN-STARTDAT FILL-IN-STOPPDAT 
          FILL-IN_EFAKTNR FILL-IN-HAMT FILL-IN_EFAKUNR FILL-IN_SFAKTNR 
          FILL-IN_SNAMN FILL-IN_EAONR-4 FILL-IN_DELNR-4 
      WITH FRAME FRAME-FAKT IN WINDOW C-Win.
  ENABLE IMAGE-8 RECT-20 RECT-21 CMB_FAK-4 FILL-IN_PROJEKTKOD CMB_OMR-4 
         CMB_BESORG-4 TOG_ANSV TOG_HUVA TOG_PREL TOG_GOD TOG_GAMLA 
         FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_HAMT-4 BTN_KOPP BRW_UFAKT 
         BRW_VFAKT BTN_Kred BTN_HAOF-4 BTN_PRELB BTN_ALLOVER-4 BTN_UPP-4 
         BTN_VISAO-2 BTN_VFAK BTN_FAK BTN_ADM-3 BTN_FLISTA BTN_AONRM BTN_OVER-4 
         BTN_AONRU BTN_AOF-4 BTN_BACK-4 BTN_ALLBACK-4 BTN_NY-4 BTN_BORT-4 
         FILL-IN_EFAKTNR FILL-IN_EFAKUNR FILL-IN_SFAKTNR FILL-IN_SNAMN 
         FILL-IN_EAONR-4 FILL-IN_DELNR-4 BTN_AVB-4 
      WITH FRAME FRAME-FAKT IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-FAKT}
  DISPLAY TOG_KALKAO TOG_AKT-3 TOG_INAK-3 CMB_JURP-3 CMB_AVD-3 CMB_OMR-3 
          CMB_UTF-3 CMB_BESORG-3 CMB_KANSV CMB_KTYP-3 FILL-IN_KAONR 
          FILL-IN_KALKYL FILL-IN-KPLANNR FILL-IN_KALKB FILL-IN_EKALNR 
          FILL-IN_EAONR-3 FILL-IN_DELNR-3 FILL-IN-KATEXT FILL-IN-VALK 
      WITH FRAME FRAME-KALK IN WINDOW C-Win.
  ENABLE IMAGE-9 BTN_ALLOVER-3 BTN_ALLBACK-3 RECT-51 RECT-53 TOG_KALKAO 
         TOG_AKT-3 TOG_INAK-3 CMB_JURP-3 CMB_AVD-3 CMB_OMR-3 CMB_UTF-3 
         CMB_BESORG-3 CMB_KANSV CMB_KTYP-3 BTN_HAMT-3 BTN_UPP-3 BRW_UKALK 
         BRW_VKALK BTN_AOF-3 BTN_HAOF-3 BTN_KALK-3 BTN_VISKAL BTN_KOPI-3 
         BTN_KONV BTN_INAKTIV-2 BTN_ADM-2 BTN_EXP-2 BTN_IMP-2 BTN_NY-3 
         BTN_BORT-3 FILL-IN_KAONR FILL-IN_KALKYL BTN_BACK-3 FILL-IN-KPLANNR 
         FILL-IN_KALKB FILL-IN_EKALNR FILL-IN_EAONR-3 FILL-IN_DELNR-3 BTN_AVB-3 
         BTN_OVER-3 
      WITH FRAME FRAME-KALK IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KALK}
  DISPLAY TOG_AKT-5 TOG_INAKT-5 CMB_OMR-5 CMB_UTF-5 CMB_ANSV-5 FILL-IN_VARDANV 
          FILL-IN_SVARD FILL-IN_AONR-5 FILL-IN_BEN-5 FILL-IN_EVARD 
          FILL-IN_EAONR-5 FILL-IN_DELNR-5 FILL-IN-AOTEXT-5 FILL-IN-HTEXT-5 
      WITH FRAME FRAME-MARK IN WINDOW C-Win.
  ENABLE IMAGE-10 RECT-23 RECT-52 TOG_AKT-5 TOG_INAKT-5 CMB_OMR-5 CMB_UTF-5 
         CMB_ANSV-5 BTN_HAMT-5 BRW_URMARK BRW_VMARK BTN_UPP-5 BTN_AOF-5 
         BTN_FASTIGHET BTN_HAOF-5 BTN_ALLOVER-5 BTN_VARD BTN_VISKAL-2 
         BTN_OVER-5 BTN_VISMARK BTN_OMRAKNA BTN_BACK-5 BTN_INAKTIV-3 BTN_ADM-4 
         BTN_ALLBACK-5 BTN_EXPM BTN_IMPM BTN_NY-5 BTN_BORT-5 FILL-IN_VARDANV 
         FILL-IN_SVARD FILL-IN_AONR-5 FILL-IN_BEN-5 FILL-IN_EVARD 
         FILL-IN_EAONR-5 FILL-IN_DELNR-5 BTN_AVB-5 
      WITH FRAME FRAME-MARK IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MARK}
  DISPLAY CMB_LEV FILL-IN-ENR FILL-IN-BEN-6 FILL-IN-ENR2 FILL-IN-ANTAL RAD_SOK 
          FILL-IN-SOKALT 
      WITH FRAME FRAME-MTRL IN WINDOW C-Win.
  ENABLE IMAGE-1 RECT-2 RECT-4 BRW_HLEV BRW_MTRL btn_over-6 BTN_MTRL BTN_VISA 
         btn_back-6 BTN_OFF BTN_IEXC BTN_SKRIV CMB_LEV BTN_LEVH FILL-IN-ENR 
         BTN_UP FILL-IN-BEN-6 FILL-IN-ENR2 FILL-IN-ANTAL BTN_MIN RAD_SOK 
         BTN_AVB-6 FILL-IN-SOKALT 
      WITH FRAME FRAME-MTRL IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MTRL}
  DISPLAY CMB_JURP-10 CMB_AKTIV-10 CMB_SEMFOR-10 CMB_AVD-10 CMB_VECKO-10 
          CMB_DELTID-10 CMB_OMR-10 CMB_BEFATTNING-10 CMB_FLEX-10 CMB_TIDSGODK-10 
          CMB_TRA-10 FILL-IN_FORNAMN-10 CMB_BER-10 FILL-IN_EFTERNAMN-10 
          CMB_ANST-10 FILL-IN_SFORNAMN-10 FILL-IN_SPERSONALKOD-10 
          FILL-IN_SEFTERNAMN-10 FILL-IN_EPERSONALKOD-10 FILL-IN-PERSONAL 
      WITH FRAME FRAME-PERS IN WINDOW C-Win.
  ENABLE IMAGE-11 RECT-24 RECT-25 CMB_JURP-10 CMB_AKTIV-10 CMB_SEMFOR-10 
         CMB_AVD-10 CMB_VECKO-10 CMB_DELTID-10 CMB_OMR-10 CMB_BEFATTNING-10 
         CMB_FLEX-10 CMB_TIDSGODK-10 CMB_TRA-10 FILL-IN_FORNAMN-10 CMB_BER-10 
         FILL-IN_EFTERNAMN-10 CMB_ANST-10 BTN_HAMT-10 BTN_UPP-10 BRW_PERS-10 
         BRW_VPERS BTN_AOF-10 BTN_SCH BTN_HAOF-10 BTN_ARB-10 BTN_ALLOVER-10 
         BTN_VISA-10 BTN_DEBPR BTN_OVER-10 BTN_VISAP BTN_SEK-10 BTN_BACK-10 
         BTN_OT BTN_ALLBACK-10 BTN_TELEFONLISTA BTN_NY-10 BTN_BORT-10 
         FILL-IN_SFORNAMN-10 FILL-IN_SPERSONALKOD-10 FILL-IN_SEFTERNAMN-10 
         FILL-IN_EPERSONALKOD-10 BTN_AVB-10 FILL-IN-PERSONAL 
      WITH FRAME FRAME-PERS IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PERS}
  DISPLAY TOG_FASTA-13 TOG_AVSLUTADE-13 FILL-IN-MELL-13 FILL-IN-AVSTARTD-13 
          FILL-IN-OCH-13 FILL-IN-AVSLUTD-13 TOG_TILLF-13 TOG_PAGA-13 CMB_OMR-13 
          RAD_PERIOD-13 CMB_BESORG-13 FILL-IN-K1-13 CMB_ARTAL-13 FILL-IN-K2-13 
          CMB_ANSV-13 CMB_FRAN-13 CMB_TILL-13 FILL-IN_SPLANNR-13 FILL-IN_ORT-13 
          FILL-IN_EPLANNR-13 FILL-IN_ARTAL-13 FILL-IN_PLANNRVAL FILL-IN-KTO-13 
          FILL-IN-VAL 
      WITH FRAME FRAME-PLAN IN WINDOW C-Win.
  ENABLE IMAGE-12 RECT-27 RECT-28 BTN_NVE-133 BTN_NVE-134 TOG_FASTA-13 
         TOG_AVSLUTADE-13 FILL-IN-MELL-13 FILL-IN-AVSTARTD-13 FILL-IN-OCH-13 
         FILL-IN-AVSLUTD-13 BTN_FVE-133 BTN_FVE-134 TOG_TILLF-13 TOG_PAGA-13 
         CMB_OMR-13 RAD_PERIOD-13 CMB_BESORG-13 FILL-IN-K1-13 CMB_ARTAL-13 
         FILL-IN-K2-13 CMB_ANSV-13 CMB_FRAN-13 CMB_TILL-13 CMB_ARBART-13 
         BTN_HAMT-6 BRW_PLAN BRW_VPLAN BTN_UPP-6 BTN_HAOF-13 BTN_ALLOVER-13 
         BTN_PTIDPLAN BTN_UNDER-2 BTN_OVER-13 BTN_BUNDER BTN_BACK-13 BTN_BUD 
         BTN_KALK-4 BTN_ALLBACK-13 BTN_RAPP-2 BTN_AVSAONR-2 BTN_VISAO-3 
         BTN_AOF-13 BTN_NY-13 BTN_BORT-13 FILL-IN_SPLANNR-13 FILL-IN_ORT-13 
         FILL-IN_EPLANNR-13 FILL-IN_ARTAL-13 BTN_AVB-13 
      WITH FRAME FRAME-PLAN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PLAN}
  DISPLAY FILL-IN_TEXT 
      WITH FRAME FRAME-REG IN WINDOW C-Win.
  ENABLE BRW_REG BTN_KOR BTN_AVB-12 FILL-IN_TEXT 
      WITH FRAME FRAME-REG IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-REG}
  DISPLAY RAD_VAL 
      WITH FRAME FRAME-SEK IN WINDOW C-Win.
  ENABLE MBTN_ANV MBTN_BEH MBTN_OVR RECT-MENY-2 RAD_VAL BTN_VIALLA BTN_SKR-2 
         BTN_VPERS BTN_SKRIV-2 BTN_SKRTEST BTN_AVB-11 
      WITH FRAME FRAME-SEK IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-SEK}
  DISPLAY EDD_FUNK ED_WWW FILL-IN-GURU 
      WITH FRAME FRAME-START IN WINDOW C-Win.
  ENABLE IMAGE-5 EDD_FUNK BTN_MEDD BTN_BYT BTN_BYTW BTN_UPPDAT BTN_AVBGURU 
         ED_WWW 
      WITH FRAME FRAME-START IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-START}
  ENABLE BRW_AOTID BTN_ATER 
      WITH FRAME FRAME-STATUS IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-STATUS}
  DISPLAY RAD_VAL-14 CMB_ANL-14 CMB_FOR-14 CMB_SYS-14 CMB_OMR-14 CMB_AR-14 
          CMB_SYS2-14 CMB_BEL-14 CMB_FEL-14 FILL-IN-BEN-14 FILL-IN-STOR-14 
          FILL-IN-BEN-142 FILL-IN-STRNR-14 FILL-IN-AOTEXT-14 
      WITH FRAME FRAME-STOR IN WINDOW C-Win.
  ENABLE IMAGE-13 RECT-29 RECT-33 RAD_VAL-14 CMB_ANL-14 CMB_FOR-14 CMB_SYS-14 
         CMB_OMR-14 CMB_AR-14 CMB_SYS2-14 CMB_BEL-14 CMB_FEL-14 BTN_HAMT-14 
         BRW_URSTR BRW_VSTR BTN_AOF-14 BTN_HAOF-14 BTN_ALLOVER-14 BTN_OVER-14 
         BTN_AND-14 BTN_BACK-14 BTN_RAPP-14 BTN_ALLBACK-14 BTN_ADM-14 
         BTN_LAS-14 BTN_VIS-14 BTN_NY-14 BTN_BORT-14 FILL-IN-BEN-14 
         FILL-IN-STOR-14 FILL-IN-BEN-142 FILL-IN-STRNR-14 BTN_AVB-14 
      WITH FRAME FRAME-STOR IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-STOR}
  DISPLAY FILL-IN_DATUM RAD_ALLVAL CMB_OMR-8 
      WITH FRAME FRAME-TID IN WINDOW C-Win.
  ENABLE MBTN_TILLAGG MBTN_TIDSEDEL MBTN_TJANSTERESA MBTN_TIDADM MBTN_FLEXADM 
         MBTN_STANSA BTN_PALLBACK BTN_PALLOVER IMAGE-14 RECT-S BTN_PBACK 
         RECT-MENY-3 BTN_TID BTN_TBRES BTN_UTRES CMB_TARST CMB_TMANADST 
         CMB_TARSL CMB_TMANADSL RAD_TIDSVAL BTN_REG BTN_ARES BTN_NDA BTN_LON 
         CMB_AR CMB_MANAD FILL-IN_DATUM TOG_MAN CMB_TAR CMB_TMANAD BTN_FDA 
         BTN_BER-8 BTN_GOD RAD_ALLVAL BTN_GRAN BTN_TRA BTN_KONT BRW_AVD 
         BRW_VAVD BTN_OVERT CMB_OMR-8 BTN_VISA-8 BRW_PERS BRW_ANSV BRW_GODK 
         BRW_MARK BTN_ARB BTN_VOVER BTN_ANDGOD BTN_VTIDV BTN_HAOF-8 BTN_LGOD 
         BTN_SKR BTN_VBACK BTN_VECK BTN_LONA BTN_VTID BTN_AOF-8 BTN_ANDT 
         BRW_OMR BRW_VOMR BTN_PERIOD BTN_FRAMAN BTN_ALLOVER-8 BTN_FRAPP 
         BTN_AVVIK BTN_VVOVER-8 BTN_ANOV BTN_BACK-8 BTN_SALDO BTN_MANSAL 
         BTN_ALLBACK-8 BTN_KONTROLL BTN_FLKORN BTN_SALMAN 
         FILL-IN_SPERSONALKOD-8 FILL-IN_SFORNAMN-8 FILL-IN_SEFTERNAMN-8 
         FILL-IN-TDAG-2 FILL-IN-TDATUM-2 FILL-IN_EPERSONALKOD-8 FILL-IN-TDAG 
         BTN_PVVOVER BTN_AVB-8 
      WITH FRAME FRAME-TID IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TID}
  ENABLE BRW_UPP BTN_VISA-9 BTN_AVB-9 
      WITH FRAME FRAME-UPP IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-UPP}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE feldum_UI C-Win 
PROCEDURE feldum_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fileinfo_UI C-Win 
PROCEDURE fileinfo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*{filinfo.i}*/
   DEFINE VARIABLE datoranv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempfilinfo AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vc AS CHARACTER FORMAT "x(20)". 
   DEFINE VARIABLE vi AS INTEGER NO-UNDO.
   vc = FILL( ' ', 256 ).
   IF SEARCH("versioninfo.dll") NE ? THEN RUN getFullVersion IN btnguruh ( OUTPUT vc, INPUT 256, OUTPUT vi ). 
   ELSE vc = PROVERSION.
   RUN INLOAPI.P (OUTPUT datoranv).
   IF INDEX(PROGRAM-NAME(1),"_UI") > 0 THEN DO:    
      FILE-INFO:FILE-NAME = TRIM(SUBSTRING(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"_UI") + 3)).
   END.
   IF FILE-INFO:FILE-SIZE = ? THEN DO:
      FILE-INFO:FILE-NAME = REPLACE(FILE-INFO:FILE-NAME,".w",".r").   
   END.
   {FILINFO2.I}      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frame_UI C-Win 
PROCEDURE frame_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS CHARACTER NO-UNDO.  
   IF vilkenframe = "START" AND FRAME FRAME-START:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "AONR" AND FRAME FRAME-AONR:HIDDEN = FALSE THEN RETURN. 
   IF vilkenframe = "STATUS" AND FRAME FRAME-STATUS:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "BER" AND FRAME FRAME-BER:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "FAKT" AND FRAME FRAME-FAKT:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "KALK" AND FRAME FRAME-KALK:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "MARK" AND FRAME FRAME-MARK:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "MTRL" AND FRAME FRAME-MTRL:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "DEPA" AND FRAME FRAME-DEPA:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "TID" AND FRAME FRAME-TID:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "UPP" AND FRAME FRAME-UPP:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "PERS" AND FRAME FRAME-PERS:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "SEK" AND FRAME FRAME-SEK:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "REG" AND FRAME FRAME-REG:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "PLAN" AND FRAME FRAME-PLAN:HIDDEN = FALSE THEN RETURN.
   IF vilkenframe = "STOR" AND FRAME FRAME-STOR:HIDDEN = FALSE THEN RETURN.
   ASSIGN
   FRAME FRAME-START:HIDDEN = TRUE 
   FRAME FRAME-AONR:HIDDEN = TRUE  
   FRAME FRAME-BER:HIDDEN = TRUE   
   FRAME FRAME-STATUS:HIDDEN = TRUE
   FRAME FRAME-FAKT:HIDDEN = TRUE  
   FRAME FRAME-KALK:HIDDEN = TRUE  
   FRAME FRAME-MARK:HIDDEN = TRUE  
   FRAME FRAME-MTRL:HIDDEN = TRUE  
   FRAME FRAME-DEPA:HIDDEN = TRUE  
   FRAME FRAME-TID:HIDDEN = TRUE   
   FRAME FRAME-UPP:HIDDEN = TRUE   
   FRAME FRAME-PERS:HIDDEN = TRUE  
   FRAME FRAME-SEK:HIDDEN = TRUE   
   FRAME FRAME-REG:HIDDEN = TRUE   
   FRAME FRAME-PLAN:HIDDEN = TRUE  
   FRAME FRAME-STOR:HIDDEN = TRUE.
   IF vilkenframe = "START" THEN FRAME FRAME-START:HIDDEN = FALSE.
   IF vilkenframe = "AONR" THEN FRAME FRAME-AONR:HIDDEN = FALSE. 
   IF vilkenframe = "STATUS" THEN FRAME FRAME-STATUS:HIDDEN = FALSE.
   IF vilkenframe = "BER" THEN FRAME FRAME-BER:HIDDEN = FALSE.
   IF vilkenframe = "FAKT" THEN FRAME FRAME-FAKT:HIDDEN = FALSE.
   IF vilkenframe = "KALK" THEN FRAME FRAME-KALK:HIDDEN = FALSE.
   IF vilkenframe = "MARK" THEN FRAME FRAME-MARK:HIDDEN = FALSE.
   IF vilkenframe = "MTRL" THEN FRAME FRAME-MTRL:HIDDEN = FALSE.
   IF vilkenframe = "DEPA" THEN FRAME FRAME-DEPA:HIDDEN = FALSE.
   IF vilkenframe = "TID" THEN FRAME FRAME-TID:HIDDEN = FALSE.
   IF vilkenframe = "UPP" THEN FRAME FRAME-UPP:HIDDEN = FALSE.
   IF vilkenframe = "PERS" THEN FRAME FRAME-PERS:HIDDEN = FALSE.
   IF vilkenframe = "SEK" THEN FRAME FRAME-SEK:HIDDEN = FALSE.
   IF vilkenframe = "REG" THEN FRAME FRAME-REG:HIDDEN = FALSE.
   IF vilkenframe = "PLAN" THEN FRAME FRAME-PLAN:HIDDEN = FALSE.
   IF vilkenframe = "STOR" THEN FRAME FRAME-STOR:HIDDEN = FALSE.
   FIND FIRST titletemp WHERE titletemp.VART = vilkenframe NO-LOCK NO-ERROR.
   IF NOT AVAILABLE titletemp THEN DO:
      CREATE titletemp.
      ASSIGN
      titletemp.VART = vilkenframe
      titletemp.VTITLE = C-Win:TITLE.
   END.
   C-Win:TITLE = titletemp.VTITLE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER gomvar AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER vad AS CHARACTER NO-UNDO.
   IF gomvar = TRUE THEN DO:
      BTN_GURU:HIDDEN IN FRAME AMULTISTART-FRAME = TRUE.
      RUN gomvar_UI IN btnguruh.
      BTN_AVB-2:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-BER NO-ERROR. 
      BTN_AVB-3:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-KALK NO-ERROR. 
      BTN_AVB-4:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-FAKT NO-ERROR. 
      BTN_AVB-5:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-MARK NO-ERROR. 
      
   END.
   ELSE DO:
      /*
      BTN_AVB-2:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-BER. 
      BTN_AVB-3:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-KALK. 
      BTN_AVB-4:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-FAKT. 
      BTN_AVB-5:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-MARK. 
      */
      RUN frame_UI (INPUT vad).
      RUN sekbtn_UI IN btnguruh.
      BTN_GURU:HIDDEN IN FRAME AMULTISTART-FRAME = FALSE.
   END.    
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hoj_UI C-Win 
PROCEDURE hoj_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER varifran AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER brworgwith   AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER brworghight  AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER brworgrow    AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER brworgcol    AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER btnorgrow    AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER btnorgcol    AS DECIMAL NO-UNDO.   
   IF varifran = "BER" THEN DO:
      ASSIGN
      brworgwith = BRW_VBER:WIDTH-CHARS IN FRAME FRAME-BER
      brworghight = BRW_VBER:HEIGHT-CHARS
      brworgrow = BRW_VBER:ROW
      brworgcol = BRW_VBER:COLUMN
      btnorgrow = BTN_NY-2:ROW IN FRAME FRAME-BER
      btnorgcol = BTN_NY-2:COL.   
      RETURN.
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gomfalt_UI C-Win 
PROCEDURE gomfalt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER prog AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF prog = "KALK" THEN DO:
      IF vad = 2 THEN DO:
         ASSIGN
         valdfasttemp.AONR:VISIBLE IN BROWSE BRW_VKALK = TRUE 
         valdfasttemp.DELNR:VISIBLE IN BROWSE BRW_VKALK = TRUE
         valdfasttemp.PLANNR:VISIBLE IN BROWSE BRW_VKALK = FALSE
         valdfasttemp.ARTAL:VISIBLE IN BROWSE BRW_VKALK = FALSE.
      END.
      ELSE IF vad = 3 THEN DO:
         ASSIGN
         valdfasttemp.PLANNR:VISIBLE IN BROWSE BRW_VKALK = TRUE
         valdfasttemp.ARTAL:VISIBLE IN BROWSE BRW_VKALK = TRUE
         valdfasttemp.AONR:VISIBLE IN BROWSE BRW_VKALK = FALSE
         valdfasttemp.DELNR:VISIBLE IN BROWSE BRW_VKALK = FALSE.        
      END.
      ELSE DO:
         ASSIGN
         valdfasttemp.PLANNR:VISIBLE IN BROWSE BRW_VKALK = TRUE
         valdfasttemp.ARTAL:VISIBLE IN BROWSE BRW_VKALK = TRUE
         valdfasttemp.AONR:VISIBLE IN BROWSE BRW_VKALK = TRUE
         valdfasttemp.DELNR:VISIBLE IN BROWSE BRW_VKALK = TRUE.        
      END.
             
   END.
   IF prog = "FAKT" THEN DO:
      ASSIGN
      faktplantemp.VIBESTID:LABEL IN BROWSE BRW_UFAKT = Guru.Konstanter:gbestk  
      vfaktplantemp.VIBESTID:LABEL IN BROWSE BRW_VFAKT = Guru.Konstanter:gbestk.       
   END.
   IF prog = "PERS" THEN DO:
      IF vad = 1 THEN DO:
         ASSIGN
         pmpersonaltemp.AKTIV:VISIBLE IN BROWSE BRW_PERS-10 = FALSE.
      END.
   END.
   IF prog = "PERS" THEN DO:
      IF vad = 1 THEN DO:
         ASSIGN
         plannrtemp.AONR:LABEL IN BROWSE BRW_PLAN = Guru.Konstanter:gaok
         plannrtemp.OMRADE:LABEL IN BROWSE BRW_PLAN = Guru.Konstanter:gomrk
         plannrtemp.PLANNR:LABEL IN BROWSE BRW_PLAN = Guru.Konstanter:gplk
         valplantemp.AONR:LABEL IN BROWSE BRW_VPLAN = Guru.Konstanter:gaok
         valplantemp.OMRADE:LABEL IN BROWSE BRW_VPLAN = Guru.Konstanter:gomrk
         valplantemp.PLANNR:LABEL IN BROWSE BRW_VPLAN = Guru.Konstanter:gplk.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud1_UI C-Win 
PROCEDURE huvud1_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = ED_WWW:LOAD-MOUSE-POINTER("GLOVE":U) IN FRAME FRAME-START.
   ASSIGN
   ED_WWW:FGCOLOR = 1
   ED_WWW:FONT = 6
   Guru.GlobalaVariabler:retvalkoll =  TRUE    
   vartpro = "".
   IF Guru.Konstanter:globforetag = "SWEC" OR
      Guru.Konstanter:globforetag = "BIRK" OR 
      Guru.Konstanter:globforetag = "TREC" OR
      Guru.Konstanter:globforetag = "ELCO" OR
      Guru.Konstanter:globforetag = "STRA" OR 
      Guru.Konstanter:globforetag = "REJL" OR
      Guru.Konstanter:globforetag = "PINN"  /*USB PINNE*/ THEN DO:
      IF TODAY >= 09/01/2007 THEN DO:
         MESSAGE "Vill du använda Guru kontakta Elpool 090-184540"
         VIEW-AS ALERT-BOX.
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
         QUIT.
      END.   
   END.
   
   IF foretemp.VERSION = " " THEN DO TRANSACTION:
      FIND CURRENT foretemp EXCLUSIVE-LOCK NO-ERROR.
      foretemp.VERSION = "GURU !".
   END.
   IF Guru.Konstanter:appfel = FALSE THEN DO: 
      RUN INLOAPI.P (OUTPUT anvdator).
      ASSIGN
      globanvnt = TRIM(SUBSTRING(anvdator,1,20))
      Guru.Konstanter:globanv = TRIM(SUBSTRING(anvdator,1,20)) 
      datornamn = TRIM(SUBSTRING(anvdator,25,20)).
      IF globanvbyt NE "" THEN Guru.Konstanter:globanv = Guru.Konstanter:globanvbyt.
      IF Guru.Konstanter:globforetag = "ELPA" AND globanvbyt = "" THEN DO:
             
         ASSIGN
         globanvbyt = ""
         Guru.Konstanter:globanv = "ELPAO"
         globlos = "KAGGEN".         
          /*
         ASSIGN
         globanvbyt = ""
         Guru.Konstanter:globanv = "demok"
         globlos = "demok".         
         */
         RUN storh_UI.
      END.
      ELSE DO:
         globanvbyt = "".
         /*AUTOLOGIN*/         
         IF Guru.Konstanter:appcon THEN DO:
            RUN LOSENKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT 2,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
             OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
             OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).
         END.
         ELSE DO:
            RUN LOSENKOLL.P 
            (INPUT 2,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
             OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
             OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).
         END.
         RUN storh_UI.
         /*EJ AUTOINLOGG*/
         IF Guru.Konstanter:globforetag = "BIRK" THEN musz = TRUE.
         IF musz = TRUE THEN DO:
            Guru.Konstanter:globanv = "".
            musz = FALSE.
           /* {&WINDOW-NAME}:HIDDEN = FALSE.                       */
            RUN GURU.W.  
           /* {&WINDOW-NAME}:HIDDEN = TRUE.       */                
            IF musz = TRUE THEN musz = musz.
            ELSE DO:
               IF Guru.Konstanter:appcon THEN DO:
                  IF Guru.Konstanter:globanv = "elpao" THEN Guru.Konstanter:globanv = Guru.Konstanter:globanv.
                  ELSE DO:
                     /*VISAR NT-LOGG OCH GURULOGG*/
                     RUN INLFEL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
                     (INPUT Guru.Konstanter:globanvnt,INPUT Guru.Konstanter:globanv).
                  END.                  
               END.
            END.            
         END.         
         IF musz = TRUE THEN DO:
            IF Guru.Konstanter:globanv = "" OR Guru.Konstanter:globanv = "DEMO" THEN DO:
               RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
               QUIT.
            END.
            RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
            APPLY "CLOSE" TO THIS-PROCEDURE. 
            RETURN.    
         END.       
      END.      
   END.
   ELSE DO:
      Guru.Konstanter:appfel = FALSE.
      Guru.Konstanter:globanv = "FLEX".
      globlos = "FLEX".
   END.        
   IF Guru.Konstanter:globanv = "ALLMÄN" THEN globallm = TRUE.
   ELSE IF Guru.Konstanter:globanv = "ALLMÄN2" THEN globallm = TRUE.
   ELSE globallm = FALSE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN LOSENKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 1,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,
       OUTPUT globallao,
       OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
       OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).    
   END.
   ELSE DO:
      RUN LOSENKOLL.P 
      (INPUT 1,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
       OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
       OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).
   END.
   RUN storh_UI.
   IF Guru.Konstanter:globanv = "demok" THEN DO:
      demokvar = TRUE.
      IF TODAY >= 05/15/2009 THEN DO:
         MESSAGE "Vill du använda Guru kontakta Elpool 090-184540"
         VIEW-AS ALERT-BOX.
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
         QUIT.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud2_UI C-Win 
PROCEDURE huvud2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*RÄKNAR ANTAL ANVÄNDARE in*/   
   IF Guru.Konstanter:appcon THEN DO:
      anvdator = "".
      ASSIGN
      SUBSTRING(anvdator,1,20) = Guru.Konstanter:globanv
      SUBSTRING(anvdator,25,20) = datornamn
      SUBSTRING(anvdator,50,20) = Guru.Konstanter:globanvnt
      SUBSTRING(anvdator,75,20) = SESSION:CLIENT-TYPE.
      RUN INLOGRAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT anvdator,INPUT TRUE).
   END.        
   IF PROGRESS = "FULL" AND Guru.Konstanter:globanv = "ELPAO" THEN DO:
      SESSION:DEBUG-ALERT = YES.
   END.
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AONR:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_BERM:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_DEPA:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_FAKTM:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_FLEX:HANDLE IN FRAME AMULTISTART-FRAME).              
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_GURU:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_KALKM:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_MARKM:HANDLE IN FRAME AMULTISTART-FRAME). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_MTRLM:HANDLE IN FRAME AMULTISTART-FRAME). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_PERS:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_PLAN:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_REGM:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_SEKM:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_SMS:HANDLE IN FRAME AMULTISTART-FRAME).              
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_STOR:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_TIDM:HANDLE IN FRAME AMULTISTART-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_UPPF:HANDLE IN FRAME AMULTISTART-FRAME). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_AVBGURU:HANDLE IN FRAME FRAME-START). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_ATER:HANDLE IN FRAME FRAME-STATUS). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, BTN_STATUS:HANDLE IN FRAME FRAME-AONR). 
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, IMAGE-4:HANDLE IN FRAME AMULTISTART-FRAME). 
   RUN MULTITRIGG.P PERSISTENT SET multitriggh (INPUT THIS-PROCEDURE ,INPUT TABLE whandltemp).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud3_UI C-Win 
PROCEDURE huvud3_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN sek_UI.
   IF varforetypval[17] >= 1 THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 88.
      {SOKANROP.I}
      IF soktemp.SOKLOG[1] = TRUE THEN varforetypval[17] = 0.
      ELSE DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 89.
         soktemp.SOKCHAR[1] = datornamn.
         {SOKANROP.I}
         FIND FIRST soktemp WHERE soktemp.SOKCHAR[2] = "1-5" NO-LOCK NO-ERROR.
         IF AVAILABLE soktemp THEN DO:
            ASSIGN
            gastsek[1] = soktemp.SOKLOG[1]
            gastsek[2] = soktemp.SOKLOG[2]
            gastsek[3] = soktemp.SOKLOG[3]
            gastsek[4] = soktemp.SOKLOG[4]
            gastsek[5] = soktemp.SOKLOG[5].
         END.
         FIND FIRST soktemp WHERE soktemp.SOKCHAR[2] = "6-10" NO-LOCK NO-ERROR.
         IF AVAILABLE soktemp THEN DO:
            ASSIGN
            gastsek[6] = soktemp.SOKLOG[1]
            gastsek[7] = soktemp.SOKLOG[2]
            gastsek[8] = soktemp.SOKLOG[3]
            gastsek[9] = soktemp.SOKLOG[4]
            gastsek[10] = soktemp.SOKLOG[5].
         END.
         FIND FIRST soktemp WHERE soktemp.SOKCHAR[2] = "11-15" NO-LOCK NO-ERROR.
         IF AVAILABLE soktemp THEN DO:
            ASSIGN
            gastsek[11] = soktemp.SOKLOG[1]
            gastsek[12] = soktemp.SOKLOG[2]
            gastsek[13] = soktemp.SOKLOG[3]
            gastsek[14] = soktemp.SOKLOG[4]
            gastsek[15] = soktemp.SOKLOG[5].
         END.
      END.
   END.      
   IF varforetypval[17] = 0 THEN EDD_FUNK:HIDDEN IN FRAME FRAME-START = TRUE.
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN RUN eddstart_UI.
   ELSE RUN eddstart_UI.
      
   ASSIGN
   {&WINDOW-NAME}:MIN-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS 
   {&WINDOW-NAME}:MIN-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
   {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = globstorh
   {&WINDOW-NAME}:MAX-WIDTH-PIXELS = globstorb
   ph_frame = FRAME {&FRAME-NAME}:HANDLE.
   ON MOUSE-MENU-DBLCLICK OF CURRENT-WINDOW PERSISTENT RUN fileinfo_UI IN THIS-PROCEDURE.
   ON MOUSE-MENU-DBLCLICK OF ph_frame PERSISTENT RUN fileinfo_UI IN THIS-PROCEDURE.
   IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
   ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   /*updateringsknapp*/ 
   IF bloblog = TRUE THEN DO:
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN BTN_UPPDAT:HIDDEN = TRUE.
      ELSE IF varforetypval[14] = 1 THEN BTN_UPPDAT:HIDDEN = FALSE.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN BTN_UPPDAT:HIDDEN = FALSE.
      ELSE IF Guru.SharedVariable:singel = TRUE THEN BTN_UPPDAT:HIDDEN = FALSE.
      ELSE IF varforetypval[19] = 1 THEN BTN_UPPDAT:HIDDEN = FALSE.
      ELSE BTN_UPPDAT:HIDDEN = TRUE.
   END.
   ELSE BTN_UPPDAT:HIDDEN = TRUE.
   IF Guru.Konstanter:globanv = "elpao" THEN BTN_UPPDAT:HIDDEN = FALSE.
   IF Guru.SharedVariable:singel = TRUE THEN BTN_UPPDAT:HIDDEN = FALSE.
   IF Guru.Konstanter:globanv = "demok" THEN BTN_UPPDAT:HIDDEN = TRUE.
   /*vattenfalls citrix*/
   IF datornamn = "thnmf" THEN BTN_UPPDAT:HIDDEN = TRUE.
   IF datornamn = "thnblade01" THEN BTN_UPPDAT:HIDDEN = TRUE.  
   IF datornamn = "TSBLRA02" THEN BTN_UPPDAT:HIDDEN = TRUE.
   IF varforetypval[20] = 0 THEN DO:
      BTN_MEDD:LABEL = "Skapa meddelande~ntill Guruanvändare". 
      RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME FRAME-START,-16,1409294336,OUTPUT ReturnValue).
      RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME FRAME-START,244,1409294336,1,OUTPUT ReturnValue).
   END.
   IF musz = TRUE THEN RETURN.  
   C-WIN:TITLE = foretemp.ATRHOME + " - " + "Systemsupport 090/184540".   
   IF Guru.Konstanter:globanv = "FLEX"  THEN DO:
     vartpro = "FLX".
     APPLY "CHOOSE" TO BTN_FLEX IN FRAME AMULTISTART-FRAME. 
   END.
   IF Guru.Konstanter:globanv = "ELPAO" THEN DO:
      foretemp.GRAFTECK = TRUE.
   END.
   ELSE foretemp.GRAFTECK = FALSE.        
   IF Guru.Konstanter:globanv = "FLEX" THEN musz = musz.      
   ELSE DO:
      RUN VISMEDDU.W.      
   END.
   RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW). 
   IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
   END.
  /* {AVBFRAM.I}*/
   {musarrow.i}      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insatt_UI C-Win 
PROCEDURE insatt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN insatt_UI IN btnguruh.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kolltt_UI C-Win 
PROCEDURE kolltt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   BTN_AVB-2:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-BER. 
   BTN_AVB-3:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-KALK. 
   BTN_AVB-4:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-FAKT. 
   BTN_AVB-5:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-MARK. 
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER koll AS LOGICAL NO-UNDO.
   IF vad = 1 THEN DO:
      FIND FIRST valberedningtemp NO-LOCK NO-ERROR.
      IF AVAILABLE valberedningtemp THEN DO:
         koll = TRUE.
         RETURN.                                    
      END.
      IF VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
         IF NOT AVAILABLE valdaaotemp THEN DO: 
            koll = TRUE.         
            RETURN.
         END.
         ELSE RUN btnber_UI IN Guru.SharedVariable:btnaonrh.
         /*
         BTN_AVB-2:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-BER NO-ERROR.
         */
         
      END.
      ELSE koll = TRUE.
   END.
   IF vad = 2 THEN DO:
      FIND FIRST valdfasttemp NO-LOCK NO-ERROR.
      IF AVAILABLE valdfasttemp THEN DO:
         koll = TRUE.
         RETURN.
      END.      
      IF FRAME FRAME-AONR:HIDDEN = FALSE THEN DO:
         IF VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
            /*
            BTN_AVB-3:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-KALK NO-ERROR.
            */
            IF NOT AVAILABLE valdaaotemp THEN DO: 
               koll = TRUE.         
               RETURN.
            END.
            ELSE RUN btnkalk_UI IN Guru.SharedVariable:btnaonrh.      
         END.
         ELSE koll = TRUE.     
      END.
      ELSE IF FRAME FRAME-PLAN:HIDDEN = FALSE THEN DO:
         IF VALID-HANDLE(Guru.SharedVariable:btnplanh) THEN DO:
            /*
            BTN_AVB-3:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-KALK NO-ERROR.
            */
            IF NOT AVAILABLE valplantemp THEN DO: 
               koll = TRUE.         
               RETURN.
            END.
            ELSE RUN btnkalk_UI IN Guru.SharedVariable:btnplanh.      
         END.
         ELSE koll = TRUE.     
      END.
      ELSE koll = TRUE.     
   END.
   IF vad = 3 THEN DO:
      FIND FIRST mvalvardtemp NO-LOCK NO-ERROR.
      IF AVAILABLE mvalvardtemp THEN DO:
         koll = TRUE.
         RETURN.
      END.
      IF VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
         /*
         BTN_AVB-5:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-FAKT NO-ERROR.
         */
         IF NOT AVAILABLE valdaaotemp THEN DO: 
            koll = TRUE.         
            RETURN.
         END.
         ELSE RUN btnmark_UI IN Guru.SharedVariable:btnaonrh.      
      END.
      ELSE koll = TRUE.
   END.
   IF vad = 4 THEN DO:
      FIND FIRST vfaktplantemp NO-LOCK NO-ERROR.
      IF AVAILABLE vfaktplantemp THEN DO:
         koll = TRUE.
         RETURN.
      END.
      IF VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
         /*
         BTN_AVB-4:LOAD-IMAGE ("BILDER\xbtn_ater.gif") IN FRAME FRAME-FAKT NO-ERROR.
         */
         IF NOT AVAILABLE valdaaotemp THEN DO: 
            koll = TRUE.         
            RETURN.
         END.
         ELSE RUN btnfakt_UI IN Guru.SharedVariable:btnaonrh.      
      END.
      ELSE koll = TRUE.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE manadsl_UI C-Win 
PROCEDURE manadsl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER cmbmh  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER cmbarh AS HANDLE NO-UNDO.
   
   ASSIGN
   CMB_TMANAD = cmbmh:SCREEN-VALUE
   CMB_TAR =   INTEGER(cmbarh:SCREEN-VALUE).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextguru_UI C-Win 
PROCEDURE nextguru_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST xsektemp WHERE xsektemp.MENYVART = xhop AND
   xsektemp.AV-LEVEL = Guru.Konstanter:globniv NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI C-Win 
PROCEDURE rowdispmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.   
   IF vad = 1 THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
         IF mtrltemp.KUND = TRUE THEN DO: 
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[28].
            /*nettopris gul fortum turkos övriga*/
            /*
            IF varforetypval[28] = 1  THEN mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = 14.
            ELSE mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = 11.
            */
         END.
      END.
   END.
   IF vad = 2 THEN DO:
      IF AVAILABLE mspec_mtrlextra THEN DO:
         FIND FIRST mkmtrltemp WHERE mkmtrltemp.ENR = mspec_mtrlextra.ENR AND mkmtrltemp.LEVKOD = mspec_mtrlextra.LEVKOD AND mkmtrltemp.KALKNR = 0
         AND mkmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE mkmtrltemp THEN DO:
            mspec_mtrlextra.NPRIS:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[28].
            /*nettopris gul fortum turkos övriga*/
            /*
            IF varforetypval[28] = 1  THEN mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = 14.
            ELSE mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = 11.
            */
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sek_UI C-Win 
PROCEDURE sek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   
   RUN sek_UI IN btnguruh.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE storh_UI C-Win 
PROCEDURE storh_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*
   ASSIGN
   globstorh = 682
   globstorb = 1000.
   
   IF storkollbredd > globstorb THEN globstorb = storkollbredd.
   IF storkollhojd  > globstorh THEN globstorh = storkollhojd.
   ASSIGN
   globstorb = SESSION:WORK-AREA-WIDTH-PIXELS 
   globstorh = SESSION:WORK-AREA-HEIGHT-PIXELS.
   */
   ASSIGN
   globstorb = SESSION:WORK-AREA-WIDTH-PIXELS 
   globstorh = SESSION:WORK-AREA-HEIGHT-PIXELS.

   IF storkollbredd NE 0 THEN DO:
      IF storkollbredd < SESSION:WORK-AREA-WIDTH-PIXELS THEN DO:
         IF storkollbredd > 1000 THEN globstorb = storkollbredd.
      END.
   END.
   IF storkollhojd NE 0 THEN DO:
      IF storkollhojd < SESSION:WORK-AREA-HEIGHT-PIXELS THEN DO:
         IF storkollhojd > 682 THEN globstorh = storkollhojd.
      END.
   END.         
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttcopy_UI C-Win 
PROCEDURE ttcopy_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE-HANDLE tthandle.
   DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.
   ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.  
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(ttbuffh).

   /*
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(tthandle:NAME).
   */
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
   ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.
   /*ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.  
   */
   komcop = "FOR EACH " + ttbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttqh.
   ttqh:SET-BUFFERS(ttbuffh).
   ttqh:QUERY-PREPARE(komcop).
   ttqh:QUERY-OPEN().
   ttqh:GET-FIRST(NO-LOCK).
   DO WHILE ttqh:QUERY-OFF-END = FALSE:
      ttbuffcopyh:BUFFER-CREATE().
      ttbuffcopyh:BUFFER-COPY(ttbuffh).
      ttqh:GET-NEXT(NO-LOCK).
   END.
   ttqh:QUERY-CLOSE.
   DELETE OBJECT ttqh NO-ERROR.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttjmf_UI C-Win 
PROCEDURE ttjmf_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER favbuffh AS HANDLE.
   DEFINE VARIABLE compsave AS LOGICAL NO-UNDO.
   DEFINE VARIABLE komfav AS CHARACTER NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttcopqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttfavqh AS HANDLE NO-UNDO.
   komcop = "FOR EACH " + ttbuffcopyh:TABLE + " NO-LOCK.".
   komfav = "FOR EACH " + favbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttcopqh.
   ttcopqh:SET-BUFFERS(ttbuffcopyh).
   ttcopqh:QUERY-PREPARE(komcop).
   ttcopqh:QUERY-OPEN().
   CREATE QUERY ttfavqh.
   ttfavqh:SET-BUFFERS(favbuffh).
   ttfavqh:QUERY-PREPARE(komfav).
   ttfavqh:QUERY-OPEN().
   ttcopqh:GET-FIRST(NO-LOCK).
   DO WHILE ttcopqh:QUERY-OFF-END = FALSE:
      ttfavqh:GET-FIRST(NO-LOCK).
      compsave = FALSE.
      DO WHILE ttfavqh:QUERY-OFF-END = FALSE:
         IF favbuffh:BUFFER-COMPARE(ttbuffcopyh) THEN DO:
            compsave = TRUE.       
            ttfavqh:GET-LAST(NO-LOCK).
            ttfavqh:GET-NEXT(NO-LOCK).
         END.         
         ttfavqh:GET-NEXT(NO-LOCK).
      END.                  
      IF compsave = TRUE THEN DO:
         ttbuffcopyh:BUFFER-DELETE().  
      END.
      ELSE DO:
         favbuffh:BUFFER-CREATE().
         favbuffh:BUFFER-COPY(ttbuffcopyh).
      END.
      ttcopqh:GET-NEXT(NO-LOCK).
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uttstart_UI C-Win 
PROCEDURE uttstart_UI :
/*------------------------------------------------------------------------------
  Purpose:    körs från palanmeny och aonrmeny för att hålla koll på redan vala beredningar mm 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   
   IF vad = 1 THEN DO:
      EMPTY TEMP-TABLE copyvalberedningtemp NO-ERROR. 
      EMPTY TEMP-TABLE copyvfaktplantemp    NO-ERROR. 
      EMPTY TEMP-TABLE copyvaldfasttemp     NO-ERROR. 
      EMPTY TEMP-TABLE copymvalvardtemp      NO-ERROR. 
      FOR EACH valberedningtemp:
         CREATE copyvalberedningtemp.
         BUFFER-COPY valberedningtemp TO copyvalberedningtemp.         
      END.
      FOR EACH vfaktplantemp:
         CREATE copyvfaktplantemp.
         BUFFER-COPY vfaktplantemp TO copyvfaktplantemp.
      END.
      FOR EACH valdfasttemp:
         CREATE copyvaldfasttemp.
         BUFFER-COPY valdfasttemp TO copyvaldfasttemp.
      END.
      FOR EACH mvalvardtemp:
         CREATE copymvalvardtemp.
         BUFFER-COPY mvalvardtemp TO copymvalvardtemp.
      END.
   END.
   IF vad = 2 THEN DO:
      EMPTY TEMP-TABLE valberedningtemp NO-ERROR. 
      EMPTY TEMP-TABLE vfaktplantemp    NO-ERROR. 
      EMPTY TEMP-TABLE valdfasttemp     NO-ERROR. 
      EMPTY TEMP-TABLE mvalvardtemp      NO-ERROR. 
      FOR EACH valdaaotemp:
         IF valdaaotemp.aonravdatum = 01/01/91 THEN DO:
            FOR EACH copyvalberedningtemp WHERE copyvalberedningtemp.AONR = valdaaotemp.AONR AND copyvalberedningtemp.DELNR = valdaaotemp.DELNR:
               copyvalberedningtemp.AKTIV = TRUE.  
            END.
            FOR EACH copyvaldfasttemp WHERE copyvaldfasttemp.AONR = valdaaotemp.AONR AND copyvaldfasttemp.DELNR = valdaaotemp.DELNR:
               copyvaldfasttemp.AKTIV = TRUE.  
            END.
            FOR EACH copymvalvardtemp WHERE copymvalvardtemp.AONR = valdaaotemp.AONR AND copymvalvardtemp.DELNR = valdaaotemp.DELNR:
               copymvalvardtemp.AKTIV = TRUE.  
            END.
         END.
         ELSE DO:
            FOR EACH copyvalberedningtemp WHERE copyvalberedningtemp.AONR = valdaaotemp.AONR AND copyvalberedningtemp.DELNR = valdaaotemp.DELNR:
               copyvalberedningtemp.AKTIV = FALSE. 
               IF copyvalberedningtemp.BERNR = ? THEN DELETE copyvalberedningtemp.
            END.
            FOR EACH copyvaldfasttemp WHERE copyvaldfasttemp.AONR = valdaaotemp.AONR AND copyvaldfasttemp.DELNR = valdaaotemp.DELNR:
               copyvaldfasttemp.AKTIV = FALSE.  
               IF copyvaldfasttemp.KALKNR = ? THEN DELETE copyvaldfasttemp.
            END.
            FOR EACH copymvalvardtemp WHERE copymvalvardtemp.AONR = valdaaotemp.AONR AND copymvalvardtemp.DELNR = valdaaotemp.DELNR:
               copymvalvardtemp.AKTIV = FALSE.  
               IF copymvalvardtemp.VARDNR = ? THEN DELETE copymvalvardtemp.
            END.
         END.
      END.
      FOR EACH copyvalberedningtemp:
         CREATE valberedningtemp.
         BUFFER-COPY copyvalberedningtemp TO valberedningtemp.        
      END.
      FOR EACH copyvfaktplantemp:
         CREATE vfaktplantemp.
         BUFFER-COPY copyvfaktplantemp TO vfaktplantemp.
      END.
      FOR EACH copyvaldfasttemp:
         CREATE valdfasttemp.
         BUFFER-COPY copyvaldfasttemp TO valdfasttemp.        
      END.
      FOR EACH copymvalvardtemp:
         CREATE mvalvardtemp.
         BUFFER-COPY copymvalvardtemp TO mvalvardtemp.
      END.      
      IF VALID-HANDLE(btnberh) THEN RUN openb_UI IN btnberh.  
      IF VALID-HANDLE(Guru.SharedVariable:btnfakth) THEN RUN openb_UI IN Guru.SharedVariable:btnfakth. 
      IF VALID-HANDLE(Guru.SharedVariable:btnmarkh) THEN RUN openb_UI IN Guru.SharedVariable:btnmarkh. 
      IF VALID-HANDLE(btnkalkh) THEN RUN openb_UI IN btnkalkh. 
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI C-Win 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

