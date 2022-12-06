&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 


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
DEFINE INPUT PARAMETER nyttaovarin AS LOGICAL NO-UNDO. 
DEFINE INPUT PARAMETER kopiavar AS LOGICAL NO-UNDO. 
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER aonrvar2 AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER delnrvar2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER plannrvar2 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar2 AS INTEGER NO-UNDO.



DEFINE NEW SHARED VARIABLE omrpersanvok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nyttaovar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrvar   AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE plannrvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE artalvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE slutaejnu AS LOGICAL NO-UNDO.
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

ASSIGN
plannrvar = plannrvar2
artalvar =  artalvar2 
aonrvar =  aonrvar2 
delnrvar = delnrvar2.
nyttaovar = nyttaovarin.
/* Local Variable Definitions ---                                       */



DEFINE SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", NO ,
"Fasta aonr", YES 
     SIZE 24 BY 1 NO-UNDO.


DEFINE NEW SHARED VARIABLE bortaoapph AS HANDLE NO-UNDO.                     /*BORTAOAPP.P*/
DEFINE NEW SHARED VARIABLE nyttaoapph AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE NEW SHARED VARIABLE nyttaoejapph AS HANDLE NO-UNDO.                              /* dokhant.P */
DEFINE NEW SHARED VARIABLE fbestapph AS HANDLE NO-UNDO.                      /*EXTRATABHMT*/
DEFINE NEW SHARED VARIABLE visaaonrh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE avtaodatapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE anvapph AS HANDLE NO-UNDO.
DEFINE VARIABLE aonrMh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE grundmappvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE omrbildvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE kalknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kalktypvar AS INTEGER NO-UNDO.
DEFINE VARIABLE tidbgamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidagamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidpgamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE valnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrvalvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE aoomradenr AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrserierec AS RECID NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE NEW SHARED VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE mappvarhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE avtrec AS RECID NO-UNDO.
DEFINE VARIABLE sokmapp AS CHARACTER NO-UNDO.
DEFINE VARIABLE varfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE Okvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE openqkoll AS LOGICAL EXTENT 50 NO-UNDO.
DEFINE VARIABLE sldatum AS DATE NO-UNDO.
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
DEFINE VARIABLE projrapp AS CHARACTER FORMAT "X(101)" NO-UNDO.  
DEFINE VARIABLE omradespar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE bestspar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE aonrrecsp AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecsp2 AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecdel AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecdel2 AS RECID NO-UNDO.
DEFINE VARIABLE stat AS INTEGER NO-UNDO.
DEFINE VARIABLE akoden AS INTEGER NO-UNDO.
DEFINE VARIABLE montbervar AS INTEGER NO-UNDO.
DEFINE VARIABLE projektledvar AS INTEGER NO-UNDO.
DEFINE VARIABLE projektorvar AS INTEGER NO-UNDO.
DEFINE VARIABLE dokuvar AS INTEGER NO-UNDO.
DEFINE VARIABLE rowgatill AS ROWID NO-UNDO.
DEFINE VARIABLE editanm AS CHARACTER FORMAT "x(68)" NO-UNDO.
DEFINE VARIABLE flagga AS INTEGER NO-UNDO.
DEFINE VARIABLE aokopprec AS RECID NO-UNDO.
DEFINE VARIABLE aotidrec AS RECID NO-UNDO.
DEFINE VARIABLE tabvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bestvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE radfast AS LOGICAL NO-UNDO.
DEFINE VARIABLE autoregvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_BERKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_KALKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_KOPPKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_TIDLKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE reslog AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ordnvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE idvar AS INTEGER NO-UNDO.

DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE refvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE extraomr AS CHARACTER NO-UNDO EXTENT 2.
DEFINE NEW SHARED VARIABLE sparavdat AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE brwkoll AS INTEGER NO-UNDO EXTENT 10.
DEFINE NEW SHARED VARIABLE gfastkh AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE gtillkh AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE mainvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE arbbspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbfspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE kollref AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE fbaonrvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttsparatao AS LOGICAL NO-UNDO. 
DEFINE VARIABLE gamomrnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE gambestnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE styrserie AS LOGICAL NO-UNDO.
{WHANDLTEMP.I}
{EXTRATAB.I}  

{ALLDEF.I}
{SOKDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{FAKTTYPDEF.I}
{ANSPROJBER.I}
{OMRTEMPW.I}
{HOPPSEK2W.I}
{AVTPLANTEMP.I}
&Scoped-define SHARED SHARED 
{ANVTEMPS.I}   
DEFINE NEW SHARED TEMP-TABLE omranvandartemp NO-UNDO LIKE anvandartemp.
{KTOTEMP.I}
{PERSONALTEMP.I}
{AVDTEMP.I}
{DIRDEF.I}
{AONRDEF.I}
{BESTKUNDALLT.I}
{JURPERST.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{GATILL.I}
{FAKTIN.I}
{KALKIN.I}
{BERIN.I}
{MARKVARDIN.I}
{AUTOMREGTEMP.I}
{AVTAONRTEMP.I}
{AONRTIDPERS.I}
{DAGBOKTEMP.I}
{BERATEMP.I}

DEFINE NEW SHARED TEMP-TABLE aonrdeltemp
   FIELD AONR   AS CHARACTER 
   FIELD DELNR      AS INTEGER
   FIELD OMRADE  AS CHARACTER 
   FIELD BESTID  AS CHARACTER 
   FIELD FASTAAONR AS LOGICAL
   FIELD FASTKALK AS LOGICAL.

DEFINE BUFFER gabuff FOR gatill.
DEFINE BUFFER aonrbuff FOR uppaonrtemp.
DEFINE QUERY abq FOR aonrbuff FIELDS (aonrbuff.FASTAAONR).


RUN pstart_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-ANM
&Scoped-define BROWSE-NAME BRW_ANL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anlaggtemp anvandartemp omranvandartemp ~
aonrkonttemp aotidslagtemp ansvaraotemp arbarttemp avtalaonrtemp beratemp ~
bestkundallt bestkundextra dagboktemp avtaodattemp gatill personaltemp ~
priotemp projtemp aonrtidperstemp

/* Definitions for BROWSE BRW_ANL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANL anlaggtemp.ANLNR ~
anlaggtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANL 
&Scoped-define QUERY-STRING-BRW_ANL FOR EACH anlaggtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANL OPEN QUERY BRW_ANL FOR EACH anlaggtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANL anlaggtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANL anlaggtemp


/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE ~
anvandartemp.AV-NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV 
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH anvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH anvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV anvandartemp


/* Definitions for BROWSE BRW_ANVOMR                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ANVOMR omranvandartemp.ANVANDARE ~
omranvandartemp.AV-NAMN omranvandartemp.PERSONALKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANVOMR 
&Scoped-define QUERY-STRING-BRW_ANVOMR FOR EACH omranvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANVOMR OPEN QUERY BRW_ANVOMR FOR EACH omranvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANVOMR omranvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANVOMR omranvandartemp


/* Definitions for BROWSE BRW_AONRKTO                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_AONRKTO aonrkonttemp.K1 aonrkonttemp.K2 ~
aonrkonttemp.K3 aonrkonttemp.K4 aonrkonttemp.K5 aonrkonttemp.SATS% 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONRKTO 
&Scoped-define QUERY-STRING-BRW_AONRKTO FOR EACH aonrkonttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AONRKTO OPEN QUERY BRW_AONRKTO FOR EACH aonrkonttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AONRKTO aonrkonttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONRKTO aonrkonttemp


/* Definitions for BROWSE BRW_AOTID                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AOTID aotidslagtemp.TIDLAGE ~
aotidslagtemp.AKTIVITET1 aotidslagtemp.DAT1 aotidslagtemp.ANVANDARE1 ~
aotidslagtemp.AKTIVITET2 aotidslagtemp.DAT2 aotidslagtemp.ANVANDARE2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AOTID 
&Scoped-define QUERY-STRING-BRW_AOTID FOR EACH aotidslagtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AOTID OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AOTID aotidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AOTID aotidslagtemp


/* Definitions for BROWSE BRW_ARBAN                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_ARBAN ansvaraotemp.PERSONALKOD ~
ansvaraotemp.FORNAMN ansvaraotemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ARBAN 
&Scoped-define QUERY-STRING-BRW_ARBAN FOR EACH ansvaraotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ARBAN OPEN QUERY BRW_ARBAN FOR EACH ansvaraotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ARBAN ansvaraotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ARBAN ansvaraotemp


/* Definitions for BROWSE BRW_ARBART                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ARBART arbarttemp.ARBARTKOD ~
arbarttemp.ARBBENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ARBART 
&Scoped-define QUERY-STRING-BRW_ARBART FOR EACH arbarttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ARBART OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ARBART arbarttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ARBART arbarttemp


/* Definitions for BROWSE BRW_ARBARTOV                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_ARBARTOV arbarttemp.ARBARTKOD ~
arbarttemp.ARBBENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ARBARTOV 
&Scoped-define QUERY-STRING-BRW_ARBARTOV FOR EACH arbarttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ARBARTOV OPEN QUERY BRW_ARBARTOV FOR EACH arbarttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ARBARTOV arbarttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ARBARTOV arbarttemp


/* Definitions for BROWSE BRW_AVTAL                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AVTAL avtalaonrtemp.AVTAL ~
avtalaonrtemp.BEN avtalaonrtemp.KLR avtalaonrtemp.FIL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVTAL 
&Scoped-define QUERY-STRING-BRW_AVTAL FOR EACH avtalaonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVTAL OPEN QUERY BRW_AVTAL FOR EACH avtalaonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVTAL avtalaonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVTAL avtalaonrtemp


/* Definitions for BROWSE BRW_AVTALDOK                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_AVTALDOK avtalaonrtemp.AVTAL ~
avtalaonrtemp.BEN avtalaonrtemp.FIL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVTALDOK 
&Scoped-define QUERY-STRING-BRW_AVTALDOK FOR EACH avtalaonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVTALDOK OPEN QUERY BRW_AVTALDOK FOR EACH avtalaonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVTALDOK avtalaonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVTALDOK avtalaonrtemp


/* Definitions for BROWSE BRW_BERED                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_BERED beratemp.PERSONALKOD ~
beratemp.FORNAMN beratemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BERED 
&Scoped-define QUERY-STRING-BRW_BERED FOR EACH beratemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BERED OPEN QUERY BRW_BERED FOR EACH beratemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BERED beratemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BERED beratemp


/* Definitions for BROWSE BRW_BEST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BEST bestkundallt.VIBESTID ~
bestkundallt.BESTNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEST 
&Scoped-define QUERY-STRING-BRW_BEST FOR EACH bestkundallt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEST OPEN QUERY BRW_BEST FOR EACH bestkundallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEST bestkundallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEST bestkundallt


/* Definitions for BROWSE BRW_BESTAVD                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_BESTAVD bestkundextra.VIBESTID ~
bestkundextra.BESTNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BESTAVD 
&Scoped-define QUERY-STRING-BRW_BESTAVD FOR EACH bestkundextra NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BESTAVD OPEN QUERY BRW_BESTAVD FOR EACH bestkundextra NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BESTAVD bestkundextra
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BESTAVD bestkundextra


/* Definitions for BROWSE BRW_DAGBOK                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_DAGBOK dagboktemp.DAGBOKAO ~
dagboktemp.KILOMETER dagboktemp.TIMMAR dagboktemp.DATUM ~
dagboktemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DAGBOK 
&Scoped-define QUERY-STRING-BRW_DAGBOK FOR EACH dagboktemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DAGBOK OPEN QUERY BRW_DAGBOK FOR EACH dagboktemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DAGBOK dagboktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DAGBOK dagboktemp


/* Definitions for BROWSE BRW_HAND                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HAND avtaodattemp.DATUM ~
avtaodattemp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HAND 
&Scoped-define QUERY-STRING-BRW_HAND FOR EACH avtaodattemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HAND OPEN QUERY BRW_HAND FOR EACH avtaodattemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HAND avtaodattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HAND avtaodattemp


/* Definitions for BROWSE BRW_KOPP                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KOPP gatill.F1 gatill.F2 gatill.F3 ~
gatill.STATUSNIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOPP gatill.STATUSNIV 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KOPP gatill
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KOPP gatill
&Scoped-define QUERY-STRING-BRW_KOPP FOR EACH gatill NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOPP OPEN QUERY BRW_KOPP FOR EACH gatill NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOPP gatill
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOPP gatill


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.FORNAMN personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for BROWSE BRW_PRIO                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PRIO priotemp.PKOD priotemp.P-BENAMNING ~
priotemp.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRIO 
&Scoped-define QUERY-STRING-BRW_PRIO FOR EACH priotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PRIO OPEN QUERY BRW_PRIO FOR EACH priotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PRIO priotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRIO priotemp


/* Definitions for BROWSE BRW_PRIOOV                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_PRIOOV priotemp.PKOD ~
priotemp.P-BENAMNING priotemp.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRIOOV 
&Scoped-define QUERY-STRING-BRW_PRIOOV FOR EACH priotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PRIOOV OPEN QUERY BRW_PRIOOV FOR EACH priotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PRIOOV priotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRIOOV priotemp


/* Definitions for BROWSE BRW_PROJ                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PROJ projtemp.PERSONALKOD ~
projtemp.FORNAMN projtemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PROJ 
&Scoped-define QUERY-STRING-BRW_PROJ FOR EACH projtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PROJ OPEN QUERY BRW_PROJ FOR EACH projtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PROJ projtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PROJ projtemp


/* Definitions for BROWSE BRW_TIDPAONR                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDPAONR aonrtidperstemp.PERSONALKOD ~
aonrtidperstemp.FORNAMN aonrtidperstemp.EFTERNAMN aonrtidperstemp.ANVANDARE ~
aonrtidperstemp.DATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDPAONR 
&Scoped-define QUERY-STRING-BRW_TIDPAONR FOR EACH aonrtidperstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TIDPAONR OPEN QUERY BRW_TIDPAONR FOR EACH aonrtidperstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TIDPAONR aonrtidperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDPAONR aonrtidperstemp


/* Definitions for FRAME FRAME-ANM                                      */

/* Definitions for FRAME FRAME-BARABREDD                                */

/* Definitions for FRAME FRAME-BEH                                      */

/* Definitions for FRAME FRAME-BERPROJ                                  */

/* Definitions for FRAME FRAME-BEST                                     */

/* Definitions for FRAME FRAME-DOK                                      */

/* Definitions for FRAME FRAME-FAKT                                     */

/* Definitions for FRAME FRAME-KONTO                                    */

/* Definitions for FRAME FRAME-KOPP                                     */

/* Definitions for FRAME FRAME-OVRIGT                                   */

/* Definitions for FRAME FRAME-TIDL                                     */

/* Definitions for FRAME FRAME-TIDPERS                                  */

/* Definitions for FRAME FRAME-TILLAVTAL                                */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-ecopy EDITOR_ANM BRW_DAGBOK ~
BTN_GATILLANM BTN_NYDAGBOK BTN_ANDDAGBOK BTN_BORTDAGBOK BTN_VISADAGBOK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ecopy EDITOR_ANM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_BEST 
       MENU-ITEM m_Info         LABEL "Info"          .

DEFINE MENU POPUP-MENU-BRW_BEST-2 
       MENU-ITEM m_Info-2       LABEL "Info"          .

DEFINE MENU POPUP-MENU-FILL-IN-KONAKT 
       MENU-ITEM m_Kund_information2 LABEL "Kund information".

DEFINE MENU POPUP-MENU-FILL-IN_ARBANSVARIG 
       MENU-ITEM m_Vlj_Personal3 LABEL "Välj Personal" .

DEFINE MENU POPUP-MENU-FILL-IN_BEREDARE 
       MENU-ITEM m_Vlj_Personal2 LABEL "Välj Personal" .

DEFINE MENU POPUP-MENU-FILL-IN_BEST 
       MENU-ITEM m_Kund_information LABEL "Kund information".

DEFINE MENU POPUP-MENU-FILL-IN_STARTDAG 
       MENU-ITEM m_Vlj_Personal LABEL "Välj Personal" .

DEFINE MENU POPUP-MENU-FILL-IN_UTFARDAT 
       MENU-ITEM m_Referens_personer LABEL "Referens personer".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDDAGBOK 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTDAGBOK 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_GATILLANM 
     LABEL "Fasta anm." 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NYDAGBOK 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISADAGBOK 
     LABEL "VISA" 
     SIZE 12 BY 1.

DEFINE VARIABLE EDITOR_ANM AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 71 BY 11 NO-UNDO.

DEFINE VARIABLE FILL-IN-ecopy AS CHARACTER FORMAT "X(256)":U 
     LABEL "ecopy" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_ANM  NO-FOCUS
     LABEL "Anmärkningar" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_AOPERSSEK  NO-FOCUS
     LABEL "Arbetandepersonal" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_AVT  NO-FOCUS
     LABEL "Tillstånd/Avtal" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_DOK  NO-FOCUS
     LABEL "Dokument" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_FAK  NO-FOCUS
     LABEL "Faktura" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_KONT  NO-FOCUS
     LABEL "Konton" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_KOPPL  NO-FOCUS
     LABEL "Kopplingar" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_OVR  NO-FOCUS
     LABEL "Övrigt" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_PLIGG  NO-FOCUS
     LABEL "Personalliggare" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_TIDL  NO-FOCUS
     LABEL "Tidlägen" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_TIDPERS  NO-FOCUS
     LABEL "Tidskrivandepersonal" 
     SIZE 7.25 BY 2.33.

DEFINE RECTANGLE RECT-MENY
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124.88 BY .08.

DEFINE BUTTON BTN_ACCESS 
     LABEL "Export Access" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FASTTRAC 
     LABEL "Export Fasttrac" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_PAG 
     LABEL "Pågående" 
     SIZE 12 BY 1 TOOLTIP "Ändrar avslutat/pågående".

DEFINE VARIABLE CMB_DEBPR AS CHARACTER FORMAT "X(9)":U 
     LABEL "Debitering" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utförande omr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 45.75 BY 1 NO-UNDO.

DEFINE VARIABLE EDITOR_AUPP AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 46 BY 6.25 NO-UNDO.

DEFINE VARIABLE FILL-IN-EDELNR AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 8.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KONAKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 43.5 BY 1 TOOLTIP "För vallista kontaktpersoner tryck högra mustangenten." NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-REF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ref.nr Beställare" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_AONRAVDATUM AS DATE FORMAT "99/99/99" INITIAL 01/01/91 
     LABEL "Avslutat" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Ange ? för bortag av avslutning" NO-UNDO.

DEFINE VARIABLE FILL-IN_ARBANSVARIG AS CHARACTER FORMAT "x(256)" 
     LABEL "Arbetsansvarig" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "För att få vallista på personer tryck högra mustangenten.".

DEFINE VARIABLE FILL-IN_AVDELNING AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avdelning" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_BEREDARE AS CHARACTER FORMAT "x(256)" 
     LABEL "Beredare/Projektör" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "För att få vallista på personer tryck högra mustangenten.".

DEFINE VARIABLE FILL-IN_BEST AS CHARACTER FORMAT "x(256)" 
     LABEL "BESTÄLLARE" 
     VIEW-AS FILL-IN 
     SIZE 45.75 BY 1 TOOLTIP "För att se kund information och kontaktpersoner tryck högra mustangenten"
     BGCOLOR 15 .

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_FASTAAONR AS LOGICAL FORMAT "Fast/Tillf" INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(256)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 45.75 BY 1.

DEFINE VARIABLE FILL-IN_PAGAENDE AS CHARACTER FORMAT "X(256)":U INITIAL "Pågående" 
      VIEW-AS TEXT 
     SIZE 9 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN_STARTDAG AS CHARACTER FORMAT "x(256)" 
     LABEL "DAG" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "För att få vallista på personer tryck högra mustangenten.".

DEFINE VARIABLE FILL-IN_UTFARDAT AS CHARACTER FORMAT "x(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1 NO-UNDO.

DEFINE BUTTON BTN_BEHORAND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_BESTAVD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Alla","Allmän" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALJBEST AS CHARACTER FORMAT "X(256)":U INITIAL "Välj xxxx" 
      VIEW-AS TEXT 
     SIZE 27.5 BY 1.21
     FONT 17 NO-UNDO.

DEFINE BUTTON BTN_ANDADOK 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BDOCTIDDOK 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MAPPDOK 
     LABEL "Öppna dokumentmappen" 
     SIZE 16 BY 1.

DEFINE BUTTON BTN_NYADOK 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISADOK 
     LABEL "Visa" 
     SIZE 16 BY 1.

DEFINE VARIABLE CMB_FAK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fakturakategori" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 36.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "x(12)" 
     LABEL "Ansvarig" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EXTERNT AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Planerat externt offertpris" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE FILL-IN_FAKTNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Ingår i fakturaplannr." 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_FAKTTYP AS CHARACTER FORMAT "x(12)" 
     LABEL "FAKTTYP" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_INTERNT AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Planerat internt offertpris" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     FGCOLOR 9 .

DEFINE BUTTON BTN_GATILLKONTO 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_ARBARTKOD AS INTEGER FORMAT ">>>" INITIAL 0 
     LABEL "Arbetsart" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_PKOD AS INTEGER FORMAT ">>" INITIAL 0 
     LABEL "Prioritet" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_AKTIV 
     LABEL "Status" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ATGBER 
     LABEL "Åtgärder" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BERBER 
     IMAGE-UP FILE "BILDER\xbtn_bered.gif":U
     LABEL "Bereda":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BERKALK 
     LABEL "Kalkyl av beredning" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXPBER 
     LABEL "Export" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXPMARK 
     LABEL "Export" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FAKFAK 
     IMAGE-UP FILE "BILDER\xbtn_faktu.gif":U
     LABEL "Fakturera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FASTIGHETMARK 
     LABEL "Fastigh./Markägare" 
     SIZE 14 BY 1 TOOLTIP "Registrera/ändra fastigheter och markägare, skapa kopplingar".

DEFINE BUTTON BTN_FLISTAFAK 
     LABEL "Fakturerat kr":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_GATILL 
     LABEL "NY" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IMPBER 
     LABEL "Import beredning" 
     SIZE 14 BY 1 TOOLTIP "Import beredning Excel".

DEFINE BUTTON BTN_IMPORTBER 
     LABEL "Import" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INKBER 
     IMAGE-UP FILE "bilder/xbtn_inkopsmal.gif":U
     LABEL "Inköp" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INKBERDIREKT 
     IMAGE-UP FILE "bilder/xbtn_direktinkopl.gif":U
     LABEL "Inköp från materiellista" 
     SIZE 14 BY 1 TOOLTIP "Inköp från materiellista".

DEFINE BUTTON BTN_JMFBERINK 
     LABEL "Jmf Ber vs Inköp":L 
     SIZE 14 BY 1 TOOLTIP "Jmf Ber vs Inköp för samtliga delnr".

DEFINE BUTTON BTN_KALKAR 
     LABEL "Kopiera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALKKALK 
     IMAGE-UP FILE "BILDER\xbtn_kalkyl.gif":U
     LABEL "Kalkylera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KONVKALK 
     LABEL "Konvertera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOPIBER 
     LABEL "Kopiera Bef. beredning" 
     SIZE 14 BY 1 TOOLTIP "Kopiera och koppla annan beredning".

DEFINE BUTTON BTN_KOPPFAK 
     LABEL "Koppla":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOPPLAAONR 
     LABEL "Koppla" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KredFAK 
     LABEL "Kreditfakt." 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LASBER 
     IMAGE-UP FILE "bilder/xbtn_lastaber.gif":U
     LABEL "Låsta beredningar" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LASBERDIREKT 
     IMAGE-UP FILE "bilder/xbtn_lasuppinkop.gif":U
     LABEL "Låsta upp InköpLista" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LISTBER 
     LABEL "Listor" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OMRAKNA 
     LABEL "Omräkna m.v. KPI" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PRELBFAK 
     LABEL "Ta bort prel.fakt.":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SCHAKTP 
     IMAGE-UP FILE "bilder/xbtn_smalschakt.gif":U
     LABEL "Schaktprotokoll":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SLAIHOPKALK 
     IMAGE-UP FILE "bilder/xbtn_slaihop.gif":U
     LABEL "" 
     SIZE 14 BY 1 TOOLTIP "Slå ihop UF kalkyl för alla delnr".

DEFINE BUTTON BTN_UPPBER 
     LABEL "Beredningshuvud":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPPFAK 
     LABEL "Fakturaplan":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPPKALK 
     LABEL "Kalkyl huvud":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPPMARK 
     LABEL "Markv. huvud":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VARDMARK 
     IMAGE-UP FILE "BILDER\xbtn_markvard.gif":U
     LABEL "Markvärdering":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VFAKFAK 
     LABEL "Visa fakturor" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISAOFAK 
     LABEL "Visa fakturaplan":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISKALK 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISKALMARK 
     LABEL "Visa markvärdering":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISMARK 
     LABEL "Visa markägare":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_DOKSTATUS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_DOKTYP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Typ" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TRAK AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Trakt.zon" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0","1" 
     DROP-DOWN-LIST
     SIZE 5.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DOKU AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Dokumentation" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FORDEL AS CHARACTER FORMAT "X(256)":U INITIAL "Fördelningsnyckel i %:" 
      VIEW-AS TEXT 
     SIZE 24.38 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-MONTBER AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Montör/beredare" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROJEKTLED AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Projektledare" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROJEKTOR AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Projektör" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANLNR AS CHARACTER FORMAT "x(15)" 
     LABEL "Anl. nr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE FILL-IN_ARBARTKODOV AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Arbetsart" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_PKODOV AS INTEGER FORMAT ">>" INITIAL 0 
     LABEL "Prioritet" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1.

DEFINE VARIABLE FILL-IN_TRAKTAMENTE AS INTEGER FORMAT "99" INITIAL 0 
     LABEL "Trakt.zon" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_UTRYCKNING AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Utryckning" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE BUTTON BTN_ANMTIDL 
     LABEL "Anmärkning" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BESTTIDL 
     LABEL "Beställningsstatus" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORTATIDL 
     LABEL "Ta bort dokument" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_GATILLTIDL 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KOPPTIDL 
     LABEL "Koppla Doku." 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISATIDL 
     LABEL "Visa doku." 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ANM AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Anmärkning" 
      VIEW-AS TEXT 
     SIZE 4 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-DOK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Dokument" 
      VIEW-AS TEXT 
     SIZE 4 BY .67 NO-UNDO.

DEFINE BUTTON BTN_GATILLTP 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDHAND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDTV 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BDOCTIDTV 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTHAND 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MAPPTV 
     LABEL "Öppna dokumentmappen" 
     SIZE 16 BY 1.

DEFINE BUTTON BTN_MINDAT1HAND 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NYATV 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NYHAND 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPDAT1HAND 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_VISATV 
     LABEL "Visa" 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN-STARTDATHAND AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .96 NO-UNDO.

DEFINE VARIABLE TOG_KLAR AS LOGICAL INITIAL no 
     LABEL "Klar" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.5 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANL FOR 
      anlaggtemp SCROLLING.

DEFINE QUERY BRW_ANV FOR 
      anvandartemp SCROLLING.

DEFINE QUERY BRW_ANVOMR FOR 
      omranvandartemp SCROLLING.

DEFINE QUERY BRW_AONRKTO FOR 
      aonrkonttemp SCROLLING.

DEFINE QUERY BRW_AOTID FOR 
      aotidslagtemp SCROLLING.

DEFINE QUERY BRW_ARBAN FOR 
      ansvaraotemp SCROLLING.

DEFINE QUERY BRW_ARBART FOR 
      arbarttemp SCROLLING.

DEFINE QUERY BRW_ARBARTOV FOR 
      arbarttemp SCROLLING.

DEFINE QUERY BRW_AVTAL FOR 
      avtalaonrtemp SCROLLING.

DEFINE QUERY BRW_AVTALDOK FOR 
      avtalaonrtemp SCROLLING.

DEFINE QUERY BRW_BERED FOR 
      beratemp SCROLLING.

DEFINE QUERY BRW_BEST FOR 
      bestkundallt SCROLLING.

DEFINE QUERY BRW_BESTAVD FOR 
      bestkundextra SCROLLING.

DEFINE QUERY BRW_DAGBOK FOR 
      dagboktemp SCROLLING.

DEFINE QUERY BRW_HAND FOR 
      avtaodattemp SCROLLING.

DEFINE QUERY BRW_KOPP FOR 
      gatill SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.

DEFINE QUERY BRW_PRIO FOR 
      priotemp SCROLLING.

DEFINE QUERY BRW_PRIOOV FOR 
      priotemp SCROLLING.

DEFINE QUERY BRW_PROJ FOR 
      projtemp SCROLLING.

DEFINE QUERY BRW_TIDPAONR FOR 
      aonrtidperstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANL C-Win _STRUCTURED
  QUERY BRW_ANL NO-LOCK DISPLAY
      anlaggtemp.ANLNR COLUMN-LABEL "Anl.nr" FORMAT "X(256)":U
            WIDTH 15
      anlaggtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 34.88
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 53.38 BY 9
         TITLE "Välj anläggning".

DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV C-Win _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Ansvarig" FORMAT "x(12)":U
      anvandartemp.AV-NAMN COLUMN-LABEL "Namn" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 49.5 BY 12.25.

DEFINE BROWSE BRW_ANVOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANVOMR C-Win _STRUCTURED
  QUERY BRW_ANVOMR NO-LOCK DISPLAY
      omranvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(256)":U
            WIDTH 12
      omranvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(256)":U
            WIDTH 34.13
      omranvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 56.13 BY 10.83
         FONT 4
         TITLE "Behöriga".

DEFINE BROWSE BRW_AONRKTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONRKTO C-Win _STRUCTURED
  QUERY BRW_AONRKTO NO-LOCK DISPLAY
      aonrkonttemp.K1 FORMAT "x(256)":U WIDTH 6
      aonrkonttemp.K2 FORMAT "x(256)":U WIDTH 6
      aonrkonttemp.K3 FORMAT "x(256)":U WIDTH 6
      aonrkonttemp.K4 FORMAT "x(256)":U WIDTH 6
      aonrkonttemp.K5 FORMAT "x(256)":U WIDTH 6
      aonrkonttemp.SATS% COLUMN-LABEL "Sats%" FORMAT ">99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 41.5 BY 11
         TITLE "Kontosträngar lagrade på aonr:et".

DEFINE BROWSE BRW_AOTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AOTID C-Win _STRUCTURED
  QUERY BRW_AOTID NO-LOCK DISPLAY
      aotidslagtemp.TIDLAGE COLUMN-LABEL "Tidläge" FORMAT "X(256)":U
            WIDTH 25
      aotidslagtemp.AKTIVITET1 COLUMN-LABEL "Aktivitet" FORMAT "X(256)":U
            WIDTH 20 COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      aotidslagtemp.DAT1 FORMAT "99/99/99":U COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      aotidslagtemp.ANVANDARE1 COLUMN-LABEL "Ändrad av" FORMAT "x(256)":U
            WIDTH 10 COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      aotidslagtemp.AKTIVITET2 COLUMN-LABEL "Aktivitet" FORMAT "X(256)":U
            WIDTH 20 COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      aotidslagtemp.DAT2 FORMAT "99/99/99":U COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      aotidslagtemp.ANVANDARE2 COLUMN-LABEL "Ändrad av" FORMAT "x(256)":U
            WIDTH 10 COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 107.5 BY 10.21
         TITLE "Valda tidlägen".

DEFINE BROWSE BRW_ARBAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ARBAN C-Win _STRUCTURED
  QUERY BRW_ARBAN NO-LOCK DISPLAY
      ansvaraotemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(256)":U
            WIDTH 5
      ansvaraotemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U
            WIDTH 11.5
      ansvaraotemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 19.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 40 BY 10.83
         TITLE "Välj Arbetsansvarig".

DEFINE BROWSE BRW_ARBART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ARBART C-Win _STRUCTURED
  QUERY BRW_ARBART NO-LOCK DISPLAY
      arbarttemp.ARBARTKOD COLUMN-LABEL "Arbetsart" FORMAT ">>>":U
            WIDTH 9.38
      arbarttemp.ARBBENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 27.75 BY 8.92
         TITLE "Välj arbetsart".

DEFINE BROWSE BRW_ARBARTOV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ARBARTOV C-Win _STRUCTURED
  QUERY BRW_ARBARTOV NO-LOCK DISPLAY
      arbarttemp.ARBARTKOD COLUMN-LABEL "Arbetsart" FORMAT ">>9":U
      arbarttemp.ARBBENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 27.75 BY 9
         TITLE "Välj arbetsart".

DEFINE BROWSE BRW_AVTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVTAL C-Win _STRUCTURED
  QUERY BRW_AVTAL NO-LOCK DISPLAY
      avtalaonrtemp.AVTAL COLUMN-LABEL "Tillstånd/Avtal" FORMAT "X(256)":U
            WIDTH 18.5
      avtalaonrtemp.BEN COLUMN-LABEL "Egen text" FORMAT "X(256)":U
            WIDTH 28.63
      avtalaonrtemp.KLR FORMAT "99/99/99":U WIDTH 6.88
      avtalaonrtemp.FIL COLUMN-LABEL "Söknamn" FORMAT "X(256)":U
            WIDTH 32
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 58 BY 9.

DEFINE BROWSE BRW_AVTALDOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVTALDOK C-Win _STRUCTURED
  QUERY BRW_AVTALDOK NO-LOCK DISPLAY
      avtalaonrtemp.AVTAL COLUMN-LABEL "Dokument" FORMAT "X(256)":U
            WIDTH 20
      avtalaonrtemp.BEN COLUMN-LABEL "Egen text" FORMAT "X(256)":U
            WIDTH 20
      avtalaonrtemp.FIL COLUMN-LABEL "Sökväg" FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 71 BY 11.

DEFINE BROWSE BRW_BERED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BERED C-Win _STRUCTURED
  QUERY BRW_BERED NO-LOCK DISPLAY
      beratemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(256)":U
            WIDTH 5
      beratemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 11.5
      beratemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 19.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 40 BY 10.83
         TITLE "Välj Beredare/Projektör".

DEFINE BROWSE BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEST C-Win _STRUCTURED
  QUERY BRW_BEST NO-LOCK DISPLAY
      bestkundallt.VIBESTID COLUMN-LABEL "bestid" FORMAT "X(256)":U
            WIDTH 6
      bestkundallt.BESTNAMN COLUMN-LABEL "Namn" FORMAT "x(256)":U
            WIDTH 47
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 56.5 BY 10.83
         TITLE "Välj BESTÄLLARE".

DEFINE BROWSE BRW_BESTAVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BESTAVD C-Win _STRUCTURED
  QUERY BRW_BESTAVD NO-LOCK DISPLAY
      bestkundextra.VIBESTID COLUMN-LABEL "bestid" FORMAT "X(256)":U
            WIDTH 6
      bestkundextra.BESTNAMN COLUMN-LABEL "Namn" FORMAT "x(256)":U
            WIDTH 47
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 56.5 BY 10.83
         TITLE "Välj BESTÄLLARE".

DEFINE BROWSE BRW_DAGBOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DAGBOK C-Win _STRUCTURED
  QUERY BRW_DAGBOK NO-LOCK DISPLAY
      dagboktemp.DAGBOKAO COLUMN-LABEL "Dagbok" FORMAT "X(256)":U
            WIDTH 21.75
      dagboktemp.KILOMETER COLUMN-LABEL "Km" FORMAT "->,>>>,>>9":U
            WIDTH 3.5
      dagboktemp.TIMMAR COLUMN-LABEL "Timmar" FORMAT "->>>>9.99":U
            WIDTH 6.5
      dagboktemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      dagboktemp.ANVANDARE COLUMN-LABEL "Inlagd av" FORMAT "X(15)":U
            WIDTH 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 50.88 BY 11
         TITLE "Dagbok".

DEFINE BROWSE BRW_HAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HAND C-Win _STRUCTURED
  QUERY BRW_HAND NO-LOCK DISPLAY
      avtaodattemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
            WIDTH 8.38
      avtaodattemp.ANMARK COLUMN-LABEL "Händelse" FORMAT "X(256)":U
            WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 48 BY 9.

DEFINE BROWSE BRW_KOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOPP C-Win _STRUCTURED
  QUERY BRW_KOPP NO-LOCK DISPLAY
      gatill.F1 COLUMN-LABEL "Typ av koppling" FORMAT "X(256)":U
            WIDTH 20
      gatill.F2 COLUMN-LABEL "Nummer" FORMAT "X(256)":U WIDTH 10
      gatill.F3 COLUMN-LABEL "Benämning" FORMAT "X(256)":U WIDTH 33.38
      gatill.STATUSNIV COLUMN-LABEL "Status" FORMAT "X(3)":U
  ENABLE
      gatill.STATUSNIV
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 74 BY 12.5.

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(256)":U
            WIDTH 7
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
            WIDTH 12.5
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
            WIDTH 19.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 40 BY 9.75
         FONT 4
         TITLE "Välj Personal".

DEFINE BROWSE BRW_PRIO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRIO C-Win _STRUCTURED
  QUERY BRW_PRIO NO-LOCK DISPLAY
      priotemp.PKOD COLUMN-LABEL "Prioritet" FORMAT ">>":U WIDTH 11
      priotemp.P-BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 20
      priotemp.OMRADE FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 34.5 BY 9
         TITLE "Välj prioritet".

DEFINE BROWSE BRW_PRIOOV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRIOOV C-Win _STRUCTURED
  QUERY BRW_PRIOOV NO-LOCK DISPLAY
      priotemp.PKOD COLUMN-LABEL "Prioritet" FORMAT ">>":U
      priotemp.P-BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 15
      priotemp.OMRADE FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 34.5 BY 9
         TITLE "Välj prioritet".

DEFINE BROWSE BRW_PROJ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PROJ C-Win _STRUCTURED
  QUERY BRW_PROJ NO-LOCK DISPLAY
      projtemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(256)":U
            WIDTH 5
      projtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 11.5
      projtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 19.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 40 BY 10.83
         TITLE "Välj Projektör".

DEFINE BROWSE BRW_TIDPAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDPAONR C-Win _STRUCTURED
  QUERY BRW_TIDPAONR NO-LOCK DISPLAY
      aonrtidperstemp.PERSONALKOD COLUMN-LABEL "Enhet!/Sign" FORMAT "x(5)":U
      aonrtidperstemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 15
      aonrtidperstemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 25
      aonrtidperstemp.ANVANDARE COLUMN-LABEL "Upplagd av" FORMAT "X(9)":U
      aonrtidperstemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 68.5 BY 10.83
         TITLE "Tidskrivande personal".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-AONR
     MBTN_AOPERSSEK AT ROW 1 COL 81.88
     BTN_OK AT ROW 1.25 COL 111.5
     MBTN_PLIGG AT ROW 1 COL 90.75 WIDGET-ID 2
     BTN_AVB AT ROW 2.5 COL 111
     MBTN_KOPPL AT ROW 1 COL 1
     MBTN_ANM AT ROW 1 COL 18.88
     MBTN_AVT AT ROW 1 COL 54.5
     MBTN_DOK AT ROW 1 COL 45.38
     MBTN_FAK AT ROW 1 COL 36.38
     MBTN_KONT AT ROW 1 COL 27.63
     MBTN_OVR AT ROW 1 COL 63.88
     MBTN_TIDL AT ROW 1 COL 9.88
     MBTN_TIDPERS AT ROW 1 COL 72.63
     RECT-MENY AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.42.

DEFINE FRAME FRAME-OVRIGT
     FILL-IN_TRAKTAMENTE AT ROW 1.75 COL 113.5 COLON-ALIGNED
     FILL-IN-MONTBER AT ROW 2 COL 74.88 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-PROJEKTOR AT ROW 2 COL 97 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-PROJEKTLED AT ROW 3.08 COL 74.88 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-DOKU AT ROW 3.08 COL 97 COLON-ALIGNED WIDGET-ID 8
     CMB_TRAK AT ROW 3.17 COL 10.38 COLON-ALIGNED
     FILL-IN_UTRYCKNING AT ROW 3.17 COL 46.88 COLON-ALIGNED
     FILL-IN_PKODOV AT ROW 4.25 COL 10.38 COLON-ALIGNED
     FILL-IN_ARBARTKODOV AT ROW 4.25 COL 46.75 COLON-ALIGNED
     FILL-IN_ANLNR AT ROW 4.25 COL 72.13 COLON-ALIGNED
     BRW_PRIOOV AT ROW 5.67 COL 1.5
     BRW_ARBARTOV AT ROW 5.67 COL 36.5
     BRW_ANL AT ROW 5.67 COL 64.75
     FILL-IN-FORDEL AT ROW 1.17 COL 66.63 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     "Övrigt" VIEW-AS TEXT
          SIZE 23.5 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-KOPP
     BTN_IMPORTBER AT ROW 1 COL 77.5 WIDGET-ID 14
     BTN_IMPBER AT ROW 1.25 COL 77.5 WIDGET-ID 4
     BTN_UPPKALK AT ROW 2.75 COL 77.5
     BRW_KOPP AT ROW 3 COL 1.5
     BTN_GATILL AT ROW 3 COL 77.5
     BTN_UPPFAK AT ROW 3.5 COL 77.5
     BTN_KOPPFAK AT ROW 3.5 COL 108.5
     BTN_UPPBER AT ROW 3.75 COL 77.5
     BTN_KALKKALK AT ROW 4 COL 77.5
     BTN_SLAIHOPKALK AT ROW 4.25 COL 77.5 WIDGET-ID 12
     BTN_KOPPLAAONR AT ROW 4.25 COL 77.5
     BTN_BERKALK AT ROW 4.25 COL 77.5
     BTN_VISAOFAK AT ROW 4.75 COL 77.5
     BTN_SCHAKTP AT ROW 4.79 COL 77.5
     BTN_BERBER AT ROW 4.79 COL 77.5
     BTN_JMFBERINK AT ROW 4.79 COL 77.5 WIDGET-ID 10
     BTN_VISKALK AT ROW 5 COL 77.5
     BTN_UPPMARK AT ROW 5 COL 77.5
     BTN_KOPIBER AT ROW 5.25 COL 77.5 WIDGET-ID 2
     BTN_AKTIV AT ROW 5.29 COL 77.5
     BTN_INKBER AT ROW 5.75 COL 77.5
     BTN_INKBERDIREKT AT ROW 5.75 COL 77.5 WIDGET-ID 6
     BTN_FAKFAK AT ROW 6 COL 77.5
     BTN_FASTIGHETMARK AT ROW 6.25 COL 77.5
     BTN_KONVKALK AT ROW 6.25 COL 77.5
     BTN_LISTBER AT ROW 6.83 COL 77.5
     BTN_PRELBFAK AT ROW 7.38 COL 77.5
     BTN_VARDMARK AT ROW 7.38 COL 77.5
     BTN_ATGBER AT ROW 7.92 COL 77.5
     BTN_KALKAR AT ROW 8.29 COL 77.5
     BTN_VISKALMARK AT ROW 8.42 COL 77.5
     BTN_VFAKFAK AT ROW 8.5 COL 77.5
     BTN_LASBERDIREKT AT ROW 9 COL 77.5 WIDGET-ID 8
     BTN_LASBER AT ROW 9 COL 77.5
     BTN_VISMARK AT ROW 9.5 COL 77.5
     BTN_KredFAK AT ROW 10 COL 77.5
     BTN_EXPBER AT ROW 10.08 COL 77.5
     BTN_OMRAKNA AT ROW 10.75 COL 77.5
     BTN_FLISTAFAK AT ROW 11.25 COL 77.5
     BTN_EXPMARK AT ROW 12.25 COL 77.5
     CMB_DOKTYP AT ROW 14.25 COL 89 COLON-ALIGNED
     CMB_DOKSTATUS AT ROW 14.25 COL 110.5 COLON-ALIGNED
     "Kopplingar" VIEW-AS TEXT
          SIZE 20.5 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-DOK
     BRW_AVTALDOK AT ROW 3 COL 1.5
     BTN_MAPPDOK AT ROW 4 COL 74
     BTN_VISADOK AT ROW 5.08 COL 74
     BTN_NYADOK AT ROW 14.25 COL 9.5
     BTN_ANDADOK AT ROW 14.25 COL 27.25
     BTN_BDOCTIDDOK AT ROW 14.25 COL 45
     "Dokumenthantering" VIEW-AS TEXT
          SIZE 31.5 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-PLIGG
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75 WIDGET-ID 200.

DEFINE FRAME FRAME-TIDPERS
     BRW_TIDPAONR AT ROW 3 COL 1.5
     BTN_GATILLTP AT ROW 14.25 COL 27
     "Tidskrivande personal" VIEW-AS TEXT
          SIZE 30 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-TIDL
     BRW_AOTID AT ROW 2.79 COL 1.5
     BTN_ANMTIDL AT ROW 5 COL 110.63
     BTN_BESTTIDL AT ROW 6.08 COL 110.63
     BTN_VISATIDL AT ROW 7.21 COL 110.63
     BTN_KOPPTIDL AT ROW 8.29 COL 110.63
     BTN_BORTATIDL AT ROW 9.42 COL 110.63
     BTN_GATILLTIDL AT ROW 13.33 COL 41.13
     FILL-IN-DOK AT ROW 13.46 COL 18 COLON-ALIGNED
     FILL-IN-ANM AT ROW 13.46 COL 33.63 COLON-ALIGNED
     "Finns" VIEW-AS TEXT
          SIZE 7 BY 1 AT ROW 13.13 COL 1.5
          FONT 17
     "Tidlägen" VIEW-AS TEXT
          SIZE 15.5 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-BARABREDD
     FILL-IN-EDELNR AT ROW 1.13 COL 36.5 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     FILL-IN_AONR AT ROW 1.13 COL 11.38 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 1.13 COL 30.25 COLON-ALIGNED
     BTN_PAG AT ROW 1.13 COL 76.13
     FILL-IN_AONRAVDATUM AT ROW 1.13 COL 96.25 COLON-ALIGNED
     FILL-IN_FASTAAONR AT ROW 1.13 COL 108 COLON-ALIGNED NO-LABEL
     BRW_PERS AT ROW 1.25 COL 80
     FILL-IN_ORT AT ROW 2.25 COL 11.38 COLON-ALIGNED
     CMB_OMR AT ROW 3.33 COL 11.38 COLON-ALIGNED
     EDITOR_AUPP AT ROW 3.46 COL 79.75 NO-LABEL
     FILL-IN_BEST AT ROW 4.42 COL 11.38 COLON-ALIGNED
     FILL-IN_AVDELNING AT ROW 4.42 COL 11.38 COLON-ALIGNED WIDGET-ID 2
     CMB_DEBPR AT ROW 4.42 COL 64.5 COLON-ALIGNED
     FILL-IN-REF AT ROW 5.54 COL 11.38 COLON-ALIGNED
     FILL-IN-KONAKT AT ROW 5.54 COL 34 COLON-ALIGNED
     FILL-IN_STARTDAG AT ROW 6.63 COL 11.38 COLON-ALIGNED
     FILL-IN_BEREDARE AT ROW 7.71 COL 11.38 COLON-ALIGNED
     FILL-IN_ARBANSVARIG AT ROW 8.83 COL 11.38 COLON-ALIGNED
     FILL-IN_UTFARDAT AT ROW 9.92 COL 12.75
     BTN_FASTTRAC AT ROW 9.92 COL 88
     BTN_ACCESS AT ROW 9.92 COL 103.38
     FILL-IN-NAMN-1 AT ROW 6.63 COL 20.25 COLON-ALIGNED NO-LABEL
     FILL-IN-NAMN-2 AT ROW 7.71 COL 20.25 COLON-ALIGNED NO-LABEL
     FILL-IN-NAMN-3 AT ROW 8.83 COL 20.25 COLON-ALIGNED NO-LABEL
     FILL-IN_PAGAENDE AT ROW 1.33 COL 98.25 NO-LABEL
     "Arbetsuppgift:" VIEW-AS TEXT
          SIZE 14.5 BY 1 AT ROW 2.17 COL 77.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 124.88 BY 10
         FONT 4.

DEFINE FRAME FRAME-KONTO
     BRW_AONRKTO AT ROW 3 COL 1.5
     FILL-IN_ARBARTKOD AT ROW 3 COL 59 COLON-ALIGNED
     FILL-IN_PKOD AT ROW 3 COL 74.13 COLON-ALIGNED WIDGET-ID 2
     BRW_ARBART AT ROW 5.08 COL 48.63
     BRW_PRIO AT ROW 5.08 COL 48.88 WIDGET-ID 100
     BTN_GATILLKONTO AT ROW 14.5 COL 15
     "Konton" VIEW-AS TEXT
          SIZE 11.5 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-BERPROJ
     BRW_PROJ AT ROW 3 COL 1.5
     BRW_BERED AT ROW 3 COL 42.75
     BRW_ARBAN AT ROW 3 COL 83.88
     "Välj ansvariga" VIEW-AS TEXT
          SIZE 21 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-BEST
     CMB_BESTAVD AT ROW 3 COL 1.5
     BRW_BESTAVD AT ROW 3 COL 31.5
     BRW_BEST AT ROW 3 COL 31.5
     FILL-IN-VALJBEST AT ROW 1.5 COL 1.5 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-FAKT
     BRW_ANV AT ROW 1.5 COL 72
     FILL-IN_FAKTNR AT ROW 3 COL 29.25 COLON-ALIGNED
     FILL-IN_FAKTTYP AT ROW 3.25 COL 90 COLON-ALIGNED
     FILL-IN_ANVANDARE AT ROW 4.08 COL 29.25 COLON-ALIGNED
     CMB_FAK AT ROW 5.21 COL 29.25 COLON-ALIGNED
     FILL-IN_INTERNT AT ROW 6.29 COL 29.25 COLON-ALIGNED
     FILL-IN_EXTERNT AT ROW 7.42 COL 29.25 COLON-ALIGNED
     "Fakturauppgifter" VIEW-AS TEXT
          SIZE 30 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-BEH
     BRW_ANVOMR AT ROW 3 COL 1.5
     BTN_BEHORAND AT ROW 14.25 COL 21.75
     "Behörig personal" VIEW-AS TEXT
          SIZE 45 BY 1.21 AT ROW 1.25 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-ANM
     FILL-IN-ecopy AT ROW 1.25 COL 53.5 COLON-ALIGNED WIDGET-ID 2
     EDITOR_ANM AT ROW 3 COL 1.5 NO-LABEL
     BRW_DAGBOK AT ROW 3 COL 73.25
     BTN_GATILLANM AT ROW 14.17 COL 26.88
     BTN_NYDAGBOK AT ROW 14.25 COL 74.38
     BTN_ANDDAGBOK AT ROW 14.25 COL 86.75
     BTN_BORTDAGBOK AT ROW 14.25 COL 99.13
     BTN_VISADAGBOK AT ROW 14.25 COL 111.38
     "Anmärkningar och dagboksanteckningar" VIEW-AS TEXT
          SIZE 50.5 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.

DEFINE FRAME FRAME-TILLAVTAL
     BTN_UPDAT1HAND AT ROW 1.25 COL 121
     FILL-IN-STARTDATHAND AT ROW 1.67 COL 109 COLON-ALIGNED
     TOG_KLAR AT ROW 1.71 COL 95.13
     BTN_MINDAT1HAND AT ROW 2.13 COL 121
     BRW_AVTAL AT ROW 3 COL 1.5
     BRW_HAND AT ROW 3 COL 76.63
     BTN_MAPPTV AT ROW 4 COL 60.13
     BTN_VISATV AT ROW 5.08 COL 60.13
     BTN_NYATV AT ROW 12.5 COL 7.5
     BTN_ANDTV AT ROW 12.5 COL 20.5
     BTN_BDOCTIDTV AT ROW 12.5 COL 33.5
     BTN_NYHAND AT ROW 12.5 COL 81
     BTN_ANDHAND AT ROW 12.5 COL 94
     BTN_BORTHAND AT ROW 12.5 COL 108
     "Tillstånd och avtal" VIEW-AS TEXT
          SIZE 22.5 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
     "Händelser" VIEW-AS TEXT
          SIZE 13.88 BY 1.21 AT ROW 1.5 COL 76.63
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 13.5
         SIZE 124.5 BY 14.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: anlaggtemp T "?" NO-UNDO temp-db anlaggtemp
      TABLE: ansvaraotemp T "?" NO-UNDO temp-db ansvaraotemp
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: aonrkonttemp T "?" NO-UNDO temp-db aonrkonttemp
      TABLE: aonrtidperstemp T "?" NO-UNDO temp-db aonrtidperstemp
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: arbarttemp T "?" NO-UNDO temp-db arbarttemp
      TABLE: avtalaonrtemp T "?" NO-UNDO temp-db avtalaonrtemp
      TABLE: avtaodattemp T "?" NO-UNDO temp-db avtaodattemp
      TABLE: beratemp T "?" NO-UNDO temp-db beratemp
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: bestkundextra T "?" NO-UNDO temp-db bestkundextra
      TABLE: dagboktemp T "?" NO-UNDO temp-db dagboktemp
      TABLE: gatill T "?" NO-UNDO temp-db gatill
      TABLE: ktotemp T "?" NO-UNDO temp-db ktotemp
      TABLE: omranvandartemp T "?" NO-UNDO temp-db omranvandartemp
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: priotemp T "?" NO-UNDO temp-db priotemp
      TABLE: projtemp T "?" NO-UNDO temp-db projtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Guru-Projekthuvud"
         HEIGHT             = 28.33
         WIDTH              = 125
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
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
ASSIGN FRAME FRAME-ANM:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-BARABREDD:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-BEH:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-BERPROJ:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-BEST:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-DOK:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-FAKT:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-KONTO:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-KOPP:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-OVRIGT:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-PLIGG:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-TIDL:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-TIDPERS:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-TILLAVTAL:FRAME = FRAME FRAME-AONR:HANDLE.

/* SETTINGS FOR FRAME FRAME-ANM
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_DAGBOK EDITOR_ANM FRAME-ANM */
ASSIGN 
       FRAME FRAME-ANM:HIDDEN           = TRUE.

ASSIGN 
       BRW_DAGBOK:HIDDEN  IN FRAME FRAME-ANM                = TRUE
       BRW_DAGBOK:MAX-DATA-GUESS IN FRAME FRAME-ANM         = 300
       BRW_DAGBOK:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-ANM = TRUE
       BRW_DAGBOK:COLUMN-RESIZABLE IN FRAME FRAME-ANM       = TRUE.

ASSIGN 
       BTN_ANDDAGBOK:HIDDEN IN FRAME FRAME-ANM           = TRUE.

ASSIGN 
       BTN_BORTDAGBOK:HIDDEN IN FRAME FRAME-ANM           = TRUE.

ASSIGN 
       BTN_GATILLANM:HIDDEN IN FRAME FRAME-ANM           = TRUE.

ASSIGN 
       BTN_NYDAGBOK:HIDDEN IN FRAME FRAME-ANM           = TRUE.

ASSIGN 
       BTN_VISADAGBOK:HIDDEN IN FRAME FRAME-ANM           = TRUE.

/* SETTINGS FOR FRAME FRAME-AONR
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-AONR:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-BARABREDD
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BRW_PERS FILL-IN_FASTAAONR FRAME-BARABREDD */
ASSIGN 
       FRAME FRAME-BARABREDD:HIDDEN           = TRUE.

ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME FRAME-BARABREDD                = TRUE.

ASSIGN 
       BTN_ACCESS:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

ASSIGN 
       BTN_FASTTRAC:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

ASSIGN 
       BTN_PAG:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-EDELNR IN FRAME FRAME-BARABREDD
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-EDELNR:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE
       FILL-IN-EDELNR:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KONAKT IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-KONAKT:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE
       FILL-IN-KONAKT:POPUP-MENU IN FRAME FRAME-BARABREDD       = MENU POPUP-MENU-FILL-IN-KONAKT:HANDLE.

ASSIGN 
       FILL-IN-NAMN-1:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

ASSIGN 
       FILL-IN-NAMN-2:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

ASSIGN 
       FILL-IN-NAMN-3:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-REF IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-REF:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONRAVDATUM IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_AONRAVDATUM:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE
       FILL-IN_AONRAVDATUM:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

ASSIGN 
       FILL-IN_ARBANSVARIG:POPUP-MENU IN FRAME FRAME-BARABREDD       = MENU POPUP-MENU-FILL-IN_ARBANSVARIG:HANDLE.

/* SETTINGS FOR FILL-IN FILL-IN_AVDELNING IN FRAME FRAME-BARABREDD
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AVDELNING:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

ASSIGN 
       FILL-IN_BEREDARE:POPUP-MENU IN FRAME FRAME-BARABREDD       = MENU POPUP-MENU-FILL-IN_BEREDARE:HANDLE.

ASSIGN 
       FILL-IN_BEST:POPUP-MENU IN FRAME FRAME-BARABREDD       = MENU POPUP-MENU-FILL-IN_BEST:HANDLE
       FILL-IN_BEST:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_PAGAENDE IN FRAME FRAME-BARABREDD
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN_PAGAENDE:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

ASSIGN 
       FILL-IN_STARTDAG:POPUP-MENU IN FRAME FRAME-BARABREDD       = MENU POPUP-MENU-FILL-IN_STARTDAG:HANDLE.

/* SETTINGS FOR FILL-IN FILL-IN_UTFARDAT IN FRAME FRAME-BARABREDD
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN_UTFARDAT:POPUP-MENU IN FRAME FRAME-BARABREDD       = MENU POPUP-MENU-FILL-IN_UTFARDAT:HANDLE.

/* SETTINGS FOR FRAME FRAME-BEH
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_ANVOMR TEXT-4 FRAME-BEH */
ASSIGN 
       FRAME FRAME-BEH:HIDDEN           = TRUE.

ASSIGN 
       BRW_ANVOMR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BEH = TRUE.

ASSIGN 
       BTN_BEHORAND:HIDDEN IN FRAME FRAME-BEH           = TRUE.

/* SETTINGS FOR FRAME FRAME-BERPROJ
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_PROJ TEXT-6 FRAME-BERPROJ */
/* BROWSE-TAB BRW_BERED BRW_PROJ FRAME-BERPROJ */
/* BROWSE-TAB BRW_ARBAN BRW_BERED FRAME-BERPROJ */
ASSIGN 
       FRAME FRAME-BERPROJ:HIDDEN           = TRUE.

ASSIGN 
       BRW_ARBAN:HIDDEN  IN FRAME FRAME-BERPROJ                = TRUE
       BRW_ARBAN:MAX-DATA-GUESS IN FRAME FRAME-BERPROJ         = 300
       BRW_ARBAN:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BERPROJ = TRUE
       BRW_ARBAN:COLUMN-RESIZABLE IN FRAME FRAME-BERPROJ       = TRUE.

ASSIGN 
       BRW_BERED:HIDDEN  IN FRAME FRAME-BERPROJ                = TRUE
       BRW_BERED:MAX-DATA-GUESS IN FRAME FRAME-BERPROJ         = 300
       BRW_BERED:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BERPROJ = TRUE
       BRW_BERED:COLUMN-RESIZABLE IN FRAME FRAME-BERPROJ       = TRUE.

ASSIGN 
       BRW_PROJ:HIDDEN  IN FRAME FRAME-BERPROJ                = TRUE
       BRW_PROJ:MAX-DATA-GUESS IN FRAME FRAME-BERPROJ         = 300
       BRW_PROJ:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BERPROJ = TRUE
       BRW_PROJ:COLUMN-RESIZABLE IN FRAME FRAME-BERPROJ       = TRUE.

/* SETTINGS FOR FRAME FRAME-BEST
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_BESTAVD CMB_BESTAVD FRAME-BEST */
/* BROWSE-TAB BRW_BEST BRW_BESTAVD FRAME-BEST */
ASSIGN 
       FRAME FRAME-BEST:HIDDEN           = TRUE.

ASSIGN 
       BRW_BEST:HIDDEN  IN FRAME FRAME-BEST                = TRUE
       BRW_BEST:POPUP-MENU IN FRAME FRAME-BEST             = MENU POPUP-MENU-BRW_BEST:HANDLE
       BRW_BEST:MAX-DATA-GUESS IN FRAME FRAME-BEST         = 300
       BRW_BEST:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BEST = TRUE
       BRW_BEST:COLUMN-RESIZABLE IN FRAME FRAME-BEST       = TRUE.

ASSIGN 
       BRW_BESTAVD:HIDDEN  IN FRAME FRAME-BEST                = TRUE
       BRW_BESTAVD:POPUP-MENU IN FRAME FRAME-BEST             = MENU POPUP-MENU-BRW_BEST-2:HANDLE
       BRW_BESTAVD:MAX-DATA-GUESS IN FRAME FRAME-BEST         = 300
       BRW_BESTAVD:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BEST = TRUE
       BRW_BESTAVD:COLUMN-RESIZABLE IN FRAME FRAME-BEST       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_BESTAVD IN FRAME FRAME-BEST
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-VALJBEST IN FRAME FRAME-BEST
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-VALJBEST:READ-ONLY IN FRAME FRAME-BEST        = TRUE.

/* SETTINGS FOR FRAME FRAME-DOK
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AVTALDOK TEXT-12 FRAME-DOK */
ASSIGN 
       FRAME FRAME-DOK:HIDDEN           = TRUE.

ASSIGN 
       BRW_AVTALDOK:HIDDEN  IN FRAME FRAME-DOK                = TRUE
       BRW_AVTALDOK:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-DOK = TRUE
       BRW_AVTALDOK:COLUMN-RESIZABLE IN FRAME FRAME-DOK       = TRUE.

ASSIGN 
       BTN_ANDADOK:HIDDEN IN FRAME FRAME-DOK           = TRUE.

/* SETTINGS FOR BUTTON BTN_BDOCTIDDOK IN FRAME FRAME-DOK
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BDOCTIDDOK:HIDDEN IN FRAME FRAME-DOK           = TRUE.

/* SETTINGS FOR BUTTON BTN_MAPPDOK IN FRAME FRAME-DOK
   NO-ENABLE                                                            */
ASSIGN 
       BTN_MAPPDOK:HIDDEN IN FRAME FRAME-DOK           = TRUE.

ASSIGN 
       BTN_NYADOK:HIDDEN IN FRAME FRAME-DOK           = TRUE.

ASSIGN 
       BTN_VISADOK:HIDDEN IN FRAME FRAME-DOK           = TRUE.

/* SETTINGS FOR FRAME FRAME-FAKT
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_ANV TEXT-5 FRAME-FAKT */
ASSIGN 
       FRAME FRAME-FAKT:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FAKTNR IN FRAME FRAME-FAKT
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_FAKTNR:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FAKTTYP IN FRAME FRAME-FAKT
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FAKTTYP:HIDDEN IN FRAME FRAME-FAKT           = TRUE.

/* SETTINGS FOR FRAME FRAME-KONTO
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AONRKTO TEXT-7 FRAME-KONTO */
/* BROWSE-TAB BRW_ARBART FILL-IN_PKOD FRAME-KONTO */
/* BROWSE-TAB BRW_PRIO BRW_ARBART FRAME-KONTO */
ASSIGN 
       FRAME FRAME-KONTO:HIDDEN           = TRUE.

ASSIGN 
       BRW_AONRKTO:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KONTO = TRUE
       BRW_AONRKTO:COLUMN-RESIZABLE IN FRAME FRAME-KONTO       = TRUE.

ASSIGN 
       BRW_ARBART:HIDDEN  IN FRAME FRAME-KONTO                = TRUE
       BRW_ARBART:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KONTO = TRUE
       BRW_ARBART:COLUMN-RESIZABLE IN FRAME FRAME-KONTO       = TRUE.

ASSIGN 
       BRW_PRIO:HIDDEN  IN FRAME FRAME-KONTO                = TRUE
       BRW_PRIO:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KONTO = TRUE
       BRW_PRIO:COLUMN-RESIZABLE IN FRAME FRAME-KONTO       = TRUE.

/* SETTINGS FOR FRAME FRAME-KOPP
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KOPP BTN_UPPKALK FRAME-KOPP */
ASSIGN 
       FRAME FRAME-KOPP:HIDDEN           = TRUE.

ASSIGN 
       BRW_KOPP:COLUMN-RESIZABLE IN FRAME FRAME-KOPP       = TRUE.

ASSIGN 
       BTN_AKTIV:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_BERKALK:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_FLISTAFAK:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_GATILL:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_KALKAR:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_KALKKALK:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_KONVKALK:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_KOPPLAAONR:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_PRELBFAK:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_SLAIHOPKALK:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_UPPKALK:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

ASSIGN 
       BTN_VISKALK:HIDDEN IN FRAME FRAME-KOPP           = TRUE.

/* SETTINGS FOR FRAME FRAME-OVRIGT
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_PRIOOV FILL-IN_ANLNR FRAME-OVRIGT */
/* BROWSE-TAB BRW_ARBARTOV BRW_PRIOOV FRAME-OVRIGT */
/* BROWSE-TAB BRW_ANL BRW_ARBARTOV FRAME-OVRIGT */
ASSIGN 
       FRAME FRAME-OVRIGT:HIDDEN           = TRUE.

ASSIGN 
       BRW_ANL:HIDDEN  IN FRAME FRAME-OVRIGT                = TRUE
       BRW_ANL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-OVRIGT = TRUE
       BRW_ANL:COLUMN-RESIZABLE IN FRAME FRAME-OVRIGT       = TRUE.

ASSIGN 
       BRW_ARBARTOV:HIDDEN  IN FRAME FRAME-OVRIGT                = TRUE
       BRW_ARBARTOV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-OVRIGT = TRUE
       BRW_ARBARTOV:COLUMN-RESIZABLE IN FRAME FRAME-OVRIGT       = TRUE.

ASSIGN 
       BRW_PRIOOV:HIDDEN  IN FRAME FRAME-OVRIGT                = TRUE
       BRW_PRIOOV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-OVRIGT = TRUE
       BRW_PRIOOV:COLUMN-RESIZABLE IN FRAME FRAME-OVRIGT       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DOKU IN FRAME FRAME-OVRIGT
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-DOKU:HIDDEN IN FRAME FRAME-OVRIGT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FORDEL IN FRAME FRAME-OVRIGT
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-FORDEL:HIDDEN IN FRAME FRAME-OVRIGT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MONTBER IN FRAME FRAME-OVRIGT
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-MONTBER:HIDDEN IN FRAME FRAME-OVRIGT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PROJEKTLED IN FRAME FRAME-OVRIGT
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-PROJEKTLED:HIDDEN IN FRAME FRAME-OVRIGT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PROJEKTOR IN FRAME FRAME-OVRIGT
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-PROJEKTOR:HIDDEN IN FRAME FRAME-OVRIGT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_TRAKTAMENTE IN FRAME FRAME-OVRIGT
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_TRAKTAMENTE:HIDDEN IN FRAME FRAME-OVRIGT           = TRUE.

/* SETTINGS FOR FRAME FRAME-PLIGG
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-PLIGG:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-TIDL
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AOTID TEXT-10 FRAME-TIDL */
ASSIGN 
       FRAME FRAME-TIDL:HIDDEN           = TRUE.

ASSIGN 
       BRW_AOTID:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-TIDL = TRUE
       BRW_AOTID:COLUMN-RESIZABLE IN FRAME FRAME-TIDL       = TRUE.

/* SETTINGS FOR BUTTON BTN_ANMTIDL IN FRAME FRAME-TIDL
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ANMTIDL:HIDDEN IN FRAME FRAME-TIDL           = TRUE.

/* SETTINGS FOR BUTTON BTN_BESTTIDL IN FRAME FRAME-TIDL
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BESTTIDL:HIDDEN IN FRAME FRAME-TIDL           = TRUE.

ASSIGN 
       BTN_GATILLTIDL:HIDDEN IN FRAME FRAME-TIDL           = TRUE.

/* SETTINGS FOR FRAME FRAME-TIDPERS
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_TIDPAONR TEXT-11 FRAME-TIDPERS */
ASSIGN 
       FRAME FRAME-TIDPERS:HIDDEN           = TRUE.

ASSIGN 
       BRW_TIDPAONR:HIDDEN  IN FRAME FRAME-TIDPERS                = TRUE
       BRW_TIDPAONR:MAX-DATA-GUESS IN FRAME FRAME-TIDPERS         = 300
       BRW_TIDPAONR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-TIDPERS = TRUE
       BRW_TIDPAONR:COLUMN-RESIZABLE IN FRAME FRAME-TIDPERS       = TRUE.

ASSIGN 
       BTN_GATILLTP:HIDDEN IN FRAME FRAME-TIDPERS           = TRUE.

/* SETTINGS FOR FRAME FRAME-TILLAVTAL
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AVTAL BTN_MINDAT1HAND FRAME-TILLAVTAL */
/* BROWSE-TAB BRW_HAND BRW_AVTAL FRAME-TILLAVTAL */
ASSIGN 
       FRAME FRAME-TILLAVTAL:HIDDEN           = TRUE.

ASSIGN 
       BRW_AVTAL:HIDDEN  IN FRAME FRAME-TILLAVTAL                = TRUE
       BRW_AVTAL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-TILLAVTAL = TRUE
       BRW_AVTAL:COLUMN-RESIZABLE IN FRAME FRAME-TILLAVTAL       = TRUE.

ASSIGN 
       FILL-IN-STARTDATHAND:HIDDEN IN FRAME FRAME-TILLAVTAL           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANL
/* Query rebuild information for BROWSE BRW_ANL
     _TblList          = "Temp-Tables.anlaggtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.anlaggtemp.ANLNR
"anlaggtemp.ANLNR" "Anl.nr" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.anlaggtemp.BENAMNING
"anlaggtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "34.88" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Ansvarig" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANVOMR
/* Query rebuild information for BROWSE BRW_ANVOMR
     _TblList          = "Temp-Tables.omranvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omranvandartemp.ANVANDARE
"omranvandartemp.ANVANDARE" "Användare" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.omranvandartemp.AV-NAMN
"omranvandartemp.AV-NAMN" "Användarnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "34.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.omranvandartemp.PERSONALKOD
"omranvandartemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANVOMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONRKTO
/* Query rebuild information for BROWSE BRW_AONRKTO
     _TblList          = "Temp-Tables.aonrkonttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aonrkonttemp.K1
"aonrkonttemp.K1" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aonrkonttemp.K2
"aonrkonttemp.K2" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aonrkonttemp.K3
"aonrkonttemp.K3" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.aonrkonttemp.K4
"aonrkonttemp.K4" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.aonrkonttemp.K5
"aonrkonttemp.K5" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.aonrkonttemp.SATS%
"aonrkonttemp.SATS%" "Sats%" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONRKTO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AOTID
/* Query rebuild information for BROWSE BRW_AOTID
     _TblList          = "Temp-Tables.aotidslagtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aotidslagtemp.TIDLAGE
"aotidslagtemp.TIDLAGE" "Tidläge" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aotidslagtemp.AKTIVITET1
"aotidslagtemp.AKTIVITET1" "Aktivitet" "X(256)" "character" ? 4 ? ? 4 ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aotidslagtemp.DAT1
"aotidslagtemp.DAT1" ? ? "date" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.aotidslagtemp.ANVANDARE1
"aotidslagtemp.ANVANDARE1" "Ändrad av" "x(256)" "character" ? 4 ? ? 4 ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.aotidslagtemp.AKTIVITET2
"aotidslagtemp.AKTIVITET2" "Aktivitet" "X(256)" "character" ? 3 ? ? 3 ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.aotidslagtemp.DAT2
"aotidslagtemp.DAT2" ? ? "date" ? 3 ? ? 3 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.aotidslagtemp.ANVANDARE2
"aotidslagtemp.ANVANDARE2" "Ändrad av" "x(256)" "character" ? 3 ? ? 3 ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AOTID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ARBAN
/* Query rebuild information for BROWSE BRW_ARBAN
     _TblList          = "Temp-Tables.ansvaraotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ansvaraotemp.PERSONALKOD
"ansvaraotemp.PERSONALKOD" "Enhet/!Sign" "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ansvaraotemp.FORNAMN
"ansvaraotemp.FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "11.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ansvaraotemp.EFTERNAMN
"ansvaraotemp.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ARBAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ARBART
/* Query rebuild information for BROWSE BRW_ARBART
     _TblList          = "Temp-Tables.arbarttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.arbarttemp.ARBARTKOD
"arbarttemp.ARBARTKOD" "Arbetsart" ? "integer" ? ? ? ? ? ? no ? no no "9.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.arbarttemp.ARBBENAMNING
"arbarttemp.ARBBENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ARBART */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ARBARTOV
/* Query rebuild information for BROWSE BRW_ARBARTOV
     _TblList          = "Temp-Tables.arbarttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.arbarttemp.ARBARTKOD
"arbarttemp.ARBARTKOD" "Arbetsart" ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.arbarttemp.ARBBENAMNING
"arbarttemp.ARBBENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ARBARTOV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVTAL
/* Query rebuild information for BROWSE BRW_AVTAL
     _TblList          = "Temp-Tables.avtalaonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avtalaonrtemp.AVTAL
"avtalaonrtemp.AVTAL" "Tillstånd/Avtal" "X(256)" "character" ? ? ? ? ? ? no ? no no "18.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.avtalaonrtemp.BEN
"avtalaonrtemp.BEN" "Egen text" "X(256)" "character" ? ? ? ? ? ? no ? no no "28.63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.avtalaonrtemp.KLR
"avtalaonrtemp.KLR" ? ? "date" ? ? ? ? ? ? no ? no no "6.88" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.avtalaonrtemp.FIL
"avtalaonrtemp.FIL" "Söknamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "32" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVTAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVTALDOK
/* Query rebuild information for BROWSE BRW_AVTALDOK
     _TblList          = "Temp-Tables.avtalaonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avtalaonrtemp.AVTAL
"avtalaonrtemp.AVTAL" "Dokument" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.avtalaonrtemp.BEN
"avtalaonrtemp.BEN" "Egen text" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.avtalaonrtemp.FIL
"avtalaonrtemp.FIL" "Sökväg" "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVTALDOK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BERED
/* Query rebuild information for BROWSE BRW_BERED
     _TblList          = "Temp-Tables.beratemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.beratemp.PERSONALKOD
"beratemp.PERSONALKOD" "Enhet/!Sign" "x(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.beratemp.FORNAMN
"beratemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "11.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.beratemp.EFTERNAMN
"beratemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BERED */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEST
/* Query rebuild information for BROWSE BRW_BEST
     _TblList          = "Temp-Tables.bestkundallt"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bestkundallt.VIBESTID
"bestkundallt.VIBESTID" "bestid" "X(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.bestkundallt.BESTNAMN
"bestkundallt.BESTNAMN" "Namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "47" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BESTAVD
/* Query rebuild information for BROWSE BRW_BESTAVD
     _TblList          = "Temp-Tables.bestkundextra"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bestkundextra.VIBESTID
"bestkundextra.VIBESTID" "bestid" "X(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.bestkundextra.BESTNAMN
"bestkundextra.BESTNAMN" "Namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "47" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BESTAVD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DAGBOK
/* Query rebuild information for BROWSE BRW_DAGBOK
     _TblList          = "Temp-Tables.dagboktemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.dagboktemp.DAGBOKAO
"dagboktemp.DAGBOKAO" "Dagbok" "X(256)" "character" ? ? ? ? ? ? no ? no no "21.75" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.dagboktemp.KILOMETER
"dagboktemp.KILOMETER" "Km" ? "integer" ? ? ? ? ? ? no ? no no "3.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.dagboktemp.TIMMAR
"dagboktemp.TIMMAR" "Timmar" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no "6.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.dagboktemp.DATUM
"dagboktemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.dagboktemp.ANVANDARE
"dagboktemp.ANVANDARE" "Inlagd av" "X(15)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DAGBOK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HAND
/* Query rebuild information for BROWSE BRW_HAND
     _TblList          = "Temp-Tables.avtaodattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avtaodattemp.DATUM
"avtaodattemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no "8.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.avtaodattemp.ANMARK
"avtaodattemp.ANMARK" "Händelse" "X(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HAND */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOPP
/* Query rebuild information for BROWSE BRW_KOPP
     _TblList          = "Temp-Tables.gatill"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.gatill.F1
"gatill.F1" "Typ av koppling" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.gatill.F2
"gatill.F2" "Nummer" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.gatill.F3
"gatill.F3" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "33.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.gatill.STATUSNIV
"gatill.STATUSNIV" "Status" "X(3)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" "x(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no "12.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRIO
/* Query rebuild information for BROWSE BRW_PRIO
     _TblList          = "Temp-Tables.priotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.priotemp.PKOD
"priotemp.PKOD" "Prioritet" ? "integer" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.priotemp.P-BENAMNING
"priotemp.P-BENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.priotemp.OMRADE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PRIO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRIOOV
/* Query rebuild information for BROWSE BRW_PRIOOV
     _TblList          = "Temp-Tables.priotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.priotemp.PKOD
"priotemp.PKOD" "Prioritet" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.priotemp.P-BENAMNING
"priotemp.P-BENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.priotemp.OMRADE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PRIOOV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PROJ
/* Query rebuild information for BROWSE BRW_PROJ
     _TblList          = "Temp-Tables.projtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.projtemp.PERSONALKOD
"projtemp.PERSONALKOD" "Enhet/!Sign" "x(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.projtemp.FORNAMN
"projtemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "11.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.projtemp.EFTERNAMN
"projtemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PROJ */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDPAONR
/* Query rebuild information for BROWSE BRW_TIDPAONR
     _TblList          = "Temp-Tables.aonrtidperstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aonrtidperstemp.PERSONALKOD
"aonrtidperstemp.PERSONALKOD" "Enhet!/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aonrtidperstemp.FORNAMN
"aonrtidperstemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aonrtidperstemp.EFTERNAMN
"aonrtidperstemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.aonrtidperstemp.ANVANDARE
"aonrtidperstemp.ANVANDARE" "Upplagd av" "X(9)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.aonrtidperstemp.DATUM
"aonrtidperstemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TIDPAONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ANM
/* Query rebuild information for FRAME FRAME-ANM
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ANM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-AONR
/* Query rebuild information for FRAME FRAME-AONR
     _Query            is NOT OPENED
*/  /* FRAME FRAME-AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BEH
/* Query rebuild information for FRAME FRAME-BEH
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BEH */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BERPROJ
/* Query rebuild information for FRAME FRAME-BERPROJ
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BERPROJ */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BEST
/* Query rebuild information for FRAME FRAME-BEST
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-DOK
/* Query rebuild information for FRAME FRAME-DOK
     _Query            is NOT OPENED
*/  /* FRAME FRAME-DOK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-FAKT
/* Query rebuild information for FRAME FRAME-FAKT
     _Query            is NOT OPENED
*/  /* FRAME FRAME-FAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KONTO
/* Query rebuild information for FRAME FRAME-KONTO
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KONTO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KOPP
/* Query rebuild information for FRAME FRAME-KOPP
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KOPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-OVRIGT
/* Query rebuild information for FRAME FRAME-OVRIGT
     _Query            is NOT OPENED
*/  /* FRAME FRAME-OVRIGT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-PLIGG
/* Query rebuild information for FRAME FRAME-PLIGG
     _Query            is NOT OPENED
*/  /* FRAME FRAME-PLIGG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDL
/* Query rebuild information for FRAME FRAME-TIDL
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDPERS
/* Query rebuild information for FRAME FRAME-TIDPERS
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDPERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TILLAVTAL
/* Query rebuild information for FRAME FRAME-TILLAVTAL
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TILLAVTAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Guru-Projekthuvud */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   slutaejnu = FALSE.
   RUN btnavb_UI IN aonrMh.
   IF slutaejnu = TRUE THEN DO:
      slutaejnu = FALSE.
      RETURN NO-APPLY.
   END.
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Guru-Projekthuvud */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANL
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   
   RUN avsluta_UI.   
   RUN disable_UI.
END.   
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   mainvar = TRUE.
   &Scoped-define FRAME-NAME FRAME-AONR
 
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I} 
  
   FILL-IN_ORT:LABEL = Guru.Konstanter:gaonamnk.   
   CMB_BESTAVD = "Alla".
  
   nyttsparatao = FALSE.   
   EMPTY TEMP-TABLE uppaonrtemp NO-ERROR. 
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE valdaao THEN DO:
      CREATE uppaonrtemp.
      BUFFER-COPY valdaao TO uppaonrtemp.
   END.
   IF NOT AVAILABLE uppaonrtemp THEN DO:
      IF aonrvar = "" OR aonrvar = ? THEN musz = musz.
      ELSE DO:
         RUN hamtuppaonr IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,INPUT-OUTPUT TABLE uppaonrtemp).
         FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
         uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.       
      END.
   END.
   RUN laddaao IN nyttaoapph (INPUT aonrvar,
                              INPUT delnrvar,
                              INPUT Guru.Konstanter:globforetag,
                              INPUT nyttaovar,
                              INPUT kalkrec,
                              INPUT Guru.Konstanter:globanv,
                              OUTPUT TABLE anlaggtemp,
                              OUTPUT TABLE aonrkonttemp,
                              OUTPUT TABLE ansvaraotemp,
                              OUTPUT TABLE aotidslagtemp,
                              OUTPUT TABLE aotidkopp,
                              OUTPUT TABLE arbarttemp,
                              OUTPUT TABLE avtalaonrtemp,
                              OUTPUT TABLE priotemp,
                              OUTPUT TABLE automregtemp,
                              OUTPUT TABLE beredningtemp,                               
                              OUTPUT TABLE varderingtemp,
                              OUTPUT TABLE fastspectemp,
                              OUTPUT TABLE kbenamntemp,
                              OUTPUT TABLE anvandartemp,
                              OUTPUT TABLE beratemp,
                              INPUT-OUTPUT TABLE uppaonrtemp).
   Guru.Konstanter:StoppaOmrSek(TEMP-TABLE ansvaraotemp:HANDLE).
   Guru.Konstanter:StoppaOmrSek(TEMP-TABLE beratemp:HANDLE).                                                       
   FILL-IN_ARBARTKOD:LABEL = Guru.Konstanter:gartk.
   FILL-IN_ARBARTKODOV:LABEL = Guru.Konstanter:gartk.
   BRW_ARBARTOV:TITLE = "Välj " + Guru.Konstanter:gartk.
   BRW_ARBART:TITLE = "Välj " + Guru.Konstanter:gartk.
   FILL-IN_PKODOV:LABEL = Guru.Konstanter:gpriok.
   FILL-IN_PKOD:LABEL = Guru.Konstanter:gpriok.  
   priotemp.PKOD:LABEL IN BROWSE BRW_PRIOOV = Guru.Konstanter:gpriok.
   priotemp.PKOD:LABEL IN BROWSE BRW_PRIO = Guru.Konstanter:gpriok.
   BRW_PRIOOV:TITLE = "Välj " + Guru.Konstanter:gpriok.
   BRW_PRIO:TITLE = "Välj " + Guru.Konstanter:gpriok.
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   ASSIGN
   aotidslagtemp.TIDLAGE:LABEL IN BROWSE BRW_AOTID = Guru.Konstanter:gtidlk.
   &Scoped-define BROWSE-NAME BRW_AOTID
   BRW_AOTID:TITLE = "Valda " + LC(Guru.Konstanter:gtidlk).
   FILL-IN_UTFARDAT:LABEL = CAPS(SUBSTRING(Guru.Konstanter:gutfardk,1,1)) + LC(SUBSTRING(Guru.Konstanter:gutfardk,2)).
   &Scoped-define FRAME-NAME FRAME-BARABREDD
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}   
   RUN huvud_UI.
   IF musz = TRUE THEN DO:
      IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
         Guru.GlobalaVariabler:retvalkoll = FALSE.
      END.
      LEAVE MAIN-BLOCK.
   END.
   ASSIGN
   arbbspar = BRW_ARBART:COLUMN   
   arbfspar = FILL-IN_ARBARTKOD:COLUMN.
   IF vartpro = "NAOPLAN" THEN DO:
      IF nyttaovar = TRUE AND plannrvar NE ? THEN DO:
         IF AVAILABLE uppaonrtemp THEN DO:
            RUN plankontohmt_UI IN nyttaoapph (INPUT plannrvar,INPUT artalvar,INPUT aonrvar,
                                            INPUT delnrvar,OUTPUT TABLE aonrkonttemp,
                                            INPUT-OUTPUT TABLE uppaonrtemp).
            FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
            uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
            IF AVAILABLE uppaonrtemp THEN DO:
               IF uppaonrtemp.ANM[1] NE "" THEN DO:
                  ASSIGN EDITOR_ANM = uppaonrtemp.ANM[1].
                  DISPLAY EDITOR_ANM WITH FRAME FRAME-ANM.
               END.
            END.
         END.         
      END.
   END.    
   
   IF musz = TRUE THEN DO:
      IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
         Guru.GlobalaVariabler:retvalkoll = FALSE.
      END.
      LEAVE MAIN-BLOCK.
   END.
   
   RUN dagbokhmt IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,OUTPUT TABLE dagboktemp).
   
   RUN enable_UI.
 
    {FRMSIZEF.I}  
   RUN btnstart_UI. 
   RUN menyrad_UI.
   RUN main2_UI.  
   
   BTN_IMPBER:LABEL = "Import beredning Excel".
   /*aonrsund*/
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "cELPA" THEN DO:  
      IF plannrvar = ? AND nyttaovar = TRUE  THEN DO:
         /*HÄR SELNÄT  ej elnät .....*/
         IF Guru.Konstanter:varforetypval[45] = 0 THEN DO:
           /* IF aonrvar BEGINS "38" OR aonrvar BEGINS "36" OR aonrvar BEGINS "46" OR aonrvar BEGINS "63" OR aonrvar BEGINS "79" OR aonrvar BEGINS "39" OR aonrvar BEGINS "47"
            OR aonrvar BEGINS "30" THEN DO:
            IF aonrvar BEGINS "30" OR aonrvar BEGINS "47"     THEN DO:*/
            
            RUN kollstyr_UI IN nyttaoapph (INPUT aonrvar, OUTPUT styrserie ).            
            IF styrserie = TRUE THEN DO:   
               /*slut på nummer i 46 övergå till 47 Lena 20180611*/
               /*slut på nummer i 39 övergå till 30 Lena 20190627*/
               FIND FIRST omrtemp WHERE omrtemp.OMRADE = "1611" USE-INDEX OMR NO-LOCK NO-ERROR.
               IF AVAILABLE omrtemp THEN DO:
                  ASSIGN  CMB_OMR:SCREEN-VALUE IN FRAME FRAME-BARABREDD = omrtemp.NAMN.
                  FILL-IN_BEST = omrtemp.NAMN.
               END. 
               RUN autokonto_UI IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR).                        
               EMPTY TEMP-TABLE aonrkonttemp NO-ERROR. 
               RUN konthmt IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,INPUT-OUTPUT TABLE aonrkonttemp).
            END.
            ELSE DO:                  
               status-ok = CMB_OMR:ADD-FIRST(" ") IN FRAME FRAME-BARABREDD.
               ASSIGN 
               CMB_OMR:SCREEN-VALUE IN FRAME FRAME-BARABREDD = " "
               FILL-IN_BEST = " " .
            END.
            DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.
         END.   
         ELSE DO:
            status-ok = CMB_OMR:ADD-FIRST(" ") IN FRAME FRAME-BARABREDD.
            ASSIGN 
            CMB_OMR:SCREEN-VALUE IN FRAME FRAME-BARABREDD = " "
            FILL-IN_BEST = " " .
            DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.            
         END.            
         APPLY "ENTRY" TO CMB_OMR.
      END.        
   END.
   IF  Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:  
      IF plannrvar = ? AND nyttaovar = TRUE  THEN DO:        
         status-ok = CMB_OMR:ADD-FIRST(" ") IN FRAME FRAME-BARABREDD.
         ASSIGN 
         CMB_OMR:SCREEN-VALUE IN FRAME FRAME-BARABREDD = " "
         FILL-IN_BEST = " " .
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.         
         APPLY "ENTRY" TO CMB_OMR.
      END.        
   END.
   IF  Guru.Konstanter:varforetypval[45] = 1 THEN DO:
      /*HÄR SELNÄT*/      
      FILL-IN_BEST = " " .
      FILL-IN_BEST:HIDDEN = TRUE.
         
   END.   
   IF nyttaovar = FALSE THEN DO:
      IF CMB_OMR = "" AND FILL-IN_BEST = " " THEN
      DISABLE FILL-IN_BEST CMB_OMR WITH FRAME {&FRAME-NAME}.       
   END.
   
    Guru.GlobalaVariabler:BtnOkh = BTN_OK:HANDLE. 
    Guru.GlobalaVariabler:BtnAvbh = BTN_AVB:HANDLE.    
   {WIN_M_SLUT.I}   
   {KRYSSBORT.I}
   IF Guru.Konstanter:globforetag = "snat" AND Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN  ENABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
   ELSE IF Guru.Konstanter:globforetag = "lule" AND Guru.Konstanter:globanv = "sög" THEN  ENABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
   ELSE IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN  ENABLE FILL-IN_BEST CMB_OMR WITH FRAME FRAME-BARABREDD.   
  IF Guru.Konstanter:globforetag = "ORBI" OR Guru.Konstanter:globforetag = "CGETB" THEN DO:
      ASSIGN
      FILL-IN-NAMN-1:HIDDEN = TRUE
      FILL-IN-NAMN-3:HIDDEN = TRUE
      FILL-IN_STARTDAG:HIDDEN = TRUE
      FILL-IN_ARBANSVARIG:HIDDEN = TRUE.
   END.     
   IF NOT THIS-PROCEDURE:PERSISTENT THEN    
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
   
   
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN BORTAOAPP.P PERSISTENT SET bortaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN BORTAOAPP.P PERSISTENT SET bortaoapph. 
   END.
   IF Guru.Konstanter:appcon THEN DO:     
      /*FAKTFOR*/
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      /*FAKTFOR*/
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
  
   IF Guru.Konstanter:varforetypval[21] = 1 THEN DO:
      RUN DOKHANT.P PERSISTENT SET nyttaoejapph.
   END.
   ELSE DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN DOKHANTAPP.P PERSISTENT SET nyttaoejapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN DOKHANTAPP.P PERSISTENT SET nyttaoejapph.
      END.   
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN AVTAODATAPP.P PERSISTENT SET avtaodatapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN AVTAODATAPP.P PERSISTENT SET avtaodatapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN VISAAPP.P PERSISTENT SET visaaonrh ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:varforetypchar[48]). 
   END.
   ELSE DO:
      RUN VISAAPP.P PERSISTENT SET visaaonrh (INPUT Guru.Konstanter:varforetypchar[48]).
   END.   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aokoll_UI C-Win 
PROCEDURE aokoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF FILL-IN_AONR = ? THEN DO:
      MESSAGE Guru.Konstanter:gaok + " kan inte vara okänt!" VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN NO-APPLY.
   END.
   IF FILL-IN_AONR = "" THEN DO:
      MESSAGE Guru.Konstanter:gaol + " kan inte vara blankt!" VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN NO-APPLY. 
   END.
   IF nyttaovar = TRUE THEN DO:
      RUN aokoll IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT aonrrec,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      musz = musz.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         musz = TRUE.
         RETURN NO-APPLY.
      END.
      MESSAGE  "Du har valt " Guru.Konstanter:gaok  fill-in_aonr  
      SKIP     "är detta rätt ?      "
      SKIP     "  "
      SKIP     "Obs !! Detta går ej att ändra senare."
      SKIP     "  "   
      SKIP     "Om det ändå blir fel kontakta Elpool tel 090/184540"    
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
      CASE val1:
         WHEN TRUE THEN DO:
            musz = FALSE.              
            RETURN.
         END.
         WHEN FALSE THEN DO:
            musz = TRUE. 
         END.      
      END CASE.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI C-Win 
PROCEDURE avb_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                     
   IF nyttaovar = TRUE AND nyttsparatao = TRUE THEN DO: 
      APPLY "CHOOSE" TO BTN_OK IN FRAME FRAME-AONR.
   END.
   ELSE DO:
       
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
      IF svar THEN DO:      
         APPLY "CHOOSE" TO BTN_OK IN FRAME FRAME-AONR.
      END.
      ELSE IF NOT svar THEN DO:  
         ASSIGN
         aonrvar2 =  aonrvar 
         delnrvar2 = delnrvar.
         {muswait.i}            
         IF nyttaovar = TRUE THEN DO:
            ASSIGN
            mappvar = ""
            mappvarhj = "".
            mappvar = uppaonrtemp.AONR + "000".                       
            IF grundmappvar NE "" THEN DO:
               mappvarhj = grundmappvar + mappvar.
               RUN delmappvar IN nyttaoejapph (INPUT mappvarhj).               
            END.
         END.
         musz = TRUE.
         {BESTALLTTEMPBORT.I}     
         RUN avsluta_UI.
         {musarrow.i}
         APPLY "END-ERROR":U TO SELF.         
      END.
      ELSE DO:
         slutaejnu = TRUE.
      END.
   END.                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsluta_UI C-Win 
PROCEDURE avsluta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF VALID-HANDLE(aonrMh) THEN DO:
      RUN borthand_UI IN aonrMh.
      DELETE PROCEDURE aonrMh NO-ERROR.      
   END.
   IF VALID-HANDLE(nyttaoapph) then DELETE PROCEDURE nyttaoapph.
   IF VALID-HANDLE(bortaoapph) THEN DELETE PROCEDURE bortaoapph.      
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph.
           
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph NO-ERROR.
      nyttaoapph = ?.
   END.      
   
   IF VALID-HANDLE(visaaonrh) THEN DELETE PROCEDURE visaaonrh.
   IF VALID-HANDLE(nyttaoejapph) THEN DELETE PROCEDURE nyttaoejapph NO-ERROR.
   APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btngatill_UI C-Win 
PROCEDURE btngatill_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   MESSAGE "fixa" VIEW-AS ALERT-BOX.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnnya_UI C-Win 
PROCEDURE btnnya_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "fix2" VIEW-AS ALERT-BOX.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI C-Win 
PROCEDURE btnok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
   DEFINE INPUT  PARAMETER kontokoll AS LOGICAL NO-UNDO.
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE uppaonrtemp  THEN DO:   
      ASSIGN
      omradespar = uppaonrtemp.OMRADE
      bestspar = uppaonrtemp.BESTID.   
   END.
   ELSE DO:
      ASSIGN
      omradespar = ""
      bestspar = "".   
   END.
   IF AVAILABLE uppaonrtemp THEN DO:
      ASSIGN
      musz = FALSE
      FILL-IN_AONRAVDATUM = INPUT FRAME FRAME-BARABREDD FILL-IN_AONRAVDATUM
      FILL-IN_BEST = INPUT FILL-IN_BEST
      CMB_DEBPR = INPUT CMB_DEBPR
      CMB_OMR = INPUT CMB_OMR
      CMB_FAK = INPUT FRAME FRAME-FAKT CMB_FAK
      FILL-IN_FASTAAONR = INPUT FILL-IN_FASTAAONR
      FILL-IN_ANLNR = INPUT FRAME FRAME-OVRIGT FILL-IN_ANLNR 
      FILL-IN_AONR = INPUT FILL-IN_AONR 
      FILL-IN_ARBANSVARIG = INPUT FILL-IN_ARBANSVARIG      
      FILL-IN_ARBARTKOD = INPUT FRAME FRAME-KONTO FILL-IN_ARBARTKOD
      FILL-IN_PKOD = INPUT FRAME FRAME-KONTO FILL-IN_PKOD
      EDITOR_AUPP = INPUT EDITOR_AUPP 
      EDITOR_ANM = INPUT FRAME FRAME-ANM EDITOR_ANM
      FILL-IN_BEREDARE = INPUT FILL-IN_BEREDARE
      FILL-IN_STARTDAG = INPUT FILL-IN_STARTDAG
      FILL-IN_DELNR = INPUT FILL-IN_DELNR
      FILL-IN_ORT = INPUT FILL-IN_ORT
      /*FILL-IN_PKODOV = INPUT FILL-IN_PKODOV */
      FILL-IN_TRAKTAMENTE = INPUT CMB_TRAK
      FILL-IN_UTRYCKNING = INPUT FILL-IN_UTRYCKNING
      FILL-IN_UTFARDAT = INPUT FILL-IN_UTFARDAT
      FILL-IN-KONAKT = INPUT FILL-IN-KONAKT
      FILL-IN-REF = INPUT FILL-IN-REF.      
      FILL-IN_ANVANDARE = INPUT FILL-IN_ANVANDARE.
      FILL-IN-MONTBER = INPUT FRAME FRAME-OVRIGT FILL-IN-MONTBER.
      FILL-IN-PROJEKTLED = INPUT FRAME FRAME-OVRIGT FILL-IN-PROJEKTLED.
      FILL-IN-PROJEKTOR = INPUT FRAME FRAME-OVRIGT FILL-IN-PROJEKTOR.
      FILL-IN-DOKU = INPUT FRAME FRAME-OVRIGT FILL-IN-DOKU.            
      IF FILL-IN_AONRAVDATUM:HIDDEN = TRUE THEN FILL-IN_AONRAVDATUM = 01/01/1991.
      IF uppaonrtemp.OMRADE = "" THEN .
      ELSE DO:
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
         NO-LOCK NO-ERROR.
         FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bestkundallt THEN DO:
            FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = CMB_OMR 
            NO-LOCK NO-ERROR.
         END. 
      END.
      /*aonrsund*/
      /*FAKTFOR*/
      musz = FALSE.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                       
         IF CMB_FAK = "Faktureras ej" THEN musz = musz.
         ELSE IF fbaonrvar  = FALSE THEN fbaonrvar = FALSE.
         ELSE DO:
            fbaonrvar = FALSE.
            FOR EACH gatill WHERE gatill.TYPCHAR = "FBAONR":
               IF gatill.F2 NE ? THEN musz = TRUE.
            END.
            IF musz = FALSE THEN DO:
            END.
         END.
         musz = FALSE.
      END.
      IF FILL-IN_ORT = "" THEN DO:
         MESSAGE "Ort/Benämning får inte vara blank!" VIEW-AS ALERT-BOX.            
         musz = TRUE.
         RETURN NO-APPLY.
      END.   
      IF FILL-IN_BEST = "" THEN DO:
         IF CMB_OMR NE "" THEN DO:
            MESSAGE Guru.Konstanter:gbestk "får inte vara blank!" VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.         
         END.
      END.      
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         IF FILL-IN_ORT = "" THEN DO:
            MESSAGE "Benämning är obligatorisk!" 
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.
         END.         
         IF Guru.Konstanter:varforetypval[45] = 0 THEN DO:
            FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR  NO-LOCK NO-ERROR.         
            IF AVAILABLE omrtemp AND omrtemp.OMRADE BEGINS "16" THEN DO:
               IF FILL-IN_ARBARTKOD = 1 THEN DO:
                  MESSAGE Guru.Konstanter:gartk " får inte vara 1"
                  VIEW-AS ALERT-BOX.            
                  musz = TRUE.
                  RETURN NO-APPLY.            
               END.
               FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.   
               IF FILL-IN_ARBARTKOD = 11 THEN DO:
                  FIND FIRST aonrkonttemp WHERE aonrkonttemp.K1 NE "1622" NO-LOCK NO-ERROR.
                  IF AVAILABLE aonrkonttemp THEN DO:
                     MESSAGE Guru.Konstanter:gartk " 11 måste ha " + kbenamntemp.K1 + " 1622"
                     VIEW-AS ALERT-BOX.            
                     musz = TRUE.
                     RETURN NO-APPLY.            
                  END.
               END.
               IF FILL-IN_ARBARTKOD = 12 THEN DO:
                  FIND FIRST aonrkonttemp WHERE aonrkonttemp.K1 NE "1623" NO-LOCK NO-ERROR.
                  IF AVAILABLE aonrkonttemp THEN DO:
                     MESSAGE Guru.Konstanter:gartk " 12 måste ha " + kbenamntemp.K1 + " 1623"
                     VIEW-AS ALERT-BOX.            
                     musz = TRUE.
                     RETURN NO-APPLY.            
                  END.
               END.
            END.
         END.
         IF Guru.Konstanter:varforetypval[45] = 1  THEN DO:
            /*HÄR SELNÄT*/
            FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR  NO-LOCK NO-ERROR.
            FIND FIRST priotemp WHERE priotemp.OMRADE = omrtemp.OMRADE NO-ERROR.
            IF AVAILABLE priotemp THEN DO:
               IF FILL-IN_PKOD:SCREEN-VALUE = "" OR FILL-IN_PKOD:SCREEN-VALUE = "?" OR FILL-IN_PKOD:SCREEN-VALUE = "0" THEN DO:                                    
                  MESSAGE Guru.Konstanter:gpriok " är obligatorisk."
                  VIEW-AS ALERT-BOX.            
                  musz = TRUE.
                  RETURN NO-APPLY.            
               END.
            END.   
         END.            
         IF AVAILABLE omrtemp THEN DO:
            FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
            IF AVAILABLE judavdtemp THEN DO:
               IF judavdtemp.JUDID = "SEAB" THEN DO:              
                  IF SUBSTRING(FILL-IN_AONR,1,1) NE "S" THEN DO:
                     MESSAGE Guru.Konstanter:gaok + " ska börja med ett- S" VIEW-AS ALERT-BOX.            
                     musz = TRUE.
                     RETURN NO-APPLY.            
                 END.
              END.
            END.         
         END.
      END.        
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:         
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
         NO-LOCK NO-ERROR.       

         IF AVAILABLE omrtemp AND omrtemp.OMRADE BEGINS "10" THEN DO:
            IF FILL-IN_ARBARTKOD = 1 THEN DO:
               MESSAGE Guru.Konstanter:gartk " får inte vara 1"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
            FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.   
            IF FILL-IN_ARBARTKOD = 32 THEN DO:
               FIND FIRST aonrkonttemp WHERE aonrkonttemp.K1 NE "016" NO-LOCK NO-ERROR.
               IF AVAILABLE aonrkonttemp THEN DO:
                  MESSAGE Guru.Konstanter:gartk " 32 måste ha " + kbenamntemp.K1 + " 016"
                  VIEW-AS ALERT-BOX.            
                  musz = TRUE.
                  RETURN NO-APPLY.            
               END.            
            END.
         END.         
         IF AVAILABLE omrtemp THEN DO:
            FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
            IF AVAILABLE judavdtemp THEN DO:
               IF judavdtemp.JUDID = "BYGG" THEN DO:              
                  IF SUBSTRING(FILL-IN_AONR,1,1) NE "S" THEN DO:
                     MESSAGE Guru.Konstanter:gaok + " ska börja med ett- S" VIEW-AS ALERT-BOX.            
                     musz = TRUE.
                     RETURN NO-APPLY.            
                 END.
              END.
            END.         
         END.
      END.
      IF Guru.Konstanter:globforetag = "CGRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:       
         kollref = INTEGER(FILL-IN-REF) NO-ERROR.
         IF FILL-IN-REF NE ""  THEN DO:
            IF LENGTH(FILL-IN-REF) = 6 OR LENGTH(FILL-IN-REF) = 7 THEN.
            ELSE DO:
               MESSAGE Guru.Konstanter:grefbefk " skall vara numeriskt 6 el 7 positioner!" 
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.         
            END.                     
         END.
         ELSE IF FILL-IN-REF NE "" AND kollref = 0 THEN DO:            
            MESSAGE Guru.Konstanter:grefbefk " skall vara numeriskt 6 el 7 positioner!" 
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.         
         END.
      END.
      IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
         IF uppaonrtemp.FAKTTYP = "Faktureras ej" THEN DO:
            MESSAGE "Faktureringstyp måste vara löpande räkning eller fast pris"
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.            
         END.
      END.
      RUN fbaonrkoll_UI IN aonrMh (OUTPUT musz). 
      IF musz = TRUE THEN RETURN.     
      IF CMB_DEBPR NE "TOT.PRIS." THEN DO:
         DEFINE VARIABLE val2 AS LOGICAL NO-UNDO.
         MESSAGE "Vill du att " LC(Guru.Konstanter:gdebk) " skall vara ? "  CMB_DEBPR skip
         "normalt brukar den vara 'TOT.PRIS.' !"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2.
         CASE val2:
            WHEN TRUE THEN DO:
               musz = musz.
            END.
            WHEN FALSE THEN DO:
               musz = TRUE.
               RETURN NO-APPLY.
            END.
         END CASE.
      END.      
      IF uppaonrtemp.DELNR = 0 THEN DO:   
         FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST NO-LOCK NO-ERROR.
         IF AVAILABLE bestkundallt THEN DO:
            IF Guru.Konstanter:varforetypval[11] = 0 THEN DO:
               IF nyttaovar = FALSE THEN DO:               
                  IF bestkundallt.BESTID NE bestspar THEN DO:
                     MESSAGE "Du har även ändrat på Beställare/Kund" SKIP 
                     "Detta innebär att alla " LC(Guru.Konstanter:gaok) " med samma huvudnummer också byter." SKIP
                     "Vill du ändå fortsätta ?"
                     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar1 AS LOGICAL.
                     CASE svar1:
                        WHEN TRUE THEN DO:
                           RUN andrabestk_UI IN nyttaoapph (INPUT aonrvar,INPUT bestkundallt.BESTID).                                                      
                           ASSIGN uppaonrtemp.BESTID = bestkundallt.BESTID.
                           FOR EACH valdaao WHERE valdaao.AONR = aonrvar NO-LOCK:
                              ASSIGN valdaao.BESTID = bestkundallt.BESTID.
                           END.
                           FOR EACH utsokaonr WHERE utsokaonr.AONR = aonrvar NO-LOCK:
                              ASSIGN utsokaonr.BESTID = bestkundallt.BESTID.
                           END.
                           FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND
                           uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
                        END.
                        WHEN FALSE THEN DO:
                           musz = TRUE.    
                           RETURN NO-APPLY.
                        END.
                     END.
                  END.
               END.
            END.
         END.
      END.
      IF FILL-IN_FASTAAONR NE uppaonrtemp.FASTAAONR THEN DO:
         DEFINE VARIABLE val3 AS LOGICAL NO-UNDO.
         MESSAGE "Du byter " LC(Guru.Konstanter:gaok) ":ets tillhörighet mellan " Guru.Konstanter:gtillk " och " Guru.Konstanter:gfastk "." SKIP 
         "Detta innebär att alla " LC(Guru.Konstanter:gaok) " med samma huvudnummer också byter." SKIP 
         "Vill du ändå fortsätta ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3.
         CASE val3:
            WHEN TRUE THEN DO:
               RUN andrafast IN nyttaoapph (INPUT aonrvar,INPUT FILL-IN_FASTAAONR).
               OPEN QUERY vbq FOR EACH uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar NO-LOCK.
               GET FIRST vbq NO-LOCK.
               DO WHILE AVAILABLE(uppaonrtemp):
                  DO TRANSACTION:
                     GET CURRENT vbq NO-LOCK.
                     uppaonrtemp.FASTAAONR = FILL-IN_FASTAAONR.
                  END.
                  GET NEXT vbq NO-LOCK.
               END.
               FOR EACH valdaao WHERE valdaao.AONR = aonrvar NO-LOCK:
                  ASSIGN valdaao.FASTAAONR = FILL-IN_FASTAAONR.
               END.
               FOR EACH utsokaonr WHERE utsokaonr.AONR = aonrvar NO-LOCK:
                  ASSIGN utsokaonr.FASTAAONR = FILL-IN_FASTAAONR.
               END.
               
               FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
               uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
            END.
            WHEN FALSE THEN DO:
               musz = TRUE.
               RETURN NO-APPLY.          
            END.
         END CASE.
      END.
      RUN dagbokspar IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT TABLE dagboktemp).
   END.
   ELSE DO:
      MESSAGE "Det gick inte att spara!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   regdatum = TODAY.
   RUN REGVEC.P.
   RUN REGDAG.P.   
   IF FILL-IN_ANLNR = "" THEN DO:
      FIND FIRST anlaggtemp USE-INDEX ANLAGGNING NO-LOCK NO-ERROR.
      IF AVAILABLE anlaggtemp THEN ASSIGN FILL-IN_ANLNR = anlaggtemp.ANLNR.
   END.      
   IF FILL-IN_ARBARTKOD = ? OR FILL-IN_ARBARTKOD = 0 THEN DO:
      FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
      IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
   END.   
   IF FILL-IN_PKOD = ? OR FILL-IN_PKOD = 0 THEN DO:
      /*HÄR SELNÄT LENA*/       
      IF Guru.Konstanter:varforetypval[45] = 1 THEN.
      ELSE DO:
         FIND FIRST priotemp USE-INDEX PRI NO-LOCK NO-ERROR.
         IF AVAILABLE priotemp THEN ASSIGN FILL-IN_PKOD = priotemp.PKOD.
      END.   
   END.       
   IF nyttaovar = TRUE AND plannrvar NE ? THEN DO:
      RUN aonrkonspar_UI IN nyttaoapph (INPUT aonrrec,INPUT TABLE aonrkonttemp).
   END.
   
   IF AVAILABLE omrtemp THEN DO:
      IF kontokoll = TRUE THEN DO:
         RUN strangkoll IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT omrtemp.OMRADE,OUTPUT TABLE felmeddtemp).
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
            musz = TRUE.
            DELETE felmeddtemp.
            RETURN.
         END.
      END.  
   END.
   RUN kontrolln IN nyttaoapph (INPUT aonrrec,INPUT tidagamla,INPUT tidbgamla,
                INPUT-OUTPUT FILL-IN_BEREDARE,INPUT-OUTPUT FILL-IN_STARTDAG,
                INPUT-OUTPUT FILL-IN_ARBANSVARIG, 
                INPUT-OUTPUT TABLE beratemp, INPUT-OUTPUT TABLE ansvaraotemp,
                OUTPUT TABLE felmeddtemp).
   OPEN QUERY BRW_ARBAN FOR EACH ansvaraotemp NO-LOCK.
   OPEN QUERY BRW_BERED FOR EACH beratemp NO-LOCK.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 1 THEN DO:
         DELETE felmeddtemp.
         RUN AOKOPIA.W (OUTPUT TOG_BERKOP,
                     OUTPUT TOG_KALKOP,
                     OUTPUT TOG_KOPPKOP,OUTPUT TOG_TIDLKOP).   
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.
         END.
         
         RUN aokopia IN nyttaoapph (INPUT aonrrec,INPUT aonrrec2,
                                    INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,
                                    INPUT Guru.Konstanter:globforetag,INPUT vadgora,INPUT Guru.Konstanter:globanv,
                                    INPUT TOG_BERKOP,INPUT TOG_KALKOP,INPUT TOG_KOPPKOP,
                                    OUTPUT TABLE beredningtemp,OUTPUT TABLE aovardtemp,
                                    OUTPUT TABLE varderingtemp,OUTPUT TABLE felmeddtemp).
         
         RUN konthmt IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT-OUTPUT TABLE aonrkonttemp).
         IF TOG_TIDLKOP = TRUE THEN DO:
            RUN tidlkop_UI IN nyttaoapph (INPUT aonrrec2,INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR).
         END.
         FOR EACH felmeddtemp NO-LOCK :
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
            musz = TRUE.
            DELETE felmeddtemp.
         END.         
      END.
      ELSE DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         musz = TRUE.
         DELETE felmeddtemp.
         IF Guru.Konstanter:globforetag = "ORBI" OR Guru.Konstanter:globforetag = "CGETB" THEN DO:
            ASSIGN
            FILL-IN_STARTDAG:HIDDEN IN FRAME FRAME-BARABREDD = FALSE
            FILL-IN_ARBANSVARIG:HIDDEN IN FRAME FRAME-BARABREDD = FALSE.      
         END.  
         RETURN.
      END.
   END.
   FIND FIRST projtemp WHERE projtemp.PERSONALKOD = FILL-IN_STARTDAG NO-ERROR.
   IF NOT AVAILABLE projtemp THEN DO:
      RUN projextra IN nyttaoapph (INPUT FILL-IN_STARTDAG,OUTPUT TABLE projtemp APPEND).
   END.
   RUN projextra2 IN nyttaoapph (INPUT tidpgamla).
   OPEN QUERY BRW_PROJ FOR EACH projtemp NO-LOCK.                                  
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE uppaonrtemp THEN DO:
      
      IF Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GRAN" THEN DO:
         IF FILL-IN_ORT:MODIFIED = TRUE OR FILL-IN_AONRAVDATUM:MODIFIED = TRUE THEN DO:
            uppaonrtemp.AUTOREG = TRUE.
         END.
      END.
      ELSE DO:
         uppaonrtemp.AUTOREG = TRUE.
      END.
      ASSIGN
      uppaonrtemp.FASTAAONR = FILL-IN_FASTAAONR 
      uppaonrtemp.ANM[1] = EDITOR_ANM                
      uppaonrtemp.ANLNR = FILL-IN_ANLNR              
      uppaonrtemp.AONR = FILL-IN_AONR                
      uppaonrtemp.ARBANSVARIG = FILL-IN_ARBANSVARIG  
      uppaonrtemp.ARBARTKOD = FILL-IN_ARBARTKOD      
      uppaonrtemp.ARBUPPG[1] = EDITOR_AUPP           
      uppaonrtemp.BEREDARE = FILL-IN_BEREDARE        
      uppaonrtemp.STARTDAG = FILL-IN_STARTDAG        
      uppaonrtemp.DELNR = FILL-IN_DELNR              
      uppaonrtemp.ORT = FILL-IN_ORT                  
      uppaonrtemp.PKOD = FILL-IN_PKOD 
      uppaonrtemp.FAKTTYP = CMB_FAK  
      uppaonrtemp.PRISTYP = CMB_DEBPR                
      uppaonrtemp.TRAKTAMENTE = FILL-IN_TRAKTAMENTE  
      uppaonrtemp.UTRYCKNING = FILL-IN_UTRYCKNING    
      uppaonrtemp.AONRAVDATUM = FILL-IN_AONRAVDATUM  
      uppaonrtemp.UTFARDAT = FILL-IN_UTFARDAT
      uppaonrtemp.ANVANDARE = FILL-IN_ANVANDARE.
      IF AVAILABLE bestkundallt THEN uppaonrtemp.BESTID = bestkundallt.BESTID.          
      IF FILL-IN_UTFARDAT NE "" THEN uppaonrtemp.ARBBESKED = TRUE.
      ELSE uppaonrtemp.ARBBESKED = FALSE.     
      RUN projextra4 IN nyttaoapph (INPUT uppaonrtemp.AONR, INPUT uppaonrtemp.DELNR, INPUT FILL-IN-REF,INPUT FILL-IN-KONAKT).      
      IF AVAILABLE omrtemp THEN ASSIGN uppaonrtemp.OMRADE = omrtemp.OMRADE.
      ELSE IF uppaonrtemp.FASTAAONR = TRUE AND LENGTH(uppaonrtemp.AONR) = 3 THEN uppaonrtemp.OMRADE = "".
      ELSE IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "snat" OR Guru.Konstanter:globforetag = "misv") AND LENGTH(uppaonrtemp.AONR) =  3 THEN uppaonrtemp.OMRADE = "".       
      IF uppaonrtemp.AONRAVDATUM = ? THEN DO:
         FILL-IN_PAGAENDE:HIDDEN = FALSE.
      END.     
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      
      IF sparavdat = FILL-IN_AONRAVDATUM THEN FILL-IN_AONRAVDATUM:MODIFIED = FALSE.
      ELSE FILL-IN_AONRAVDATUM:MODIFIED = TRUE.
      CREATE valsoktemp.
      ASSIGN
      valsoktemp.SOKINT[1] = vadgora  
      valsoktemp.SOKINT[2] = aonrrec  
      valsoktemp.SOKINT[3] = aonrrec2 
      valsoktemp.SOKINT[4] = artalvar     
      valsoktemp.SOKCHAR[1] = Guru.Konstanter:globanv  
      valsoktemp.SOKCHAR[2] = FILL-IN_AONR 
      valsoktemp.SOKCHAR[3] = plannrvar
      valsoktemp.SOKCHAR[4] = omradespar
      valsoktemp.SOKCHAR[5] = tidagamla
      valsoktemp.SOKCHAR[7] = bestspar   
      valsoktemp.SOKLOG[1] = nyttaovar
      valsoktemp.SOKLOG[2] = FILL-IN_ORT:MODIFIED
      valsoktemp.SOKLOG[3] = FILL-IN_AONRAVDATUM:MODIFIED
      valsoktemp.SOKLOG[4] = TOG_BERKOP  
      valsoktemp.SOKLOG[5] = TOG_KALKOP  
      valsoktemp.SOKLOG[6] = TOG_KOPPKOP
      valsoktemp.SOKLOG[7] = TOG_TIDLKOP.
      {muswait.i}
      IF AVAILABLE bestkundallt THEN valsoktemp.SOKCHAR[6] = bestkundallt.BESTID.
      ELSE DO:
         valsoktemp.SOKCHAR[6] = bestspar.
      END.
      RUN btnokn IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,
                                INPUT TABLE valsoktemp,
                               INPUT-OUTPUT TABLE uppaonrtemp,
                               OUTPUT TABLE felmeddtemp).
      RUN relaonr IN nyttaoapph.
      FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
      uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
                 RETURN.
      END.
      ELSE DO:
         IF vartpro = "NAOPLAN" THEN musz = musz.
         ELSE DO:   
            FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
            valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valdaao THEN DO:
               CREATE valdaao.
            END.
            BUFFER-COPY uppaonrtemp TO valdaao.
            FIND FIRST utsokaonr WHERE utsokaonr.AONR = aonrvar AND 
            utsokaonr.DELNR = delnrvar NO-LOCK NO-ERROR.
            IF NOT AVAILABLE utsokaonr THEN DO:
               CREATE utsokaonr.
            END.
            BUFFER-COPY uppaonrtemp TO utsokaonr.
         END.
      END.
      nyttsparatao = TRUE.
      
   END.
   IF BTN_PAG:HIDDEN = FALSE THEN RUN vaonrfix IN nyttaoapph (INPUT-OUTPUT TABLE valdaao).
   ASSIGN
   aonrvar2 =  aonrvar 
   delnrvar2 = delnrvar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnstart_UI C-Win 
PROCEDURE btnstart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   
   IF NOT VALID-HANDLE(aonrMh) THEN DO:
      CREATE whandltemp.
      ordningnr = 0.   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,C-WIN:HANDLE).    
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-AONR:HANDLE).    
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BARABREDD:HANDLE).    
      RUN whandle_UI (INPUT ordningnr,FILL-IN_AONR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_DELNR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-EDELNR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,BTN_PAG:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_AONRAVDATUM:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,MBTN_ANM:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_FASTAAONR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ORT:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,EDITOR_AUPP:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,CMB_OMR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_BEST:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,MBTN_AVT:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,CMB_DEBPR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-REF:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_STARTDAG:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_BEREDARE:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ARBANSVARIG:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_UTFARDAT:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,BTN_FASTTRAC:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,BTN_ACCESS:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,MBTN_DOK:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_FAK:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_KONT:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_KOPPL:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_OVR:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_TIDL:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_TIDPERS:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,BTN_OK:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,BTN_AVB:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_PAGAENDE:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NAMN-1:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NAMN-2:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NAMN-3:HANDLE IN FRAME FRAME-BARABREDD).   
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BERPROJ:HANDLE). 
      RUN whandle_UI (INPUT ordningnr,BRW_PROJ:HANDLE IN FRAME FRAME-BERPROJ). 
      RUN whandle_UI (INPUT ordningnr,BRW_BERED:HANDLE IN FRAME FRAME-BERPROJ). 
      RUN whandle_UI (INPUT ordningnr,BRW_ARBAN:HANDLE IN FRAME FRAME-BERPROJ).    
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BEST:HANDLE). 
      RUN whandle_UI (INPUT ordningnr,CMB_BESTAVD:HANDLE IN FRAME FRAME-BEST). 
      RUN whandle_UI (INPUT ordningnr,BRW_BESTAVD:HANDLE IN FRAME FRAME-BEST). 
      RUN whandle_UI (INPUT ordningnr,BRW_BEST:HANDLE IN FRAME FRAME-BEST). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-VALJBEST:HANDLE IN FRAME FRAME-BEST). 
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Info:HANDLE IN MENU POPUP-MENU-BRW_BEST).                                                      
      RUN whandle_UI (INPUT ordningnr, MENU-ITEM m_Info-2:HANDLE IN MENU POPUP-MENU-BRW_BEST-2).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-KOPP:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_KOPP:HANDLE IN FRAME FRAME-KOPP). 
      RUN whandle_UI (INPUT ordningnr,BTN_KALKAR:HANDLE IN FRAME FRAME-KOPP).  
      RUN whandle_UI (INPUT ordningnr,BTN_AKTIV:HANDLE IN FRAME FRAME-KOPP).  
      RUN whandle_UI (INPUT ordningnr,BTN_KOPPLAAONR:HANDLE IN FRAME FRAME-KOPP). 
      RUN whandle_UI (INPUT ordningnr,BTN_GATILL:HANDLE IN FRAME FRAME-KOPP). 
      RUN whandle_UI (INPUT ordningnr,gatill.STATUSNIV:HANDLE IN BROWSE BRW_KOPP). 
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-FAKT:HANDLE).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_FAKTNR:HANDLE IN FRAME FRAME-FAKT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_FAKTTYP:HANDLE IN FRAME FRAME-FAKT).    
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ANVANDARE:HANDLE IN FRAME FRAME-FAKT).  
      RUN whandle_UI (INPUT ordningnr,CMB_FAK:HANDLE IN FRAME FRAME-FAKT).    
      RUN whandle_UI (INPUT ordningnr,FILL-IN_INTERNT:HANDLE IN FRAME FRAME-FAKT).    
      RUN whandle_UI (INPUT ordningnr,FILL-IN_EXTERNT:HANDLE IN FRAME FRAME-FAKT).    
      RUN whandle_UI (INPUT ordningnr,BRW_ANV:HANDLE IN FRAME FRAME-FAKT).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-TILLAVTAL:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_AVTAL:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_MAPPTV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_VISATV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_NYATV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_ANDTV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_BDOCTIDTV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_NYHAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_ANDHAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_BORTHAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BRW_HAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_UPDAT1HAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-STARTDATHAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,TOG_KLAR:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_MINDAT1HAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-TIDPERS:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_TIDPAONR:HANDLE IN FRAME FRAME-TIDPERS).
      RUN whandle_UI (INPUT ordningnr,BTN_GATILLTP:HANDLE IN FRAME FRAME-TIDPERS).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-TIDL:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_AOTID:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_KOPPTIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_ANMTIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_BESTTIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_VISATIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_GATILLTIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_BORTATIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-DOK:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-ANM:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-OVRIGT:HANDLE).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_TRAKTAMENTE:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,CMB_TRAK:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_UTRYCKNING:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_PKODOV:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ARBARTKODOV:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ANLNR:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,BRW_PRIOOV:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,BRW_ARBARTOV:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,BRW_ANL:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-FORDEL:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-MONTBER:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-PROJEKTLED:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-PROJEKTOR:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-DOKU:HANDLE IN FRAME FRAME-OVRIGT).      
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-KONTO:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_AONRKTO:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ARBARTKOD:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,BRW_ARBART:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,BTN_GATILLKONTO:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,BRW_PRIO:HANDLE IN FRAME FRAME-KONTO).  
      RUN whandle_UI (INPUT ordningnr,FILL-IN_PKOD:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-DOK:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_AVTALDOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_MAPPDOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_VISADOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_NYADOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_ANDADOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_BDOCTIDDOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-ANM:HANDLE).
      RUN whandle_UI (INPUT ordningnr,EDITOR_ANM:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BRW_DAGBOK:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-ecopy:HANDLE IN FRAME FRAME-ANM).      
      RUN whandle_UI (INPUT ordningnr,BTN_GATILLANM:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BTN_NYDAGBOK:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BTN_ANDDAGBOK:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BTN_BORTDAGBOK:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BRW_PERS:HANDLE IN FRAME FRAME-BARABREDD).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Vlj_Personal:HANDLE IN MENU POPUP-MENU-FILL-IN_STARTDAG).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Vlj_Personal2:HANDLE IN MENU POPUP-MENU-FILL-IN_BEREDARE).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Vlj_Personal3:HANDLE IN MENU POPUP-MENU-FILL-IN_ARBANSVARIG).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Kund_information:HANDLE IN MENU POPUP-MENU-FILL-IN_BEST).   
      RUN whandle_UI (INPUT ordningnr,FILL-IN-KONAKT:HANDLE IN FRAME FRAME-BARABREDD).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Kund_information2:HANDLE IN MENU POPUP-MENU-FILL-IN-KONAKT).   
      RUN whandle_UI (INPUT ordningnr,BTN_VISADAGBOK:HANDLE IN FRAME FRAME-ANM). 
      RUN whandle_UI (INPUT ordningnr,BTN_UPPKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_KALKKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VISKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_KONVKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_UPPBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_BERBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_INKBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_INKBERDIREKT:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_LASBERDIREKT:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_LISTBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_ATGBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_LASBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_EXPBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_FAKFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_FLISTAFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_KredFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_PRELBFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_UPPFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VFAKFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VISAOFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_KOPPFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_UPPMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_FASTIGHETMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VARDMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VISKALMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VISMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_OMRAKNA:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_EXPMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Referens_personer:HANDLE IN MENU POPUP-MENU-FILL-IN_UTFARDAT).   
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BEH:HANDLE). 
      RUN whandle_UI (INPUT ordningnr, MBTN_AOPERSSEK:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr, BRW_ANVOMR:HANDLE IN FRAME FRAME-BEH). 
      RUN whandle_UI (INPUT ordningnr, BTN_BEHORAND:HANDLE IN FRAME FRAME-BEH). 
      RUN whandle_UI (INPUT ordningnr, BTN_SCHAKTP:HANDLE IN FRAME FRAME-KOPP).      
      RUN whandle_UI (INPUT ordningnr, BTN_BERKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr, CMB_DOKTYP:HANDLE IN FRAME FRAME-KOPP). /*DDD*/
      RUN whandle_UI (INPUT ordningnr, CMB_DOKSTATUS:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr, BTN_KOPIBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr, BTN_IMPBER:HANDLE IN FRAME FRAME-KOPP).  /*endast lule*/
      RUN whandle_UI (INPUT ordningnr, BTN_IMPORTBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr, MBTN_PLIGG:HANDLE IN FRAME FRAME-AONR).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-PLIGG:HANDLE).      
      RUN whandle_UI (INPUT ordningnr, BTN_JMFBERINK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr, BTN_SLAIHOPKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN AONRM.P PERSISTENT SET aonrMh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp). 
   END.
   IF FILL-IN_AONRAVDATUM = 01/01/1991 THEN FILL-IN_AONRAVDATUM:HIDDEN = TRUE.
   RUN main_UI IN aonrMh. 
   RUN frame_UI (INPUT "AONR").
   RUN frame_UI (INPUT "KOPP").
   RUN vcbrwkopp_UI IN aonrMh.
   IF vartpro = "NAOPLAN" AND nyttaovar = TRUE THEN DO:
      FRAME FRAME-KOPP:HIDDEN = TRUE.
      DISABLE MBTN_DOK MBTN_FAK MBTN_KOPPL MBTN_TIDL MBTN_TIDPERS WITH FRAME FRAME-AONR.
      ASSIGN
      BRW_DAGBOK:HIDDEN IN FRAME FRAME-ANM = TRUE                  
      BTN_GATILLANM:HIDDEN IN FRAME FRAME-ANM = TRUE                   
      BTN_NYDAGBOK:HIDDEN IN FRAME FRAME-ANM = TRUE                   
      BTN_ANDDAGBOK:HIDDEN IN FRAME FRAME-ANM = TRUE                   
      BTN_BORTDAGBOK:HIDDEN IN FRAME FRAME-ANM = TRUE
      BTN_GATILLKONTO:HIDDEN IN FRAME FRAME-KONTO = TRUE.
   END.
      
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvisa_UI C-Win 
PROCEDURE btnvisa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "fix3" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buttplace_UI C-Win 
PROCEDURE buttplace_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MBTN_ANM:HIDDEN IN FRAME FRAME-AONR = FALSE.
   IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
      MBTN_KONT:HIDDEN IN FRAME FRAME-AONR = FALSE.
   END.
   IF (Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4) OR Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "PINN" THEN DO: 
      MBTN_FAK:HIDDEN IN FRAME FRAME-AONR = FALSE.
   END.
   IF grundmappvar NE "" THEN DO:
      MBTN_DOK:HIDDEN IN FRAME FRAME-AONR = FALSE.
      MBTN_AVT:HIDDEN IN FRAME FRAME-AONR = FALSE.
   END.  
   IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:  
      MBTN_TIDPERS:HIDDEN IN FRAME FRAME-AONR = FALSE.
   END.
   MBTN_OVR:HIDDEN IN FRAME FRAME-AONR = FALSE.
   IF Guru.Konstanter:varforetypchar[4] NE "" THEN DO:
      IF omrpersanvok = TRUE OR Guru.Konstanter:globniv = 0 THEN DO:
         MBTN_AOPERSSEK:HIDDEN IN FRAME FRAME-AONR = FALSE. 
      END.
      ELSE MBTN_AOPERSSEK:HIDDEN IN FRAME FRAME-AONR = TRUE.
   END.
   ELSE MBTN_AOPERSSEK:HIDDEN IN FRAME FRAME-AONR = TRUE.
   
   IF Guru.Konstanter:varforetypchar[9] = "1" THEN DO:      
      MBTN_PLIGG:HIDDEN IN FRAME FRAME-AONR = FALSE. 
   END.
   ELSE MBTN_PLIGG:HIDDEN IN FRAME FRAME-AONR = TRUE.
   RUN PlaceraKnapp_UI IN aonrMh.  

   
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
  ENABLE RECT-MENY MBTN_AOPERSSEK BTN_OK MBTN_PLIGG BTN_AVB MBTN_KOPPL MBTN_ANM 
         MBTN_AVT MBTN_DOK MBTN_FAK MBTN_KONT MBTN_OVR MBTN_TIDL MBTN_TIDPERS 
      WITH FRAME FRAME-AONR IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-AONR}
  DISPLAY FILL-IN_AONR FILL-IN_DELNR FILL-IN_AONRAVDATUM FILL-IN_FASTAAONR 
          FILL-IN_ORT CMB_OMR EDITOR_AUPP FILL-IN_BEST CMB_DEBPR FILL-IN-REF 
          FILL-IN-KONAKT FILL-IN_STARTDAG FILL-IN_BEREDARE FILL-IN_ARBANSVARIG 
          FILL-IN_UTFARDAT FILL-IN-NAMN-1 FILL-IN-NAMN-2 FILL-IN-NAMN-3 
          FILL-IN_PAGAENDE 
      WITH FRAME FRAME-BARABREDD IN WINDOW C-Win.
  ENABLE FILL-IN_AONR BTN_PAG FILL-IN_FASTAAONR BRW_PERS FILL-IN_ORT 
         EDITOR_AUPP FILL-IN_BEST CMB_DEBPR FILL-IN_STARTDAG FILL-IN_BEREDARE 
         FILL-IN_ARBANSVARIG FILL-IN_UTFARDAT BTN_FASTTRAC BTN_ACCESS 
         FILL-IN-NAMN-1 FILL-IN-NAMN-2 FILL-IN-NAMN-3 FILL-IN_PAGAENDE 
      WITH FRAME FRAME-BARABREDD IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BARABREDD}
  DISPLAY FILL-IN-ecopy EDITOR_ANM 
      WITH FRAME FRAME-ANM IN WINDOW C-Win.
  ENABLE FILL-IN-ecopy EDITOR_ANM BRW_DAGBOK BTN_GATILLANM BTN_NYDAGBOK 
         BTN_ANDDAGBOK BTN_BORTDAGBOK BTN_VISADAGBOK 
      WITH FRAME FRAME-ANM IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANM}
  ENABLE BRW_ANVOMR BTN_BEHORAND 
      WITH FRAME FRAME-BEH IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BEH}
  ENABLE BRW_PROJ BRW_BERED BRW_ARBAN 
      WITH FRAME FRAME-BERPROJ IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BERPROJ}
  DISPLAY CMB_BESTAVD FILL-IN-VALJBEST 
      WITH FRAME FRAME-BEST IN WINDOW C-Win.
  ENABLE CMB_BESTAVD BRW_BESTAVD BRW_BEST 
      WITH FRAME FRAME-BEST IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BEST}
  ENABLE BRW_AVTALDOK BTN_VISADOK BTN_NYADOK BTN_ANDADOK 
      WITH FRAME FRAME-DOK IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-DOK}
  DISPLAY FILL-IN_FAKTNR FILL-IN_ANVANDARE CMB_FAK FILL-IN_INTERNT 
          FILL-IN_EXTERNT 
      WITH FRAME FRAME-FAKT IN WINDOW C-Win.
  ENABLE BRW_ANV FILL-IN_ANVANDARE CMB_FAK FILL-IN_INTERNT FILL-IN_EXTERNT 
      WITH FRAME FRAME-FAKT IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-FAKT}
  DISPLAY FILL-IN_ARBARTKOD FILL-IN_PKOD 
      WITH FRAME FRAME-KONTO IN WINDOW C-Win.
  ENABLE BRW_AONRKTO FILL-IN_ARBARTKOD FILL-IN_PKOD BRW_ARBART BRW_PRIO 
         BTN_GATILLKONTO 
      WITH FRAME FRAME-KONTO IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KONTO}
  DISPLAY CMB_DOKTYP CMB_DOKSTATUS 
      WITH FRAME FRAME-KOPP IN WINDOW C-Win.
  ENABLE BTN_IMPORTBER BTN_IMPBER BTN_UPPKALK BRW_KOPP BTN_GATILL BTN_UPPFAK 
         BTN_KOPPFAK BTN_UPPBER BTN_KALKKALK BTN_SLAIHOPKALK BTN_KOPPLAAONR 
         BTN_BERKALK BTN_VISAOFAK BTN_SCHAKTP BTN_BERBER BTN_JMFBERINK 
         BTN_VISKALK BTN_UPPMARK BTN_KOPIBER BTN_AKTIV BTN_INKBER 
         BTN_INKBERDIREKT BTN_FAKFAK BTN_FASTIGHETMARK BTN_KONVKALK BTN_LISTBER 
         BTN_PRELBFAK BTN_VARDMARK BTN_ATGBER BTN_KALKAR BTN_VISKALMARK 
         BTN_VFAKFAK BTN_LASBERDIREKT BTN_LASBER BTN_VISMARK BTN_KredFAK 
         BTN_EXPBER BTN_OMRAKNA BTN_FLISTAFAK BTN_EXPMARK CMB_DOKTYP 
         CMB_DOKSTATUS 
      WITH FRAME FRAME-KOPP IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KOPP}
  DISPLAY CMB_TRAK FILL-IN_UTRYCKNING FILL-IN_PKODOV FILL-IN_ARBARTKODOV 
          FILL-IN_ANLNR 
      WITH FRAME FRAME-OVRIGT IN WINDOW C-Win.
  ENABLE FILL-IN-MONTBER FILL-IN-PROJEKTOR FILL-IN-PROJEKTLED FILL-IN-DOKU 
         CMB_TRAK FILL-IN_UTRYCKNING FILL-IN_PKODOV FILL-IN_ARBARTKODOV 
         FILL-IN_ANLNR BRW_PRIOOV BRW_ARBARTOV BRW_ANL FILL-IN-FORDEL 
      WITH FRAME FRAME-OVRIGT IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-OVRIGT}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PLIGG}
  DISPLAY FILL-IN-DOK FILL-IN-ANM 
      WITH FRAME FRAME-TIDL IN WINDOW C-Win.
  ENABLE BRW_AOTID BTN_VISATIDL BTN_KOPPTIDL BTN_BORTATIDL BTN_GATILLTIDL 
         FILL-IN-DOK FILL-IN-ANM 
      WITH FRAME FRAME-TIDL IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDL}
  ENABLE BRW_TIDPAONR BTN_GATILLTP 
      WITH FRAME FRAME-TIDPERS IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDPERS}
  DISPLAY FILL-IN-STARTDATHAND TOG_KLAR 
      WITH FRAME FRAME-TILLAVTAL IN WINDOW C-Win.
  ENABLE BTN_UPDAT1HAND FILL-IN-STARTDATHAND TOG_KLAR BTN_MINDAT1HAND BRW_AVTAL 
         BRW_HAND BTN_MAPPTV BTN_VISATV BTN_NYATV BTN_ANDTV BTN_BDOCTIDTV 
         BTN_NYHAND BTN_ANDHAND BTN_BORTHAND 
      WITH FRAME FRAME-TILLAVTAL IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TILLAVTAL}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryarban_UI C-Win 
PROCEDURE entryarban_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN frame_UI (INPUT "BERP").
   ASSIGN
   FILL-IN_ARBANSVARIG = INPUT FRAME FRAME-BARABREDD FILL-IN_ARBANSVARIG.
   FILL-IN-NAMN-3 = INPUT FILL-IN-NAMN-3.
   
   IF Guru.Konstanter:globforetag = "ORBI" OR Guru.Konstanter:globforetag = "CGETB" THEN DO:
      ASSIGN
      BRW_PROJ:HIDDEN IN FRAME FRAME-BERPROJ = TRUE 
      BRW_ARBAN:HIDDEN IN FRAME FRAME-BERPROJ = TRUE.         
   END.   
   ELSE DISPLAY FILL-IN_ARBANSVARIG FILL-IN-NAMN-3 WITH FRAME FRAME-BARABREDD.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixaStorlek_UI C-Win 
PROCEDURE FixaStorlek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frameh_UI C-Win 
PROCEDURE frameh_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS LOGICAL NO-UNDO. 
   ASSIGN
   FRAME FRAME-TILLAVTAL:HIDDEN = vilkenframe
   FRAME FRAME-TIDPERS:HIDDEN = vilkenframe
   FRAME FRAME-TIDL:HIDDEN = vilkenframe
   FRAME FRAME-OVRIGT:HIDDEN = vilkenframe
   FRAME FRAME-KONTO:HIDDEN = vilkenframe
   FRAME FRAME-KOPP:HIDDEN = vilkenframe
   FRAME FRAME-DOK:HIDDEN = vilkenframe
   FRAME FRAME-BEST:HIDDEN = vilkenframe
   FRAME FRAME-BERPROJ:HIDDEN = vilkenframe
   FRAME FRAME-ANM:HIDDEN = vilkenframe
   FRAME FRAME-FAKT:HIDDEN = vilkenframe
   FRAME FRAME-BEH:HIDDEN = vilkenframe
   FRAME FRAME-PLIGG:HIDDEN = vilkenframe.  
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
   RUN frameh_UI (INPUT TRUE).
   IF vilkenframe = "AONR" THEN DO:
      ASSIGN
      FRAME FRAME-BARABREDD:HIDDEN = FALSE
      FRAME FRAME-AONR:HIDDEN = FALSE.
   END.
   IF vilkenframe = "TAVT" THEN FRAME FRAME-TILLAVTAL:HIDDEN = FALSE.
   IF vilkenframe = "TPERS" THEN FRAME FRAME-TIDPERS:HIDDEN = FALSE.
   IF vilkenframe = "TIDL" THEN FRAME FRAME-TIDL:HIDDEN = FALSE.
   IF vilkenframe = "OVR" THEN FRAME FRAME-OVRIGT:HIDDEN = FALSE.
   IF vilkenframe = "KONT" THEN FRAME FRAME-KONTO:HIDDEN = FALSE.
   IF vilkenframe = "KOPP" THEN FRAME FRAME-KOPP:HIDDEN = FALSE.
   IF vilkenframe = "DOK" THEN FRAME FRAME-DOK:HIDDEN = FALSE.
   IF vilkenframe = "ANM" THEN FRAME FRAME-ANM:HIDDEN = FALSE.
   IF vilkenframe = "FAKT" THEN FRAME FRAME-FAKT:HIDDEN = FALSE.
   IF vilkenframe = "BEST" THEN FRAME FRAME-BEST:HIDDEN = FALSE.
   IF vilkenframe = "BERP" THEN FRAME FRAME-BERPROJ:HIDDEN = FALSE.
   IF vilkenframe = "BEH" THEN FRAME FRAME-BEH:HIDDEN = FALSE.
   IF vilkenframe = "PLI" THEN FRAME FRAME-PLIGG:HIDDEN = FALSE.
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
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "ORBI" OR Guru.Konstanter:globforetag = "GRAN" {GLOBVES.I} OR Guru.Konstanter:globforetag = "CGETB" THEN DO:
      DISPLAY FILL-IN_UTFARDAT WITH FRAME FRAME-BARABREDD.
      ENABLE FILL-IN_UTFARDAT WITH FRAME FRAME-BARABREDD.
            
   END.
   ELSE DO:
      ASSIGN FILL-IN_UTFARDAT:HIDDEN = TRUE.
      DISABLE FILL-IN_UTFARDAT WITH FRAME FRAME-BARABREDD.      
   END.
   /*dokument*/        
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "TRAS" OR 
      Guru.Konstanter:globforetag = "PINN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      ENABLE BTN_BDOCTIDDOK BTN_VISADOK BTN_MAPPDOK WITH FRAME FRAME-DOK.
      ENABLE BTN_ANMTIDL BTN_BESTTIDL WITH FRAME FRAME-TIDL.
   END.
   ELSE IF Guru.Konstanter:globforetag = "PELLE" {GLOBVES.I} THEN DO:
      ENABLE BTN_ANMTIDL BTN_BESTTIDL WITH FRAME FRAME-TIDL.
      ASSIGN
      BTN_KOPPTIDL:HIDDEN = TRUE     
      BTN_BORTATIDL:HIDDEN = TRUE 
      BTN_VISATIDL:HIDDEN = TRUE.       
   END.
   ELSE IF Guru.Konstanter:globforetag = "KRAF" THEN DO:
      ENABLE BTN_BDOCTIDDOK BTN_MAPPDOK WITH FRAME FRAME-DOK.
             
   END.
   /*cccc DISABLE CMB_OMR DÅ OMRÅDET REDAN ÄR VALT*/   
   IF nrvalvar = FALSE THEN DISABLE FILL-IN_AONR CMB_OMR WITH FRAME FRAME-BARABREDD.
   ELSE ENABLE FILL-IN_AONR CMB_OMR WITH FRAME FRAME-BARABREDD.
   DISABLE FILL-IN_AONR WITH FRAME FRAME-BARABREDD.
   
   BTN_PAG:HIDDEN = TRUE.
   IF Guru.Konstanter:aonrsekvar[9] = TRUE THEN DO:
      IF uppaonrtemp.AONRAVDATUM NE 01/01/91 THEN DO:
         IF Guru.Konstanter:globforetag = "krin" THEN DO:
            BTN_PAG:HIDDEN = FALSE.
            DISPLAY FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
            ASSIGN FILL-IN_PAGAENDE:HIDDEN = TRUE.
            IF Guru.Konstanter:aonrsekvar[9] = TRUE THEN DO:
               ENABLE FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
            END.
         END.   
         ELSE IF uppaonrtemp.DELNR NE 0 THEN DO:
            RUN avslutkoll_UI IN nyttaoapph (INPUT uppaonrtemp.AONR,OUTPUT musz).
            IF musz = TRUE THEN DO:
               musz = FALSE.
               BTN_PAG:HIDDEN = FALSE.
               DISPLAY FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
               ASSIGN FILL-IN_PAGAENDE:HIDDEN = TRUE.
               IF Guru.Konstanter:aonrsekvar[9] = TRUE THEN DO:
                  ENABLE FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
               END.     
            END.
         END.
         ELSE DO:
            BTN_PAG:HIDDEN = FALSE.
            DISPLAY FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
            ASSIGN FILL-IN_PAGAENDE:HIDDEN = TRUE.
            IF Guru.Konstanter:aonrsekvar[9] = TRUE THEN DO:
               ENABLE FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
            END.     
         END.
      END.
      ELSE BTN_PAG:HIDDEN = TRUE.
   END.
    
   IF uppaonrtemp.FASTAAONR = TRUE THEN DO:
      musz = musz.      
   END.        
   ELSE IF uppaonrtemp.FASTAAONR = FALSE THEN DO:
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
         DISABLE FILL-IN_FASTAAONR FILL-IN_AONR WITH FRAME FRAME-BARABREDD.
      END.       
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      DISABLE FILL-IN_FASTAAONR WITH FRAME FRAME-BARABREDD.         
   END.
   IF  Guru.Konstanter:globforetag = "CSNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      /*HÄR*/
      FILL-IN_AVDELNING:HIDDEN IN FRAME FRAME-BARABREDD = FALSE.         
   END.
   ELSE FILL-IN_AVDELNING:HIDDEN IN FRAME FRAME-BARABREDD = TRUE.
   
   IF uppaonrtemp.DELNR NE 0 THEN DO:
      IF Guru.Konstanter:globforetag = "KRIN" THEN.
      ELSE DISABLE FILL-IN_FASTAAONR WITH FRAME FRAME-BARABREDD.
   END.    
   IF nyttaovar = FALSE OR kopiavar = TRUE THEN DO:
      DISABLE FILL-IN_AONR WITH FRAME FRAME-BARABREDD.
   END.
   ASSIGN   
   CMB_DEBPR:LABEL = Guru.Konstanter:gdebk
   CMB_OMR = INPUT CMB_OMR. 
   IF Guru.Konstanter:varforetypval[2] = 1 THEN DO:
      ASSIGN      
      CMB_TRAK:HIDDEN IN FRAME FRAME-OVRIGT = TRUE 
      FILL-IN_UTRYCKNING:HIDDEN = TRUE.
   END.
   /* undernummer skall ha samma område och beställare som huvudnumret för tidskrivande företag*/      
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GRAN" 
    OR Guru.Konstanter:globforetag = "GKAL"   THEN DO:   
      IF FILL-IN_DELNR NE 0 THEN DO:      
         DISABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
         DISABLE  CMB_BESTAVD BRW_BEST BRW_BESTAVD WITH FRAME FRAME-BEST.                       
      END.   
   END.
   IF nyttaovar = FALSE THEN DO:      
      IF FILL-IN_DELNR = 0 THEN DO:      
         ENABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
      END.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "FORS" THEN DO:
         DISABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
      END.   
      ELSE IF Guru.Konstanter:varforetypval[2] = 1 THEN DO:
         /*ej tidskrivande företag skall kunna ändra område och beställare*/
         ENABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
         ENABLE  CMB_BESTAVD BRW_BEST BRW_BESTAVD WITH FRAME FRAME-BEST.                                    
      END.
      ELSE DO:
         RUN btnok IN bortaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_delnr,OUTPUT musz).
         IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN musz = FALSE.
         IF Guru.Konstanter:globforetag = "lule" AND Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN musz = FALSE.
         
         IF musz = TRUE THEN DISABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
         musz = FALSE.
      END.
      IF Guru.Konstanter:globforetag = "UMEA" THEN DISABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
      /* Om det finns en beredning får den ligga kvar på orginalområde
      men aonr får inte ha fakturaplan*/
      IF uppaonrtemp.FAKTNR NE 0 THEN DO:
         DISABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
         DISABLE CMB_BESTAVD BRW_BEST BRW_BESTAVD WITH FRAME FRAME-BEST.                                
      END.                  
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI C-Win 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   C-WIN:TITLE = "Guru-" + Guru.Konstanter:gaol.                  
   /*här nytt aonr*/ 
   IF nyttaovar = TRUE THEN DO:    
     /*
      CURRENT-WINDOW:HIDDEN = FALSE.
      */
      IF Guru.GlobalaVariabler:fonstercol > 0 THEN CURRENT-WINDOW:X = Guru.GlobalaVariabler:fonstercol.
     
      {NRAOSKAPA.I}         
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR 
      Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO: 
         nrvalvar = TRUE.
      END.
      ELSE DO:
         nrvalvar = FALSE.
      END.
      delnrvar = 0.      
      RUN hamtaonr IN nyttaoapph (INPUT aonrrec,OUTPUT aonrvar,OUTPUT delnrvar,
                                  OUTPUT omrvar,OUTPUT bestvar,OUTPUT radfast,
                                  OUTPUT autoregvar).
      ASSIGN
      aonrvar2 =  aonrvar 
      delnrvar2 = delnrvar.
      FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND
      uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE uppaonrtemp THEN DO:
         EMPTY TEMP-TABLE uppaonrtemp NO-ERROR. 
         CREATE uppaonrtemp.
         uppaonrtemp.PLANNR = ?.
      END.
      ASSIGN       
      uppaonrtemp.AONRAVDATUM = 01/01/1991
      uppaonrtemp.AONR = aonrvar
      uppaonrtemp.DELNR = delnrvar
      uppaonrtemp.OMRADE = omrvar
      uppaonrtemp.BESTID = bestvar
      uppaonrtemp.FASTAAONR = radfast
      uppaonrtemp.AUTOREG = autoregvar.
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:  
         IF plannrvar = ? AND nyttaovar = TRUE  THEN DO:
            /* HÄR SELNÄT LENA   j elnät   */
            IF Guru.Konstanter:varforetypval[45] = 0 THEN DO:
               /*IF aonrvar BEGINS "38" OR aonrvar BEGINS "36" OR aonrvar BEGINS "46" OR aonrvar BEGINS "63" OR aonrvar BEGINS "79" OR aonrvar BEGINS "39" OR aonrvar BEGINS "47"
                OR aonrvar BEGINS "30"  THEN DO:
               IF aonrvar BEGINS "30" OR aonrvar BEGINS "47" THEN DO:    
                   /*slut på nummer i 46 övergå till 47 Lena 20180611*/
                   /*slut på nummer i 39 övergå till 30 Lena 20190627*/*/
               RUN kollstyr_UI IN nyttaoapph (INPUT aonrvar, OUTPUT styrserie ).            
               IF styrserie = TRUE THEN DO:    
                   
                  FIND FIRST omrtemp WHERE omrtemp.OMRADE = "1611" USE-INDEX OMR NO-LOCK NO-ERROR.
                  IF AVAILABLE omrtemp THEN DO:
                     uppaonrtemp.OMRADE = omrtemp.OMRADE.               
                  END.                                 
               END.
            END.                                   
         END.        
      END.
      ASSIGN
      sparavdat = uppaonrtemp.AONRAVDATUM
      FILL-IN_AONRAVDATUM = uppaonrtemp.AONRAVDATUM
      FILL-IN_AONR = uppaonrtemp.AONR 
      FILL-IN_DELNR = uppaonrtemp.DELNR.       
      IF kopiavar = TRUE THEN DO:
         RUN AOKOPIA.W (OUTPUT TOG_BERKOP,
                        OUTPUT TOG_KALKOP,
                        OUTPUT TOG_KOPPKOP,OUTPUT TOG_TIDLKOP).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.
         END.
         RUN aokopia IN nyttaoapph (INPUT aonrrec,INPUT aonrrec2,
                                    INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,
                                    INPUT Guru.Konstanter:globforetag,INPUT vadgora,INPUT Guru.Konstanter:globanv,
                                    INPUT TOG_BERKOP,INPUT TOG_KALKOP,INPUT TOG_KOPPKOP,
                                    OUTPUT TABLE beredningtemp,OUTPUT TABLE aovardtemp,
                                    OUTPUT TABLE varderingtemp,OUTPUT TABLE felmeddtemp).
         
         RUN konthmt IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT-OUTPUT TABLE aonrkonttemp).
         IF TOG_TIDLKOP = TRUE THEN DO:
            RUN tidlkop_UI IN nyttaoapph (INPUT aonrrec2,INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR).
         END.
            
         RUN kopupp_UI IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,INPUT-OUTPUT TABLE uppaonrtemp).
         nyttaovar = FALSE.         
         FOR EACH felmeddtemp NO-LOCK :
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
            DELETE felmeddtemp.
         END.
      END.
   END.
   ELSE DO:
      FIND uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
      uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.             
   END.
   RUN hamtaorec IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   
   IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "MALDATUM").
      END.
      ELSE DO:
         RUN TLAGAUTO.P  
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "MALDATUM").                  
      END.    
      RUN laddaaotid IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE aotidslagtemp).
   END.
        
   RUN laddao_UI.    
   IF FILL-IN_UTFARDAT = "" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:
         IF anvandartemp.PERSONALKOD NE "" THEN DO: 
            FILL-IN_UTFARDAT = anvandartemp.AV-NAMN.
         END.      
      END.
   END.      
   RUN projextra3 IN nyttaoapph (INPUT uppaonrtemp.AONR, INPUT uppaonrtemp.DELNR, OUTPUT refvar,OUTPUT FILL-IN-KONAKT).   
   ASSIGN   
   FILL-IN-REF = refvar.  
   DISPLAY FILL-IN-REF FILL-IN-KONAKT WITH FRAME FRAME-BARABREDD.

   IF FILL-IN_ARBANSVARIG = "" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:
         IF anvandartemp.PERSONALKOD NE "" THEN DO: 
            FILL-IN_ARBANSVARIG = anvandartemp.PERSONALKOD.
            FILL-IN-NAMN-3 = anvandartemp.AV-NAMN.
         END.      
      END.
   END.
   IF FILL-IN_BEREDARE = "" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:
         IF anvandartemp.PERSONALKOD NE "" THEN DO: 
            FILL-IN_BEREDARE = anvandartemp.PERSONALKOD.
            FILL-IN-NAMN-2 = anvandartemp.AV-NAMN.
         END.      
      END.       
   END.  
   IF FILL-IN_STARTDAG = "" THEN DO:      
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:
         IF anvandartemp.PERSONALKOD NE "" THEN DO: 
            FILL-IN_STARTDAG = anvandartemp.PERSONALKOD.
            FILL-IN-NAMN-1 = anvandartemp.AV-NAMN.
         END.      
         ELSE FILL-IN_STARTDAG = FILL-IN_BEREDARE.
      END. 
      ELSE FILL-IN_STARTDAG = FILL-IN_BEREDARE.
   END.
   ELSE DO:
      IF FILL-IN_STARTDAG = "mån" OR 
      FILL-IN_STARTDAG = "tis" OR 
      FILL-IN_STARTDAG = "ons" OR 
      FILL-IN_STARTDAG = "tor" OR 
      FILL-IN_STARTDAG = "fre" OR 
      FILL-IN_STARTDAG = "lör" OR 
      FILL-IN_STARTDAG = "sön" THEN DO:
         FILL-IN_STARTDAG = FILL-IN_BEREDARE.
      END.
   END.
   IF kalkrec = ? THEN DO:
      musz = musz.
   END.   
   ELSE DO:
      FIND FIRST fastspectemp WHERE fastspectemp.FASTSPECREC = kalkrec NO-LOCK NO-ERROR.
   END.
   FIND FIRST bestkundallt NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:    
      bestvad = 2.
      {BESTHMT.I}
      /*HÄR EJ OM BESTÄLLAR EJ ÄR EGNA ORG = FAKT*/
      IF Guru.Konstanter:varforetypval[3] >= 1 OR vartpro = "NAOPLAN" THEN DO:

      END.
      ELSE DO:            
         OPEN QUERY oq FOR EACH omrtemp USE-INDEX OMR NO-LOCK.
         GET FIRST oq NO-LOCK.
         DO WHILE AVAILABLE(omrtemp):
            CREATE bestkundallt.
            ASSIGN 
            bestkundallt.VIBESTID = omrtemp.OMRADE 
            bestkundallt.BESTID = omrtemp.OMRADE 
            bestkundallt.BESTNAMN = omrtemp.NAMN.                      
            CREATE bestkundextra.
            ASSIGN 
            bestkundextra.VIBESTID = omrtemp.OMRADE 
            bestkundextra.BESTID = omrtemp.OMRADE 
            bestkundextra.BESTNAMN = omrtemp.NAMN
            bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
            GET NEXT oq NO-LOCK.
         END.           
      END.
   END.   
   /* lena tillägg 20130411 om man har varit in i register beställare har dessa bestkundallt tagits bort*/
   IF Guru.Konstanter:varforetypval[3] >= 1 OR vartpro = "NAOPLAN" THEN.
   ELSE DO:
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = uppaonrtemp.OMRADE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bestkundallt THEN DO:                 
         OPEN QUERY oq FOR EACH omrtemp USE-INDEX OMR NO-LOCK.
         GET FIRST oq NO-LOCK.
         DO WHILE AVAILABLE(omrtemp):
            CREATE bestkundallt.
            ASSIGN 
            bestkundallt.VIBESTID = omrtemp.OMRADE 
            bestkundallt.BESTID = omrtemp.OMRADE 
            bestkundallt.BESTNAMN = omrtemp.NAMN.                      
            CREATE bestkundextra.
            ASSIGN 
            bestkundextra.VIBESTID = omrtemp.OMRADE 
            bestkundextra.BESTID = omrtemp.OMRADE 
            bestkundextra.BESTNAMN = omrtemp.NAMN
            bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
            GET NEXT oq NO-LOCK.         
         END.         
      END.         
   END.
   
   IF vartpro = "NAOPLAN" THEN musz = musz.
   ELSE DO:
      /*visa bara eget bolags områden*/
      IF nyttaovar = TRUE THEN DO:         
         RUN anvavdhmt IN nyttaoapph (INPUT Guru.Konstanter:globanv,INPUT "",INPUT "",OUTPUT jid, OUTPUT TABLE eavdtemp ).                 
      END.
      ELSE DO:
         RUN anvavdhmt IN nyttaoapph (INPUT Guru.Konstanter:globanv,INPUT "",INPUT uppaonrtemp.OMRADE,OUTPUT jid, OUTPUT TABLE eavdtemp ).                 
      END.
      FOR EACH eavdtemp,         
      EACH omrtemp WHERE omrtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
      END.
      
   END.
   FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.
   ASSIGN
   aonrkonttemp.K1:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K1,1,1)) + LC(SUBSTRING(kbenamntemp.K1,2))
   aonrkonttemp.K2:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K2,1,1)) + LC(SUBSTRING(kbenamntemp.K2,2))
   aonrkonttemp.K3:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K3,1,1)) + LC(SUBSTRING(kbenamntemp.K3,2))
   aonrkonttemp.K4:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K4,1,1)) + LC(SUBSTRING(kbenamntemp.K4,2))
   aonrkonttemp.K5:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K5,1,1)) + LC(SUBSTRING(kbenamntemp.K5,2)).
   
   
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      /*HÄR SELNÄT*/
      ASSIGN
      CMB_OMR:LABEL IN FRAME FRAME-BARABREDD =  Guru.Konstanter:gomrl.
     
   END.  
   ELSE CMB_OMR:LABEL IN FRAME FRAME-BARABREDD = Guru.Konstanter:gutfk + " " + LC(Guru.Konstanter:gomrk).
   ASSIGN
    
   FILL-IN_BEST:LABEL = Guru.Konstanter:gbestk
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok            
   FILL-IN_STARTDAG:LABEL = Guru.Konstanter:gprojl
   CMB_BESTAVD:LABEL IN FRAME FRAME-BEST = Guru.Konstanter:gavdk
   bestkundallt.VIBESTID:LABEL IN BROWSE BRW_BEST = Guru.Konstanter:gbestk
   bestkundextra.VIBESTID:LABEL IN BROWSE BRW_BESTAVD = Guru.Konstanter:gbestk
   BRW_BEST:TITLE = "Välj " + LC(Guru.Konstanter:gbestk)
   BRW_BESTAVD:TITLE = "Välj " + LC(Guru.Konstanter:gbestk) + ". Högerklicka för vallista kontaktperson"
   BRW_ARBAN:TITLE IN FRAME FRAME-BERPROJ = "Välj " + LC(Guru.Konstanter:garbal)
   BRW_BERED:TITLE = "Välj " + LC(Guru.Konstanter:gberek)
   BRW_AONRKTO:TITLE IN FRAME FRAME-KONTO = "Kontosträngar lagrade på " + LC(Guru.Konstanter:gaok) + " :et"
   FILL-IN_ARBANSVARIG:LABEL = Guru.Konstanter:garbal 
   FILL-IN_BEREDARE:LABEL = Guru.Konstanter:gberel.
    
   IF vartpro = "NAOPLAN" THEN DO:     
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = uppaonrtemp.BESTID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bestkundallt THEN DO:
         CREATE bestkundallt.
         ASSIGN 
         bestkundallt.VIBESTID = omrtemp.OMRADE 
         bestkundallt.BESTID = omrtemp.OMRADE 
         bestkundallt.BESTNAMN = omrtemp.NAMN.                      
         CREATE bestkundextra.
         ASSIGN 
         bestkundextra.VIBESTID = omrtemp.OMRADE 
         bestkundextra.BESTID = omrtemp.OMRADE 
         bestkundextra.BESTNAMN = omrtemp.NAMN
         bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
      END.
      IF AVAILABLE bestkundallt THEN DO:
         
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.         
      END.
   END.
   ELSE DO:     
      IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
         IF uppaonrtemp.OMRADE = "" THEN .
         ELSE DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = uppaonrtemp.OMRADE
            USE-INDEX OMR NO-LOCK NO-ERROR.
            IF AVAILABLE omrtemp THEN DO:
               FIND FIRST bestkundallt WHERE bestkundallt.BESTID = omrtemp.OMRADE
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE bestkundallt THEN DO:
                  CREATE bestkundallt.
                  ASSIGN 
                  bestkundallt.VIBESTID = omrtemp.OMRADE 
                  bestkundallt.BESTID = omrtemp.OMRADE 
                  bestkundallt.BESTNAMN = omrtemp.NAMN.                      
                  CREATE bestkundextra.
                  ASSIGN 
                  bestkundextra.VIBESTID = omrtemp.OMRADE 
                  bestkundextra.BESTID = omrtemp.OMRADE 
                  bestkundextra.BESTNAMN = omrtemp.NAMN
                  bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
               END.
            END.
         END.
      END.             
      IF uppaonrtemp.FASTAAONR = TRUE THEN status-ok = CMB_OMR:ADD-LAST("").
      ASSIGN
      gambestnamn = ""
      gamomrnamn = "".
      
      IF uppaonrtemp.FASTAAONR = FALSE THEN DO:
         IF uppaonrtemp.OMRADE = "" THEN DO:
            status-ok = CMB_OMR:ADD-LAST("").
            ASSIGN  CMB_OMR:SCREEN-VALUE = " ".
         END.
         ELSE DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = uppaonrtemp.OMRADE
            USE-INDEX OMR NO-LOCK NO-ERROR.
            IF AVAILABLE omrtemp THEN DO:
               ASSIGN  CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
            END.
            ELSE DO:
               RUN gamomr IN nyttaoapph (INPUT uppaonrtemp.OMRADE, OUTPUT gamomrnamn).
               IF gamomrnamn NE "" THEN DO:
                  status-ok = CMB_OMR:ADD-LAST(gamomrnamn) IN FRAME FRAME-BARABREDD.
                  ASSIGN  CMB_OMR:SCREEN-VALUE = gamomrnamn.
               END.
            END.
            FIND FIRST bestkundallt WHERE bestkundallt.BESTID = uppaonrtemp.BESTID
            NO-LOCK NO-ERROR.
            IF AVAILABLE bestkundallt THEN DO:
               FILL-IN_BEST = bestkundallt.BESTNAMN.                                        
            END.
            ELSE DO:        
               RUN gambest IN nyttaoapph (INPUT uppaonrtemp.BESTID, OUTPUT gambestnamn).
               IF gambestnamn NE "" THEN FILL-IN_BEST = gambestnamn.
               ELSE IF gamomrnamn NE "" THEN DO:
                  FILL-IN_BEST = gamomrnamn.                            
               END.               
            END.
            DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.  
         END.         
      END.
      ELSE DO:
         IF uppaonrtemp.OMRADE = "" THEN DO:
            ASSIGN  CMB_OMR:SCREEN-VALUE = " ".
         END.
         ELSE DO:
             FIND FIRST omrtemp WHERE omrtemp.OMRADE = uppaonrtemp.OMRADE
             USE-INDEX OMR NO-LOCK NO-ERROR.
             ASSIGN  CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
         END.
      END.
      
   END.
   OPEN QUERY aq FOR EACH automregtemp USE-INDEX PRISTYP NO-LOCK.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(automregtemp):
      status-ok = CMB_DEBPR:ADD-LAST(automregtemp.PRISTYP).
      GET NEXT aq NO-LOCK.
   END.
   
   FIND FIRST automregtemp WHERE
   automregtemp.PRISTYP = "TOT.PRIS." USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:      
     FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
     IF AVAILABLE judavdtemp THEN DO:
        IF uppaonrtemp.FASTAAONR = TRUE AND judavdtemp.JUDID = "ELNÄT" THEN DO:
           FIND FIRST automregtemp WHERE
           automregtemp.PRISTYP = "EJ.KOSTN." USE-INDEX PRISTYP NO-LOCK NO-ERROR.
        END.
     END.
   END.
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:      
     FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
     IF AVAILABLE judavdtemp THEN DO:
        IF uppaonrtemp.FASTAAONR = TRUE AND judavdtemp.JUDID = "EL" THEN DO:
           FIND FIRST automregtemp WHERE
           automregtemp.PRISTYP = "EJ.KOSTN." USE-INDEX PRISTYP NO-LOCK NO-ERROR.
        END.
     END.
   END.
   IF uppaonrtemp.FASTAAONR = TRUE THEN DO:
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = uppaonrtemp.BESTID
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bestkundallt THEN DO:
         FIND FIRST bestkundallt NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD. 
         
      END.
   END.
   ASSIGN  CMB_DEBPR:SCREEN-VALUE = automregtemp.PRISTYP.
   IF nyttaovar = FALSE THEN DO:
      ASSIGN  CMB_DEBPR:SCREEN-VALUE = uppaonrtemp.PRISTYP.
   END.
   
   IF kalkrec = ? THEN DO:
      musz = musz.
   END.
   ELSE DO:
      /*SKAPA AONR FRÅN KALK*/
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = fastspectemp.OMRADE
      USE-INDEX OMR NO-LOCK NO-ERROR.
      ASSIGN
      CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = fastspectemp.BESTID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bestkundallt THEN FIND FIRST bestkundallt NO-LOCK NO-ERROR.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.          
      END.      
      FILL-IN_ORT = fastspectemp.BENAMNING.
   END.
   IF Guru.Konstanter:globforetag = "SOLE" THEN DO:
      status-ok = CMB_TRAK:ADD-LAST("2") IN FRAME FRAME-OVRIGT.
   END.
   ASSIGN
   CMB_TRAK:SCREEN-VALUE = STRING(uppaonrtemp.TRAKTAMENTE).
   CMB_TRAK = INPUT CMB_TRAK.
   IF FILL-IN_ANLNR = "" THEN DO:
      FIND FIRST anlaggtemp USE-INDEX ANLAGGNING NO-LOCK NO-ERROR.
      IF AVAILABLE anlaggtemp THEN ASSIGN FILL-IN_ANLNR = anlaggtemp.ANLNR.
   END.      
   IF FILL-IN_ARBARTKOD = ? OR FILL-IN_ARBARTKOD = 0 THEN DO:
      
      IF Guru.Konstanter:varforetypval[45] = 0 THEN DO:
         FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
         IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
         /* dessa styrningar är gamla enda nummerserierna av dessa som är kvar är 30 och 79 och dessa skall ej styra mot arbetsart Lena 20211020
         IF aonrvar BEGINS "38" OR aonrvar BEGINS "36" OR aonrvar BEGINS "46" OR aonrvar BEGINS "63" OR aonrvar BEGINS "79" OR aonrvar BEGINS "39" OR aonrvar BEGINS "47"
         OR aonrvar BEGINS "30"  THEN DO:           
           /*slut på nummer i 46 övergå till 47 Lena 20180611*/
           /*slut på nummer i 39 övergå till 30 Lena 20190627*/
           IF aonrvar BEGINS "386" THEN akoden = 220.
           ELSE IF aonrvar BEGINS "387" THEN akoden = 221.
           ELSE IF aonrvar BEGINS "388" THEN akoden = 222.
           ELSE IF aonrvar BEGINS "360" THEN akoden = 230.
           ELSE IF aonrvar BEGINS "361" THEN akoden = 231.
           ELSE IF aonrvar BEGINS "362" THEN akoden = 232.
           ELSE IF aonrvar BEGINS "363" THEN akoden = 232.
           ELSE IF aonrvar BEGINS "364" THEN akoden = 232.
           ELSE IF aonrvar BEGINS "365" THEN akoden = 232.
           ELSE IF aonrvar BEGINS "460" THEN akoden = 233.
           ELSE IF aonrvar BEGINS "461" THEN akoden = 233.
           ELSE IF aonrvar BEGINS "462" THEN akoden = 233.
           ELSE IF aonrvar BEGINS "632" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "633" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "634" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "635" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "636" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "790" THEN akoden = 234.
           ELSE akoden = 1.
           FIND FIRST arbarttemp WHERE arbarttemp.ARBARTKOD = akoden  USE-INDEX ARB NO-LOCK NO-ERROR.
           IF NOT AVAILABLE arbarttemp THEN DO:
              FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
           END.
           IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.

        END.
        ELSE DO:
           FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
           IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
        END.*/
      END.
      ELSE DO:      
         FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
         IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
      END.
   END.
   
    
   IF FILL-IN_PKOD = ? OR FILL-IN_PKOD = 0 THEN DO:
      /*HÄR SELNÄT LENA*/       
      IF Guru.Konstanter:varforetypval[45] = 1 THEN.
      ELSE DO:
         FIND FIRST priotemp USE-INDEX PRI NO-LOCK NO-ERROR.
         IF AVAILABLE priotemp THEN ASSIGN FILL-IN_PKOD = priotemp.PKOD.
      END.   
   END.
   ASSIGN
   EDITOR_AUPP = uppaonrtemp.ARBUPPG[1]
   EDITOR_ANM = uppaonrtemp.ANM[1].
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   FOR EACH faktyptemp WHERE faktyptemp.ORDNING >= 0:
      status-ok = CMB_FAK:ADD-LAST(faktyptemp.VIFAKTTYP) IN FRAME FRAME-FAKT.
   END.
   IF uppaonrtemp.ANVANDARE = "" THEN ASSIGN FILL-IN_ANVANDARE = Guru.Konstanter:globanv.
   ELSE FILL-IN_ANVANDARE = uppaonrtemp.ANVANDARE.
   IF uppaonrtemp.FAKTTYP = "" THEN DO:
      IF Guru.Konstanter:globforetag = "ELPAc" {GLOBVES.I} THEN DO:
         FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Löpande räkning" NO-ERROR.
         FILL-IN_FAKTTYP = faktyptemp.VIFAKTTYP.
         uppaonrtemp.FAKTTYP  = faktyptemp.FAKTTYP.
      END.
      ELSE DO:
         FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Faktureras ej" NO-ERROR.
         FILL-IN_FAKTTYP = faktyptemp.VIFAKTTYP.
      END.
   END.
   ELSE DO:
      FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = uppaonrtemp.FAKTTYP NO-ERROR.
      IF NOT AVAILABLE faktyptemp THEN DO:
         IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
            FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Löpande räkning" NO-ERROR.
            FILL-IN_FAKTTYP = faktyptemp.VIFAKTTYP.
            uppaonrtemp.FAKTTYP  = faktyptemp.FAKTTYP.
         END.
      END.
      ELSE FILL-IN_FAKTTYP = faktyptemp.VIFAKTTYP.
   END.
   ASSIGN
   FILL-IN_INTERNT = uppaonrtemp.BETNR /*- uppaonrtemp.STARTVNR*/
   FILL-IN_EXTERNT = uppaonrtemp.STARTVNR.
   IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:     
      DISPLAY FILL-IN_INTERNT FILL-IN_EXTERNT WITH FRAME FRAME-FAKT.
   END.
   ELSE DO:
      DISPLAY FILL-IN_INTERNT  WITH FRAME FRAME-FAKT.
   END.
   ASSIGN
   FILL-IN_FAKTNR = uppaonrtemp.FAKTNR.
   ASSIGN CMB_FAK:SCREEN-VALUE = FILL-IN_FAKTTYP.
   FILL-IN_FASTAAONR = uppaonrtemp.FASTAAONR.
   gfastkh = REPLACE(Guru.Konstanter:gfastl,"/","-").
   gtillkh = REPLACE(Guru.Konstanter:gtillk,"/","-").
   FILL-IN_FASTAAONR:FORMAT = gfastkh + "/" + gtillkh.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddao_UI C-Win 
PROCEDURE laddao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF nyttaovar = TRUE OR plannrvar NE ? THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 51
      soktemp.SOKCHAR[1] = plannrvar
      soktemp.SOKINT[1] = artalvar.
      {SOKANROP.I}
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = soktemp.SOKCHAR[1]
      USE-INDEX OMR NO-LOCK NO-ERROR.
      IF AVAILABLE omrtemp THEN DO:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN) IN FRAME FRAME-BARABREDD.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
         uppaonrtemp.OMRADE = omrtemp.OMRADE.      
         
      END.
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = soktemp.SOKCHAR[2]
      NO-LOCK NO-ERROR.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD. 
         uppaonrtemp.BESTID = bestkundallt.BESTID.
      END.
      IF nyttaovar = TRUE THEN DO:
         ASSIGN
         FILL-IN_ARBARTKODOV = soktemp.SOKINT[2]
         FILL-IN_ARBARTKOD = soktemp.SOKINT[2]
         FILL-IN_ORT = soktemp.SOKCHAR[3] 
         FILL-IN_ARBANSVARIG = soktemp.SOKCHAR[4]
         FILL-IN_BEREDARE = soktemp.SOKCHAR[4].        
      END.
      
   END.
   IF nyttaovar = FALSE THEN DO:
      ASSIGN
      sparavdat = uppaonrtemp.AONRAVDATUM
      FILL-IN_AONRAVDATUM = uppaonrtemp.AONRAVDATUM
      FILL-IN_ANLNR = uppaonrtemp.ANLNR 
      FILL-IN_AONR = uppaonrtemp.AONR 
      FILL-IN_ARBANSVARIG = uppaonrtemp.ARBANSVARIG
      FILL-IN_ARBARTKODOV = uppaonrtemp.ARBARTKOD
      FILL-IN_ARBARTKOD = uppaonrtemp.ARBARTKOD
      FILL-IN_BEREDARE = uppaonrtemp.BEREDARE
      tidagamla = uppaonrtemp.ARBANSVARIG
      tidbgamla = uppaonrtemp.BEREDARE 
      FILL-IN_STARTDAG = uppaonrtemp.STARTDAG
      tidpgamla = uppaonrtemp.STARTDAG
      FILL-IN_DELNR = uppaonrtemp.DELNR
      FILL-IN_ORT = uppaonrtemp.ORT
      FILL-IN_PKODOV = uppaonrtemp.PKOD
      FILL-IN_PKOD = uppaonrtemp.PKOD 
      FILL-IN_TRAKTAMENTE = uppaonrtemp.TRAKTAMENTE
      FILL-IN_UTRYCKNING = uppaonrtemp.UTRYCKNING
      FILL-IN_UTFARDAT = uppaonrtemp.UTFARDAT.  
   END.     
   mappvar = "".   
   mappvar = uppaonrtemp.AONR + STRING(uppaonrtemp.DELNR).
   mappvarhj = grundmappvar +  mappvar.     
   IF grundmappvar NE "" THEN DO:
      RUN mappvar IN nyttaoejapph (INPUT mappvarhj).      
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main2_UI C-Win 
PROCEDURE main2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FILL-IN-REF:LABEL IN FRAME FRAME-BARABREDD = Guru.Konstanter:grefbefk.
   ENABLE FILL-IN-REF FILL-IN-KONAKT WITH FRAME FRAME-BARABREDD.
  
   FOR EACH avdtemp:
      status-ok = CMB_BESTAVD:ADD-LAST(avdtemp.AVDELNINGNAMN) IN FRAME FRAME-BEST.
   END.
   
   IF CMB_BESTAVD:NUM-ITEMS > 18 THEN CMB_BESTAVD:INNER-LINES = 18.
   ELSE CMB_BESTAVD:INNER-LINES = CMB_BESTAVD:NUM-ITEMS.
   
   CMB_BESTAVD:SCREEN-VALUE = "Alla".
   CMB_BESTAVD:LABEL = Guru.Konstanter:gavdk.
   RUN huvud2_UI.   
   FILL-IN_ORT:MODIFIED = FALSE.
   FILL-IN_AONRAVDATUM:MODIFIED = FALSE.
   {musarrow.i} 
   IF delnrvar NE 0 THEN DO: 
      reslog = ?.
      RUN delnrcheck IN nyttaoapph (INPUT aonrvar, OUTPUT reslog).
      IF reslog = FALSE THEN DISABLE FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
   END.   
   IF vartpro = "NAOPLAN" THEN DO:
      ASSIGN
      BTN_ACCESS:HIDDEN = TRUE 
      BTN_KALKAR:HIDDEN IN FRAME FRAME-KOPP = TRUE
      BTN_AKTIV:HIDDEN = TRUE 
      BTN_BORTATIDL:HIDDEN IN FRAME FRAME-TIDL = TRUE 
      BTN_FASTTRAC:HIDDEN = TRUE 
      BTN_GATILL:HIDDEN = TRUE          
      BTN_BDOCTIDTV:HIDDEN IN FRAME FRAME-TILLAVTAL = TRUE
      BTN_KOPPTIDL:HIDDEN = TRUE 
      BTN_ANMTIDL:HIDDEN = TRUE 
      BTN_BESTTIDL:HIDDEN = TRUE 
      BTN_KOPPLAAONR:HIDDEN = TRUE
      BTN_BERKALK:HIDDEN = TRUE 
      /*Lena  Olsson Elpool i Umeå AB  4 sep 2014 11:55:56        
       tänt dokumentknapparna när man kommer från plan*/
      BTN_ANDADOK:HIDDEN IN FRAME FRAME-DOK = FALSE 
      BTN_NYADOK:HIDDEN = FALSE 
      BTN_VISADOK:HIDDEN = FALSE 
      BTN_VISATIDL:HIDDEN = TRUE
      BTN_MAPPDOK:HIDDEN = FALSE. 
      IF nyttaovar = TRUE THEN DISABLE CMB_OMR FILL-IN_BEST WITH FRAME FRAME-BARABREDD.
      ELSE DISABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
   END.
   IF nyttaovar = TRUE AND plannrvar NE ? THEN DO:
      DISABLE BRW_KOPP  WITH FRAME FRAME-KOPP. 
   END.
   FIND FIRST projtemp  WHERE projtemp.PERSONALKOD = FILL-IN_STARTDAG NO-LOCK NO-ERROR.
   IF AVAILABLE projtemp THEN DO:
      FILL-IN-NAMN-1 = projtemp.FORNAMN + " " + projtemp.EFTERNAMN + " " + projtemp.TELEFON + " " + projtemp.EPOST.
   END.
   FIND FIRST beratemp  WHERE beratemp.PERSONALKOD = FILL-IN_BEREDARE NO-LOCK NO-ERROR.
   IF AVAILABLE beratemp THEN DO:
      FILL-IN-NAMN-2 = beratemp.FORNAMN + " " + beratemp.EFTERNAMN + " " + beratemp.TELEFON + " " + beratemp.EPOST.
   END.
   FIND FIRST ansvaraotemp  WHERE ansvaraotemp.PERSONALKOD = FILL-IN_ARBANSVARIG NO-LOCK NO-ERROR.
   IF AVAILABLE ansvaraotemp THEN DO:
      FILL-IN-NAMN-3 = ansvaraotemp.FORNAMN + " " + ansvaraotemp.EFTERNAMN + " " + ansvaraotemp.TELEFON + " " + ansvaraotemp.EPOST.
   END.
   DISPLAY FILL-IN_STARTDAG FILL-IN-NAMN-1 FILL-IN-NAMN-2 FILL-IN-NAMN-3
      WITH FRAME FRAME-BARABREDD.   
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE menyrad_UI C-Win 
PROCEDURE menyrad_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN
   MBTN_KONT:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_FAK:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_DOK:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_AVT:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_OVR:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_TIDPERS:HIDDEN IN FRAME FRAME-AONR = TRUE.     
   MBTN_KOPPL:LOAD-IMAGE("BILDER\xbtn_kopp.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_TIDL:LOAD-IMAGE("BILDER\xbtn_tidlage.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_ANM:LOAD-IMAGE("BILDER\xbtn_anmdag.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_AOPERSSEK:LOAD-IMAGE("BILDER\xbtn_pbehor.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_DOK:LOAD-IMAGE("BILDER\xbtn_dok.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_AVT:LOAD-IMAGE("BILDER\xbtn_avtal.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_OVR:LOAD-IMAGE("BILDER\xbtn_ovr.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_KONT:LOAD-IMAGE("BILDER\xbtn_konto.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_FAK:LOAD-IMAGE("BILDER\xbtn_faktura.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_TIDPERS:LOAD-IMAGE("BILDER\xbtn_tidpers.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_PLIGG:LOAD-IMAGE("BILDER\xbtn_gpl.gif") IN FRAME FRAME-AONR NO-ERROR.
    /*DOKUMENT*/
   musz = FALSE.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ALTE" OR  
   Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "VAST" OR  Guru.Konstanter:globforetag = "VNAT" OR Guru.Konstanter:globforetag = "BORL" OR 
   Guru.Konstanter:globforetag = "STRA" OR Guru.Konstanter:globforetag = "TECT" THEN DO:
      grundmappvar = "".
   END.    
   RUN buttplace_UI.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pstart_UI C-Win 
PROCEDURE pstart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 77.
   {SOKANROP.I}    
   RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
   datornamn = TRIM(outdatornamn).   
   {DOKUMENTMAP.I}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE radval_UI C-Win 
PROCEDURE radval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE status-mus2 AS LOGICAL NO-UNDO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rlwkopp_UI C-Win 
PROCEDURE rlwkopp_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/      
   IF AVAILABLE gatill THEN DO:
      DISPLAY gatill.STATUSNIV WITH BROWSE BRW_KOPP NO-ERROR.
      gatill.STATUSNIV = INPUT BROWSE BRW_KOPP gatill.STATUSNIV.
      DISPLAY gatill.STATUSNIV WITH BROWSE BRW_KOPP NO-ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispkopp_UI C-Win 
PROCEDURE rowdispkopp_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT PARAMETER fargnr AS INTEGER NO-UNDO.
   IF AVAILABLE gatill THEN DO:
      IF gatill.TYPCHAR = "KAL" THEN fargnr = 1.
      ELSE IF gatill.TYPCHAR = "ÄRENDE" THEN fargnr = 1.        
      ELSE IF gatill.TYPCHAR = "FAK" THEN. 
      ELSE IF gatill.TYPCHAR = "BER" THEN fargnr = 5.
      ELSE IF gatill.TYPCHAR = "MAR" THEN.
      ELSE IF gatill.TYPCHAR = "FBAONR" THEN fargnr = 2.
      ELSE IF gatill.TYPCHAR = "INTAONR" THEN fargnr = 2.
   END.
   IF fargnr > 0 THEN DO:
      ASSIGN
      gatill.F1:FGCOLOR IN BROWSE BRW_KOPP = fargnr
      gatill.F2:FGCOLOR = fargnr
      gatill.F3:FGCOLOR = fargnr
      gatill.STATUSNIV:FGCOLOR = fargnr.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokaonr_UI C-Win 
PROCEDURE sokaonr_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN FILL-IN_AONR = INPUT FRAME FRAME-BARABREDD FILL-IN_AONR. 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF Guru.Konstanter:globniv NE 15 THEN DO:
         IF FILL-IN_FASTAAONR = TRUE THEN DO:
            IF SUBSTRING(FILL-IN_AONR,1,1) NE STRING(0) THEN DO:
               MESSAGE Guru.Konstanter:gfastk Guru.Konstanter:gaol + " måste börja med en 0:a" VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.      
            END.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "VORD" {GLOBVES.I} THEN DO:
      IF LENGTH(FILL-IN_AONR) < 6 THEN DO:
         MESSAGE Guru.Konstanter:gaol + " måste innehålla 6 tecken." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.      
      END.
   END.
   RUN aokoll_UI.
   IF musz = FALSE THEN DO:
      FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
      uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.      
      ASSIGN aonrvar = uppaonrtemp.AONR.
      ASSIGN
      aonrvar2 =  aonrvar 
      delnrvar2 = delnrvar.
      RUN andraaonr IN nyttaoapph (INPUT aonrvar,INPUT FILL-IN_AONR).
      mappvar = "".
      mappvar = FILL-IN_AONR + "0". 
      IF grundmappvar NE "" THEN DO:
         mappvarhj = grundmappvar + mappvar.
         RUN mappvar IN nyttaoejapph (INPUT mappvarhj).         
      END.

      DISABLE FILL-IN_AONR WITH FRAME FRAME-BARABREDD. 
   END.  
   ELSE DO:
      musz = FALSE.
      APPLY "ENTRY" TO FILL-IN_AONR IN FRAME FRAME-BARABREDD.
      RETURN NO-APPLY.      
   END.   
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valomr_UI C-Win 
PROCEDURE valomr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF SUBSTRING(Guru.Konstanter:globomr,1,1) = "4" THEN DO:
      IF SUBSTRING(omrtemp.OMRADE,1,1) = "4" THEN DO:  
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN) IN FRAME FRAME-BARABREDD.
      END.
   END.
   ELSE status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).   
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
   ordningnr = ordningnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgeth_UI C-Win 
PROCEDURE widgeth_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      ENABLE CMB_FAK WITH FRAME FRAME-FAKT.   
      RETURN.
   END.
   IF vad = 2 THEN DO:
      DISABLE CMB_FAK WITH FRAME FRAME-FAKT.   
      RETURN.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

