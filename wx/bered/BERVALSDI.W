&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-3





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------


  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>
  Author: 

  Created: 01/28/97 -  3:43 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER kalkbervarSUND AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER instartaannatpro AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE startaannatpro AS CHARACTER NO-UNDO.
startaannatpro = instartaannatpro. 
/* Local Variable Definitions ---                                       */
{BERTEMPTAB.I}
DEFINE NEW SHARED VARIABLE bervalapph AS HANDLE NO-UNDO.
{ALLDEF.I}
DEFINE NEW SHARED VARIABLE brwNrandkonst AS INTEGER NO-UNDO.
/*
DEFINE VARIABLE logprogh AS HANDLE NO-UNDO.
RUN BORTLOGG.P PERSISTENT SET logprogh.
FILE-INFO:FILE-NAME = TRIM(SUBSTRING(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"_UI") + 3)).
RUN loggskap_UI IN logprogh (INPUT 2,INPUT SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)), INPUT "WCCHB1", INPUT "").
*/
{WHANDLTEMP.I}
   FIELD WF AS HANDLE EXTENT 200.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{APPARAT.I}
{INKSUMBERTEMP.I}  
DEFINE NEW SHARED TEMP-TABLE mtrl_tab NO-UNDO   
   FIELD BENAMNING AS CHARACTER LABEL "Benämning".    
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{DEFSOK.I}
&Scoped-define SHARED SHARED
{BERANN.I}
{MTRLTEMP.I}
DEFINE NEW SHARED TEMP-TABLE soktamtrlfavo NO-UNDO LIKE mtrltemp.
{KONVALTEMP.I}
{KONID.I}   
{LISTMTRL.I}   
{KOPPLINA.I}    
{FRITEMP.I}  
{KALKTEMP2.I}
{SMTRL.I}
{BILDBERTEMP.I}
{AVDTEMP.I}
{AVDELNINGTEMP.I}
{OMRTEMPW.I}
{MARKGRUPP.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{BERUPPTEMP.I}
{BERSKAP.I}  
{KONID2.I}    
{BERVALH.I}
{SOKDEF.I}
{LEVTEMP.I}
{HUVLEVTEMP.I}
{ANMARKTEMP2.I}
{BBENAMNTEMP.I}
{KONSTRMTRL.I}
{BERSTOLP.I}
{BERBILD.I}
{ANNAMN.I}
{PARSTOLP.I}
{HOPPSEK2W.I}
/*LISTOR*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
IF Guru.Konstanter:globforetag = "elpa" THEN brwNrandkonst = 28.
ELSE brwNrandkonst = 7.
brwNrandkonst = 28.
{BERKALKTEMP.I}
DEFINE BUFFER konbuff FOR kon_val.
DEFINE NEW SHARED VARIABLE entrymtrlantal AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE entrymtrlantalskap AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE entrylinledare AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE entrylinmeter  AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE stolpval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bryt AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE edmedd AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valkonst AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE RAD_STOLP AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE num1var AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE num2var AS INTEGER NO-UNDO.   
DEFINE NEW SHARED VARIABLE slutaejnu AS LOGICAL NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE lin_tab2 NO-UNDO
   FIELD ENR AS CHARACTER         
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD METER AS INTEGER      
   FIELD PRIS AS DECIMAL
   FIELD ENHET AS CHARACTER   
   FIELD TOTMETER AS INTEGER
   FIELD UPPLAG AS INTEGER INITIAL "?"
   FIELD LEVKOD AS CHARACTER       
   INDEX ENR ENR ASCENDING 
   INDEX UPP UPPLAG ASCENDING.     


&Scoped-define NEW
&Scoped-define SHARED 
{KONVAL2TEMP.I}
DEFINE NEW SHARED VARIABLE meter AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE lin_rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE liggande AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE sida AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kon_rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE varupp AS INTEGER NO-UNDO.   
DEFINE NEW SHARED VARIABLE varadr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varanm AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE ny AS LOGICAL NO-UNDO.       
DEFINE NEW SHARED VARIABLE typval AS INTEGER NO-UNDO.     


DEFINE NEW SHARED VARIABLE franrowid AS ROWID NO-UNDO.      
DEFINE NEW SHARED VARIABLE tillrowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE valnum AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE kabkortvar AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE btnbervalh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnordh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnkopupph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnfrih AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnkskaph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnkskapmtrlh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnklinh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnlinupph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnlistorh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE btnatgh AS HANDLE NO-UNDO.

DEFINE NEW SHARED VARIABLE valskapnum AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE snabbspar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar3 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar4 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar5 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildant AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE skapapar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE typkod AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varbild AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE berval2 AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE xtillyvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE val_row AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE repovar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nettoh AS HANDLE NO-UNDO.
/*CONNECTIT VARIABLER*/
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.

DEFINE SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE ejanv AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE nyvar AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE SHARED VARIABLE skapamtrl AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE VARIABLE komplikoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE globanv2 AS CHARACTER NO-UNDO.        
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE gruppkod AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE dirnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE bild_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE kompmtrl AS LOGICAL NO-UNDO.
DEFINE VARIABLE kompmtrlnum AS INTEGER NO-UNDO.
DEFINE VARIABLE vismtrl AS CHARACTER NO-UNDO.
DEFINE VARIABLE visval AS INTEGER NO-UNDO.

DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE frikortapph AS HANDLE NO-UNDO.
DEFINE VARIABLE kopimtrlapph AS HANDLE NO-UNDO.

DEFINE VARIABLE valtab AS INTEGER NO-UNDO.
DEFINE VARIABLE gommtrl AS LOGICAL NO-UNDO.
DEFINE VARIABLE counter AS INTEGER NO-UNDO.        
DEFINE VARIABLE counter2 AS INTEGER NO-UNDO. 
DEFINE VARIABLE counterord AS INTEGER NO-UNDO.
DEFINE VARIABLE numval AS INTEGER NO-UNDO.
DEFINE VARIABLE cmbspar AS CHARACTER NO-UNDO.
DEFINE VARIABLE deletenum AS INTEGER NO-UNDO. 
DEFINE VARIABLE summeter AS INTEGER NO-UNDO. 
DEFINE VARIABLE aonrrow2 AS ROWID NO-UNDO.
DEFINE VARIABLE avbrytvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE newskap AS LOGICAL NO-UNDO.
DEFINE VARIABLE trafovar AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE sokannan AS LOGICAL NO-UNDO.
DEFINE VARIABLE avar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE cvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE valet AS LOGICAL NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE procasynch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE klar AS LOGICAL NO-UNDO.
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE enrvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE charvar1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE charvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE laddabrwproch AS HANDLE NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE setcolvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE spar_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
/*ID*/
DEFINE NEW SHARED VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE vallinje AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valnat AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE siff AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE andrakod AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE id AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE nyttidnr AS CHARACTER.
DEFINE VARIABLE fordrow AS ROWID NO-UNDO.      
DEFINE VARIABLE linjerow AS ROWID NO-UNDO.
DEFINE VARIABLE natrow AS ROWID NO-UNDO.  
DEFINE VARIABLE brw AS LOGICAL NO-UNDO. 
DEFINE VARIABLE mess AS LOGICAL NO-UNDO. 
DEFINE VARIABLE sparfor AS CHARACTER NO-UNDO.     
DEFINE VARIABLE sparlin AS CHARACTER NO-UNDO. 
DEFINE VARIABLE sparnat AS CHARACTER NO-UNDO. 
DEFINE VARIABLE sparfri1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE sparfri2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sparfri3 AS INTEGER NO-UNDO.

DEFINE VARIABLE idenable AS LOGICAL NO-UNDO.
DEFINE VARIABLE anmenable AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrlenable AS LOGICAL NO-UNDO.
DEFINE VARIABLE kalkenable AS LOGICAL NO-UNDO.
DEFINE VARIABLE sermtrl AS LOGICAL NO-UNDO.
DEFINE VARIABLE bildenable AS LOGICAL NO-UNDO.

DEFINE VARIABLE frannr AS INTEGER NO-UNDO.
DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
DEFINE VARIABLE till_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE rowkonval AS ROWID NO-UNDO.
DEFINE VARIABLE bbnamningvar AS CHARACTER EXTENT 10 NO-UNDO.

DEFINE VARIABLE ttbuffcopyh AS HANDLE NO-UNDO.
/*ID*/

/*MTRL*/
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
/*skapMTRL*/

/*TABELL FÖR KALKYLERING ENLIGT P3*/
DEFINE SHARED VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkvar AS LOGICAL NO-UNDO.
kalkvar = TRUE.
/*upplag*/
DEFINE NEW SHARED TEMP-TABLE kombtemp NO-UNDO
   {KOMPTEMP.I}
   
DEFINE NEW SHARED VARIABLE komb_rowid AS ROWID NO-UNDO.

IF Guru.Konstanter:beresekvar[4] = FALSE  THEN RETURN.

&Scoped-define NEW NEW

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-BARABREDD
&Scoped-define BROWSE-NAME BRW_ANDKONST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES andrakon_val fastanmtemp apparattemp ~
konstvaltemp bildbertemp favberupptemp kon_id mtrltemp fri_temp kon_val ~
kalk_temp kombtemp konsttemp kopp_lina lin_tab2 list_mtrl mtrl_tab ~
idkon_val satstemp berskaptemp soktamtrlfavo berstolptemp berfortemp ~
berlinjetemp bernattemp kon_id2 berupptemp kon_display

/* Definitions for BROWSE BRW_ANDKONST                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_ANDKONST andrakon_val.F1 andrakon_val.F2 ~
andrakon_val.F3 andrakon_val.F4 andrakon_val.F5 andrakon_val.F6 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANDKONST 
&Scoped-define QUERY-STRING-BRW_ANDKONST FOR EACH andrakon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANDKONST OPEN QUERY BRW_ANDKONST FOR EACH andrakon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANDKONST andrakon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANDKONST andrakon_val


/* Definitions for BROWSE BRW_ANDRAK                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ANDRAK andrakon_val.F1 andrakon_val.F2 ~
andrakon_val.F3 andrakon_val.F4 andrakon_val.F5 andrakon_val.F6 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANDRAK 
&Scoped-define QUERY-STRING-BRW_ANDRAK FOR EACH andrakon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANDRAK OPEN QUERY BRW_ANDRAK FOR EACH andrakon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANDRAK andrakon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANDRAK andrakon_val


/* Definitions for BROWSE BRW_ANM                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANM fastanmtemp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANM 
&Scoped-define QUERY-STRING-BRW_ANM FOR EACH fastanmtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANM OPEN QUERY BRW_ANM FOR EACH fastanmtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANM fastanmtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANM fastanmtemp


/* Definitions for BROWSE BRW_APP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_APP apparattemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_APP 
&Scoped-define QUERY-STRING-BRW_APP FOR EACH apparattemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_APP OPEN QUERY BRW_APP FOR EACH apparattemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_APP apparattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_APP apparattemp


/* Definitions for BROWSE BRW_B2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B2 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B2 
&Scoped-define QUERY-STRING-BRW_B2 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B2 OPEN QUERY BRW_B2 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B2 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B2 konstvaltemp


/* Definitions for BROWSE BRW_B3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B3 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B3 
&Scoped-define QUERY-STRING-BRW_B3 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B3 OPEN QUERY BRW_B3 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B3 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B3 konstvaltemp


/* Definitions for BROWSE BRW_B4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B4 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B4 
&Scoped-define QUERY-STRING-BRW_B4 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B4 OPEN QUERY BRW_B4 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B4 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B4 konstvaltemp


/* Definitions for BROWSE BRW_B5                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B5 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B5 
&Scoped-define QUERY-STRING-BRW_B5 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B5 OPEN QUERY BRW_B5 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B5 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B5 konstvaltemp


/* Definitions for BROWSE BRW_B6                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B6 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B6 
&Scoped-define QUERY-STRING-BRW_B6 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B6 OPEN QUERY BRW_B6 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B6 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B6 konstvaltemp


/* Definitions for BROWSE BRW_BILD                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BILD bildbertemp.NAMN ~
bildbertemp.FILNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BILD bildbertemp.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BILD bildbertemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BILD bildbertemp
&Scoped-define QUERY-STRING-BRW_BILD FOR EACH bildbertemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BILD OPEN QUERY BRW_BILD FOR EACH bildbertemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BILD bildbertemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BILD bildbertemp


/* Definitions for BROWSE BRW_FAVOUPPLAG                                */
&Scoped-define FIELDS-IN-QUERY-BRW_FAVOUPPLAG favberupptemp.ADRESS ~
favberupptemp.POSTNR favberupptemp.ORT favberupptemp.XKORDCH ~
favberupptemp.YKORDCH 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FAVOUPPLAG 
&Scoped-define QUERY-STRING-BRW_FAVOUPPLAG FOR EACH favberupptemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_FAVOUPPLAG OPEN QUERY BRW_FAVOUPPLAG FOR EACH favberupptemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_FAVOUPPLAG favberupptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FAVOUPPLAG favberupptemp


/* Definitions for BROWSE BRW_FRAN                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FRAN kon_id.LINNR kon_id.NATNR ~
kon_id.FRI1 kon_id.FRI2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRAN 
&Scoped-define QUERY-STRING-BRW_FRAN FOR EACH kon_id NO-LOCK ~
    BY kon_id.LINNR ~
       BY kon_id.NATNR ~
        BY kon_id.FRI1 ~
         BY kon_id.FRI2
&Scoped-define OPEN-QUERY-BRW_FRAN OPEN QUERY BRW_FRAN FOR EACH kon_id NO-LOCK ~
    BY kon_id.LINNR ~
       BY kon_id.NATNR ~
        BY kon_id.FRI1 ~
         BY kon_id.FRI2.
&Scoped-define TABLES-IN-QUERY-BRW_FRAN kon_id
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRAN kon_id


/* Definitions for BROWSE BRW_FRANLIN                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_FRANLIN kon_id.LINNR kon_id.NATNR ~
kon_id.FRI1 kon_id.FRI2 kon_id.FRI3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRANLIN 
&Scoped-define QUERY-STRING-BRW_FRANLIN FOR EACH kon_id NO-LOCK ~
    BY kon_id.LINNR ~
       BY kon_id.NATNR ~
        BY kon_id.FRI1 ~
         BY kon_id.FRI2
&Scoped-define OPEN-QUERY-BRW_FRANLIN OPEN QUERY BRW_FRANLIN FOR EACH kon_id NO-LOCK ~
    BY kon_id.LINNR ~
       BY kon_id.NATNR ~
        BY kon_id.FRI1 ~
         BY kon_id.FRI2.
&Scoped-define TABLES-IN-QUERY-BRW_FRANLIN kon_id
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRANLIN kon_id


/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS mtrltemp.LEVKOD mtrltemp.LEVNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_HLEVSM                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEVSM mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS mtrltemp.LEVKOD mtrltemp.LEVNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEVSM 
&Scoped-define QUERY-STRING-BRW_HLEVSM FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEVSM OPEN QUERY BRW_HLEVSM FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEVSM mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEVSM mtrltemp


/* Definitions for BROWSE BRW_KABFRI                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_KABFRI fri_temp.GRUPPNR ~
fri_temp.BENAMNING fri_temp.KABADR fri_temp.METER fri_temp.TYP ~
fri_temp.APPARAT fri_temp.SAKR fri_temp.MAXSAKR fri_temp.SKAPADR ~
fri_temp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KABFRI 
&Scoped-define QUERY-STRING-BRW_KABFRI FOR EACH fri_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KABFRI OPEN QUERY BRW_KABFRI FOR EACH fri_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KABFRI fri_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KABFRI fri_temp


/* Definitions for BROWSE BRW_KABKAB                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_KABKAB kon_val.F2 kon_val.F3 kon_val.F4 ~
kon_val.F5 kon_val.F6 kon_val.TYP fri_temp.KABADR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KABKAB 
&Scoped-define QUERY-STRING-BRW_KABKAB FOR EACH kon_val NO-LOCK, ~
      EACH fri_temp WHERE TRUE /* Join to kon_val incomplete */ NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KABKAB OPEN QUERY BRW_KABKAB FOR EACH kon_val NO-LOCK, ~
      EACH fri_temp WHERE TRUE /* Join to kon_val incomplete */ NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KABKAB kon_val fri_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KABKAB kon_val
&Scoped-define SECOND-TABLE-IN-QUERY-BRW_KABKAB fri_temp


/* Definitions for BROWSE BRW_KABSM                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KABSM kon_val.F1 kon_val.F2 kon_val.F3 ~
kon_val.F4 kon_val.F5 kon_val.F6 fri_temp.KABADR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KABSM 
&Scoped-define QUERY-STRING-BRW_KABSM FOR EACH kon_val NO-LOCK, ~
      EACH fri_temp WHERE TRUE /* Join to kon_val incomplete */ NO-LOCK ~
    BY kon_val.F3 ~
       BY kon_val.F4 ~
        BY kon_val.F5 ~
         BY kon_val.F6
&Scoped-define OPEN-QUERY-BRW_KABSM OPEN QUERY BRW_KABSM FOR EACH kon_val NO-LOCK, ~
      EACH fri_temp WHERE TRUE /* Join to kon_val incomplete */ NO-LOCK ~
    BY kon_val.F3 ~
       BY kon_val.F4 ~
        BY kon_val.F5 ~
         BY kon_val.F6.
&Scoped-define TABLES-IN-QUERY-BRW_KABSM kon_val fri_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KABSM kon_val
&Scoped-define SECOND-TABLE-IN-QUERY-BRW_KABSM fri_temp


/* Definitions for BROWSE BRW_KALAKONS                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_KALAKONS kalk_temp.ARBKOD ~
kalk_temp.LOPNR kalk_temp.BENAMNING kalk_temp.ANTAL kalk_temp.ENHET ~
kalk_temp.NUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALAKONS 
&Scoped-define QUERY-STRING-BRW_KALAKONS FOR EACH kalk_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALAKONS OPEN QUERY BRW_KALAKONS FOR EACH kalk_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALAKONS kalk_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALAKONS kalk_temp


/* Definitions for BROWSE BRW_KALK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KALK kalk_temp.ARBKOD kalk_temp.LOPNR ~
kalk_temp.BENAMNING kalk_temp.ANTAL kalk_temp.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALK 
&Scoped-define QUERY-STRING-BRW_KALK FOR EACH kalk_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALK OPEN QUERY BRW_KALK FOR EACH kalk_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALK kalk_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALK kalk_temp


/* Definitions for BROWSE BRW_KB2                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB2 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB2 
&Scoped-define QUERY-STRING-BRW_KB2 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB2 OPEN QUERY BRW_KB2 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB2 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB2 konstvaltemp


/* Definitions for BROWSE BRW_KB3                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB3 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB3 
&Scoped-define QUERY-STRING-BRW_KB3 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB3 OPEN QUERY BRW_KB3 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB3 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB3 konstvaltemp


/* Definitions for BROWSE BRW_KB4                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB4 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB4 
&Scoped-define QUERY-STRING-BRW_KB4 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB4 OPEN QUERY BRW_KB4 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB4 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB4 konstvaltemp


/* Definitions for BROWSE BRW_KB5                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB5 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB5 
&Scoped-define QUERY-STRING-BRW_KB5 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB5 OPEN QUERY BRW_KB5 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB5 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB5 konstvaltemp


/* Definitions for BROWSE BRW_KB6                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB6 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB6 
&Scoped-define QUERY-STRING-BRW_KB6 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB6 OPEN QUERY BRW_KB6 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB6 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB6 konstvaltemp


/* Definitions for BROWSE BRW_KOMBKOPUPP                                */
&Scoped-define FIELDS-IN-QUERY-BRW_KOMBKOPUPP kombtemp.F1 kombtemp.FORNR ~
kombtemp.LINNR kombtemp.NATNR kombtemp.FRI1 kombtemp.FRI2 kombtemp.ID ~
kombtemp.ID2 kombtemp.UPPLAG kombtemp.KOMB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOMBKOPUPP 
&Scoped-define QUERY-STRING-BRW_KOMBKOPUPP FOR EACH kombtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOMBKOPUPP OPEN QUERY BRW_KOMBKOPUPP FOR EACH kombtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOMBKOPUPP kombtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOMBKOPUPP kombtemp


/* Definitions for BROWSE BRW_KON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KON konsttemp.KTYPKOD ~
konsttemp.BENAMNING konsttemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KON 
&Scoped-define QUERY-STRING-BRW_KON FOR EACH konsttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KON OPEN QUERY BRW_KON FOR EACH konsttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KON konsttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KON konsttemp


/* Definitions for BROWSE BRW_KONANDKONST                               */
&Scoped-define FIELDS-IN-QUERY-BRW_KONANDKONST konsttemp.KTYPKOD ~
konsttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KONANDKONST 
&Scoped-define QUERY-STRING-BRW_KONANDKONST FOR EACH konsttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KONANDKONST OPEN QUERY BRW_KONANDKONST FOR EACH konsttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KONANDKONST konsttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KONANDKONST konsttemp


/* Definitions for BROWSE BRW_LIN                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LIN kopp_lina.ENR kopp_lina.BENAMNING ~
kopp_lina.METER kopp_lina.LEDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LIN kopp_lina.METER ~
kopp_lina.LEDARE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_LIN kopp_lina
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_LIN kopp_lina
&Scoped-define QUERY-STRING-BRW_LIN FOR EACH kopp_lina NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LIN OPEN QUERY BRW_LIN FOR EACH kopp_lina NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LIN kopp_lina
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LIN kopp_lina


/* Definitions for BROWSE BRW_LINLINUPP                                 */
&Scoped-define FIELDS-IN-QUERY-BRW_LINLINUPP lin_tab2.ENR ~
lin_tab2.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LINLINUPP 
&Scoped-define QUERY-STRING-BRW_LINLINUPP FOR EACH lin_tab2 NO-LOCK ~
    BY lin_tab2.ENR
&Scoped-define OPEN-QUERY-BRW_LINLINUPP OPEN QUERY BRW_LINLINUPP FOR EACH lin_tab2 NO-LOCK ~
    BY lin_tab2.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_LINLINUPP lin_tab2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LINLINUPP lin_tab2


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL list_mtrl.ENR list_mtrl.BENAMNING ~
list_mtrl.ANTAL list_mtrl.ENHET list_mtrl.LINKAB list_mtrl.PRIS ~
list_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL list_mtrl.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL list_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL list_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH list_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH list_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL list_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL list_mtrl


/* Definitions for BROWSE BRW_MTRLFRI                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRLFRI mtrl_tab.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRLFRI 
&Scoped-define QUERY-STRING-BRW_MTRLFRI FOR EACH mtrl_tab NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRLFRI OPEN QUERY BRW_MTRLFRI FOR EACH mtrl_tab NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRLFRI mtrl_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRLFRI mtrl_tab


/* Definitions for BROWSE BRW_MTRLSM                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRLSM list_mtrl.ENR list_mtrl.BENAMNING ~
list_mtrl.ANTAL list_mtrl.ENHET list_mtrl.LINKAB list_mtrl.PRIS ~
list_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRLSM list_mtrl.ANTAL ~
list_mtrl.LINKAB 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRLSM list_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRLSM list_mtrl
&Scoped-define QUERY-STRING-BRW_MTRLSM FOR EACH list_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRLSM OPEN QUERY BRW_MTRLSM FOR EACH list_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRLSM list_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRLSM list_mtrl


/* Definitions for BROWSE BRW_ORD                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ORD kon_val.ID2 kon_val.EXTRA1 ~
kon_val.F1 kon_val.F2 kon_val.F3 kon_val.F4 kon_val.F5 kon_val.F6 ~
kon_val.EXTRA3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ORD 
&Scoped-define QUERY-STRING-BRW_ORD FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ORD OPEN QUERY BRW_ORD FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ORD kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ORD kon_val


/* Definitions for BROWSE BRW_ORDID                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_ORDID kon_val.ID2 kon_val.EXTRA1 ~
kon_val.F1 kon_val.F2 kon_val.F3 kon_val.F4 kon_val.F5 kon_val.F6 ~
kon_val.EXTRA3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ORDID 
&Scoped-define QUERY-STRING-BRW_ORDID FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ORDID OPEN QUERY BRW_ORDID FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ORDID kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ORDID kon_val


/* Definitions for BROWSE BRW_ORDIDVAL                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_ORDIDVAL idkon_val.ID2 idkon_val.EXTRA1 ~
idkon_val.F1 idkon_val.F2 idkon_val.F3 idkon_val.F4 idkon_val.F5 ~
idkon_val.F6 idkon_val.EXTRA3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ORDIDVAL 
&Scoped-define QUERY-STRING-BRW_ORDIDVAL FOR EACH idkon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ORDIDVAL OPEN QUERY BRW_ORDIDVAL FOR EACH idkon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ORDIDVAL idkon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ORDIDVAL idkon_val


/* Definitions for BROWSE BRW_SATS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_SATS satstemp.KOD satstemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SATS 
&Scoped-define QUERY-STRING-BRW_SATS FOR EACH satstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SATS OPEN QUERY BRW_SATS FOR EACH satstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SATS satstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SATS satstemp


/* Definitions for BROWSE BRW_SATSSM                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_SATSSM satstemp.KOD satstemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SATSSM 
&Scoped-define QUERY-STRING-BRW_SATSSM FOR EACH satstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SATSSM OPEN QUERY BRW_SATSSM FOR EACH satstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SATSSM satstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SATSSM satstemp


/* Definitions for BROWSE BRW_SKAPKAB                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_SKAPKAB berskaptemp.KOD ~
berskaptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SKAPKAB 
&Scoped-define QUERY-STRING-BRW_SKAPKAB FOR EACH berskaptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SKAPKAB OPEN QUERY BRW_SKAPKAB FOR EACH berskaptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SKAPKAB berskaptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SKAPKAB berskaptemp


/* Definitions for BROWSE BRW_SOKMTRLF                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_SOKMTRLF soktamtrlfavo.Enr ~
soktamtrlfavo.Benamning soktamtrlfavo.Enhet soktamtrlfavo.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SOKMTRLF 
&Scoped-define QUERY-STRING-BRW_SOKMTRLF FOR EACH soktamtrlfavo NO-LOCK ~
    BY soktamtrlfavo.Enr
&Scoped-define OPEN-QUERY-BRW_SOKMTRLF OPEN QUERY BRW_SOKMTRLF FOR EACH soktamtrlfavo NO-LOCK ~
    BY soktamtrlfavo.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_SOKMTRLF soktamtrlfavo
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SOKMTRLF soktamtrlfavo


/* Definitions for BROWSE BRW_SOKMTRLFSM                                */
&Scoped-define FIELDS-IN-QUERY-BRW_SOKMTRLFSM soktamtrlfavo.Enr ~
soktamtrlfavo.Benamning soktamtrlfavo.Enhet soktamtrlfavo.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SOKMTRLFSM 
&Scoped-define QUERY-STRING-BRW_SOKMTRLFSM FOR EACH soktamtrlfavo NO-LOCK ~
    BY soktamtrlfavo.Enr
&Scoped-define OPEN-QUERY-BRW_SOKMTRLFSM OPEN QUERY BRW_SOKMTRLFSM FOR EACH soktamtrlfavo NO-LOCK ~
    BY soktamtrlfavo.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_SOKMTRLFSM soktamtrlfavo
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SOKMTRLFSM soktamtrlfavo


/* Definitions for BROWSE BRW_STOLP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_STOLP berstolptemp.ENR ~
berstolptemp.BENAMNING berstolptemp.PRIS berstolptemp.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STOLP 
&Scoped-define QUERY-STRING-BRW_STOLP FOR EACH berstolptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STOLP OPEN QUERY BRW_STOLP FOR EACH berstolptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STOLP berstolptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STOLP berstolptemp


/* Definitions for BROWSE BRW_STOLPSM                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_STOLPSM berstolptemp.ENR ~
berstolptemp.BENAMNING berstolptemp.PRIS berstolptemp.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STOLPSM 
&Scoped-define QUERY-STRING-BRW_STOLPSM FOR EACH berstolptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STOLPSM OPEN QUERY BRW_STOLPSM FOR EACH berstolptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STOLPSM berstolptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STOLPSM berstolptemp


/* Definitions for BROWSE BRW_TAB1                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_TAB1 berfortemp.FORNR berfortemp.NAMN ~
berfortemp.OMRADE berfortemp.AVDNR berfortemp.VIJUDID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TAB1 
&Scoped-define QUERY-STRING-BRW_TAB1 FOR EACH berfortemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TAB1 OPEN QUERY BRW_TAB1 FOR EACH berfortemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TAB1 berfortemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TAB1 berfortemp


/* Definitions for BROWSE BRW_TAB2                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_TAB2 berlinjetemp.LINNR ~
berlinjetemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TAB2 
&Scoped-define QUERY-STRING-BRW_TAB2 FOR EACH berlinjetemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TAB2 OPEN QUERY BRW_TAB2 FOR EACH berlinjetemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TAB2 berlinjetemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TAB2 berlinjetemp


/* Definitions for BROWSE BRW_TAB3                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_TAB3 bernattemp.NATNR bernattemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TAB3 
&Scoped-define QUERY-STRING-BRW_TAB3 FOR EACH bernattemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TAB3 OPEN QUERY BRW_TAB3 FOR EACH bernattemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TAB3 bernattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TAB3 bernattemp


/* Definitions for BROWSE BRW_TILLLIN                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_TILLLIN kon_id2.LINNR kon_id2.NATNR ~
kon_id2.FRI1 kon_id2.FRI2 kon_id2.FRI3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TILLLIN 
&Scoped-define QUERY-STRING-BRW_TILLLIN FOR EACH kon_id2 NO-LOCK ~
    BY kon_id2.LINNR ~
       BY kon_id2.NATNR ~
        BY kon_id2.FRI1 ~
         BY kon_id2.FRI2
&Scoped-define OPEN-QUERY-BRW_TILLLIN OPEN QUERY BRW_TILLLIN FOR EACH kon_id2 NO-LOCK ~
    BY kon_id2.LINNR ~
       BY kon_id2.NATNR ~
        BY kon_id2.FRI1 ~
         BY kon_id2.FRI2.
&Scoped-define TABLES-IN-QUERY-BRW_TILLLIN kon_id2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TILLLIN kon_id2


/* Definitions for BROWSE BRW_UPPKOPUPP                                 */
&Scoped-define FIELDS-IN-QUERY-BRW_UPPKOPUPP berupptemp.UPPLAG ~
berupptemp.ADRESS berupptemp.POSTNR berupptemp.ORT berupptemp.XKORDCH ~
berupptemp.YKORDCH 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPPKOPUPP 
&Scoped-define QUERY-STRING-BRW_UPPKOPUPP FOR EACH berupptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UPPKOPUPP OPEN QUERY BRW_UPPKOPUPP FOR EACH berupptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UPPKOPUPP berupptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPPKOPUPP berupptemp


/* Definitions for BROWSE BRW_UPPLINUPP                                 */
&Scoped-define FIELDS-IN-QUERY-BRW_UPPLINUPP berupptemp.UPPLAG ~
berupptemp.ADRESS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPPLINUPP 
&Scoped-define QUERY-STRING-BRW_UPPLINUPP FOR EACH berupptemp NO-LOCK ~
    BY berupptemp.UPPLAG
&Scoped-define OPEN-QUERY-BRW_UPPLINUPP OPEN QUERY BRW_UPPLINUPP FOR EACH berupptemp NO-LOCK ~
    BY berupptemp.UPPLAG.
&Scoped-define TABLES-IN-QUERY-BRW_UPPLINUPP berupptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPPLINUPP berupptemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_display.ORD kon_display.ID2 ~
kon_display.EXTRA1 kon_display.F1 kon_display.F2 kon_display.F4 ~
kon_display.F3 kon_display.F5 kon_display.F6 kon_display.EXTRA3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL kon_display.EXTRA1 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VAL kon_display
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VAL kon_display
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_display NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_display NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_display
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_display


/* Definitions for FRAME FRAME-ANDKONST                                 */

/* Definitions for FRAME FRAME-ANDRAK                                   */

/* Definitions for FRAME FRAME-ANM                                      */

/* Definitions for FRAME FRAME-BARABREDD                                */

/* Definitions for FRAME FRAME-BILD                                     */

/* Definitions for FRAME FRAME-FRI                                      */

/* Definitions for FRAME FRAME-ID                                       */

/* Definitions for FRAME FRAME-KABSKAP                                  */

/* Definitions for FRAME FRAME-KALK                                     */

/* Definitions for FRAME FRAME-KONST                                    */

/* Definitions for FRAME FRAME-KOPUPP                                   */

/* Definitions for FRAME FRAME-LIN                                      */

/* Definitions for FRAME FRAME-LINUPP                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-LINUPP ~
    ~{&OPEN-QUERY-BRW_UPPLINUPP}

/* Definitions for FRAME FRAME-MTRL                                     */

/* Definitions for FRAME FRAME-ORDN                                     */

/* Definitions for FRAME FRAME-SKAPMTRL                                 */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_OK RECT-MENY MBTN_ATERGA BTN_AVB ~
MBTN_SCHAKT MBTN_INK MBTN_FLYG MBTN_ANDORD MBTN_ATG MBTN_FRIKORT MBTN_KALK ~
MBTN_KOPI BRW_VAL MBTN_KSKAP BTN_SOLEN MBTN_LIN BTN_ORDUPP BTN_ORDNER ~
MBTN_LIST MBTN_SNABB MBTN_UPPL MBTN_ANM MBTN_DOK MBTN_ID MBTN_KONST ~
MBTN_MTRL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SKAPMTRL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_B2 
       MENU-ITEM m_mtrlB2       LABEL "Visa materiel" 
       MENU-ITEM m_bildB2       LABEL "Visa bild"     
       MENU-ITEM m_AvmarkeraB2  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_B3 
       MENU-ITEM m_mtrlB3       LABEL "Visa materiel" 
       MENU-ITEM m_bildB3       LABEL "Visa bild"     
       MENU-ITEM m_AvmarkeraB3  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_B4 
       MENU-ITEM m_mtrlB4       LABEL "Visa materiel" 
       MENU-ITEM m_bildB4       LABEL "Visa bild"     
       MENU-ITEM m_AvmarkeraB4  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_B5 
       MENU-ITEM m_mtrlB5       LABEL "Visa materiel" 
       MENU-ITEM m_bildB5       LABEL "Visa bild"     
       MENU-ITEM m_AvmarkeraB5  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_B6 
       MENU-ITEM m_mtrlB6       LABEL "Visa materiel" 
       MENU-ITEM m_bildB6       LABEL "Visa bild"     
       MENU-ITEM m_AvmarkeraB6  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_FRANLIN 
       MENU-ITEM m_Visa_informationLIN LABEL "Visa information".

DEFINE MENU POPUP-MENU-BRW_HLEV 
       MENU-ITEM m_AvmarkeraHLEV LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_HLEVSM 
       MENU-ITEM m_AvmarkeraHLEVSM LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_KB2 
       MENU-ITEM m_mtrlKB2      LABEL "Visa materiel" 
       MENU-ITEM m_bildKB2      LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_KB3 
       MENU-ITEM m_mtrlKB3      LABEL "Visa materiel" 
       MENU-ITEM m_bildKB3      LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_KB4 
       MENU-ITEM m_mtrlKB4      LABEL "Visa materiel" 
       MENU-ITEM m_bildKB4      LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_KB5 
       MENU-ITEM m_mtrlKB5      LABEL "Visa materiel" 
       MENU-ITEM m_bildKB5      LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_KB6 
       MENU-ITEM m_mtrlKB6      LABEL "Visa materiel" 
       MENU-ITEM m_bildKB6      LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_KOMBKOPUPP 
       MENU-ITEM m_KonstruktionerKOPUPP LABEL "Konstruktioner".

DEFINE MENU POPUP-MENU-BRW_KON 
       MENU-ITEM m_Visa_mtrl    LABEL "Visa materiel" 
       MENU-ITEM m_Visa_bild    LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_KON-2 
       MENU-ITEM m_Visa_mtrl-2  LABEL "Visa materiel" 
       MENU-ITEM m_Visa_bild-2  LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_KON-3 
       MENU-ITEM m_Visa_mtrl-3  LABEL "Visa materiel" 
       MENU-ITEM m_Visa_bild-3  LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_SATS 
       MENU-ITEM m_Pris_information LABEL "Satsinformation".

DEFINE MENU POPUP-MENU-BRW_TILLLIN 
       MENU-ITEM m_Visa_information2LIN LABEL "Visa information".

DEFINE MENU POPUP-MENU-BRW_VAL 
       MENU-ITEM m_Visa_informationVAL LABEL "Visa information".

DEFINE MENU POPUP-MENU-BRW_VAL-2 
       MENU-ITEM m_Visa_informationVAL-2 LABEL "Visa information".

DEFINE MENU POPUP-MENU-BRW_VAL-3 
       MENU-ITEM m_Visa_informationVAL-3 LABEL "Visa information".

DEFINE MENU POPUP-MENU-BRW_VAL-4 
       MENU-ITEM m_Visa_informationVAL-4 LABEL "Visa information".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_BYTANGANDKONST 
     LABEL "Byt konstruktionsgrupp" 
     SIZE 12 BY 1 TOOLTIP "Obs! Materielen byts inte!".

DEFINE BUTTON BTN_STANGANDKONST 
     LABEL "OK" 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_VALANDKONST AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE BUTTON BTN_STANG 
     LABEL "OK" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KOPPANM 
     LABEL "Koppla Anmärkning" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OVER-2 
     IMAGE-UP FILE "BILDER\next-u.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21.

DEFINE VARIABLE ED_ANM AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 8
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 29.13 BY 1.46
     FONT 17 NO-UNDO.

DEFINE VARIABLE TOG_ANMLOP AS LOGICAL INITIAL no 
     LABEL "Anmärkning som löpande text" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .79 NO-UNDO.

DEFINE BUTTON BTN_AVBATG AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KORATG 
     LABEL "Kör funktion" 
     SIZE 14 BY 1.

DEFINE VARIABLE SEL_UPPATG AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 45 BY 12.38 NO-UNDO.

DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_ORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta ner".

DEFINE BUTTON BTN_ORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta upp".

DEFINE BUTTON BTN_SOLEN 
     IMAGE-UP FILE "BILDER/glob.gif":U
     LABEL "" 
     SIZE 4.5 BY 1.71 TOOLTIP "Solen - karta".

DEFINE BUTTON MBTN_ANDORD  NO-FOCUS
     LABEL "Ordning" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE BUTTON MBTN_ANM  NO-FOCUS
     LABEL "Anmärkning" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_ATERGA  NO-FOCUS
     LABEL "Återgå" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_ATG  NO-FOCUS
     LABEL "Åtgärder" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_DOK  NO-FOCUS
     LABEL "Bild/Dokument" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_FLYG  NO-FOCUS
     LABEL "Flygnr" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_FRIKORT  NO-FOCUS
     LABEL "Frikort" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE BUTTON MBTN_ID  NO-FOCUS
     LABEL "Identitet" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_INK  NO-FOCUS
     LABEL "Inköp" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_KALK  NO-FOCUS
     LABEL "Kalkyl" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_KONST  NO-FOCUS
     LABEL "Konstruktioner" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_KOPI  NO-FOCUS
     LABEL "CKopiera" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE BUTTON MBTN_KSKAP  NO-FOCUS
     LABEL "Kabelskåp" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE BUTTON MBTN_LIN  NO-FOCUS
     LABEL "Lin/Kab" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE BUTTON MBTN_LIST  NO-FOCUS
     LABEL "Listor" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_MTRL  NO-FOCUS
     LABEL "Materiel" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_SCHAKT  NO-FOCUS
     LABEL "Schakt" 
     SIZE 7.25 BY 2.33
     FONT 4.

DEFINE BUTTON MBTN_SNABB  NO-FOCUS
     LABEL "Snabbspara" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE BUTTON MBTN_UPPL  NO-FOCUS
     LABEL "Uppl-komb" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE VARIABLE FILL-IN-SKAPMTRL AS CHARACTER FORMAT "X(256)":U INITIAL "För att nå materiel för skåp Tryck på materielknappen!" 
      VIEW-AS TEXT 
     SIZE 57.5 BY 1
     FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Konstruktioner", 1,
"Identitet", 2,
"Anmärkning", 3,
"Materiel", 4
     SIZE 2.25 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-MENY
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124.63 BY .08.

DEFINE BUTTON BTN_BILDCOPY 
     LABEL "Kopiera" 
     SIZE 12 BY 1 TOOLTIP "Kopierar bild från beredningsupplägget".

DEFINE BUTTON BTN_BORTA 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NYA 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_EXKSKO 
     LABEL "Kort till exel" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXMARK 
     LABEL "Markstationskort LSP" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXSTOLP 
     LABEL "Stolpstationskort" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MEXC 
     LABEL "Skyltmall Exel" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NERFRI 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "" 
     SIZE 4 BY 1.5.

DEFINE BUTTON BTN_UPPFRI 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5.

DEFINE BUTTON FBTN_ANDFRI 
     LABEL "Spara-Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_BORTFRI 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_NASTAKABEL 
     LABEL "Nästa kabel" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_PDFFRI 
     LABEL "Visa I PDF" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKAPAFRI 
     LABEL "Skapa-Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKRIVFRI 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISAFRI 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_KORTVAL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Välj korttyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25.75 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_SKYLTART AS CHARACTER FORMAT "X(256)":U 
     LABEL "Artnr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "A104","A105","A107" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TYP AS CHARACTER FORMAT "X(6)":U 
     LABEL "Typ" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "Servis","In","Ut" 
     DROP-DOWN-LIST
     SIZE 10.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ABONNENT AS CHARACTER FORMAT "X(256)":U 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 11.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ADR AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kabeladress" 
     VIEW-AS FILL-IN 
     SIZE 45.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANMARK AS CHARACTER FORMAT "X(90)":U 
     LABEL "Anmärkning" 
     VIEW-AS FILL-IN 
     SIZE 51.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-APPARAT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Apparat" 
     VIEW-AS FILL-IN 
     SIZE 30.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT ">>>>":U INITIAL ? 
     LABEL "År" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-B2 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B3 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-F2 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F3 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-GRUPPNR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Grupp-nr" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KABEL AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kabeltyp" 
     VIEW-AS FILL-IN 
     SIZE 30.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KORT AS CHARACTER FORMAT "X(256)":U INITIAL "Kabelskåpskort" 
      VIEW-AS TEXT 
     SIZE 16 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-MAX AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Max säkring (A)" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-METER AS INTEGER FORMAT ">>>>>9":U INITIAL ? 
     LABEL "Längd" 
     VIEW-AS FILL-IN 
     SIZE 10.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NISKYLTANT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inv. skylt antal" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NSKYLTANT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utv. skylt antal" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SADR AS CHARACTER FORMAT "X(30)":U 
     LABEL "Skåpadress" 
     VIEW-AS FILL-IN 
     SIZE 48.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SAK AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Säkring (A)" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKYLTANT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKYLTBEN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TYP AS CHARACTER FORMAT "X(40)":U 
     LABEL "Skåptyp" 
     VIEW-AS FILL-IN 
     SIZE 26.13 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VALFRI AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Skåp", 1,
"Apparat", 2
     SIZE 30.63 BY .71 NO-UNDO.

DEFINE VARIABLE TOG_SORT AS LOGICAL INITIAL no 
     LABEL "För excel kort sortera efter" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.5 BY .79 NO-UNDO.

DEFINE BUTTON BTN_ANL 
     LABEL "Anl.register" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTKOPPID 
     LABEL "Ta bort id" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_HMT 
     LABEL "Hämta" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KOPPID 
     LABEL "Koppla identitet" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NASTKONID 
     LABEL "Nästa Konst" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_DEL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Val Nr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-A AS CHARACTER FORMAT "X(1)":U 
      VIEW-AS TEXT 
     SIZE 2.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-B AS CHARACTER FORMAT "X(1)":U 
      VIEW-AS TEXT 
     SIZE 2.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-C AS CHARACTER FORMAT "X(1)":U 
      VIEW-AS TEXT 
     SIZE 2.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ID1 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 26 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-ID2 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 26 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR1 AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR1LABEL AS CHARACTER FORMAT "X(256)":U INITIAL "Nr:" 
      VIEW-AS TEXT 
     SIZE 7.13 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR2 AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Tryck enter för att koppla identitet." NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR2LABEL AS CHARACTER FORMAT "X(256)":U INITIAL "Nr:" 
      VIEW-AS TEXT 
     SIZE 7.13 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-KARTR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kartreferens" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Tryck enter för att koppla identitet." NO-UNDO.

DEFINE VARIABLE FILL-IN-NR1 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR2 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR3 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SUND AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Tryck enter för att koppla identitet." NO-UNDO.

DEFINE VARIABLE FILL-IN-SUNDLABEL AS CHARACTER FORMAT "X(256)":U INITIAL "Fri id:" 
      VIEW-AS TEXT 
     SIZE 7.13 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB1 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 26 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB2 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 26 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB3 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 26 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE TOG_A AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_AS AS LOGICAL INITIAL no 
     LABEL "Automatisk Sortering" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.75 BY .88 TOOLTIP "Sorterar Konstruktionen på rätt plats" NO-UNDO.

DEFINE VARIABLE TOG_B AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_C AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2 BY .88 NO-UNDO.

DEFINE BUTTON BTN_BORTKAB 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_KSKAPKAB 
     LABEL "Koppla skåp" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MINKAB 
     LABEL "Mindre" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NERKAB 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "" 
     SIZE 4 BY 1.5.

DEFINE BUTTON BTN_PLUSKAB 
     LABEL "Större" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_TYP1KAB 
     IMAGE-UP FILE "BILDER\typ1":U
     LABEL "Typ1" 
     SIZE 9 BY 2.63.

DEFINE BUTTON BTN_TYP2KAB 
     IMAGE-UP FILE "BILDER\typ2":U
     LABEL "Typ2" 
     SIZE 9 BY 2.63.

DEFINE BUTTON BTN_TYP3KAB 
     IMAGE-UP FILE "BILDER\typ3":U
     LABEL "Typ3" 
     SIZE 9 BY 2.63.

DEFINE BUTTON BTN_UPPKAB 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5.

DEFINE BUTTON FBTN_ANTKAB 
     LABEL "ConnectIT" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_KORTKAB 
     LABEL "Kort" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_MTRLKAB 
     LABEL "Materiel" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ADRKAB AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kabeladress" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-B1KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B2KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B3KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-EON AS CHARACTER FORMAT "X(256)":U INITIAL "Minimikrav på reserv E.ON: 12" 
      VIEW-AS TEXT 
     SIZE 31 BY .63
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN-F1KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F2KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F3KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOPKAB AS CHARACTER FORMAT "X(256)":U INITIAL "Kopplingstyp" 
      VIEW-AS TEXT 
     SIZE 19.63 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOPSKAP AS CHARACTER FORMAT "X(256)":U INITIAL "Inget skåp är valt!" 
      VIEW-AS TEXT 
     SIZE 21 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-KORTKAB AS CHARACTER FORMAT "X(256)":U INITIAL "Kabelskåp" 
      VIEW-AS TEXT 
     SIZE 12.5 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEDKAB AS INTEGER FORMAT "->>>9":U INITIAL 0 
     LABEL "Reserv" 
      VIEW-AS TEXT 
     SIZE 8 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-MODUL AS INTEGER FORMAT "->>>9":U INITIAL 0 
     LABEL "Använda moduler" 
      VIEW-AS TEXT 
     SIZE 8 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR1KAB AS INTEGER FORMAT ">":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 2.38 BY .75
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR2KAB AS INTEGER FORMAT ">":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 2.38 BY .75
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR3KAB AS INTEGER FORMAT ">":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 2.38 BY .75
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-T1 AS CHARACTER FORMAT "X(256)":U INITIAL "Upp till 95" 
      VIEW-AS TEXT 
     SIZE 11.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-T2 AS CHARACTER FORMAT "X(256)":U INITIAL "Area>95-150" 
      VIEW-AS TEXT 
     SIZE 12.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-T3 AS CHARACTER FORMAT "X(256)":U INITIAL "Area >150" 
      VIEW-AS TEXT 
     SIZE 11.5 BY .63 NO-UNDO.

DEFINE BUTTON BTN_BORT2 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_GRUNDUPP 
     LABEL "Hämta Kalkylkoder" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NYKOD 
     LABEL "KALKYLERA" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NER 
     IMAGE-UP FILE "BILDER\redpilner.bmp":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_UPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade väljs".

DEFINE VARIABLE CMB_VAL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-B1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B4 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B5 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B6 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE RAD_KOMP AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Skapa nytt skåp", 1,
"Komplettera skåp", 2,
"Komplettera med endast mtrl", 3
     SIZE 19.75 BY 1.67 NO-UNDO.

DEFINE VARIABLE TOG_KOMP AS LOGICAL INITIAL yes 
     LABEL "Visa hela konstruktionen" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.75 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_MARKAV AS LOGICAL INITIAL no 
     LABEL "Avmarkera samtliga val" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.75 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_RATT AS LOGICAL INITIAL no 
     LABEL "Gå till befintlig konstruktion" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.75 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_VISAGRUPP AS LOGICAL INITIAL no 
     LABEL "Visa endast vald konstruktionsgrupp" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.38 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_VISAKONST AS LOGICAL INITIAL no 
     LABEL "Visa endast vald konstruktion" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.38 BY .79 NO-UNDO.

DEFINE BUTTON BTN_ANDRAKOPUPP 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTKOPUPP 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FAVOUPP 
     LABEL "Spara upplag som favorit" 
     SIZE 25 BY 1.

DEFINE BUTTON BTN_NYKOPUPP 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_BKOMBKOPUPP 
     LABEL "Ta bort kombinationer" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_KOMBKOPUPP 
     LABEL "Skapa kombinationer" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OUPPKOPUPP 
     LABEL "Ta bort koppl. upplag" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIVKOPUPP 
     LABEL "Skriv ut upplag" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIVKOPUPP-2 
     LABEL "Skriv ut kombinationer" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_UPPKOPUPP 
     LABEL "Koppla upplag" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISAKOPUPP 
     LABEL "Visa upplag" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISAKOPUPP-2 
     LABEL "Visa kombinationer" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ANDLIN 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVBLIN 
     LABEL "Åter" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORTLIN 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_LINUPP 
     LABEL "Upplag" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MINLIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_SKAPALIN 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPLIN 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_VISA-2LIN 
     LABEL "Kontroll linor/kablar" 
     SIZE 22.5 BY 1.

DEFINE BUTTON FBTN_KORTLIN 
     LABEL "Kort ANVÄNDS EJ" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SCHAKT 
     LABEL "Schakt" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISALIN 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_VAL2LIN AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 40.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VALLIN AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTALLIN AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 3.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LANGDLIN AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Längd (m)" 
     VIEW-AS FILL-IN 
     SIZE 10.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LINKAB AS CHARACTER FORMAT "X(256)":U INITIAL "Linor och kablar" 
      VIEW-AS TEXT 
     SIZE 20 BY 1.21
     FONT 17 NO-UNDO.

DEFINE BUTTON BTN_ANDRALINUPP 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVBLINUPP 
     LABEL "Åter" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORTLINUPP 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KOPPLINUPP 
     LABEL "Koppla" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MINLINUPP 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NOLLLINUPP 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NYLINUPP 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SKRIVLINUPP 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPLINUPP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_VISALINUPP 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BENLINUPP AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 30.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENRLINUPP AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LINKABLINUPP AS CHARACTER FORMAT "X(256)":U INITIAL "Upplag för linor och kablar" 
      VIEW-AS TEXT 
     SIZE 38.5 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-METERLINUPP AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Längd" 
      VIEW-AS TEXT 
     SIZE 12 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROCLINUPP AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Procent" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SUMMALINUPP AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "på upplag" 
      VIEW-AS TEXT 
     SIZE 12 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOTMETERLINUPP AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Total längd" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPPLINUPP AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Upplag" 
      VIEW-AS TEXT 
     SIZE 3.63 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPPMETERLINUPP AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "till upplag" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE BUTTON BTN_AVBLIST AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXLIST 
     LABEL "Visa I EXCEL" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXLISTENK 
     LABEL "ENKELEXCEL" 
     SIZE 14 BY 1 TOOLTIP "Snabb, men enkel Excellista".

DEFINE BUTTON BTN_IELIST 
     LABEL "Visa I ie" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PDFLIST 
     LABEL "Visa I PDF" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIVLIST 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISALIST 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_PRIS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pris" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Visa pris","Visa inte pris" 
     DROP-DOWN-LIST
     SIZE 16.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_SKRIV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vid utskrift" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Använda standardskrivare","Välja annan skrivare" 
     DROP-DOWN-LIST
     SIZE 31.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_STOLP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Materiel" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Visa allt","Bara stolpar","Inga stolpar" 
     DROP-DOWN-LIST
     SIZE 22.88 BY 1 NO-UNDO.

DEFINE VARIABLE ED_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 51.5 BY 4
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 51.75 BY 12.75 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ANM AS LOGICAL INITIAL no 
     LABEL "Anmärkning" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EGEN AS LOGICAL INITIAL no 
     LABEL "Egenkontroll" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EGENSIDA AS LOGICAL INITIAL no 
     LABEL "Egenkontroll med sidbrytning" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.38 BY .79 TOOLTIP "Egenkontrollerna kommer att hamna på en egen sida." NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EXCELKALK AS LOGICAL INITIAL no 
     LABEL "Kalkylkoder bredvid Materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.38 BY .79 TOOLTIP "Placerar kalkylkoder på sidan om materielen. Nytt utseende på Egenkontroll" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ID AS LOGICAL INITIAL no 
     LABEL "Identitet" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.88 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_JMFKONS AS LOGICAL INITIAL no 
     LABEL "Jämför mtrl med konstruktion" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.88 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KON AS LOGICAL INITIAL no 
     LABEL "Vill Ni vid utskrift få en konstruktion / sida?" 
     VIEW-AS TOGGLE-BOX
     SIZE 51 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_KONSTRUKTION AS LOGICAL INITIAL no 
     LABEL "Konstruktionsgrupp" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KONVAL AS LOGICAL INITIAL no 
     LABEL "Vill Ni välja konstruktioner?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_MTRL AS LOGICAL INITIAL no 
     LABEL "Materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_P3 AS LOGICAL INITIAL no 
     LABEL "P3-koder" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_SIDEXCEL AS LOGICAL INITIAL no 
     LABEL "Texter i sidokolumn i Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.25 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_SPANN AS LOGICAL INITIAL no 
     LABEL "Spann/kabellängd" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .79 TOOLTIP "ANVÄNDS INTE" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_TEXT AS LOGICAL INITIAL no 
     LABEL "Mtrl text" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_UPP AS LOGICAL INITIAL no 
     LABEL "Upplag" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.25 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Konstruktionsval" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .79 NO-UNDO.

DEFINE BUTTON BTN_ALLTMTRL 
     LABEL "Allt bort" 
     SIZE 12 BY 1 TOOLTIP "Tar bort allt materiel som är lagrat på konstruktionen"
     BGCOLOR 8 .

DEFINE BUTTON BTN_ANTAL 
     LABEL "Antal" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_KUNDLISTA 
     LABEL "KUND" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_LEV 
     LABEL "Åter huvudleverantör" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_LINKAB 
     LABEL "Lin/Kab":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NOLL 
     LABEL "Ta bort 0" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NY 
     LABEL "Kopi. mtrl" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u.bmp":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_PAR 
     LABEL "Parstolpe" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_SATS 
     LABEL "Satsinformation" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_SPEC 
     LABEL "Spec.mtrl" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_TEXT 
     LABEL "Text" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 28.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BENSTOLP AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 28.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENRSTOLP AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOMM AS CHARACTER FORMAT "X(94)":U 
     VIEW-AS FILL-IN 
     SIZE 63.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14.5 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_MTRL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Mtrl", 1,
"Stolpar", 2,
"Transf.", 3,
"Favoriter", 4
     SIZE 31.5 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 40 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.5 BY 3.63
     BGCOLOR 8 .

DEFINE BUTTON BTN_BACKID 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_KOPPIDORD 
     LABEL "Koppla identitet" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta ner".

DEFINE BUTTON BTN_MORDNERID 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta ner".

DEFINE BUTTON BTN_MORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta upp".

DEFINE BUTTON BTN_MORDUPPID 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta upp".

DEFINE BUTTON BTN_NERORD 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Btn 1" 
     SIZE 5 BY 1.75.

DEFINE BUTTON BTN_OVERID 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_UPPORD 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "Btn 1" 
     SIZE 5 BY 1.75.

DEFINE VARIABLE CMB_VALORD AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ID1ORD AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 12 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-ID2ORD AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 12 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNRORDID AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNRORDID2 AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 TOOLTIP "Tryck enter för att koppla identitet." NO-UNDO.

DEFINE VARIABLE RAD_ORD AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Skapa en ny plats för konstruktioner", 1,
"Ändra på ordning för en eller flera konstruktioner", 2,
"Identifera flera konstruktioner", 3
     SIZE 95 BY 1 NO-UNDO.

DEFINE BUTTON BTN_ALLTMTRLSM 
     LABEL "Allt bort" 
     SIZE 12 BY 1 TOOLTIP "Tar bort allt materiel som är lagrat på konstruktionen"
     BGCOLOR 8 .

DEFINE BUTTON BTN_ANTALSM 
     LABEL "Antal" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BACKSM 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_KOMPSKAP 
     LABEL "Komplettera" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_KUNDLISTASM 
     LABEL "KUND" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_LEVSM 
     LABEL "Åter huvudleverantör" 
     SIZE 20 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_LINKABSM 
     LABEL "Lin/Kab":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NASTKON 
     LABEL "Nästa Konst" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NOLLSM 
     LABEL "Ta bort 0" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NYSM 
     LABEL "Kopi. mtrl" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OVERSM 
     IMAGE-UP FILE "BILDER\next-u.bmp":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_SATSSM 
     LABEL "Satsinformation" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_SPECSM 
     LABEL "Spec.mtrl" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_TEXTSM 
     LABEL "Text" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_LEVSM AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ADRKABM AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kabeladress" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-B2SM AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B3SM AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-BENSM AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 29.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BENSTOLPSM AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 29.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENRSM AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 18.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENRSTOLPSM AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 18.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F2SM AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F3SM AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KODSM AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOMMSM AS CHARACTER FORMAT "X(94)":U 
     VIEW-AS FILL-IN 
     SIZE 61.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALTSM AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

DEFINE IMAGE IMAGE-6
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .75.

DEFINE VARIABLE RAD_MTRLSM AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Mtrl", 1,
"Stolpar", 2,
"Transf.", 3,
"Favoriter", 4
     SIZE 39 BY .79 NO-UNDO.

DEFINE VARIABLE RAD_SOKSM AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 32.38 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49.5 BY 3.54.

DEFINE VARIABLE TOG_ALLSKMTRL AS LOGICAL INITIAL no 
     LABEL "Allt mtrl" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.75 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANDKONST FOR 
      andrakon_val SCROLLING.

DEFINE QUERY BRW_ANDRAK FOR 
      andrakon_val SCROLLING.

DEFINE QUERY BRW_ANM FOR 
      fastanmtemp SCROLLING.

DEFINE QUERY BRW_APP FOR 
      apparattemp SCROLLING.

DEFINE QUERY BRW_B2 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B3 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B4 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B5 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B6 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_BILD FOR 
      bildbertemp SCROLLING.

DEFINE QUERY BRW_FAVOUPPLAG FOR 
      favberupptemp SCROLLING.

DEFINE QUERY BRW_FRAN FOR 
      kon_id SCROLLING.

DEFINE QUERY BRW_FRANLIN FOR 
      kon_id SCROLLING.

DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_HLEVSM FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_KABFRI FOR 
      fri_temp SCROLLING.

DEFINE QUERY BRW_KABKAB FOR 
      kon_val, 
      fri_temp SCROLLING.

DEFINE QUERY BRW_KABSM FOR 
      kon_val, 
      fri_temp SCROLLING.

DEFINE QUERY BRW_KALAKONS FOR 
      kalk_temp SCROLLING.

DEFINE QUERY BRW_KALK FOR 
      kalk_temp SCROLLING.

DEFINE QUERY BRW_KB2 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB3 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB4 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB5 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB6 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KOMBKOPUPP FOR 
      kombtemp SCROLLING.

DEFINE QUERY BRW_KON FOR 
      konsttemp SCROLLING.

DEFINE QUERY BRW_KONANDKONST FOR 
      konsttemp SCROLLING.

DEFINE QUERY BRW_LIN FOR 
      kopp_lina SCROLLING.

DEFINE QUERY BRW_LINLINUPP FOR 
      lin_tab2 SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      list_mtrl SCROLLING.

DEFINE QUERY BRW_MTRLFRI FOR 
      mtrl_tab SCROLLING.

DEFINE QUERY BRW_MTRLSM FOR 
      list_mtrl SCROLLING.

DEFINE QUERY BRW_ORD FOR 
      kon_val SCROLLING.

DEFINE QUERY BRW_ORDID FOR 
      kon_val SCROLLING.

DEFINE QUERY BRW_ORDIDVAL FOR 
      idkon_val SCROLLING.

DEFINE QUERY BRW_SATS FOR 
      satstemp SCROLLING.

DEFINE QUERY BRW_SATSSM FOR 
      satstemp SCROLLING.

DEFINE QUERY BRW_SKAPKAB FOR 
      berskaptemp SCROLLING.

DEFINE QUERY BRW_SOKMTRLF FOR 
      soktamtrlfavo SCROLLING.

DEFINE QUERY BRW_SOKMTRLFSM FOR 
      soktamtrlfavo SCROLLING.

DEFINE QUERY BRW_STOLP FOR 
      berstolptemp SCROLLING.

DEFINE QUERY BRW_STOLPSM FOR 
      berstolptemp SCROLLING.

DEFINE QUERY BRW_TAB1 FOR 
      berfortemp SCROLLING.

DEFINE QUERY BRW_TAB2 FOR 
      berlinjetemp SCROLLING.

DEFINE QUERY BRW_TAB3 FOR 
      bernattemp SCROLLING.

DEFINE QUERY BRW_TILLLIN FOR 
      kon_id2 SCROLLING.

DEFINE QUERY BRW_UPPKOPUPP FOR 
      berupptemp SCROLLING.

DEFINE QUERY BRW_UPPLINUPP FOR 
      berupptemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_display SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANDKONST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANDKONST WINDOW-3 _STRUCTURED
  QUERY BRW_ANDKONST NO-LOCK DISPLAY
      andrakon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 12
            LABEL-FGCOLOR 12
      andrakon_val.F2 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
      andrakon_val.F3 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
      andrakon_val.F4 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
      andrakon_val.F5 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
      andrakon_val.F6 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 80.63 BY 7.29
         FONT 4.

DEFINE BROWSE BRW_ANDRAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANDRAK WINDOW-3 _STRUCTURED
  QUERY BRW_ANDRAK NO-LOCK DISPLAY
      andrakon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 12
            LABEL-FGCOLOR 12
      andrakon_val.F2 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
      andrakon_val.F3 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
      andrakon_val.F4 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
      andrakon_val.F5 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
      andrakon_val.F6 FORMAT "X(256)":U WIDTH 12 LABEL-FGCOLOR 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 80.5 BY 7.29
         FONT 4.

DEFINE BROWSE BRW_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANM WINDOW-3 _STRUCTURED
  QUERY BRW_ANM NO-LOCK DISPLAY
      fastanmtemp.ANMARK COLUMN-LABEL "Fasta anmärkningar" FORMAT "X(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 53 BY 8
         TITLE "Fasta anmärkningar".

DEFINE BROWSE BRW_APP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_APP WINDOW-3 _STRUCTURED
  QUERY BRW_APP NO-LOCK DISPLAY
      apparattemp.BENAMNING FORMAT "X(256)":U WIDTH 76
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 79 BY 7.17
         TITLE "Apparater" ROW-HEIGHT-CHARS .5.

DEFINE BROWSE BRW_B2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B2 WINDOW-3 _STRUCTURED
  QUERY BRW_B2 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B3 WINDOW-3 _STRUCTURED
  QUERY BRW_B3 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B4 WINDOW-3 _STRUCTURED
  QUERY BRW_B4 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B5 WINDOW-3 _STRUCTURED
  QUERY BRW_B5 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B6 WINDOW-3 _STRUCTURED
  QUERY BRW_B6 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_BILD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BILD WINDOW-3 _STRUCTURED
  QUERY BRW_BILD NO-LOCK DISPLAY
      bildbertemp.NAMN COLUMN-LABEL "Egen text" FORMAT "X(25)":U
            WIDTH 23.63
      bildbertemp.FILNAMN COLUMN-LABEL "Sökväg" FORMAT "X(256)":U
            WIDTH 81.5
  ENABLE
      bildbertemp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-AUTO-VALIDATE NO-COLUMN-SCROLLING SIZE 110.63 BY 8.

DEFINE BROWSE BRW_FAVOUPPLAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FAVOUPPLAG WINDOW-3 _STRUCTURED
  QUERY BRW_FAVOUPPLAG NO-LOCK DISPLAY
      favberupptemp.ADRESS COLUMN-LABEL "Adress" FORMAT "X(256)":U
            WIDTH 20
      favberupptemp.POSTNR COLUMN-LABEL "Postnr" FORMAT "xxx xx":U
            WIDTH 7
      favberupptemp.ORT COLUMN-LABEL "Ort" FORMAT "x(256)":U WIDTH 10
      favberupptemp.XKORDCH COLUMN-LABEL "X" FORMAT "x(256)":U
            WIDTH 10
      favberupptemp.YKORDCH COLUMN-LABEL "Y" FORMAT "x(256)":U
            WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 42.5 BY 6.25
         TITLE "Favoriter Upplag - Dubbelklicka för att lägga upp upplaget på beredningen".

DEFINE BROWSE BRW_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRAN WINDOW-3 _STRUCTURED
  QUERY BRW_FRAN NO-LOCK DISPLAY
      kon_id.LINNR FORMAT "X(256)":U WIDTH 22
      kon_id.NATNR FORMAT "X(256)":U WIDTH 22
      kon_id.FRI1 FORMAT ">>>>>>>>>>":U
      kon_id.FRI2 FORMAT ">>>>>>>>>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 68.5 BY 21.67
         TITLE "Identifierade konstruktioner".

DEFINE BROWSE BRW_FRANLIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRANLIN WINDOW-3 _STRUCTURED
  QUERY BRW_FRANLIN NO-LOCK DISPLAY
      kon_id.LINNR FORMAT "X(14)":U WIDTH 12
      kon_id.NATNR FORMAT "X(14)":U WIDTH 12
      kon_id.FRI1 FORMAT ">>>>>>>>>>":U WIDTH 8
      kon_id.FRI2 FORMAT ">>>>>>>>>>":U WIDTH 8
      kon_id.FRI3 FORMAT "X(10)":U WIDTH 8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 53.38 BY 12.38
         TITLE "Från".

DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-3 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 30
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
      mtrltemp.LEVKOD COLUMN-LABEL "Lev" FORMAT "X(6)":U
      mtrltemp.LEVNAMN COLUMN-LABEL "Leverantörsnamn" FORMAT "x(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 56.5 BY 10.33
         TITLE "Materiel".

DEFINE BROWSE BRW_HLEVSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEVSM WINDOW-3 _STRUCTURED
  QUERY BRW_HLEVSM NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 25
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
      mtrltemp.LEVKOD COLUMN-LABEL "Lev" FORMAT "X(6)":U
      mtrltemp.LEVNAMN COLUMN-LABEL "Leverantörsnamn" FORMAT "x(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 11
         TITLE "Materiellista".

DEFINE BROWSE BRW_KABFRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KABFRI WINDOW-3 _STRUCTURED
  QUERY BRW_KABFRI DISPLAY
      fri_temp.GRUPPNR FORMAT "X(256)":U WIDTH 4
      fri_temp.BENAMNING COLUMN-LABEL "Kabeltyp" FORMAT "x(256)":U
            WIDTH 30
      fri_temp.KABADR COLUMN-LABEL "KabelAdress" FORMAT "X(256)":U
            WIDTH 32
      fri_temp.METER FORMAT ">>>9":U
      fri_temp.TYP COLUMN-LABEL "Typ" FORMAT "X(8)":U WIDTH 5
      fri_temp.APPARAT COLUMN-LABEL "Apparat" FORMAT "X(256)":U
            WIDTH 20
      fri_temp.SAKR COLUMN-LABEL "Säkr." FORMAT ">>>":U
      fri_temp.MAXSAKR COLUMN-LABEL "Max !Säkr." FORMAT ">>>":U
      fri_temp.SKAPADR COLUMN-LABEL "SkåpAdress" FORMAT "X(256)":U
            WIDTH 17
      fri_temp.ANMARK COLUMN-LABEL "Anmärkning" FORMAT "x(256)":U
            WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 88.5 BY 6.46.

DEFINE BROWSE BRW_KABKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KABKAB WINDOW-3 _STRUCTURED
  QUERY BRW_KABKAB NO-LOCK DISPLAY
      kon_val.F2 FORMAT "X(256)":U WIDTH 17.5
      kon_val.F3 FORMAT "X(256)":U WIDTH 17.5
      kon_val.F4 FORMAT "X(256)":U WIDTH 18
      kon_val.F5 FORMAT "X(256)":U WIDTH 18
      kon_val.F6 FORMAT "X(256)":U WIDTH 15
      kon_val.TYP FORMAT ">":U
      fri_temp.KABADR FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 103 BY 6.92.

DEFINE BROWSE BRW_KABSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KABSM WINDOW-3 _STRUCTURED
  QUERY BRW_KABSM NO-LOCK DISPLAY
      kon_val.F1 FORMAT "X(256)":U WIDTH 17.75
      kon_val.F2 FORMAT "X(256)":U WIDTH 17.5
      kon_val.F3 FORMAT "X(256)":U WIDTH 17.5
      kon_val.F4 FORMAT "X(256)":U WIDTH 17.5
      kon_val.F5 FORMAT "X(256)":U WIDTH 17.5
      kon_val.F6 FORMAT "X(256)":U WIDTH 16
      fri_temp.KABADR FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 110 BY 6.38.

DEFINE BROWSE BRW_KALAKONS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALAKONS WINDOW-3 _STRUCTURED
  QUERY BRW_KALAKONS NO-LOCK DISPLAY
      kalk_temp.ARBKOD FORMAT "X(5)":U
      kalk_temp.LOPNR FORMAT ">>>":U
      kalk_temp.BENAMNING FORMAT "x(40)":U
      kalk_temp.ANTAL FORMAT "->>,>>9.999":U
      kalk_temp.ENHET FORMAT "x(5)":U
      kalk_temp.NUM FORMAT ">>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 28.5 BY 6.54
         TITLE "Byt till Konstruktion".

DEFINE BROWSE BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALK WINDOW-3 _STRUCTURED
  QUERY BRW_KALK NO-LOCK DISPLAY
      kalk_temp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(7)":U
      kalk_temp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      kalk_temp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(40)":U
      kalk_temp.ANTAL COLUMN-LABEL "Antal" FORMAT "->>>>9.999":U
      kalk_temp.ENHET COLUMN-LABEL "Enhet" FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 76.5 BY 12
         TITLE "P3 koder".

DEFINE BROWSE BRW_KB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB2 WINDOW-3 _STRUCTURED
  QUERY BRW_KB2 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29 ROW-HEIGHT-CHARS .46.

DEFINE BROWSE BRW_KB3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB3 WINDOW-3 _STRUCTURED
  QUERY BRW_KB3 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB4 WINDOW-3 _STRUCTURED
  QUERY BRW_KB4 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB5 WINDOW-3 _STRUCTURED
  QUERY BRW_KB5 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB6 WINDOW-3 _STRUCTURED
  QUERY BRW_KB6 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KOMBKOPUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOMBKOPUPP WINDOW-3 _STRUCTURED
  QUERY BRW_KOMBKOPUPP NO-LOCK DISPLAY
      kombtemp.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 15
      kombtemp.FORNR FORMAT "X(256)":U WIDTH 15
      kombtemp.LINNR FORMAT "X(256)":U WIDTH 10
      kombtemp.NATNR FORMAT "X(256)":U WIDTH 14.5
      kombtemp.FRI1 FORMAT ">>>>>>>>>>":U
      kombtemp.FRI2 FORMAT ">>>>>>>>>>":U
      kombtemp.ID FORMAT "Ja/Nej":U WIDTH 4
      kombtemp.ID2 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 19.5
      kombtemp.UPPLAG FORMAT ">>>>>9":U
      kombtemp.KOMB FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 109.5 BY 14.5
         TITLE "Konstruktioner".

DEFINE BROWSE BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON WINDOW-3 _STRUCTURED
  QUERY BRW_KON NO-LOCK DISPLAY
      konsttemp.KTYPKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U WIDTH 10
      konsttemp.BENAMNING FORMAT "X(256)":U WIDTH 25
      konsttemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KONANDKONST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KONANDKONST WINDOW-3 _STRUCTURED
  QUERY BRW_KONANDKONST NO-LOCK DISPLAY
      konsttemp.KTYPKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U WIDTH 10
      konsttemp.BENAMNING FORMAT "X(256)":U WIDTH 17
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 28.5 BY 6.54
         TITLE "Byt till Konstruktion".

DEFINE BROWSE BRW_LIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LIN WINDOW-3 _STRUCTURED
  QUERY BRW_LIN NO-LOCK DISPLAY
      kopp_lina.ENR FORMAT "X(11)":U
      kopp_lina.BENAMNING FORMAT "x(256)":U WIDTH 32
      kopp_lina.METER FORMAT ">>>,>>9":U
      kopp_lina.LEDARE COLUMN-LABEL "Antal" FORMAT "9":U
  ENABLE
      kopp_lina.METER
      kopp_lina.LEDARE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 61.88 BY 7.17
         TITLE "Linor och Kablar".

DEFINE BROWSE BRW_LINLINUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LINLINUPP WINDOW-3 _STRUCTURED
  QUERY BRW_LINLINUPP NO-LOCK DISPLAY
      lin_tab2.ENR FORMAT "X(11)":U
      lin_tab2.BENAMNING FORMAT "x(256)":U WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 54.63 BY 9.92
         TITLE "Linor/kablar".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-3 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      list_mtrl.ENR FORMAT "X(11)":U
      list_mtrl.BENAMNING FORMAT "x(256)":U WIDTH 29
      list_mtrl.ANTAL FORMAT ">>>>>9":U
      list_mtrl.ENHET COLUMN-LABEL "Enh" FORMAT "x(5)":U WIDTH 2.75
      list_mtrl.LINKAB COLUMN-LABEL "L/K" FORMAT "Ja/Nej":U WIDTH 2.25
      list_mtrl.PRIS FORMAT ">>>>99.99":U
      list_mtrl.LEVKOD COLUMN-LABEL "Lev" FORMAT "x(2)":U WIDTH 2.5
  ENABLE
      list_mtrl.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 63.25 BY 12.25
         TITLE "Vald materiel".

DEFINE BROWSE BRW_MTRLFRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRLFRI WINDOW-3 _STRUCTURED
  QUERY BRW_MTRLFRI NO-LOCK DISPLAY
      mtrl_tab.BENAMNING FORMAT "x(256)":U WIDTH 84.88
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 88.5 BY 7.17
         TITLE "Skåp".

DEFINE BROWSE BRW_MTRLSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRLSM WINDOW-3 _STRUCTURED
  QUERY BRW_MTRLSM NO-LOCK DISPLAY
      list_mtrl.ENR FORMAT "X(11)":U WIDTH 10
      list_mtrl.BENAMNING FORMAT "x(256)":U WIDTH 25
      list_mtrl.ANTAL FORMAT ">>>>>9":U
      list_mtrl.ENHET FORMAT "x(5)":U
      list_mtrl.LINKAB FORMAT "Ja/Nej":U
      list_mtrl.PRIS FORMAT ">>>>99.99":U
      list_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U WIDTH 5
  ENABLE
      list_mtrl.ANTAL
      list_mtrl.LINKAB
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 61.38 BY 13
         TITLE "Vald materiel".

DEFINE BROWSE BRW_ORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ORD WINDOW-3 _STRUCTURED
  QUERY BRW_ORD NO-LOCK DISPLAY
      kon_val.ID2 FORMAT "X(256)":U WIDTH 15.5
      kon_val.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 21.5
      kon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 12
      kon_val.F2 FORMAT "X(256)":U WIDTH 12
      kon_val.F3 FORMAT "X(256)":U WIDTH 12
      kon_val.F4 FORMAT "X(256)":U WIDTH 12
      kon_val.F5 FORMAT "X(256)":U WIDTH 12
      kon_val.F6 FORMAT "X(256)":U WIDTH 12
      kon_val.EXTRA3 COLUMN-LABEL "+KB" FORMAT "X(3)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 115 BY 21.92
         TITLE "Ändra ordning".

DEFINE BROWSE BRW_ORDID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ORDID WINDOW-3 _STRUCTURED
  QUERY BRW_ORDID NO-LOCK DISPLAY
      kon_val.ID2 FORMAT "X(256)":U WIDTH 7
      kon_val.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 9
      kon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 12
      kon_val.F2 FORMAT "X(256)":U WIDTH 12
      kon_val.F3 FORMAT "X(256)":U WIDTH 9
      kon_val.F4 FORMAT "X(256)":U WIDTH 12
      kon_val.F5 FORMAT "X(256)":U WIDTH 12
      kon_val.F6 FORMAT "X(256)":U WIDTH 12
      kon_val.EXTRA3 COLUMN-LABEL "+KB" FORMAT "X(3)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 53 BY 21.92
         TITLE "Välj konstruktioner att ändra identitet på".

DEFINE BROWSE BRW_ORDIDVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ORDIDVAL WINDOW-3 _STRUCTURED
  QUERY BRW_ORDIDVAL NO-LOCK DISPLAY
      idkon_val.ID2 FORMAT "X(256)":U WIDTH 7
      idkon_val.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(256)":U
            WIDTH 9
      idkon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 12
      idkon_val.F2 FORMAT "X(256)":U WIDTH 12
      idkon_val.F3 FORMAT "X(256)":U WIDTH 12
      idkon_val.F4 FORMAT "X(256)":U WIDTH 12
      idkon_val.F5 FORMAT "X(256)":U WIDTH 12
      idkon_val.F6 FORMAT "X(256)":U WIDTH 12
      idkon_val.EXTRA3 COLUMN-LABEL "+KB" FORMAT "X(3)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 53 BY 21.92
         TITLE "Valda konstruktioner att ändra identitet på".

DEFINE BROWSE BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SATS WINDOW-3 _STRUCTURED
  QUERY BRW_SATS NO-LOCK DISPLAY
      satstemp.KOD FORMAT "X(11)":U
      satstemp.BENAMNING FORMAT "x(256)":U WIDTH 42
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 56.5 BY 10.33.

DEFINE BROWSE BRW_SATSSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SATSSM WINDOW-3 _STRUCTURED
  QUERY BRW_SATSSM NO-LOCK DISPLAY
      satstemp.KOD FORMAT "X(11)":U
      satstemp.BENAMNING FORMAT "x(256)":U WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 11
         TITLE "Satslista".

DEFINE BROWSE BRW_SKAPKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SKAPKAB WINDOW-3 _STRUCTURED
  QUERY BRW_SKAPKAB NO-LOCK DISPLAY
      berskaptemp.KOD FORMAT "X(12)":U
      berskaptemp.BENAMNING FORMAT "x(256)":U WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 51.63 BY 5.25
         TITLE "Dubbelklicka på det skåp som ska användas".

DEFINE BROWSE BRW_SOKMTRLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SOKMTRLF WINDOW-3 _STRUCTURED
  QUERY BRW_SOKMTRLF NO-LOCK DISPLAY
      soktamtrlfavo.Enr FORMAT "X(11)":U
      soktamtrlfavo.Benamning FORMAT "x(256)":U WIDTH 25
      soktamtrlfavo.Enhet COLUMN-LABEL "Enh" FORMAT "x(3)":U
      soktamtrlfavo.NPRIS FORMAT ">>>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 56 BY 10.25
         TITLE "Sparade utsökningar".

DEFINE BROWSE BRW_SOKMTRLFSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SOKMTRLFSM WINDOW-3 _STRUCTURED
  QUERY BRW_SOKMTRLFSM NO-LOCK DISPLAY
      soktamtrlfavo.Enr FORMAT "X(11)":U
      soktamtrlfavo.Benamning FORMAT "x(256)":U WIDTH 25
      soktamtrlfavo.Enhet COLUMN-LABEL "Enh" FORMAT "x(3)":U
      soktamtrlfavo.NPRIS FORMAT ">>>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 11
         TITLE "Sparade utsökningar" ROW-HEIGHT-CHARS .25.

DEFINE BROWSE BRW_STOLP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STOLP WINDOW-3 _STRUCTURED
  QUERY BRW_STOLP NO-LOCK DISPLAY
      berstolptemp.ENR FORMAT "X(11)":U WIDTH 11.5
      berstolptemp.BENAMNING FORMAT "x(256)":U WIDTH 30
      berstolptemp.PRIS COLUMN-LABEL "Netto pris" FORMAT ">>>>99.99":U
            WIDTH 10.5
      berstolptemp.LEVKOD COLUMN-LABEL "Lev" FORMAT "x(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 56.5 BY 10.33.

DEFINE BROWSE BRW_STOLPSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STOLPSM WINDOW-3 _STRUCTURED
  QUERY BRW_STOLPSM NO-LOCK DISPLAY
      berstolptemp.ENR FORMAT "X(11)":U
      berstolptemp.BENAMNING FORMAT "x(256)":U WIDTH 25
      berstolptemp.PRIS COLUMN-LABEL "Netto pris" FORMAT ">>>>99.99":U
      berstolptemp.LEVKOD COLUMN-LABEL "Lev" FORMAT "x(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 49.5 BY 11.

DEFINE BROWSE BRW_TAB1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TAB1 WINDOW-3 _STRUCTURED
  QUERY BRW_TAB1 NO-LOCK DISPLAY
      berfortemp.FORNR FORMAT "X(10)":U
      berfortemp.NAMN FORMAT "X(30)":U
      berfortemp.OMRADE FORMAT "X(256)":U WIDTH 6
      berfortemp.AVDNR FORMAT "X(256)":U WIDTH 7
      berfortemp.VIJUDID FORMAT "X(256)":U WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 63.5 BY 4.21.

DEFINE BROWSE BRW_TAB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TAB2 WINDOW-3 _STRUCTURED
  QUERY BRW_TAB2 NO-LOCK DISPLAY
      berlinjetemp.LINNR FORMAT "X(10)":U
      berlinjetemp.NAMN FORMAT "X(256)":U WIDTH 49.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 63.5 BY 4.21.

DEFINE BROWSE BRW_TAB3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TAB3 WINDOW-3 _STRUCTURED
  QUERY BRW_TAB3 NO-LOCK DISPLAY
      bernattemp.NATNR FORMAT "X(10)":U
      bernattemp.NAMN FORMAT "X(256)":U WIDTH 49.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 63.5 BY 4.21 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BRW_TILLLIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TILLLIN WINDOW-3 _STRUCTURED
  QUERY BRW_TILLLIN NO-LOCK DISPLAY
      kon_id2.LINNR FORMAT "X(14)":U WIDTH 12
      kon_id2.NATNR FORMAT "X(14)":U WIDTH 12
      kon_id2.FRI1 FORMAT ">>>>>>>>>>":U WIDTH 8
      kon_id2.FRI2 FORMAT ">>>>>>>>>>":U WIDTH 8
      kon_id2.FRI3 FORMAT "X(10)":U WIDTH 8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52.88 BY 12.38
         TITLE "Till".

DEFINE BROWSE BRW_UPPKOPUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPPKOPUPP WINDOW-3 _STRUCTURED
  QUERY BRW_UPPKOPUPP NO-LOCK DISPLAY
      berupptemp.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>":U
      berupptemp.ADRESS COLUMN-LABEL "Adress" FORMAT "X(256)":U
            WIDTH 40
      berupptemp.POSTNR COLUMN-LABEL "Postnr" FORMAT "xxx xx":U
            WIDTH 10
      berupptemp.ORT COLUMN-LABEL "PostOrt" FORMAT "x(8)":U
      berupptemp.XKORDCH COLUMN-LABEL "X" FORMAT "x(256)":U WIDTH 13
      berupptemp.YKORDCH COLUMN-LABEL "Y" FORMAT "x(256)":U WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 81 BY 7.38
         TITLE "Upplagsregister".

DEFINE BROWSE BRW_UPPLINUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPPLINUPP WINDOW-3 _STRUCTURED
  QUERY BRW_UPPLINUPP NO-LOCK DISPLAY
      berupptemp.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>":U
      berupptemp.ADRESS COLUMN-LABEL "Adress" FORMAT "X(256)":U
            WIDTH 46
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 55.63 BY 9.67
         TITLE "Upplagsregister".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL WINDOW-3 _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_display.ORD FORMAT ">>>9":U
      kon_display.ID2 FORMAT "X(256)":U WIDTH 15.5
      kon_display.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(256)":U
            WIDTH 20.5
      kon_display.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 12
      kon_display.F2 FORMAT "X(256)":U WIDTH 12
      kon_display.F4 FORMAT "X(256)":U WIDTH 12
      kon_display.F3 FORMAT "X(256)":U WIDTH 12
      kon_display.F5 FORMAT "X(256)":U WIDTH 12
      kon_display.F6 FORMAT "X(256)":U WIDTH 12
      kon_display.EXTRA3 COLUMN-LABEL "+KB" FORMAT "X(3)":U
  ENABLE
      kon_display.EXTRA1
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 118 BY 8.92
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-BARABREDD
     BTN_OK AT ROW 1 COL 111.5
     RAD_VAL AT ROW 1 COL 122 NO-LABEL
     MBTN_ATERGA AT ROW 1 COL 103.63 WIDGET-ID 6
     BTN_AVB AT ROW 2.33 COL 111.5
     MBTN_SCHAKT AT ROW 1 COL 95.88 WIDGET-ID 4
     MBTN_INK AT ROW 1 COL 95.88 WIDGET-ID 2
     MBTN_FLYG AT ROW 1 COL 95.88
     MBTN_ANDORD AT ROW 1 COL 72.25
     MBTN_ATG AT ROW 1 COL 103.63
     MBTN_FRIKORT AT ROW 1 COL 56.5
     MBTN_KALK AT ROW 1 COL 64.38
     MBTN_KOPI AT ROW 1 COL 40.75
     BRW_VAL AT ROW 3.83 COL 1.5
     MBTN_KSKAP AT ROW 1 COL 48.63
     BTN_SOLEN AT ROW 3.83 COL 119.75
     MBTN_LIN AT ROW 1 COL 80.13
     BTN_ORDUPP AT ROW 6 COL 120
     BTN_ORDNER AT ROW 8.92 COL 120
     MBTN_LIST AT ROW 1 COL 95.88
     MBTN_SNABB AT ROW 1 COL 108
     MBTN_UPPL AT ROW 1 COL 88
     MBTN_ANM AT ROW 1 COL 17.13
     MBTN_DOK AT ROW 1 COL 32.88
     MBTN_ID AT ROW 1 COL 9.25
     MBTN_KONST AT ROW 1 COL 1
     MBTN_MTRL AT ROW 1 COL 25
     FILL-IN-SKAPMTRL AT ROW 17.5 COL 30 COLON-ALIGNED NO-LABEL
     RECT-MENY AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 28.29.

DEFINE FRAME FRAME-KABSKAP
     FILL-IN-ADRKAB AT ROW 4.63 COL 76.25 COLON-ALIGNED
     BRW_KABKAB AT ROW 5.58 COL 1.5
     FBTN_ANTKAB AT ROW 5.63 COL 110.13
     BTN_UPPKAB AT ROW 6.75 COL 105.13
     FBTN_KORTKAB AT ROW 6.75 COL 110.13
     FBTN_MTRLKAB AT ROW 7.83 COL 110.13
     BTN_NERKAB AT ROW 9.38 COL 105.13
     BTN_BORTKAB AT ROW 12.75 COL 39.5
     BTN_TYP1KAB AT ROW 15.17 COL 6.63
     BTN_TYP2KAB AT ROW 15.17 COL 26.38
     BTN_TYP3KAB AT ROW 15.17 COL 46.13
     BRW_SKAPKAB AT ROW 18.92 COL 6.38
     BTN_PLUSKAB AT ROW 20.13 COL 59.13
     BTN_MINKAB AT ROW 22.25 COL 59.13
     BTN_KSKAPKAB AT ROW 24.5 COL 25.5
     FILL-IN-KORTKAB AT ROW 1.13 COL 1.5 NO-LABEL
     FILL-IN-KOPSKAP AT ROW 1.13 COL 102 COLON-ALIGNED NO-LABEL
     FILL-IN-B1KAB AT ROW 2.38 COL 1.5 NO-LABEL
     FILL-IN-B3KAB AT ROW 2.38 COL 61.88 COLON-ALIGNED NO-LABEL
     FILL-IN-B2KAB AT ROW 2.42 COL 30.38 COLON-ALIGNED NO-LABEL
     FILL-IN-F3KAB AT ROW 3.63 COL 61.88 COLON-ALIGNED NO-LABEL
     FILL-IN-F1KAB AT ROW 3.67 COL 1.5 NO-LABEL
     FILL-IN-F2KAB AT ROW 3.71 COL 30.38 COLON-ALIGNED NO-LABEL
     FILL-IN-MODUL AT ROW 12.75 COL 16.75 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-KOPKAB AT ROW 13.5 COL 1.5 NO-LABEL
     FILL-IN-NR1KAB AT ROW 14.42 COL 8 COLON-ALIGNED NO-LABEL
     FILL-IN-NR2KAB AT ROW 14.42 COL 27.63 COLON-ALIGNED NO-LABEL
     FILL-IN-NR3KAB AT ROW 14.42 COL 47.38 COLON-ALIGNED NO-LABEL
     FILL-IN-T1 AT ROW 17.92 COL 4.75 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     FILL-IN-T2 AT ROW 17.92 COL 24.5 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     FILL-IN-T3 AT ROW 17.92 COL 44.25 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     FILL-IN-LEDKAB AT ROW 24.63 COL 12.38 COLON-ALIGNED
     FILL-IN-EON AT ROW 25.58 COL 4.5 COLON-ALIGNED NO-LABEL WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-KOPUPP
     BRW_KOMBKOPUPP AT ROW 2.67 COL 1.5
     FBTN_UPPKOPUPP AT ROW 7.33 COL 111.38
     FBTN_VISAKOPUPP AT ROW 8.42 COL 111.38
     FBTN_OUPPKOPUPP AT ROW 9.5 COL 111.38
     FBTN_SKRIVKOPUPP AT ROW 10.71 COL 111.38
     FBTN_KOMBKOPUPP AT ROW 11.79 COL 111.5
     FBTN_BKOMBKOPUPP AT ROW 12.92 COL 111.5
     FBTN_VISAKOPUPP-2 AT ROW 14 COL 111.5
     FBTN_SKRIVKOPUPP-2 AT ROW 15.08 COL 111.5
     BTN_FAVOUPP AT ROW 17.42 COL 89.5 WIDGET-ID 4
     BRW_UPPKOPUPP AT ROW 17.54 COL 1.5
     BRW_FAVOUPPLAG AT ROW 18.5 COL 83 WIDGET-ID 600
     BTN_NYKOPUPP AT ROW 25.33 COL 25.25
     BTN_ANDRAKOPUPP AT ROW 25.33 COL 38.25
     BTN_BORTKOPUPP AT ROW 25.33 COL 51.25
     "Koppla upplag samt kombinera konstruktioner" VIEW-AS TEXT
          SIZE 49.5 BY 1.21 AT ROW 1.13 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-LISTOR
     SEL_UPP AT ROW 1.42 COL 22.5 NO-LABEL
     BTN_SKRIVLIST AT ROW 7.21 COL 85
     BTN_VISALIST AT ROW 8.29 COL 85
     BTN_EXLIST AT ROW 9.42 COL 85
     BTN_EXLISTENK AT ROW 9.5 COL 85 WIDGET-ID 12
     BTN_IELIST AT ROW 10.5 COL 85
     BTN_PDFLIST AT ROW 11.63 COL 85
     CMB_SKRIV AT ROW 14.33 COL 20.63 COLON-ALIGNED
     CMB_PRIS AT ROW 15.46 COL 20.63 COLON-ALIGNED
     CMB_STOLP AT ROW 15.46 COL 49.13 COLON-ALIGNED
     TOG_MTRL AT ROW 16.5 COL 22.63
     TOG_KONSTRUKTION AT ROW 16.5 COL 36 WIDGET-ID 4
     TOG_VAL AT ROW 16.5 COL 64.75
     TOG_UPP AT ROW 17.5 COL 22.63
     TOG_ID AT ROW 17.5 COL 36
     TOG_ANM AT ROW 17.5 COL 64.75
     TOG_SPANN AT ROW 17.5 COL 89.5
     TOG_JMFKONS AT ROW 18.5 COL 22.63 WIDGET-ID 6
     TOG_P3 AT ROW 18.5 COL 64.75
     TOG_TEXT AT ROW 19.5 COL 22.63
     TOG_EGEN AT ROW 19.5 COL 36
     TOG_SIDEXCEL AT ROW 19.5 COL 64.75 WIDGET-ID 2
     TOG_KON AT ROW 20.5 COL 22.63
     TOG_EXCELKALK AT ROW 20.5 COL 64.75 WIDGET-ID 8
     TOG_KONVAL AT ROW 21.5 COL 22.63
     TOG_EGENSIDA AT ROW 21.5 COL 64.75 WIDGET-ID 10
     ED_MEDD AT ROW 22.17 COL 22.5 NO-LABEL
     BTN_AVBLIST AT ROW 25.17 COL 85.5
     "Listor" VIEW-AS TEXT
          SIZE 14 BY 1.21 AT ROW 1.13 COL 1.5
          FONT 17
     "Visa:" VIEW-AS TEXT
          SIZE 5.5 BY 1 AT ROW 16.5 COL 16.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-SKAPMTRL
     BRW_KABSM AT ROW 3.29 COL 1.5
     TOG_ALLSKMTRL AT ROW 9.83 COL 117.63
     RAD_MTRLSM AT ROW 9.75 COL 1.5 NO-LABEL
     BTN_OVERSM AT ROW 14.83 COL 51.25
     BTN_BACKSM AT ROW 17.21 COL 51.25
     BTN_LEVSM AT ROW 21.67 COL 19
     CMB_LEVSM AT ROW 21.71 COL 17.75 COLON-ALIGNED
     FILL-IN-KODSM AT ROW 23.13 COL 17.88 COLON-ALIGNED
     FILL-IN-ENRSM AT ROW 23.21 COL 17.88 COLON-ALIGNED
     FILL-IN-BENSM AT ROW 24.17 COL 17.88 COLON-ALIGNED
     RAD_SOKSM AT ROW 25.17 COL 17.63 NO-LABEL
     BTN_NYSM AT ROW 24.13 COL 57.75
     BTN_ANTALSM AT ROW 24.13 COL 70.63
     BTN_SATSSM AT ROW 24.13 COL 83.5
     BTN_TEXTSM AT ROW 24.13 COL 96.5
     BTN_NOLLSM AT ROW 25.29 COL 57.75
     BTN_ALLTMTRLSM AT ROW 25.29 COL 70.63
     BTN_SPECSM AT ROW 25.29 COL 83.5
     BTN_LINKABSM AT ROW 25.29 COL 96.5
     FILL-IN-B2SM AT ROW 1.08 COL 14.75 COLON-ALIGNED NO-LABEL
     FILL-IN-B3SM AT ROW 1.08 COL 44.38 COLON-ALIGNED NO-LABEL
     FILL-IN-F2SM AT ROW 2.17 COL 14.75 COLON-ALIGNED NO-LABEL
     FILL-IN-F3SM AT ROW 2.17 COL 44.38 COLON-ALIGNED NO-LABEL
     FILL-IN-SOKALTSM AT ROW 25.42 COL 2.88 NO-LABEL
     FILL-IN-KOMMSM AT ROW 9.71 COL 53.63 COLON-ALIGNED NO-LABEL
     FILL-IN-ADRKABM AT ROW 2.17 COL 84.25 COLON-ALIGNED WIDGET-ID 2
     BTN_KOMPSKAP AT ROW 2.25 COL 113 WIDGET-ID 4
     BTN_NASTKON AT ROW 1 COL 113 WIDGET-ID 6
     FILL-IN-ENRSTOLPSM AT ROW 23.21 COL 17.88 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-BENSTOLPSM AT ROW 24.17 COL 17.88 COLON-ALIGNED WIDGET-ID 10
     BTN_KUNDLISTASM AT ROW 21.75 COL 1 WIDGET-ID 12
     BRW_SATSSM AT ROW 10.58 COL 1.5
     BRW_HLEVSM AT ROW 10.58 COL 1.5
     BRW_STOLPSM AT ROW 10.58 COL 1.5
     BRW_MTRLSM AT ROW 10.58 COL 55.63
     BRW_SOKMTRLFSM AT ROW 10.5 COL 1.5 WIDGET-ID 100
     "Skåpmateriel" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1.04 COL 1.5
          FONT 17
     RECT-10 AT ROW 22.75 COL 1.5
     IMAGE-6 AT ROW 23.04 COL 2.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-MTRL
     BTN_KUNDLISTA AT ROW 12.5 COL 3 WIDGET-ID 2
     RAD_MTRL AT ROW 1.17 COL 1.75 NO-LABEL
     BTN_OVER AT ROW 5.58 COL 58
     BTN_BACK AT ROW 7.83 COL 58
     BTN_LEV AT ROW 12.54 COL 28.63
     CMB_LEV AT ROW 12.54 COL 14.63
     FILL-IN-ENR AT ROW 13.79 COL 26.63 COLON-ALIGNED
     FILL-IN-KOD AT ROW 13.83 COL 26.63 COLON-ALIGNED
     FILL-IN-BEN AT ROW 14.75 COL 56.51 RIGHT-ALIGNED
     RAD_SOK AT ROW 15.88 COL 17.5 NO-LABEL
     BTN_NY AT ROW 14.79 COL 63.13
     BTN_ANTAL AT ROW 14.79 COL 75.38
     BTN_SATS AT ROW 14.79 COL 87.63
     BTN_TEXT AT ROW 14.79 COL 99.88
     BTN_PAR AT ROW 14.79 COL 111.88
     BTN_NOLL AT ROW 16.29 COL 63.13
     BTN_ALLTMTRL AT ROW 16.29 COL 75.38
     BTN_SPEC AT ROW 16.29 COL 87.63
     BTN_LINKAB AT ROW 16.29 COL 99.88
     FILL-IN-SOKALT AT ROW 15.88 COL 2.88 NO-LABEL
     FILL-IN-KOMM AT ROW 1.08 COL 60 COLON-ALIGNED NO-LABEL
     FILL-IN-ENRSTOLP AT ROW 13.79 COL 26.63 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-BENSTOLP AT ROW 14.75 COL 56.51 RIGHT-ALIGNED WIDGET-ID 6
     BRW_HLEV AT ROW 2.08 COL 1.5
     BRW_SATS AT ROW 2.08 COL 1.5
     BRW_STOLP AT ROW 2.08 COL 1.5
     BRW_MTRL AT ROW 2.08 COL 62
     BRW_SOKMTRLF AT ROW 2.08 COL 1.5 WIDGET-ID 200
     RECT-22 AT ROW 13.67 COL 1.5
     IMAGE-4 AT ROW 13.83 COL 2.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.38.

DEFINE FRAME FRAME-ANDKONST
     CMB_VALANDKONST AT ROW 1.25 COL 2 NO-LABEL WIDGET-ID 2
     BRW_ANDKONST AT ROW 1.5 COL 31.5
     BRW_KONANDKONST AT ROW 2.38 COL 2 WIDGET-ID 400
     BRW_KALAKONS AT ROW 3 COL 32.5 WIDGET-ID 500
     BTN_BYTANGANDKONST AT ROW 9 COL 8.5 WIDGET-ID 4
     BTN_STANGANDKONST AT ROW 9 COL 99.75
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 4.42
         SIZE 114.5 BY 9.33 WIDGET-ID 300.

DEFINE FRAME FRAME-BILD
     BRW_BILD AT ROW 5.46 COL 5.38
     BTN_NYA AT ROW 14.5 COL 27
     BTN_BORTA AT ROW 14.5 COL 56.75
     BTN_VISA AT ROW 14.5 COL 71.75
     BTN_BILDCOPY AT ROW 14.5 COL 41.88 WIDGET-ID 2
     "Det fungerar bäst med formatet jpg eller jpeg," VIEW-AS TEXT
          SIZE 47 BY .63 AT ROW 2.75 COL 5.5
     "men systemet klarar även formaten bmp och gif." VIEW-AS TEXT
          SIZE 58 BY .63 AT ROW 3.5 COL 5.5
     "Tips vid koppling av bilder" VIEW-AS TEXT
          SIZE 28.5 BY .75 AT ROW 1.46 COL 5.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.38.

DEFINE FRAME FRAME-LIN
     CMB_VALLIN AT ROW 2.5 COL 1.5 NO-LABEL
     CMB_VAL2LIN AT ROW 2.5 COL 55.5 NO-LABEL
     BRW_FRANLIN AT ROW 3.71 COL 1.5
     BRW_TILLLIN AT ROW 3.71 COL 55.5
     BTN_LINUPP AT ROW 8 COL 109.38
     FBTN_VISALIN AT ROW 9.08 COL 109.38
     FBTN_SCHAKT AT ROW 10.21 COL 109.38
     FBTN_KORTLIN AT ROW 11.29 COL 109.38
     BTN_VISA-2LIN AT ROW 16.33 COL 43.75
     BRW_LIN AT ROW 17.58 COL 1.5
     FILL-IN-LANGDLIN AT ROW 19.58 COL 76 COLON-ALIGNED
     FILL-IN-ANTALLIN AT ROW 21.88 COL 76 COLON-ALIGNED
     BTN_UPLIN AT ROW 21.42 COL 82.88
     BTN_MINLIN AT ROW 22.58 COL 82.88
     BTN_SKAPALIN AT ROW 25.08 COL 14.38
     BTN_ANDLIN AT ROW 25.08 COL 27.38
     BTN_BORTLIN AT ROW 25.08 COL 40.25
     BTN_AVBLIN AT ROW 25.08 COL 109.38
     FILL-IN-LINKAB AT ROW 1.13 COL 1.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-ATG
     SEL_UPPATG AT ROW 2.71 COL 35.38 NO-LABEL
     BTN_KORATG AT ROW 4.21 COL 82
     BTN_AVBATG AT ROW 24.67 COL 85.5
     "Åtgärder" VIEW-AS TEXT
          SIZE 14 BY 1.21 AT ROW 1.13 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-ANM
     BRW_ANM AT ROW 2.92 COL 6.88
     TOG_ANMLOP AT ROW 1.75 COL 7
     BTN_OVER-2 AT ROW 6.13 COL 61.63
     ED_ANM AT ROW 2.92 COL 67.38 NO-LABEL
     BTN_KOPPANM AT ROW 11.08 COL 88.13
     FILL-IN-TEXT AT ROW 1.29 COL 72.38 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.38.

DEFINE FRAME FRAME-LINUPP
     BRW_LINLINUPP AT ROW 2.58 COL 6.5
     BTN_UPLINUPP AT ROW 7.33 COL 82
     BTN_VISALINUPP AT ROW 7.58 COL 109.63
     FILL-IN-PROCLINUPP AT ROW 7.79 COL 74.88 COLON-ALIGNED
     BTN_MINLINUPP AT ROW 8.5 COL 82
     BTN_SKRIVLINUPP AT ROW 8.67 COL 109.63
     FILL-IN-TOTMETERLINUPP AT ROW 9.5 COL 74.88 COLON-ALIGNED
     BTN_NOLLLINUPP AT ROW 12.92 COL 27.75
     BRW_UPPLINUPP AT ROW 14.58 COL 6.5
     FILL-IN-UPPMETERLINUPP AT ROW 15.63 COL 74.88 COLON-ALIGNED
     BTN_NYLINUPP AT ROW 24.79 COL 6.5
     BTN_ANDRALINUPP AT ROW 24.79 COL 21
     BTN_BORTLINUPP AT ROW 24.79 COL 35.5
     BTN_KOPPLINUPP AT ROW 24.79 COL 50.13
     BTN_AVBLINUPP AT ROW 24.79 COL 109.63
     FILL-IN-LINKABLINUPP AT ROW 1.13 COL 1.5 NO-LABEL
     FILL-IN-ENRLINUPP AT ROW 3.33 COL 74.75 COLON-ALIGNED
     FILL-IN-BENLINUPP AT ROW 4.71 COL 74.88 COLON-ALIGNED
     FILL-IN-METERLINUPP AT ROW 6 COL 74.88 COLON-ALIGNED
     FILL-IN-UPPLINUPP AT ROW 13.29 COL 74.88 COLON-ALIGNED
     FILL-IN-SUMMALINUPP AT ROW 18.42 COL 74.88 COLON-ALIGNED
     "Antal meter fördelat" VIEW-AS TEXT
          SIZE 23.25 BY .67 AT ROW 17.71 COL 66
     "Antal meter" VIEW-AS TEXT
          SIZE 11.38 BY .67 AT ROW 14.88 COL 64.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-ORDN
     RAD_ORD AT ROW 1.33 COL 28 NO-LABEL
     CMB_VALORD AT ROW 2.58 COL 37.5 NO-LABEL
     BRW_ORDID AT ROW 3.75 COL 1.5
     BRW_ORD AT ROW 3.75 COL 1.5
     BRW_ORDIDVAL AT ROW 3.75 COL 59
     BRW_FRAN AT ROW 3.96 COL 21.5
     BTN_OVERID AT ROW 11 COL 54.88
     BTN_UPPORD AT ROW 11.13 COL 92
     BTN_MORDUPPID AT ROW 11.33 COL 114.5
     BTN_MORDUPP AT ROW 11.33 COL 118.75
     BTN_NERORD AT ROW 14.08 COL 92
     BTN_MORDNERID AT ROW 14.25 COL 114.5
     BTN_MORDNER AT ROW 14.25 COL 118.75
     BTN_BACKID AT ROW 14.5 COL 54.88
     FILL-IN-IDNRORDID AT ROW 19.42 COL 110.63 COLON-ALIGNED NO-LABEL
     FILL-IN-IDNRORDID2 AT ROW 22.63 COL 110.63 COLON-ALIGNED NO-LABEL
     BTN_KOPPIDORD AT ROW 24.54 COL 112.63
     FILL-IN-ID1ORD AT ROW 17.83 COL 112.63 NO-LABEL
     FILL-IN-ID2ORD AT ROW 21 COL 112.63 NO-LABEL
     "Ändring av ordning" VIEW-AS TEXT
          SIZE 22 BY 1.21 AT ROW 1.13 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-ANDRAK
     BRW_ANDRAK AT ROW 1.5 COL 1.5
     BTN_STANG AT ROW 7.5 COL 84.5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11.63 ROW 4.42
         SIZE 97.5 BY 8.

DEFINE FRAME FRAME-FRI
     CMB_SKYLTART AT ROW 20.63 COL 77.38 COLON-ALIGNED
     CMB_KORTVAL AT ROW 2.79 COL 96.88 COLON-ALIGNED
     RAD_VALFRI AT ROW 3.79 COL 14.25 NO-LABEL
     BRW_APP AT ROW 4.54 COL 17.63
     BRW_MTRLFRI AT ROW 4.54 COL 14.25
     BRW_KABFRI AT ROW 11.88 COL 14.25
     FBTN_VISAFRI AT ROW 8.29 COL 111.38
     FBTN_SKRIVFRI AT ROW 9.42 COL 111.38
     BTN_EXKSKO AT ROW 10.5 COL 111.38
     FBTN_PDFFRI AT ROW 11.58 COL 111.38
     BTN_EXSTOLP AT ROW 12.71 COL 111.38
     BTN_EXMARK AT ROW 13.79 COL 111.38
     BTN_UPPFRI AT ROW 13.42 COL 106.75
     BTN_NERFRI AT ROW 15.92 COL 106.75
     FILL-IN-TYP AT ROW 18.46 COL 12.25 COLON-ALIGNED
     FILL-IN-SADR AT ROW 18.46 COL 52 COLON-ALIGNED
     FILL-IN-KABEL AT ROW 19.54 COL 12.25 COLON-ALIGNED
     FILL-IN-METER AT ROW 19.54 COL 63.5 COLON-ALIGNED
     FILL-IN-ADR AT ROW 20.63 COL 12.25 COLON-ALIGNED
     CMB_TYP AT ROW 20.63 COL 63.5 COLON-ALIGNED
     FILL-IN-APPARAT AT ROW 21.71 COL 12.25 COLON-ALIGNED
     FILL-IN-GRUPPNR AT ROW 21.71 COL 63.5 COLON-ALIGNED
     FILL-IN-ABONNENT AT ROW 21.71 COL 81.25 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-AR AT ROW 22.79 COL 12.25 COLON-ALIGNED
     FILL-IN-SAK AT ROW 22.79 COL 33.38 COLON-ALIGNED
     FILL-IN-MAX AT ROW 22.79 COL 52 COLON-ALIGNED
     FILL-IN-ANMARK AT ROW 23.88 COL 12.25 COLON-ALIGNED
     FBTN_SKAPAFRI AT ROW 25 COL 31.88
     FBTN_ANDFRI AT ROW 25 COL 53.63
     FBTN_BORTFRI AT ROW 25 COL 75.38
     FILL-IN-KORT AT ROW 1.13 COL 1.5 NO-LABEL
     FBTN_NASTAKABEL AT ROW 23.83 COL 72
     FILL-IN-B2 AT ROW 1.25 COL 15.5 COLON-ALIGNED NO-LABEL
     FILL-IN-B3 AT ROW 1.25 COL 42.13 COLON-ALIGNED NO-LABEL
     BTN_MEXC AT ROW 12.63 COL 111.38
     FILL-IN-F2 AT ROW 2.42 COL 15.5 COLON-ALIGNED NO-LABEL
     TOG_SORT AT ROW 1.75 COL 98.88
     FILL-IN-F3 AT ROW 2.42 COL 42.13 COLON-ALIGNED NO-LABEL
     FILL-IN-SKYLTANT AT ROW 20.63 COL 89 COLON-ALIGNED
     FILL-IN-SKYLTBEN AT ROW 20.63 COL 92.88 COLON-ALIGNED NO-LABEL
     FILL-IN-NSKYLTANT AT ROW 1.67 COL 78.5 COLON-ALIGNED
     FILL-IN-NISKYLTANT AT ROW 2.88 COL 78.5 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-KALK
     BRW_KALK AT ROW 1.75 COL 22
     BTN_NYKOD AT ROW 13.88 COL 46.13
     BTN_BORT2 AT ROW 13.88 COL 61.13
     BTN_GRUNDUPP AT ROW 13.88 COL 61.13 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.38.

DEFINE FRAME FRAME-ID
     CMB_JURP AT ROW 1.04 COL 8.5
     CMB_AVD AT ROW 1.04 COL 42.88
     CMB_OMR AT ROW 2.29 COL 45.88
     BTN_HMT AT ROW 3.38 COL 58.25
     FILL-IN-NR1 AT ROW 6.04 COL 9.13 COLON-ALIGNED
     BRW_TAB1 AT ROW 4.5 COL 32.5
     FILL-IN-NR2 AT ROW 10.42 COL 9.13 COLON-ALIGNED
     BRW_TAB2 AT ROW 8.79 COL 32.5
     FILL-IN-NR3 AT ROW 14.75 COL 9.13 COLON-ALIGNED
     BRW_TAB3 AT ROW 13.13 COL 32.5
     BTN_ANL AT ROW 16.29 COL 11
     CMB_DEL AT ROW 5.75 COL 106 COLON-ALIGNED
     FILL-IN-IDNR1 AT ROW 7.25 COL 106 COLON-ALIGNED
     FILL-IN-IDNR2 AT ROW 10.29 COL 105.75 COLON-ALIGNED
     FILL-IN-SUND AT ROW 11.54 COL 105.75 COLON-ALIGNED
     FILL-IN-KARTR AT ROW 12.79 COL 105.75 COLON-ALIGNED
     BTN_KOPPID AT ROW 16.29 COL 99.88
     TOG_A AT ROW 15.17 COL 100.25
     TOG_B AT ROW 15.17 COL 104
     TOG_C AT ROW 15.17 COL 107.75
     FILL-IN-TAB1 AT ROW 4.5 COL 3.5 NO-LABEL
     FILL-IN-ID1 AT ROW 4.5 COL 99 NO-LABEL
     FILL-IN-IDNR1LABEL AT ROW 7.5 COL 98.63 COLON-ALIGNED NO-LABEL
     FILL-IN-TAB2 AT ROW 8.88 COL 3.5 NO-LABEL
     FILL-IN-ID2 AT ROW 8.88 COL 99 NO-LABEL
     FILL-IN-IDNR2LABEL AT ROW 10.67 COL 98.63 COLON-ALIGNED NO-LABEL
     FILL-IN-SUNDLABEL AT ROW 11.75 COL 98.63 COLON-ALIGNED NO-LABEL
     FILL-IN-A AT ROW 14.13 COL 98.13 COLON-ALIGNED NO-LABEL
     FILL-IN-B AT ROW 14.13 COL 103.88 NO-LABEL
     FILL-IN-C AT ROW 14.13 COL 105.63 COLON-ALIGNED NO-LABEL
     FILL-IN-TAB3 AT ROW 13.21 COL 3.5 NO-LABEL
     TOG_AS AT ROW 15.17 COL 111.38 WIDGET-ID 2
     BTN_BORTKOPPID AT ROW 16.29 COL 112.63 WIDGET-ID 4
     BTN_NASTKONID AT ROW 16.25 COL 112.5 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.42.

DEFINE FRAME FRAME-KONST
     CMB_VAL AT ROW 1.08 COL 1.5 NO-LABEL
     TOG_VISAKONST AT ROW 1.08 COL 31.63 WIDGET-ID 4
     RAD_KOMP AT ROW 1.08 COL 76 NO-LABEL
     TOG_RATT AT ROW 1.08 COL 96.5
     BTN_NER AT ROW 1.21 COL 57.63
     BTN_UPP AT ROW 1.21 COL 66
     TOG_MARKAV AT ROW 1.46 COL 96.5 WIDGET-ID 2
     TOG_KOMP AT ROW 1.96 COL 96.5
     TOG_VISAGRUPP AT ROW 2 COL 1.5
     BRW_KON AT ROW 3.71 COL 1.5
     BRW_KB2 AT ROW 3.71 COL 42.5
     BRW_B2 AT ROW 3.71 COL 42.63
     BRW_KB3 AT ROW 3.71 COL 83.75
     BRW_B3 AT ROW 3.71 COL 83.88
     BRW_KB4 AT ROW 11 COL 1.5
     BRW_B4 AT ROW 11 COL 1.5
     BRW_KB5 AT ROW 11 COL 42.5
     BRW_B5 AT ROW 11 COL 42.63
     BRW_KB6 AT ROW 11 COL 83.75
     BRW_B6 AT ROW 11 COL 83.88
     FILL-IN-B1 AT ROW 2.88 COL 1.5 NO-LABEL
     FILL-IN-B2 AT ROW 2.88 COL 40.5 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 40 BY .83
          BGCOLOR 7 
     FILL-IN-B3 AT ROW 2.88 COL 81.75 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 40 BY .83
          BGCOLOR 7 
     FILL-IN-B4 AT ROW 10.17 COL 1.5 NO-LABEL
     FILL-IN-B5 AT ROW 10.17 COL 40.5 COLON-ALIGNED NO-LABEL
     FILL-IN-B6 AT ROW 10.17 COL 81.75 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: andrakon_val T "?" NO-UNDO temp-db andrakon_val
      TABLE: apparattemp T "?" NO-UNDO temp-db apparattemp
      TABLE: berfortemp T "?" NO-UNDO temp-db berfortemp
      TABLE: berlinjetemp T "?" NO-UNDO temp-db berlinjetemp
      TABLE: bernattemp T "?" NO-UNDO temp-db bernattemp
      TABLE: berskaptemp T "?" NO-UNDO temp-db berskaptemp
      TABLE: berstolptemp T "?" NO-UNDO temp-db berstolptemp
      TABLE: berupptemp T "?" NO-UNDO temp-db berupptemp
      TABLE: bildbertemp T "?" NO-UNDO temp-db bildbertemp
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
      TABLE: favberupptemp T "?" NO-UNDO temp-db favberupptemp
      TABLE: fri_temp T "?" NO-UNDO temp-db fri_temp
      TABLE: idkon_val T "?" NO-UNDO temp-db idkon_val
      TABLE: kombtemp T "?" NO-UNDO temp-db kombtemp
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
      TABLE: konsttemp T "?" NO-UNDO temp-db konsttemp
      TABLE: konstvaltemp T "?" NO-UNDO temp-db konstvaltemp
      TABLE: kon_display T "?" NO-UNDO temp-db kon_display
      TABLE: kon_id T "?" NO-UNDO temp-db kon_id
      TABLE: kon_id2 T "?" NO-UNDO temp-db kon_id2
      TABLE: kopp_lina T "?" NO-UNDO temp-db kopp_lina
      TABLE: lin_tab2 T "?" NO-UNDO temp-db lin_tab2
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: mtrl_tab T "?" NO-UNDO temp-db mtrl_tab
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
      TABLE: soktamtrlfavo T "?" NO-UNDO temp-db soktamtrlfavo
      TABLE: ? T "?" NO-UNDO temp-db list_mtrl
      TABLE: ? T "?" NO-UNDO temp-db kon_val
      TABLE: ? T "?" NO-UNDO temp-db kalk_temp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beredning"
         HEIGHT             = 28.38
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ANDKONST:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-ANDRAK:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-ANM:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-ATG:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-BILD:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-FRI:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-ID:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-KABSKAP:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-KALK:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-KONST:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-KOPUPP:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-LIN:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-LINUPP:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-LISTOR:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-MTRL:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-ORDN:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-SKAPMTRL:FRAME = FRAME FRAME-BARABREDD:HANDLE.

/* SETTINGS FOR FRAME FRAME-ANDKONST
                                                                        */
/* BROWSE-TAB BRW_ANDKONST CMB_VALANDKONST FRAME-ANDKONST */
/* BROWSE-TAB BRW_KONANDKONST BRW_ANDKONST FRAME-ANDKONST */
/* BROWSE-TAB BRW_KALAKONS BRW_KONANDKONST FRAME-ANDKONST */
ASSIGN 
       BRW_ANDKONST:MAX-DATA-GUESS IN FRAME FRAME-ANDKONST         = 300
       BRW_ANDKONST:COLUMN-RESIZABLE IN FRAME FRAME-ANDKONST       = TRUE.

ASSIGN 
       BRW_KALAKONS:HIDDEN  IN FRAME FRAME-ANDKONST                = TRUE
       BRW_KALAKONS:POPUP-MENU IN FRAME FRAME-ANDKONST             = MENU POPUP-MENU-BRW_KON-3:HANDLE.

ASSIGN 
       BRW_KONANDKONST:HIDDEN  IN FRAME FRAME-ANDKONST                = TRUE
       BRW_KONANDKONST:POPUP-MENU IN FRAME FRAME-ANDKONST             = MENU POPUP-MENU-BRW_KON-2:HANDLE.

/* SETTINGS FOR COMBO-BOX CMB_VALANDKONST IN FRAME FRAME-ANDKONST
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-ANDRAK
                                                                        */
/* BROWSE-TAB BRW_ANDRAK 1 FRAME-ANDRAK */
ASSIGN 
       BRW_ANDRAK:MAX-DATA-GUESS IN FRAME FRAME-ANDRAK         = 300
       BRW_ANDRAK:COLUMN-RESIZABLE IN FRAME FRAME-ANDRAK       = TRUE.

/* SETTINGS FOR FRAME FRAME-ANM
   Custom                                                               */
/* BROWSE-TAB BRW_ANM 1 FRAME-ANM */
ASSIGN 
       BRW_ANM:HIDDEN  IN FRAME FRAME-ANM                = TRUE.

/* SETTINGS FOR EDITOR ED_ANM IN FRAME FRAME-ANM
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME FRAME-ANM
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-TEXT:HIDDEN IN FRAME FRAME-ANM           = TRUE.

/* SETTINGS FOR FRAME FRAME-ATG
                                                                        */
/* SETTINGS FOR FRAME FRAME-BARABREDD
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_VAL FRAME-LINUPP FRAME-BARABREDD */
ASSIGN 
       BRW_VAL:POPUP-MENU IN FRAME FRAME-BARABREDD             = MENU POPUP-MENU-BRW_VAL:HANDLE
       BRW_VAL:MAX-DATA-GUESS IN FRAME FRAME-BARABREDD         = 300
       BRW_VAL:COLUMN-RESIZABLE IN FRAME FRAME-BARABREDD       = TRUE.

ASSIGN 
       BTN_ORDNER:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SKAPMTRL IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-SKAPMTRL:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME FRAME-BARABREDD
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR FRAME FRAME-BILD
   Custom                                                               */
/* BROWSE-TAB BRW_BILD 1 FRAME-BILD */
ASSIGN 
       BTN_BILDCOPY:HIDDEN IN FRAME FRAME-BILD           = TRUE.

ASSIGN 
       BTN_BORTA:HIDDEN IN FRAME FRAME-BILD           = TRUE.

ASSIGN 
       BTN_NYA:HIDDEN IN FRAME FRAME-BILD           = TRUE.

ASSIGN 
       BTN_VISA:HIDDEN IN FRAME FRAME-BILD           = TRUE.

/* SETTINGS FOR FRAME FRAME-FRI
   Custom                                                               */
/* BROWSE-TAB BRW_APP RAD_VALFRI FRAME-FRI */
/* BROWSE-TAB BRW_MTRLFRI BRW_APP FRAME-FRI */
/* BROWSE-TAB BRW_KABFRI BRW_MTRLFRI FRAME-FRI */
ASSIGN 
       BRW_APP:MAX-DATA-GUESS IN FRAME FRAME-FRI         = 1000.

ASSIGN 
       BRW_MTRLFRI:MAX-DATA-GUESS IN FRAME FRAME-FRI         = 1000.

ASSIGN 
       BTN_EXKSKO:HIDDEN IN FRAME FRAME-FRI           = TRUE.

ASSIGN 
       BTN_EXMARK:HIDDEN IN FRAME FRAME-FRI           = TRUE.

ASSIGN 
       BTN_EXSTOLP:HIDDEN IN FRAME FRAME-FRI           = TRUE.

ASSIGN 
       BTN_MEXC:HIDDEN IN FRAME FRAME-FRI           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ABONNENT IN FRAME FRAME-FRI
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-GRUPPNR IN FRAME FRAME-FRI
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-KORT IN FRAME FRAME-FRI
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-NISKYLTANT IN FRAME FRAME-FRI
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-NSKYLTANT IN FRAME FRAME-FRI
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-SKYLTANT IN FRAME FRAME-FRI
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-SKYLTBEN IN FRAME FRAME-FRI
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-ID
   Custom                                                               */
/* BROWSE-TAB BRW_TAB1 FILL-IN-NR1 FRAME-ID */
/* BROWSE-TAB BRW_TAB2 FILL-IN-NR2 FRAME-ID */
/* BROWSE-TAB BRW_TAB3 FILL-IN-NR3 FRAME-ID */
ASSIGN 
       BRW_TAB1:HIDDEN  IN FRAME FRAME-ID                = TRUE.

ASSIGN 
       BRW_TAB2:HIDDEN  IN FRAME FRAME-ID                = TRUE.

ASSIGN 
       BRW_TAB3:HIDDEN  IN FRAME FRAME-ID                = TRUE.

ASSIGN 
       BTN_ANL:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       BTN_BORTKOPPID:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       BTN_KOPPID:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME FRAME-ID
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME FRAME-ID
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME FRAME-ID
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-B IN FRAME FRAME-ID
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       FILL-IN-C:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ID1 IN FRAME FRAME-ID
   NO-DISPLAY ALIGN-L                                                   */
ASSIGN 
       FILL-IN-ID1:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ID2 IN FRAME FRAME-ID
   NO-DISPLAY ALIGN-L                                                   */
ASSIGN 
       FILL-IN-ID2:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-IDNR1 IN FRAME FRAME-ID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-IDNR1:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       FILL-IN-IDNR1LABEL:READ-ONLY IN FRAME FRAME-ID        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-IDNR2 IN FRAME FRAME-ID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-IDNR2:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       FILL-IN-IDNR2LABEL:READ-ONLY IN FRAME FRAME-ID        = TRUE.

ASSIGN 
       FILL-IN-KARTR:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-NR1 IN FRAME FRAME-ID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-NR1:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-NR2 IN FRAME FRAME-ID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-NR2:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-NR3 IN FRAME FRAME-ID
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-NR3:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       FILL-IN-SUND:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       FILL-IN-SUNDLABEL:READ-ONLY IN FRAME FRAME-ID        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TAB1 IN FRAME FRAME-ID
   NO-DISPLAY ALIGN-L                                                   */
ASSIGN 
       FILL-IN-TAB1:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TAB2 IN FRAME FRAME-ID
   NO-DISPLAY ALIGN-L                                                   */
ASSIGN 
       FILL-IN-TAB2:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TAB3 IN FRAME FRAME-ID
   NO-DISPLAY ALIGN-L                                                   */
ASSIGN 
       FILL-IN-TAB3:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       TOG_A:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       TOG_AS:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       TOG_B:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       TOG_C:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FRAME FRAME-KABSKAP
                                                                        */
/* BROWSE-TAB BRW_KABKAB FILL-IN-ADRKAB FRAME-KABSKAP */
/* BROWSE-TAB BRW_SKAPKAB BTN_TYP3KAB FRAME-KABSKAP */
ASSIGN 
       BRW_KABKAB:MAX-DATA-GUESS IN FRAME FRAME-KABSKAP         = 1000.

ASSIGN 
       BRW_SKAPKAB:HIDDEN  IN FRAME FRAME-KABSKAP                = TRUE
       BRW_SKAPKAB:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KABSKAP = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ADRKAB IN FRAME FRAME-KABSKAP
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-B1KAB IN FRAME FRAME-KABSKAP
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-EON IN FRAME FRAME-KABSKAP
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-EON:READ-ONLY IN FRAME FRAME-KABSKAP        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F1KAB IN FRAME FRAME-KABSKAP
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-KOPKAB IN FRAME FRAME-KABSKAP
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-KOPSKAP IN FRAME FRAME-KABSKAP
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-KOPSKAP:READ-ONLY IN FRAME FRAME-KABSKAP        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KORTKAB IN FRAME FRAME-KABSKAP
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-NR1KAB:HIDDEN IN FRAME FRAME-KABSKAP           = TRUE.

ASSIGN 
       FILL-IN-NR2KAB:HIDDEN IN FRAME FRAME-KABSKAP           = TRUE.

ASSIGN 
       FILL-IN-NR3KAB:HIDDEN IN FRAME FRAME-KABSKAP           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-T1 IN FRAME FRAME-KABSKAP
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-T1:READ-ONLY IN FRAME FRAME-KABSKAP        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-T2 IN FRAME FRAME-KABSKAP
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-T2:READ-ONLY IN FRAME FRAME-KABSKAP        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-T3 IN FRAME FRAME-KABSKAP
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-T3:READ-ONLY IN FRAME FRAME-KABSKAP        = TRUE.

/* SETTINGS FOR FRAME FRAME-KALK
                                                                        */
/* BROWSE-TAB BRW_KALK 1 FRAME-KALK */
ASSIGN 
       BRW_KALK:HIDDEN  IN FRAME FRAME-KALK                = TRUE
       BRW_KALK:MAX-DATA-GUESS IN FRAME FRAME-KALK         = 10000.

ASSIGN 
       BTN_BORT2:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_GRUNDUPP:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_NYKOD:HIDDEN IN FRAME FRAME-KALK           = TRUE.

/* SETTINGS FOR FRAME FRAME-KONST
                                                                        */
/* BROWSE-TAB BRW_KON TOG_VISAGRUPP FRAME-KONST */
/* BROWSE-TAB BRW_KB2 BRW_KON FRAME-KONST */
/* BROWSE-TAB BRW_B2 BRW_KB2 FRAME-KONST */
/* BROWSE-TAB BRW_KB3 BRW_B2 FRAME-KONST */
/* BROWSE-TAB BRW_B3 BRW_KB3 FRAME-KONST */
/* BROWSE-TAB BRW_KB4 BRW_B3 FRAME-KONST */
/* BROWSE-TAB BRW_B4 BRW_KB4 FRAME-KONST */
/* BROWSE-TAB BRW_KB5 BRW_B4 FRAME-KONST */
/* BROWSE-TAB BRW_B5 BRW_KB5 FRAME-KONST */
/* BROWSE-TAB BRW_KB6 BRW_B5 FRAME-KONST */
/* BROWSE-TAB BRW_B6 BRW_KB6 FRAME-KONST */
ASSIGN 
       BRW_B2:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B2:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B2:HANDLE.

ASSIGN 
       BRW_B3:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B3:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B3:HANDLE.

ASSIGN 
       BRW_B4:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B4:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B4:HANDLE.

ASSIGN 
       BRW_B5:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B5:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B5:HANDLE.

ASSIGN 
       BRW_B6:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B6:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B6:HANDLE.

ASSIGN 
       BRW_KB2:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB2:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB2:HANDLE.

ASSIGN 
       BRW_KB3:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB3:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB3:HANDLE.

ASSIGN 
       BRW_KB4:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB4:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB4:HANDLE.

ASSIGN 
       BRW_KB5:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB5:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB5:HANDLE.

ASSIGN 
       BRW_KB6:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB6:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB6:HANDLE.

ASSIGN 
       BRW_KON:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KON:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KON:HANDLE.

/* SETTINGS FOR COMBO-BOX CMB_VAL IN FRAME FRAME-KONST
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-B1 IN FRAME FRAME-KONST
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B1:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       FILL-IN-B2:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       FILL-IN-B3:HIDDEN IN FRAME FRAME-KONST           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B4 IN FRAME FRAME-KONST
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B4:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       FILL-IN-B5:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       FILL-IN-B6:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       RAD_KOMP:HIDDEN IN FRAME FRAME-KONST           = TRUE.

/* SETTINGS FOR FRAME FRAME-KOPUPP
                                                                        */
/* BROWSE-TAB BRW_KOMBKOPUPP TEXT-11 FRAME-KOPUPP */
/* BROWSE-TAB BRW_UPPKOPUPP BTN_FAVOUPP FRAME-KOPUPP */
/* BROWSE-TAB BRW_FAVOUPPLAG BRW_UPPKOPUPP FRAME-KOPUPP */
ASSIGN 
       BRW_KOMBKOPUPP:HIDDEN  IN FRAME FRAME-KOPUPP                = TRUE
       BRW_KOMBKOPUPP:POPUP-MENU IN FRAME FRAME-KOPUPP             = MENU POPUP-MENU-BRW_KOMBKOPUPP:HANDLE
       BRW_KOMBKOPUPP:MAX-DATA-GUESS IN FRAME FRAME-KOPUPP         = 1000.

ASSIGN 
       kombtemp.ID:VISIBLE IN BROWSE BRW_KOMBKOPUPP = FALSE.

ASSIGN 
       BRW_UPPKOPUPP:HIDDEN  IN FRAME FRAME-KOPUPP                = TRUE.

ASSIGN 
       BTN_ANDRAKOPUPP:HIDDEN IN FRAME FRAME-KOPUPP           = TRUE.

ASSIGN 
       BTN_BORTKOPUPP:HIDDEN IN FRAME FRAME-KOPUPP           = TRUE.

ASSIGN 
       BTN_NYKOPUPP:HIDDEN IN FRAME FRAME-KOPUPP           = TRUE.

ASSIGN 
       FBTN_BKOMBKOPUPP:HIDDEN IN FRAME FRAME-KOPUPP           = TRUE.

ASSIGN 
       FBTN_KOMBKOPUPP:HIDDEN IN FRAME FRAME-KOPUPP           = TRUE.

ASSIGN 
       FBTN_OUPPKOPUPP:HIDDEN IN FRAME FRAME-KOPUPP           = TRUE.

ASSIGN 
       FBTN_UPPKOPUPP:HIDDEN IN FRAME FRAME-KOPUPP           = TRUE.

/* SETTINGS FOR FRAME FRAME-LIN
   Custom                                                               */
/* BROWSE-TAB BRW_FRANLIN CMB_VAL2LIN FRAME-LIN */
/* BROWSE-TAB BRW_TILLLIN BRW_FRANLIN FRAME-LIN */
/* BROWSE-TAB BRW_LIN BTN_VISA-2LIN FRAME-LIN */
ASSIGN 
       BRW_FRANLIN:POPUP-MENU IN FRAME FRAME-LIN             = MENU POPUP-MENU-BRW_FRANLIN:HANDLE
       BRW_FRANLIN:MAX-DATA-GUESS IN FRAME FRAME-LIN         = 10000.

ASSIGN 
       BRW_TILLLIN:POPUP-MENU IN FRAME FRAME-LIN             = MENU POPUP-MENU-BRW_TILLLIN:HANDLE
       BRW_TILLLIN:MAX-DATA-GUESS IN FRAME FRAME-LIN         = 10000.

/* SETTINGS FOR BUTTON BTN_ANDLIN IN FRAME FRAME-LIN
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ANDLIN:HIDDEN IN FRAME FRAME-LIN           = TRUE.

/* SETTINGS FOR BUTTON BTN_MINLIN IN FRAME FRAME-LIN
   NO-ENABLE                                                            */
ASSIGN 
       BTN_MINLIN:HIDDEN IN FRAME FRAME-LIN           = TRUE.

/* SETTINGS FOR BUTTON BTN_UPLIN IN FRAME FRAME-LIN
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UPLIN:HIDDEN IN FRAME FRAME-LIN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VAL2LIN IN FRAME FRAME-LIN
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_VALLIN IN FRAME FRAME-LIN
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-ANTALLIN IN FRAME FRAME-LIN
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-ANTALLIN:HIDDEN IN FRAME FRAME-LIN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LANGDLIN IN FRAME FRAME-LIN
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-LANGDLIN:HIDDEN IN FRAME FRAME-LIN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LINKAB IN FRAME FRAME-LIN
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-LINKAB:READ-ONLY IN FRAME FRAME-LIN        = TRUE.

/* SETTINGS FOR FRAME FRAME-LINUPP
                                                                        */
/* BROWSE-TAB BRW_LINLINUPP TEXT-3 FRAME-LINUPP */
/* BROWSE-TAB BRW_UPPLINUPP BTN_NOLLLINUPP FRAME-LINUPP */
/* SETTINGS FOR FILL-IN FILL-IN-LINKABLINUPP IN FRAME FRAME-LINUPP
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-LINKABLINUPP:READ-ONLY IN FRAME FRAME-LINUPP        = TRUE.

/* SETTINGS FOR FRAME FRAME-LISTOR
                                                                        */
ASSIGN 
       ED_MEDD:HIDDEN IN FRAME FRAME-LISTOR           = TRUE
       ED_MEDD:RETURN-INSERTED IN FRAME FRAME-LISTOR  = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ANM IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_ANM:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EGEN IN FRAME FRAME-LISTOR
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR TOGGLE-BOX TOG_EGENSIDA IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_EGENSIDA:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EXCELKALK IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_EXCELKALK:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ID IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_ID:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_JMFKONS IN FRAME FRAME-LISTOR
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR TOGGLE-BOX TOG_KONSTRUKTION IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_KONSTRUKTION:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

ASSIGN 
       TOG_KONVAL:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_MTRL IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_MTRL:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_P3 IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_P3:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_SIDEXCEL IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_SIDEXCEL:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_SPANN IN FRAME FRAME-LISTOR
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_SPANN:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_TEXT IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_TEXT:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_UPP IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_UPP:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_VAL IN FRAME FRAME-LISTOR
   SHARED                                                               */
ASSIGN 
       TOG_VAL:HIDDEN IN FRAME FRAME-LISTOR           = TRUE.

/* SETTINGS FOR FRAME FRAME-MTRL
   Custom                                                               */
/* BROWSE-TAB BRW_HLEV IMAGE-4 FRAME-MTRL */
/* BROWSE-TAB BRW_SATS BRW_HLEV FRAME-MTRL */
/* BROWSE-TAB BRW_STOLP BRW_SATS FRAME-MTRL */
/* BROWSE-TAB BRW_MTRL BRW_STOLP FRAME-MTRL */
/* BROWSE-TAB BRW_SOKMTRLF BRW_MTRL FRAME-MTRL */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_HLEV:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_HLEV:HANDLE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 1000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-MTRL       = TRUE.

ASSIGN 
       BRW_MTRL:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 10000.

ASSIGN 
       BRW_SATS:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_SATS:HANDLE
       BRW_SATS:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 50000
       BRW_SATS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE
       BRW_SATS:COLUMN-RESIZABLE IN FRAME FRAME-MTRL       = TRUE.

ASSIGN 
       BRW_SOKMTRLF:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_SOKMTRLF:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 50000
       BRW_SOKMTRLF:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE
       BRW_SOKMTRLF:COLUMN-RESIZABLE IN FRAME FRAME-MTRL       = TRUE.

ASSIGN 
       BRW_STOLP:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_STOLP:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 10000
       BRW_STOLP:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE
       BRW_STOLP:COLUMN-RESIZABLE IN FRAME FRAME-MTRL       = TRUE.

ASSIGN 
       BTN_ALLTMTRL:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_BACK:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_LEV:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_LINKAB:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_NOLL:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_NY:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_OVER:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_PAR:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_SATS:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_SPEC:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       BTN_TEXT:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_LEV IN FRAME FRAME-MTRL
   ALIGN-L                                                              */
ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEN IN FRAME FRAME-MTRL
   NO-DISPLAY ALIGN-R                                                   */
ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BENSTOLP IN FRAME FRAME-MTRL
   NO-DISPLAY ALIGN-R                                                   */
ASSIGN 
       FILL-IN-BENSTOLP:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-ENRSTOLP:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-KOD:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KOMM IN FRAME FRAME-MTRL
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-KOMM:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-MTRL
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-MTRL        = TRUE.

ASSIGN 
       RAD_MTRL:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-22 IN FRAME FRAME-MTRL
   NO-ENABLE                                                            */
ASSIGN 
       RECT-22:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR FRAME FRAME-ORDN
                                                                        */
/* BROWSE-TAB BRW_ORDID CMB_VALORD FRAME-ORDN */
/* BROWSE-TAB BRW_ORD BRW_ORDID FRAME-ORDN */
/* BROWSE-TAB BRW_ORDIDVAL BRW_ORD FRAME-ORDN */
/* BROWSE-TAB BRW_FRAN BRW_ORDIDVAL FRAME-ORDN */
ASSIGN 
       BRW_FRAN:MAX-DATA-GUESS IN FRAME FRAME-ORDN         = 10000.

ASSIGN 
       BRW_ORD:POPUP-MENU IN FRAME FRAME-ORDN             = MENU POPUP-MENU-BRW_VAL-2:HANDLE
       BRW_ORD:MAX-DATA-GUESS IN FRAME FRAME-ORDN         = 300
       BRW_ORD:COLUMN-RESIZABLE IN FRAME FRAME-ORDN       = TRUE.

ASSIGN 
       BRW_ORDID:POPUP-MENU IN FRAME FRAME-ORDN             = MENU POPUP-MENU-BRW_VAL-3:HANDLE
       BRW_ORDID:MAX-DATA-GUESS IN FRAME FRAME-ORDN         = 300
       BRW_ORDID:COLUMN-RESIZABLE IN FRAME FRAME-ORDN       = TRUE.

ASSIGN 
       BRW_ORDIDVAL:POPUP-MENU IN FRAME FRAME-ORDN             = MENU POPUP-MENU-BRW_VAL-4:HANDLE
       BRW_ORDIDVAL:MAX-DATA-GUESS IN FRAME FRAME-ORDN         = 300
       BRW_ORDIDVAL:COLUMN-RESIZABLE IN FRAME FRAME-ORDN       = TRUE.

ASSIGN 
       BTN_KOPPIDORD:HIDDEN IN FRAME FRAME-ORDN           = TRUE.

ASSIGN 
       BTN_MORDNER:HIDDEN IN FRAME FRAME-ORDN           = TRUE.

ASSIGN 
       BTN_MORDNERID:HIDDEN IN FRAME FRAME-ORDN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VALORD IN FRAME FRAME-ORDN
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-ID1ORD IN FRAME FRAME-ORDN
   NO-DISPLAY ALIGN-L                                                   */
ASSIGN 
       FILL-IN-ID1ORD:HIDDEN IN FRAME FRAME-ORDN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ID2ORD IN FRAME FRAME-ORDN
   NO-DISPLAY ALIGN-L                                                   */
ASSIGN 
       FILL-IN-ID2ORD:HIDDEN IN FRAME FRAME-ORDN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-IDNRORDID IN FRAME FRAME-ORDN
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-IDNRORDID:HIDDEN IN FRAME FRAME-ORDN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-IDNRORDID2 IN FRAME FRAME-ORDN
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-IDNRORDID2:HIDDEN IN FRAME FRAME-ORDN           = TRUE.

/* SETTINGS FOR FRAME FRAME-SKAPMTRL
   Custom                                                               */
/* BROWSE-TAB BRW_KABSM 1 FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_SATSSM IMAGE-6 FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_HLEVSM BRW_SATSSM FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_STOLPSM BRW_HLEVSM FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_MTRLSM BRW_STOLPSM FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_SOKMTRLFSM BRW_MTRLSM FRAME-SKAPMTRL */
ASSIGN 
       BRW_HLEVSM:HIDDEN  IN FRAME FRAME-SKAPMTRL                = TRUE
       BRW_HLEVSM:POPUP-MENU IN FRAME FRAME-SKAPMTRL             = MENU POPUP-MENU-BRW_HLEVSM:HANDLE
       BRW_HLEVSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 10000
       BRW_HLEVSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_KABSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 1000
       BRW_KABSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_MTRLSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 10000
       BRW_MTRLSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_SATSSM:HIDDEN  IN FRAME FRAME-SKAPMTRL                = TRUE
       BRW_SATSSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 50000
       BRW_SATSSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_SOKMTRLFSM:HIDDEN  IN FRAME FRAME-SKAPMTRL                = TRUE
       BRW_SOKMTRLFSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 50000
       BRW_SOKMTRLFSM:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-SKAPMTRL = TRUE
       BRW_SOKMTRLFSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_STOLPSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 10000
       BRW_STOLPSM:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-SKAPMTRL = TRUE
       BRW_STOLPSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BTN_ALLTMTRLSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       BTN_LEVSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       BTN_LINKABSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       BTN_NOLLSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       BTN_NYSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       BTN_OVERSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       BTN_SATSSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       BTN_SPECSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       CMB_LEVSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ADRKABM IN FRAME FRAME-SKAPMTRL
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BENSM IN FRAME FRAME-SKAPMTRL
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-BENSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BENSTOLPSM IN FRAME FRAME-SKAPMTRL
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-BENSTOLPSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       FILL-IN-ENRSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       FILL-IN-ENRSTOLPSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       FILL-IN-KODSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KOMMSM IN FRAME FRAME-SKAPMTRL
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-KOMMSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALTSM IN FRAME FRAME-SKAPMTRL
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALTSM:READ-ONLY IN FRAME FRAME-SKAPMTRL        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANDKONST
/* Query rebuild information for BROWSE BRW_ANDKONST
     _TblList          = "Temp-Tables.andrakon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.andrakon_val.F1
"andrakon_val.F1" "F1" "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.andrakon_val.F2
"andrakon_val.F2" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.andrakon_val.F3
"andrakon_val.F3" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.andrakon_val.F4
"andrakon_val.F4" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.andrakon_val.F5
"andrakon_val.F5" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.andrakon_val.F6
"andrakon_val.F6" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANDKONST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANDRAK
/* Query rebuild information for BROWSE BRW_ANDRAK
     _TblList          = "Temp-Tables.andrakon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.andrakon_val.F1
"andrakon_val.F1" "F1" "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.andrakon_val.F2
"andrakon_val.F2" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.andrakon_val.F3
"andrakon_val.F3" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.andrakon_val.F4
"andrakon_val.F4" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.andrakon_val.F5
"andrakon_val.F5" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.andrakon_val.F6
"andrakon_val.F6" ? "X(256)" "character" ? ? ? ? 12 ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANDRAK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANM
/* Query rebuild information for BROWSE BRW_ANM
     _TblList          = "Temp-Tables.fastanmtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.fastanmtemp.ANMARK
"fastanmtemp.ANMARK" "Fasta anmärkningar" "X(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_APP
/* Query rebuild information for BROWSE BRW_APP
     _TblList          = "Temp-Tables.apparattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.apparattemp.BENAMNING
"apparattemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "76" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_APP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B2
/* Query rebuild information for BROWSE BRW_B2
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B3
/* Query rebuild information for BROWSE BRW_B3
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B4
/* Query rebuild information for BROWSE BRW_B4
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B5
/* Query rebuild information for BROWSE BRW_B5
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B6
/* Query rebuild information for BROWSE BRW_B6
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B6 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BILD
/* Query rebuild information for BROWSE BRW_BILD
     _TblList          = "Temp-Tables.bildbertemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bildbertemp.NAMN
"bildbertemp.NAMN" "Egen text" "X(25)" "character" ? ? ? ? ? ? yes ? no no "23.63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.bildbertemp.FILNAMN
"bildbertemp.FILNAMN" "Sökväg" "X(256)" "character" ? ? ? ? ? ? no ? no no "81.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BILD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FAVOUPPLAG
/* Query rebuild information for BROWSE BRW_FAVOUPPLAG
     _TblList          = "Temp-Tables.favberupptemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.favberupptemp.ADRESS
"favberupptemp.ADRESS" "Adress" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.favberupptemp.POSTNR
"favberupptemp.POSTNR" "Postnr" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.favberupptemp.ORT
"favberupptemp.ORT" "Ort" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.favberupptemp.XKORDCH
"favberupptemp.XKORDCH" "X" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.favberupptemp.YKORDCH
"favberupptemp.YKORDCH" "Y" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FAVOUPPLAG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRAN
/* Query rebuild information for BROWSE BRW_FRAN
     _TblList          = "Temp-Tables.kon_id"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_id.LINNR|yes,Temp-Tables.kon_id.NATNR|yes,Temp-Tables.kon_id.FRI1|yes,Temp-Tables.kon_id.FRI2|yes"
     _FldNameList[1]   > Temp-Tables.kon_id.LINNR
"kon_id.LINNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_id.NATNR
"kon_id.NATNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_id.FRI1
"kon_id.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_id.FRI2
"kon_id.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRANLIN
/* Query rebuild information for BROWSE BRW_FRANLIN
     _TblList          = "Temp-Tables.kon_id"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_id.LINNR|yes,Temp-Tables.kon_id.NATNR|yes,Temp-Tables.kon_id.FRI1|yes,Temp-Tables.kon_id.FRI2|yes"
     _FldNameList[1]   > Temp-Tables.kon_id.LINNR
"kon_id.LINNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_id.NATNR
"kon_id.NATNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_id.FRI1
"kon_id.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_id.FRI2
"kon_id.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_id.FRI3
"kon_id.FRI3" ? ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRANLIN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _FldNameList[5]   > Temp-Tables.mtrltemp.LEVKOD
"mtrltemp.LEVKOD" "Lev" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.mtrltemp.LEVNAMN
"mtrltemp.LEVNAMN" "Leverantörsnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEVSM
/* Query rebuild information for BROWSE BRW_HLEVSM
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _FldNameList[5]   > Temp-Tables.mtrltemp.LEVKOD
"mtrltemp.LEVKOD" "Lev" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.mtrltemp.LEVNAMN
"mtrltemp.LEVNAMN" "Leverantörsnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEVSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KABFRI
/* Query rebuild information for BROWSE BRW_KABFRI
     _TblList          = "Temp-Tables.fri_temp"
     _FldNameList[1]   > Temp-Tables.fri_temp.GRUPPNR
"fri_temp.GRUPPNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.fri_temp.BENAMNING
"fri_temp.BENAMNING" "Kabeltyp" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.fri_temp.KABADR
"fri_temp.KABADR" "KabelAdress" "X(256)" "character" ? ? ? ? ? ? no ? no no "32" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.fri_temp.METER
"fri_temp.METER" ? ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.fri_temp.TYP
"fri_temp.TYP" "Typ" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.fri_temp.APPARAT
"fri_temp.APPARAT" "Apparat" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.fri_temp.SAKR
"fri_temp.SAKR" "Säkr." ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.fri_temp.MAXSAKR
"fri_temp.MAXSAKR" "Max !Säkr." ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.fri_temp.SKAPADR
"fri_temp.SKAPADR" "SkåpAdress" "X(256)" "character" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.fri_temp.ANMARK
"fri_temp.ANMARK" "Anmärkning" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KABFRI */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KABKAB
/* Query rebuild information for BROWSE BRW_KABKAB
     _TblList          = "Temp-Tables.kon_val,Temp-Tables.fri_temp WHERE Temp-Tables.kon_val ..."
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.F2
"kon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val.F3
"kon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val.F4
"kon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val.F5
"kon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val.F6
"kon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.kon_val.TYP
     _FldNameList[7]   = Temp-Tables.fri_temp.KABADR
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KABKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KABSM
/* Query rebuild information for BROWSE BRW_KABSM
     _TblList          = "Temp-Tables.kon_val,Temp-Tables.fri_temp WHERE Temp-Tables.kon_val ..."
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_val.F3|yes,Temp-Tables.kon_val.F4|yes,Temp-Tables.kon_val.F5|yes,Temp-Tables.kon_val.F6|yes"
     _FldNameList[1]   > Temp-Tables.kon_val.F1
"kon_val.F1" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.75" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val.F2
"kon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val.F3
"kon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val.F4
"kon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val.F5
"kon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kon_val.F6
"kon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = Temp-Tables.fri_temp.KABADR
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KABSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALAKONS
/* Query rebuild information for BROWSE BRW_KALAKONS
     _TblList          = "Temp-Tables.kalk_temp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.kalk_temp.ARBKOD
     _FldNameList[2]   = Temp-Tables.kalk_temp.LOPNR
     _FldNameList[3]   = Temp-Tables.kalk_temp.BENAMNING
     _FldNameList[4]   = Temp-Tables.kalk_temp.ANTAL
     _FldNameList[5]   = Temp-Tables.kalk_temp.ENHET
     _FldNameList[6]   = Temp-Tables.kalk_temp.NUM
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALAKONS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALK
/* Query rebuild information for BROWSE BRW_KALK
     _TblList          = "Temp-Tables.kalk_temp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kalk_temp.ARBKOD
"kalk_temp.ARBKOD" "Kod" "X(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kalk_temp.LOPNR
"kalk_temp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kalk_temp.BENAMNING
"kalk_temp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kalk_temp.ANTAL
"kalk_temp.ANTAL" "Antal" "->>>>9.999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kalk_temp.ENHET
"kalk_temp.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB2
/* Query rebuild information for BROWSE BRW_KB2
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB3
/* Query rebuild information for BROWSE BRW_KB3
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB4
/* Query rebuild information for BROWSE BRW_KB4
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB5
/* Query rebuild information for BROWSE BRW_KB5
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB6
/* Query rebuild information for BROWSE BRW_KB6
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB6 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOMBKOPUPP
/* Query rebuild information for BROWSE BRW_KOMBKOPUPP
     _TblList          = "Temp-Tables.kombtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kombtemp.F1
"kombtemp.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kombtemp.FORNR
"kombtemp.FORNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kombtemp.LINNR
"kombtemp.LINNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kombtemp.NATNR
"kombtemp.NATNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "14.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kombtemp.FRI1
"kombtemp.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kombtemp.FRI2
"kombtemp.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.kombtemp.ID
"kombtemp.ID" ? ? "logical" ? ? ? ? ? ? no ? no no "4" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.kombtemp.ID2
"kombtemp.ID2" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.kombtemp.UPPLAG
"kombtemp.UPPLAG" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.kombtemp.KOMB
"kombtemp.KOMB" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOMBKOPUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _TblList          = "Temp-Tables.konsttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konsttemp.KTYPKOD
"konsttemp.KTYPKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konsttemp.BENAMNING
"konsttemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konsttemp.BILD
"konsttemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KONANDKONST
/* Query rebuild information for BROWSE BRW_KONANDKONST
     _TblList          = "Temp-Tables.konsttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konsttemp.KTYPKOD
"konsttemp.KTYPKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konsttemp.BENAMNING
"konsttemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KONANDKONST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LIN
/* Query rebuild information for BROWSE BRW_LIN
     _TblList          = "Temp-Tables.kopp_lina"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.kopp_lina.ENR
     _FldNameList[2]   > Temp-Tables.kopp_lina.BENAMNING
"kopp_lina.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "32" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kopp_lina.METER
"kopp_lina.METER" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kopp_lina.LEDARE
"kopp_lina.LEDARE" "Antal" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LIN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LINLINUPP
/* Query rebuild information for BROWSE BRW_LINLINUPP
     _TblList          = "Temp-Tables.lin_tab2"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.lin_tab2.ENR|yes"
     _FldNameList[1]   = Temp-Tables.lin_tab2.ENR
     _FldNameList[2]   > Temp-Tables.lin_tab2.BENAMNING
"lin_tab2.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LINLINUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.list_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.list_mtrl.ENR
     _FldNameList[2]   > Temp-Tables.list_mtrl.BENAMNING
"list_mtrl.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.list_mtrl.ANTAL
"list_mtrl.ANTAL" ? ">>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.list_mtrl.ENHET
"list_mtrl.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no "2.75" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.list_mtrl.LINKAB
"list_mtrl.LINKAB" "L/K" ? "logical" ? ? ? ? ? ? no ? no no "2.25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.list_mtrl.PRIS
     _FldNameList[7]   > Temp-Tables.list_mtrl.LEVKOD
"list_mtrl.LEVKOD" "Lev" "x(2)" "character" ? ? ? ? ? ? no ? no no "2.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRLFRI
/* Query rebuild information for BROWSE BRW_MTRLFRI
     _TblList          = "Temp-Tables.mtrl_tab"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.mtrl_tab.BENAMNING
"mtrl_tab.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "84.88" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRLFRI */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRLSM
/* Query rebuild information for BROWSE BRW_MTRLSM
     _TblList          = "Temp-Tables.list_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.list_mtrl.ENR
"list_mtrl.ENR" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.list_mtrl.BENAMNING
"list_mtrl.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.list_mtrl.ANTAL
"list_mtrl.ANTAL" ? ">>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.list_mtrl.ENHET
     _FldNameList[5]   > Temp-Tables.list_mtrl.LINKAB
"list_mtrl.LINKAB" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.list_mtrl.PRIS
     _FldNameList[7]   > Temp-Tables.list_mtrl.LEVKOD
"list_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRLSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ORD
/* Query rebuild information for BROWSE BRW_ORD
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.ID2
"kon_val.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "15.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val.EXTRA1
"kon_val.EXTRA1" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "21.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val.F1
"kon_val.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val.F2
"kon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val.F3
"kon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kon_val.F4
"kon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.kon_val.F5
"kon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.kon_val.F6
"kon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.kon_val.EXTRA3
"kon_val.EXTRA3" "+KB" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ORD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ORDID
/* Query rebuild information for BROWSE BRW_ORDID
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.ID2
"kon_val.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val.EXTRA1
"kon_val.EXTRA1" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val.F1
"kon_val.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val.F2
"kon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val.F3
"kon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kon_val.F4
"kon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.kon_val.F5
"kon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.kon_val.F6
"kon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.kon_val.EXTRA3
"kon_val.EXTRA3" "+KB" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ORDID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ORDIDVAL
/* Query rebuild information for BROWSE BRW_ORDIDVAL
     _TblList          = "Temp-Tables.idkon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.idkon_val.ID2
"idkon_val.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.idkon_val.EXTRA1
"idkon_val.EXTRA1" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.idkon_val.F1
"idkon_val.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.idkon_val.F2
"idkon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.idkon_val.F3
"idkon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.idkon_val.F4
"idkon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.idkon_val.F5
"idkon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.idkon_val.F6
"idkon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.idkon_val.EXTRA3
"idkon_val.EXTRA3" "+KB" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ORDIDVAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATS
/* Query rebuild information for BROWSE BRW_SATS
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   > Temp-Tables.satstemp.BENAMNING
"satstemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "42" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATSSM
/* Query rebuild information for BROWSE BRW_SATSSM
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   > Temp-Tables.satstemp.BENAMNING
"satstemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATSSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SKAPKAB
/* Query rebuild information for BROWSE BRW_SKAPKAB
     _TblList          = "Temp-Tables.berskaptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.berskaptemp.KOD
     _FldNameList[2]   > Temp-Tables.berskaptemp.BENAMNING
"berskaptemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SKAPKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SOKMTRLF
/* Query rebuild information for BROWSE BRW_SOKMTRLF
     _TblList          = "Temp-Tables.soktamtrlfavo"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.soktamtrlfavo.Enr|yes"
     _FldNameList[1]   = Temp-Tables.soktamtrlfavo.Enr
     _FldNameList[2]   > Temp-Tables.soktamtrlfavo.Benamning
"soktamtrlfavo.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.soktamtrlfavo.Enhet
"soktamtrlfavo.Enhet" "Enh" "x(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.soktamtrlfavo.NPRIS
"soktamtrlfavo.NPRIS" ? ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SOKMTRLF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SOKMTRLFSM
/* Query rebuild information for BROWSE BRW_SOKMTRLFSM
     _TblList          = "Temp-Tables.soktamtrlfavo"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.soktamtrlfavo.Enr|yes"
     _FldNameList[1]   = Temp-Tables.soktamtrlfavo.Enr
     _FldNameList[2]   > Temp-Tables.soktamtrlfavo.Benamning
"soktamtrlfavo.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.soktamtrlfavo.Enhet
"soktamtrlfavo.Enhet" "Enh" "x(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.soktamtrlfavo.NPRIS
"soktamtrlfavo.NPRIS" ? ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SOKMTRLFSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STOLP
/* Query rebuild information for BROWSE BRW_STOLP
     _TblList          = "Temp-Tables.berstolptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.berstolptemp.ENR
"berstolptemp.ENR" ? ? "character" ? ? ? ? ? ? no ? no no "11.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.berstolptemp.BENAMNING
"berstolptemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.berstolptemp.PRIS
"berstolptemp.PRIS" "Netto pris" ? "decimal" ? ? ? ? ? ? no ? no no "10.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.berstolptemp.LEVKOD
"berstolptemp.LEVKOD" "Lev" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STOLP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STOLPSM
/* Query rebuild information for BROWSE BRW_STOLPSM
     _TblList          = "Temp-Tables.berstolptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.berstolptemp.ENR
     _FldNameList[2]   > Temp-Tables.berstolptemp.BENAMNING
"berstolptemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.berstolptemp.PRIS
"berstolptemp.PRIS" "Netto pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.berstolptemp.LEVKOD
"berstolptemp.LEVKOD" "Lev" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STOLPSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TAB1
/* Query rebuild information for BROWSE BRW_TAB1
     _TblList          = "Temp-Tables.berfortemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.berfortemp.FORNR
     _FldNameList[2]   = Temp-Tables.berfortemp.NAMN
     _FldNameList[3]   > Temp-Tables.berfortemp.OMRADE
"berfortemp.OMRADE" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.berfortemp.AVDNR
"berfortemp.AVDNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.berfortemp.VIJUDID
"berfortemp.VIJUDID" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TAB1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TAB2
/* Query rebuild information for BROWSE BRW_TAB2
     _TblList          = "Temp-Tables.berlinjetemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.berlinjetemp.LINNR
     _FldNameList[2]   > Temp-Tables.berlinjetemp.NAMN
"berlinjetemp.NAMN" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "49.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TAB2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TAB3
/* Query rebuild information for BROWSE BRW_TAB3
     _TblList          = "Temp-Tables.bernattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.bernattemp.NATNR
     _FldNameList[2]   > Temp-Tables.bernattemp.NAMN
"bernattemp.NAMN" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "49.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TAB3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TILLLIN
/* Query rebuild information for BROWSE BRW_TILLLIN
     _TblList          = "Temp-Tables.kon_id2"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_id2.LINNR|yes,Temp-Tables.kon_id2.NATNR|yes,Temp-Tables.kon_id2.FRI1|yes,Temp-Tables.kon_id2.FRI2|yes"
     _FldNameList[1]   > Temp-Tables.kon_id2.LINNR
"kon_id2.LINNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_id2.NATNR
"kon_id2.NATNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_id2.FRI1
"kon_id2.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_id2.FRI2
"kon_id2.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_id2.FRI3
"kon_id2.FRI3" ? ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TILLLIN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPPKOPUPP
/* Query rebuild information for BROWSE BRW_UPPKOPUPP
     _TblList          = "Temp-Tables.berupptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.berupptemp.UPPLAG
"berupptemp.UPPLAG" "Upplag" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.berupptemp.ADRESS
"berupptemp.ADRESS" "Adress" "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.berupptemp.POSTNR
"berupptemp.POSTNR" "Postnr" ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.berupptemp.ORT
"berupptemp.ORT" "PostOrt" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.berupptemp.XKORDCH
"berupptemp.XKORDCH" "X" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.berupptemp.YKORDCH
"berupptemp.YKORDCH" "Y" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UPPKOPUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPPLINUPP
/* Query rebuild information for BROWSE BRW_UPPLINUPP
     _TblList          = "Temp-Tables.berupptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.berupptemp.UPPLAG|yes"
     _FldNameList[1]   > Temp-Tables.berupptemp.UPPLAG
"berupptemp.UPPLAG" "Upplag" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.berupptemp.ADRESS
"berupptemp.ADRESS" "Adress" "X(256)" "character" ? ? ? ? ? ? no ? no no "46" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_UPPLINUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_display"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.kon_display.ORD
     _FldNameList[2]   > Temp-Tables.kon_display.ID2
"kon_display.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "15.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_display.EXTRA1
"kon_display.EXTRA1" "Fri id" "X(256)" "character" ? ? ? ? ? ? yes ? no no "20.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_display.F1
"kon_display.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_display.F2
"kon_display.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kon_display.F4
"kon_display.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.kon_display.F3
"kon_display.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.kon_display.F5
"kon_display.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.kon_display.F6
"kon_display.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.kon_display.EXTRA3
"kon_display.EXTRA3" "+KB" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ATG
/* Query rebuild information for FRAME FRAME-ATG
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ATG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BARABREDD
/* Query rebuild information for FRAME FRAME-BARABREDD
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BARABREDD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-FRI
/* Query rebuild information for FRAME FRAME-FRI
     _Query            is NOT OPENED
*/  /* FRAME FRAME-FRI */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KABSKAP
/* Query rebuild information for FRAME FRAME-KABSKAP
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KABSKAP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-LIN
/* Query rebuild information for FRAME FRAME-LIN
     _Query            is NOT OPENED
*/  /* FRAME FRAME-LIN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-LINUPP
/* Query rebuild information for FRAME FRAME-LINUPP
     _Query            is NOT OPENED
*/  /* FRAME FRAME-LINUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-SKAPMTRL
/* Query rebuild information for FRAME FRAME-SKAPMTRL
     _Query            is NOT OPENED
*/  /* FRAME FRAME-SKAPMTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_FAVOUPPLAG
&Scoped-define FRAME-NAME FRAME-KOPUPP
&Scoped-define SELF-NAME BRW_FAVOUPPLAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FAVOUPPLAG WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_FAVOUPPLAG IN FRAME FRAME-KOPUPP /* Favoriter Upplag - Dubbelklicka för att lägga upp upplaget på beredningen */
DO:
  /**/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-BARABREDD
&Scoped-define BROWSE-NAME BRW_ANDKONST
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


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
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:  
   &Scoped-define FRAME-NAME FRAME-BARABREDD
   APPLY 'CHOOSE' TO BTN_AVB IN FRAME FRAME-BARABREDD.
      
          
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
   slutaejnu = FALSE.
   APPLY 'CHOOSE' TO BTN_AVB IN FRAME FRAME-BARABREDD.
   IF slutaejnu = TRUE THEN DO:
      slutaejnu = FALSE.
      RETURN NO-APPLY.
   END.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
   DEFINE VARIABLE AA AS INTEGER NO-UNDO.
   {STARTWIN2.I}   
   EMPTY TEMP-TABLE kon_display NO-ERROR.  
   WINDOW-3:TITLE = "".
   {BERTITLE.I}
   Guru.GlobalaVariabler:BeredningsNamn = {&WINDOW-NAME}:TITLE.
   BRW_KALK:TITLE = "Kalkylkoder".
   
   ON esc OF FRAME {&FRAME-NAME} ANYWHERE  DO:
      RETURN NO-APPLY.
   END.
 
   RUN DYNFRAMESIZEFM.P PERSISTENT SET framesizeh (INPUT WINDOW-3:HANDLE, INPUT TABLE frametemp,INPUT TABLE widgettemp,INPUT BRW_VAL:HANDLE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
   END. 
   RUN enable_UI. 
   ASSIGN 
   Guru.GlobalaVariabler:BeredningsNummer = INTEGER(valaonr) 
   Guru.GlobalaVariabler:BeredningsOmrade = valomrade.
   RUN btnstart_UI.
   
   ON ALT-M OF FRAME {&FRAME-NAME} ANYWHERE PERSISTENT RUN ctrlMproc_UI IN btnbervalh.
   ON CTRL-N OF FRAME {&FRAME-NAME} ANYWHERE PERSISTENT RUN ctrlMproc_UI IN btnbervalh.
   IF Guru.Konstanter:hoppsekvar[3] =  FALSE THEN RUN btnhidden_UI IN btnbervalh (INPUT "MBTN_KALK",INPUT 4). 
   RUN btnhidden_UI IN btnbervalh (INPUT "MBTN_FRIKORT",INPUT 4). 
   IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)   THEN kon_display.ORD:VISIBLE IN BROWSE BRW_VAL = FALSE .
   ELSE kon_display.ORD:VISIBLE IN BROWSE BRW_VAL = FALSE.
   FIND FIRST kon_val WHERE kon_val.KSKAP = FALSE NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:         
      APPLY "VALUE-CHANGED" TO BRW_VAL .
   END.  
   ELSE DO:
      RUN nolabel_UI.      
   END.   
 
   ASSIGN FRAME FRAME-ID:HIDDEN = TRUE.
   ASSIGN FRAME FRAME-ANM:HIDDEN = TRUE.
   ASSIGN FRAME FRAME-MTRL:HIDDEN = TRUE.
   ASSIGN FRAME FRAME-KALK:HIDDEN = TRUE.
   ASSIGN FRAME FRAME-KONST:HIDDEN = FALSE.
   
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN
      list_mtrl.PRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE
      soktamtrlfavo.NPRIS:VISIBLE IN BROWSE BRW_SOKMTRLF = FALSE
      soktamtrlfavo.NPRIS:VISIBLE IN BROWSE BRW_SOKMTRLFSM = FALSE 
      berstolptemp.PRIS:VISIBLE IN BROWSE BRW_STOLP = FALSE. 
   END. 
   IF Guru.Konstanter:globforetag = "elte" OR Guru.Konstanter:globforetag = "Celpa" THEN list_mtrl.LINKAB:VISIBLE IN BROWSE BRW_MTRL = TRUE.
   ELSE list_mtrl.LINKAB:VISIBLE IN BROWSE BRW_MTRL = FALSE.
   IF Guru.Konstanter:globforetag = "elte" OR Guru.Konstanter:globforetag = "Celpa" THEN list_mtrl.LINKAB:VISIBLE IN BROWSE BRW_MTRLSM = TRUE.
   ELSE list_mtrl.LINKAB:VISIBLE IN BROWSE BRW_MTRLSM = FALSE.  
   IF Guru.Konstanter:globforetag = "fors" THEN DO:
      status-ok = CMB_SKYLTART:DELETE("A104") IN FRAME FRAME-FRI.
      status-ok = CMB_SKYLTART:DELETE("A105") IN FRAME FRAME-FRI.
      status-ok = CMB_SKYLTART:DELETE("A107") IN FRAME FRAME-FRI.
      status-ok = CMB_SKYLTART:ADD-LAST("A202") IN FRAME FRAME-FRI.
      status-ok = CMB_SKYLTART:ADD-LAST("A204") IN FRAME FRAME-FRI.
      status-ok = CMB_SKYLTART:ADD-LAST("A203") IN FRAME FRAME-FRI.            
   END.       
   
   RUN setapplyvc IN framesizeh (INPUT RAD_KOMP:HANDLE).
   FILL-IN-SKAPMTRL:HIDDEN = TRUE.
   ASSIGN FRAME FRAME-MTRL:HIDDEN = false.
   RUN fixastorlek_UI IN btnbervalh.
   ASSIGN FRAME FRAME-MTRL:HIDDEN = TRUE.
   
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}   
   APPLY "VALUE-CHANGED" TO CMB_VAL. 
   BTN_SOLEN:HIDDEN = TRUE.
   IF kalkbervarSUND = TRUE THEN MBTN_SCHAKT:HIDDEN = TRUE.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsluta_UI WINDOW-3 
PROCEDURE avsluta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   IF VALID-HANDLE(Guru.GlobalaVariabler:BerDSapph) THEN DO:
      RUN berDSavsluta_UI IN Guru.GlobalaVariabler:BerDSapph.
      DELETE PROCEDURE Guru.GlobalaVariabler:BerDSapph NO-ERROR.   
      Guru.GlobalaVariabler:BerDSapph = ?.
   END.
   Guru.GlobalaVariabler:KorBerDS = FALSE.
   instartaannatpro = startaannatpro. 
   IF VALID-HANDLE(nettoh) THEN DO:
      DELETE PROCEDURE nettoh NO-ERROR.   
      nettoh = ?.
   END.
   IF VALID-HANDLE(btnbervalh) THEN DO:
      RUN borthand_UI IN btnbervalh.
      DELETE PROCEDURE btnbervalh NO-ERROR.   
      btnbervalh = ?.
   END.
   IF VALID-HANDLE(btnordh) THEN DO:
      RUN borthand_UI IN btnordh.
      DELETE PROCEDURE btnordh NO-ERROR.
      btnordh = ?.
   END.     
   IF VALID-HANDLE(btnfrih) THEN DO:
      RUN borthand_UI IN btnfrih.
      
      DELETE PROCEDURE btnfrih NO-ERROR.  
      btnfrih = ?.
   END.        
   IF VALID-HANDLE(btnkskaph) THEN DO:
      RUN borthand_UI IN btnkskaph.
      DELETE PROCEDURE btnkskaph NO-ERROR.
      btnkskaph = ?.
   END.                   
   IF VALID-HANDLE(btnkskapmtrlh) THEN DO:
      RUN borthand_UI IN btnkskapmtrlh.
      DELETE PROCEDURE btnkskapmtrlh NO-ERROR. 
      btnkskapmtrlh = ?.
   END.                   
   IF VALID-HANDLE(btnklinh) THEN DO:
      RUN borthand_UI IN btnklinh.
      DELETE PROCEDURE btnklinh NO-ERROR.  
      btnklinh = ?.
   END.                   
   IF VALID-HANDLE(btnlinupph) THEN DO:
      RUN borthand_UI IN btnlinupph.
      DELETE PROCEDURE btnlinupph NO-ERROR.  
      btnlinupph = ?.
   END.                   
   IF VALID-HANDLE(btnkopupph) THEN DO:
      RUN borthand_UI IN btnkopupph.
      DELETE PROCEDURE btnkopupph NO-ERROR.  
      btnkopupph = ?.
   END.                   
   IF VALID-HANDLE(btnlistorh) THEN DO:
      RUN borthand_UI IN btnlistorh.
      DELETE PROCEDURE btnlistorh NO-ERROR. 
      btnlistorh = ?.
   END.                   
   IF VALID-HANDLE(btnatgh) THEN DO:
      RUN borthand_UI IN btnatgh.
      DELETE PROCEDURE btnatgh NO-ERROR. 
      btnatgh = ?.
   END.                   
   Guru.GlobalaVariabler:BeredningsNamn = "".
   APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnatg_UI WINDOW-3 
PROCEDURE btnatg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE vart AS INTEGER NO-UNDO.
   vart = 0.
   {BTNATG.I}
   BTN_AVBATG:HIDDEN IN FRAME FRAME-ATG = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfri_UI WINDOW-3 
PROCEDURE btnfri_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   
   IF NOT VALID-HANDLE(btnfrih) THEN DO:
      CREATE whandltemp.
      ordningnr = 1.   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_KORTVAL:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_VALFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_SKAPAFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_MTRLFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_APP:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_ANDFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_BORTFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_VISAFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_SKRIVFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_EXKSKO:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_PDFFRI:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KABFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_EXSTOLP:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_EXMARK:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_UPPFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NERFRI:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-TYP:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SADR:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KABEL:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-METER:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ADR:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_TYP:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-APPARAT:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-GRUPPNR:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-AR:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SAK:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-MAX:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ANMARK:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B2:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B3:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-F2:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-F3:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KORT:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_NASTAKABEL:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_MEXC:HANDLE IN FRAME FRAME-FRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,fri_temp.GRUPPNR:HANDLE IN BROWSE BRW_KABFRI). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_SORT:HANDLE IN FRAME FRAME-FRI).      
      RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_SKYLTART:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SKYLTANT:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SKYLTBEN:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-NSKYLTANT:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-NISKYLTANT:HANDLE IN FRAME FRAME-FRI).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ABONNENT:HANDLE IN FRAME FRAME-FRI).
      
      RUN FRIKORTM.P PERSISTENT SET btnfrih (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
   END.  
   RUN main_UI IN btnfrih. 
   RUN frame_UI (INPUT "FRI").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkopupp_UI WINDOW-3 
PROCEDURE btnkopupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
  
   IF NOT VALID-HANDLE(btnkopupph) THEN DO:
      CREATE whandltemp.
      ordningnr = 1.    
      RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KOMBKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_KOMBKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_BKOMBKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_UPPKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_VISAKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_SKRIVKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_UPPKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NYKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ANDRAKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BORTKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_VISAKOPUPP-2:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_SKRIVKOPUPP-2:HANDLE IN FRAME FRAME-KOPUPP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_KonstruktionerKOPUPP:HANDLE IN MENU POPUP-MENU-BRW_KOMBKOPUPP).                
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_OUPPKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).      
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_FAVOUPPLAG:HANDLE IN FRAME FRAME-KOPUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_FAVOUPP:HANDLE IN FRAME FRAME-KOPUPP).
        
      RUN KOPUPPM.P PERSISTENT SET btnkopupph (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp). 
                                     
   END.  
   RUN main_UI IN btnkopupph. 
   RUN frame_UI (INPUT "KOPUPP"). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkskapmat_UI WINDOW-3 
PROCEDURE btnkskapmat_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   IF NOT VALID-HANDLE(btnkskapmtrlh) THEN DO:
      CREATE whandltemp.
      ordningnr = 1.   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KABSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_MTRLSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_STOLPSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_SATSSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_HLEVSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_MTRLSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_OVERSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BACKSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_LEVSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_LEVSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KODSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ENRSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NYSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ANTALSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_SATSSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_TEXTSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-BENSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NOLLSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ALLTMTRLSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_SPECSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_LINKABSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_SOKSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B2SM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B3SM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-F2SM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-F3SM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SOKALTSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,RECT-10:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,IMAGE-6:HANDLE IN FRAME FRAME-SKAPMTRL).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,list_mtrl.ANTAL:HANDLE IN BROWSE BRW_MTRLSM).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,mtrltemp.NPRIS:HANDLE IN BROWSE BRW_HLEVSM).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,list_mtrl.LINKAB:HANDLE IN BROWSE BRW_MTRLSM).
/*
      RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationHLEVSM:HANDLE IN MENU POPUP-MENU-BRW_HLEVSM).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationMTRLSM:HANDLE IN MENU POPUP-MENU-BRW_MTRLSM).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationSATSSM:HANDLE IN MENU POPUP-MENU-BRW_SATSSM).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationSTOLPSM:HANDLE IN MENU POPUP-MENU-BRW_STOLPSM).
  */
      RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_AvmarkeraHLEVSM:HANDLE IN MENU POPUP-MENU-BRW_HLEVSM).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,list_mtrl.PRIS:HANDLE IN BROWSE BRW_MTRLSM).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KOMMSM:HANDLE IN FRAME FRAME-SKAPMTRL).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-SKAPMTRL:HANDLE).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_ALLSKMTRL:HANDLE IN FRAME FRAME-SKAPMTRL).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_SOKMTRLFSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ADRKABM:HANDLE IN FRAME FRAME-SKAPMTRL).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NASTKON:HANDLE IN FRAME FRAME-SKAPMTRL). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KOMPSKAP:HANDLE IN FRAME FRAME-SKAPMTRL).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KUNDLISTASM:HANDLE IN FRAME FRAME-SKAPMTRL).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ENRSTOLPSM:HANDLE IN FRAME FRAME-SKAPMTRL).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-BENSTOLPSM:HANDLE IN FRAME FRAME-SKAPMTRL).
    
      RUN SKAPMATRLM.P PERSISTENT SET btnkskapmtrlh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).
      fri_temp.KABADR:LABEL  IN BROWSE BRW_KABSM = "Kabeladress".   
      IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "celpa" OR Guru.Konstanter:globforetag = "CPOMA" THEN DO:
         fri_temp.KABADR:LABEL IN BROWSE BRW_KABSM = "Skylttext".
      END.                                            
   END. 
   RUN main_UI IN btnkskapmtrlh. 
   RUN frame_UI (INPUT "SKAPMAT").
   RUN fixastorlek_UI IN btnkskapmtrlh.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkskap_UI WINDOW-3 
PROCEDURE btnkskap_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   IF NOT VALID-HANDLE(btnkskaph) THEN DO:
      CREATE whandltemp.
      ordningnr = 1.   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KABKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_ANTKAB:HANDLE IN FRAME FRAME-KABSKAP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_UPPKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_KORTKAB:HANDLE IN FRAME FRAME-KABSKAP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_MTRLKAB:HANDLE IN FRAME FRAME-KABSKAP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NERKAB :HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BORTKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_TYP1KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_TYP2KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_TYP3KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_SKAPKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_PLUSKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_MINKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KSKAPKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KORTKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B1KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B3KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B2KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-F3KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-F1KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-F2KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KOPKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-NR1KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-NR2KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-NR3KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-LEDKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KOPSKAP:HANDLE IN FRAME FRAME-KABSKAP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ADRKAB:HANDLE IN FRAME FRAME-KABSKAP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-MODUL:HANDLE IN FRAME FRAME-KABSKAP).      
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-T1:HANDLE IN FRAME FRAME-KABSKAP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-T2:HANDLE IN FRAME FRAME-KABSKAP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-T3:HANDLE IN FRAME FRAME-KABSKAP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-EON:HANDLE IN FRAME FRAME-KABSKAP).             
      RUN KSKAPM.P PERSISTENT SET btnkskaph (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).      
      fri_temp.KABADR:LABEL IN BROWSE BRW_KABKAB = "Kabeladress". 
      
      IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "celpa" OR Guru.Konstanter:globforetag = "CPOMA" THEN DO:
         fri_temp.KABADR:LABEL IN BROWSE BRW_KABKAB = "Skylttext".
      END.                   
   END.  
   RUN main_UI IN btnkskaph. 
   RUN frame_UI (INPUT "KSKAP").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnlinupp_UI WINDOW-3 
PROCEDURE btnlinupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   IF NOT VALID-HANDLE(btnlinupph) THEN DO:
      CREATE whandltemp.
      ordningnr = 1.   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_LINLINUPP:HANDLE IN FRAME FRAME-LINUPP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_UPLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_VISALINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-PROCLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_MINLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_SKRIVLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-TOTMETERLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NOLLLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_UPPLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-UPPMETERLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NYLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ANDRALINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BORTLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KOPPLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_AVBLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ENRLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-BENLINUPP:HANDLE IN FRAME FRAME-LINUPP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-METERLINUPP:HANDLE IN FRAME FRAME-LINUPP).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-UPPLINUPP:HANDLE IN FRAME FRAME-LINUPP).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SUMMALINUPP:HANDLE IN FRAME FRAME-LINUPP).
      RUN LINUPPM.P PERSISTENT SET btnlinupph (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
   END.  
   RUN main_UI IN btnlinupph. 
   RUN frame_UI (INPUT "LINUPP").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnlin_UI WINDOW-3 
PROCEDURE btnlin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   IF NOT VALID-HANDLE(btnklinh) THEN DO:
      CREATE whandltemp.
      ordningnr = 1.   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_VALLIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_VAL2LIN :HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_FRANLIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_TILLLIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_LINUPP:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_VISALIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_SCHAKT:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_KORTLIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_VISA-2LIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_LIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-LANGDLIN :HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_UPLIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ANTALLIN :HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_MINLIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_SKAPALIN:HANDLE IN FRAME FRAME-LIN).  
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ANDLIN:HANDLE IN FRAME FRAME-LIN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BORTLIN:HANDLE IN FRAME FRAME-LIN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-LINKAB:HANDLE IN FRAME FRAME-LIN).   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationLIN:HANDLE IN MENU POPUP-MENU-BRW_FRANLIN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_information2LIN:HANDLE IN MENU POPUP-MENU-BRW_TILLLIN). 
      RUN whandle_UI (INPUT-OUTPUT ordningnr,kopp_lina.METER:HANDLE IN BROWSE BRW_LIN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,kopp_lina.LEDARE:HANDLE IN BROWSE BRW_LIN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_AVBLIN:HANDLE IN FRAME FRAME-LIN).   
      RUN LINKABM.P PERSISTENT SET btnklinh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
   END.  
   RUN main_UI IN btnklinh. 
   RUN frame_UI (INPUT "LIN").
   APPLY "ENYRY" TO kopp_lina.METER IN BROWSE BRW_LIN.
   /*
   RUN enlmeter_UI IN btnklinh.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnlishelp_UI WINDOW-3 
PROCEDURE btnlishelp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   CMB_SKRIV  = CMB_SKRIV:SCREEN-VALUE IN FRAME FRAME-LISTOR
   CMB_PRIS   = CMB_PRIS:SCREEN-VALUE
   CMB_STOLP  = CMB_STOLP:SCREEN-VALUE     
   TOG_MTRL  = TOG_MTRL:CHECKED 
   TOG_VAL   = TOG_VAL:CHECKED
   TOG_KONSTRUKTION = TOG_KONSTRUKTION:CHECKED
   /*TOG_SPANN = TOG_SPANN:CHECKED*/
   TOG_UPP   = TOG_UPP:CHECKED
   TOG_ID    = TOG_ID:CHECKED
   TOG_ANM   = TOG_ANM:CHECKED
   TOG_TEXT  = TOG_TEXT:CHECKED
   TOG_EGEN  = TOG_EGEN:CHECKED
   TOG_P3    = TOG_P3:CHECKED
   TOG_SIDEXCEL = TOG_SIDEXCEL:CHECKED
   TOG_KON   = TOG_KON:CHECKED
   TOG_KONVAL = TOG_KONVAL:CHECKED.
   TOG_JMFKONS = TOG_JMFKONS:CHECKED.
    Guru.GlobalaVariabler:Egengkontroll = TOG_EGEN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnlistor_UI WINDOW-3 
PROCEDURE btnlistor_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {BTNLISTOR.I}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnord_UI WINDOW-3 
PROCEDURE btnord_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   IF NOT VALID-HANDLE(btnordh) THEN DO:
      CREATE whandltemp.
      ordningnr = 1.   
      RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_VALORD:HANDLE IN FRAME FRAME-ORDN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_FRAN:HANDLE IN FRAME FRAME-ORDN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_UPPORD:HANDLE IN FRAME FRAME-ORDN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NERORD:HANDLE IN FRAME FRAME-ORDN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_ORD:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_MORDUPP:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_MORDNER:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_ORD:HANDLE IN FRAME FRAME-ORDN).
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_ORDID:HANDLE IN FRAME FRAME-ORDN).              
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_ORDIDVAL:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-IDNRORDID:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-IDNRORDID2:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_OVERID:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BACKID:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_MORDUPPID:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_MORDNERID:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KOPPIDORD:HANDLE IN FRAME FRAME-ORDN).        
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ID1ORD:HANDLE IN FRAME FRAME-ORDN).        
      /*
            
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-IDNR1LABELORD:HANDLE IN FRAME FRAME-ORDN).        
      */
            
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ID2ORD:HANDLE IN FRAME FRAME-ORDN).        
      /*
            
      RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-IDNR2LABELORD:HANDLE IN FRAME FRAME-ORDN).  
      */
      RUN whandle_UI (INPUT-OUTPUT ordningnr,kon_val.EXTRA1:HANDLE IN BROWSE BRW_ORD).
      RUN BYTIDM.P PERSISTENT SET btnordh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
   END.  
   RUN main_UI IN btnordh. 
   RUN frame_UI (INPUT "ORDN").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnstart_UI WINDOW-3 
PROCEDURE btnstart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
  
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   ASSIGN
   soktamtrlfavo.ENR:LABEL IN BROWSE BRW_SOKMTRLF = Guru.Konstanter:genk
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk
   list_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk
   berfortemp.OMRADE:LABEL IN BROWSE BRW_TAB1 = Guru.Konstanter:gomrk
   berfortemp.AVDNR:LABEL IN BROWSE BRW_TAB1= Guru.Konstanter:gavdk
   berfortemp.VIJUDID:LABEL IN BROWSE BRW_TAB1 = Guru.Konstanter:gjuk.
   CREATE whandltemp.
   ordningnr = 1.   
   RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-3:HANDLE).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_VAL:HANDLE IN FRAME FRAME-BARABREDD).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_LIST:HANDLE IN FRAME FRAME-BARABREDD).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_KONST:HANDLE IN FRAME FRAME-BARABREDD).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_ANDORD:HANDLE IN FRAME FRAME-BARABREDD).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_LIN:HANDLE IN FRAME FRAME-BARABREDD).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_UPPL:HANDLE IN FRAME FRAME-BARABREDD).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_KOPI:HANDLE IN FRAME FRAME-BARABREDD).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_FRIKORT:HANDLE IN FRAME FRAME-BARABREDD).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_KSKAP:HANDLE IN FRAME FRAME-BARABREDD).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_SNABB:HANDLE IN FRAME FRAME-BARABREDD).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_OK:HANDLE IN FRAME FRAME-BARABREDD).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_ID:HANDLE IN FRAME FRAME-BARABREDD).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_AVB:HANDLE IN FRAME FRAME-BARABREDD).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_VAL:HANDLE IN FRAME FRAME-BARABREDD).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_SOLEN:HANDLE IN FRAME FRAME-BARABREDD).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ORDUPP:HANDLE IN FRAME FRAME-BARABREDD).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ORDNER:HANDLE IN FRAME FRAME-BARABREDD).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_ANM:HANDLE IN FRAME FRAME-BARABREDD).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_DOK:HANDLE IN FRAME FRAME-BARABREDD).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_KALK:HANDLE IN FRAME FRAME-BARABREDD).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_MTRL:HANDLE IN FRAME FRAME-BARABREDD).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RECT-MENY:HANDLE IN FRAME FRAME-BARABREDD).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_MTRL:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_SATS:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_STOLP:HANDLE IN FRAME FRAME-MTRL).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_HLEV:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_MTRL:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_OVER:HANDLE IN FRAME FRAME-MTRL).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BACK:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_LEV:HANDLE IN FRAME FRAME-MTRL).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_LEV:HANDLE IN FRAME FRAME-MTRL).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ENR:HANDLE IN FRAME FRAME-MTRL).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KOD:HANDLE IN FRAME FRAME-MTRL).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-BEN:HANDLE IN FRAME FRAME-MTRL).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KUNDLISTA:HANDLE IN FRAME FRAME-MTRL).   
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ENRSTOLP:HANDLE IN FRAME FRAME-MTRL).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-BENSTOLP:HANDLE IN FRAME FRAME-MTRL).
     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NY:HANDLE IN FRAME FRAME-MTRL).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ANTAL:HANDLE IN FRAME FRAME-MTRL).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_SATS:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_TEXT:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_PAR:HANDLE IN FRAME FRAME-MTRL).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_SOK:HANDLE IN FRAME FRAME-MTRL).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NOLL:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ALLTMTRL:HANDLE IN FRAME FRAME-MTRL).    
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_SPEC:HANDLE IN FRAME FRAME-MTRL).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_LINKAB:HANDLE IN FRAME FRAME-MTRL).      
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SOKALT:HANDLE IN FRAME FRAME-MTRL).  
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RECT-22:HANDLE IN FRAME FRAME-MTRL).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,IMAGE-4:HANDLE IN FRAME FRAME-MTRL).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KALK:HANDLE IN FRAME FRAME-KALK).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NYKOD:HANDLE IN FRAME FRAME-KALK).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BORT2:HANDLE IN FRAME FRAME-KALK).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_GRUNDUPP:HANDLE IN FRAME FRAME-KALK).       
   
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_BILD:HANDLE IN FRAME FRAME-BILD).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NYA:HANDLE IN FRAME FRAME-BILD).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BORTA:HANDLE IN FRAME FRAME-BILD).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_VISA:HANDLE IN FRAME FRAME-BILD).  
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BILDCOPY:HANDLE IN FRAME FRAME-BILD).
  
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_ANM:HANDLE IN FRAME FRAME-ANM).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,ED_ANM:HANDLE IN FRAME FRAME-ANM).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_OVER-2:HANDLE IN FRAME FRAME-ANM).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KOPPANM:HANDLE IN FRAME FRAME-ANM).      
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-TEXT:HANDLE IN FRAME FRAME-ANM).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_JURP:HANDLE IN FRAME FRAME-ID).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_AVD:HANDLE IN FRAME FRAME-ID).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_OMR:HANDLE IN FRAME FRAME-ID).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_HMT:HANDLE IN FRAME FRAME-ID).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_TAB1:HANDLE IN FRAME FRAME-ID).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-NR1:HANDLE IN FRAME FRAME-ID).      
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-IDNR1:HANDLE IN FRAME FRAME-ID).    
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_TAB2:HANDLE IN FRAME FRAME-ID).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-NR2:HANDLE IN FRAME FRAME-ID).      
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-IDNR2:HANDLE IN FRAME FRAME-ID).    
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SUND:HANDLE IN FRAME FRAME-ID).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_TAB3:HANDLE IN FRAME FRAME-ID).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_A:HANDLE IN FRAME FRAME-ID).            
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_B:HANDLE IN FRAME FRAME-ID).            
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_C:HANDLE IN FRAME FRAME-ID).            
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-NR3:HANDLE IN FRAME FRAME-ID).      
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ANL:HANDLE IN FRAME FRAME-ID).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KOPPID:HANDLE IN FRAME FRAME-ID).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NASTKONID:HANDLE IN FRAME FRAME-ID).
          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-TAB1:HANDLE IN FRAME FRAME-ID).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ID1:HANDLE IN FRAME FRAME-ID).      
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-IDNR1LABEL:HANDLE IN FRAME FRAME-ID).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-TAB2:HANDLE IN FRAME FRAME-ID).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-ID2:HANDLE IN FRAME FRAME-ID).      
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-IDNR2LABEL:HANDLE IN FRAME FRAME-ID).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SUNDLABEL:HANDLE IN FRAME FRAME-ID).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-A:HANDLE IN FRAME FRAME-ID).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B:HANDLE IN FRAME FRAME-ID).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-C:HANDLE IN FRAME FRAME-ID).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-TAB3:HANDLE IN FRAME FRAME-ID).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_KOMP:HANDLE IN FRAME FRAME-KONST).         
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_VAL:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NER:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_UPP:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KON:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KB2:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_B2:HANDLE IN FRAME FRAME-KONST).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KB3:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_B3:HANDLE IN FRAME FRAME-KONST).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KB4:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_B4:HANDLE IN FRAME FRAME-KONST).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KB5:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_B5:HANDLE IN FRAME FRAME-KONST).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KB6:HANDLE IN FRAME FRAME-KONST).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_B6:HANDLE IN FRAME FRAME-KONST).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B1:HANDLE IN FRAME FRAME-KONST).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B2:HANDLE IN FRAME FRAME-KONST).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B3:HANDLE IN FRAME FRAME-KONST).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B4:HANDLE IN FRAME FRAME-KONST).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B5:HANDLE IN FRAME FRAME-KONST).       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-B6:HANDLE IN FRAME FRAME-KONST).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlB2:HANDLE IN MENU POPUP-MENU-BRW_B2).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildB2:HANDLE IN MENU POPUP-MENU-BRW_B2).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_AvmarkeraB2:HANDLE IN MENU POPUP-MENU-BRW_B2).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlB3:HANDLE IN MENU POPUP-MENU-BRW_B3).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildB3:HANDLE IN MENU POPUP-MENU-BRW_B3).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_AvmarkeraB3:HANDLE IN MENU POPUP-MENU-BRW_B3).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlB4:HANDLE IN MENU POPUP-MENU-BRW_B4).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildB4:HANDLE IN MENU POPUP-MENU-BRW_B4).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_AvmarkeraB4:HANDLE IN MENU POPUP-MENU-BRW_B4).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlB5:HANDLE IN MENU POPUP-MENU-BRW_B5).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildB5:HANDLE IN MENU POPUP-MENU-BRW_B5).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_AvmarkeraB5:HANDLE IN MENU POPUP-MENU-BRW_B5).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlB6:HANDLE IN MENU POPUP-MENU-BRW_B6).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildB6:HANDLE IN MENU POPUP-MENU-BRW_B6).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_AvmarkeraB6:HANDLE IN MENU POPUP-MENU-BRW_B6).
   /*
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationHLEV:HANDLE IN MENU POPUP-MENU-BRW_HLEV). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Pris_information2:HANDLE IN MENU POPUP-MENU-BRW_HLEV).
   */
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlKB2:HANDLE IN MENU POPUP-MENU-BRW_KB2). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildKB2:HANDLE IN MENU POPUP-MENU-BRW_KB2). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlKB3:HANDLE IN MENU POPUP-MENU-BRW_KB3). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildKB3:HANDLE IN MENU POPUP-MENU-BRW_KB3). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlKB4:HANDLE IN MENU POPUP-MENU-BRW_KB4). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildKB4:HANDLE IN MENU POPUP-MENU-BRW_KB4).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlKB5:HANDLE IN MENU POPUP-MENU-BRW_KB5).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildKB5:HANDLE IN MENU POPUP-MENU-BRW_KB5).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_mtrlKB6:HANDLE IN MENU POPUP-MENU-BRW_KB6).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_bildKB6:HANDLE IN MENU POPUP-MENU-BRW_KB6).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_mtrl:HANDLE IN MENU POPUP-MENU-BRW_KON). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_bild:HANDLE IN MENU POPUP-MENU-BRW_KON).
/*
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationBRW_MTRL:HANDLE IN MENU POPUP-MENU-BRW_MTRL).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationSATS:HANDLE IN MENU POPUP-MENU-BRW_SATS).
   */
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Pris_information:HANDLE IN MENU POPUP-MENU-BRW_SATS). 
   /*
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationSTOLP:HANDLE IN MENU POPUP-MENU-BRW_STOLP).
   */
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_Visa_informationVAL:HANDLE IN MENU POPUP-MENU-BRW_VAL).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-ANM:HANDLE).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-BARABREDD:HANDLE). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-BILD:HANDLE).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-ID:HANDLE).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-KALK:HANDLE).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-KONST:HANDLE).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-MTRL:HANDLE).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,list_mtrl.ANTAL:HANDLE IN BROWSE BRW_MTRL).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-SKAPMTRL:HANDLE IN FRAME FRAME-BARABREDD).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_ATG:HANDLE IN FRAME FRAME-BARABREDD).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_ATERGA:HANDLE IN FRAME FRAME-BARABREDD). 
       
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_RATT:HANDLE IN FRAME FRAME-KONST).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_MARKAV:HANDLE IN FRAME FRAME-KONST).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_VISAGRUPP:HANDLE IN FRAME FRAME-KONST).           
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU-ITEM m_AvmarkeraHLEV:HANDLE IN MENU POPUP-MENU-BRW_HLEV). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_DEL:HANDLE IN FRAME FRAME-ID).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_KOMP:HANDLE IN FRAME FRAME-KONST).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_ANDRAK:HANDLE IN FRAME FRAME-ANDRAK).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-ANDRAK:HANDLE).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_STANG:HANDLE IN FRAME FRAME-ANDRAK).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KARTR:HANDLE IN FRAME FRAME-ID).                                            
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_ANMLOP:HANDLE IN FRAME FRAME-ANM).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_FLYG:HANDLE IN FRAME FRAME-BARABREDD).  
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-KOMM:HANDLE IN FRAME FRAME-MTRL).  
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_INK:HANDLE IN FRAME FRAME-BARABREDD).   
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_SOKMTRLF:HANDLE IN FRAME FRAME-MTRL). 
   /*
     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU POPUP-MENU-BRW_B3:HANDLE IN FRAME FRAME-KONST).
     
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MENU POPUP-MENU-BRW_B4:HANDLE IN BROWSE BRW_B4).
   */
   /*
   FILE-INFO:FILE-NAME = TRIM(SUBSTRING(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"_UI") + 3)).
   
   RUN loggskap_UI IN logprogh (INPUT 2,INPUT SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)), INPUT "WCCHB2", INPUT "").
   */
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_AS:HANDLE IN FRAME FRAME-ID). 
   
   RUN whandle_UI (INPUT-OUTPUT ordningnr,MBTN_SCHAKT:HANDLE IN FRAME FRAME-BARABREDD).
   
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_ANDKONST:HANDLE IN FRAME FRAME-ANDKONST).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-ANDKONST:HANDLE).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_STANGANDKONST :HANDLE IN FRAME FRAME-ANDKONST).        
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_VALANDKONST:HANDLE IN FRAME FRAME-ANDKONST).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KONANDKONST:HANDLE IN FRAME FRAME-ANDKONST).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_KALAKONS:HANDLE IN FRAME FRAME-ANDKONST).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BYTANGANDKONST :HANDLE IN FRAME FRAME-ANDKONST).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BORTKOPPID:HANDLE IN FRAME FRAME-ID).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,kon_display.EXTRA1:HANDLE IN BROWSE BRW_VAL).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_VISAKONST:HANDLE IN FRAME FRAME-KONST).
   RUN BERVALSDI2.P PERSISTENT SET btnbervalh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,INPUT TABLE widgettemp).
 
   /*
   IF VALID-HANDLE(logprogh) THEN DELETE PROCEDURE logprogh NO-ERROR.  
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-SKAPMTRL 
      WITH FRAME FRAME-BARABREDD IN WINDOW WINDOW-3.
  ENABLE BTN_OK RECT-MENY MBTN_ATERGA BTN_AVB MBTN_SCHAKT MBTN_INK MBTN_FLYG 
         MBTN_ANDORD MBTN_ATG MBTN_FRIKORT MBTN_KALK MBTN_KOPI BRW_VAL 
         MBTN_KSKAP BTN_SOLEN MBTN_LIN BTN_ORDUPP BTN_ORDNER MBTN_LIST 
         MBTN_SNABB MBTN_UPPL MBTN_ANM MBTN_DOK MBTN_ID MBTN_KONST MBTN_MTRL 
      WITH FRAME FRAME-BARABREDD IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BARABREDD}
  DISPLAY TOG_ALLSKMTRL RAD_MTRLSM CMB_LEVSM FILL-IN-KODSM FILL-IN-ENRSM 
          RAD_SOKSM FILL-IN-B2SM FILL-IN-B3SM FILL-IN-F2SM FILL-IN-F3SM 
          FILL-IN-SOKALTSM FILL-IN-ADRKABM FILL-IN-ENRSTOLPSM 
      WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
  ENABLE BRW_KABSM TOG_ALLSKMTRL RAD_MTRLSM BTN_OVERSM BTN_BACKSM BTN_LEVSM 
         CMB_LEVSM FILL-IN-KODSM FILL-IN-ENRSM FILL-IN-BENSM RAD_SOKSM BTN_NYSM 
         BTN_ANTALSM BTN_SATSSM BTN_TEXTSM BTN_NOLLSM BTN_ALLTMTRLSM BTN_SPECSM 
         BTN_LINKABSM FILL-IN-B2SM FILL-IN-B3SM FILL-IN-F2SM FILL-IN-F3SM 
         FILL-IN-SOKALTSM BTN_KOMPSKAP BTN_NASTKON FILL-IN-ENRSTOLPSM 
         FILL-IN-BENSTOLPSM BTN_KUNDLISTASM RECT-10 IMAGE-6 BRW_SATSSM 
         BRW_HLEVSM BRW_STOLPSM BRW_MTRLSM BRW_SOKMTRLFSM 
      WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-SKAPMTRL}
  DISPLAY RAD_ORD CMB_VALORD 
      WITH FRAME FRAME-ORDN IN WINDOW WINDOW-3.
  ENABLE RAD_ORD CMB_VALORD BRW_ORDID BRW_ORD BRW_ORDIDVAL BRW_FRAN BTN_OVERID 
         BTN_UPPORD BTN_MORDUPPID BTN_MORDUPP BTN_NERORD BTN_MORDNERID 
         BTN_MORDNER BTN_BACKID FILL-IN-IDNRORDID FILL-IN-IDNRORDID2 
         BTN_KOPPIDORD FILL-IN-ID1ORD FILL-IN-ID2ORD 
      WITH FRAME FRAME-ORDN IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ORDN}
  DISPLAY SEL_UPP CMB_SKRIV CMB_PRIS CMB_STOLP TOG_MTRL TOG_KONSTRUKTION TOG_VAL 
          TOG_UPP TOG_ID TOG_ANM TOG_JMFKONS TOG_P3 TOG_TEXT TOG_EGEN 
          TOG_SIDEXCEL TOG_KON TOG_EXCELKALK TOG_KONVAL TOG_EGENSIDA ED_MEDD 
      WITH FRAME FRAME-LISTOR IN WINDOW WINDOW-3.
  ENABLE SEL_UPP BTN_SKRIVLIST BTN_VISALIST BTN_EXLIST BTN_EXLISTENK BTN_IELIST 
         BTN_PDFLIST CMB_SKRIV CMB_PRIS CMB_STOLP TOG_MTRL TOG_KONSTRUKTION 
         TOG_VAL TOG_UPP TOG_ID TOG_ANM TOG_P3 TOG_TEXT TOG_SIDEXCEL TOG_KON 
         TOG_EXCELKALK TOG_KONVAL TOG_EGENSIDA ED_MEDD BTN_AVBLIST 
      WITH FRAME FRAME-LISTOR IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-LISTOR}
  DISPLAY SEL_UPPATG 
      WITH FRAME FRAME-ATG IN WINDOW WINDOW-3.
  ENABLE SEL_UPPATG BTN_KORATG BTN_AVBATG 
      WITH FRAME FRAME-ATG IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ATG}
  DISPLAY CMB_VALLIN CMB_VAL2LIN FILL-IN-LINKAB 
      WITH FRAME FRAME-LIN IN WINDOW WINDOW-3.
  ENABLE CMB_VALLIN CMB_VAL2LIN BRW_FRANLIN BRW_TILLLIN BTN_LINUPP FBTN_VISALIN 
         FBTN_SCHAKT FBTN_KORTLIN BTN_VISA-2LIN BRW_LIN BTN_SKAPALIN 
         BTN_BORTLIN BTN_AVBLIN 
      WITH FRAME FRAME-LIN IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-LIN}
  DISPLAY CMB_SKYLTART CMB_KORTVAL RAD_VALFRI FILL-IN-TYP FILL-IN-SADR 
          FILL-IN-KABEL FILL-IN-METER FILL-IN-ADR CMB_TYP FILL-IN-APPARAT 
          FILL-IN-AR FILL-IN-SAK FILL-IN-MAX FILL-IN-ANMARK FILL-IN-KORT 
          FILL-IN-B2 FILL-IN-B3 FILL-IN-F2 TOG_SORT FILL-IN-F3 FILL-IN-SKYLTBEN 
      WITH FRAME FRAME-FRI IN WINDOW WINDOW-3.
  ENABLE CMB_SKYLTART CMB_KORTVAL RAD_VALFRI BRW_APP BRW_MTRLFRI BRW_KABFRI 
         FBTN_VISAFRI FBTN_SKRIVFRI BTN_EXKSKO FBTN_PDFFRI BTN_EXSTOLP 
         BTN_EXMARK BTN_UPPFRI BTN_NERFRI FILL-IN-TYP FILL-IN-SADR 
         FILL-IN-KABEL FILL-IN-METER FILL-IN-ADR CMB_TYP FILL-IN-APPARAT 
         FILL-IN-GRUPPNR FILL-IN-ABONNENT FILL-IN-AR FILL-IN-SAK FILL-IN-MAX 
         FILL-IN-ANMARK FBTN_SKAPAFRI FBTN_ANDFRI FBTN_BORTFRI FBTN_NASTAKABEL 
         FILL-IN-B2 FILL-IN-B3 BTN_MEXC FILL-IN-F2 TOG_SORT FILL-IN-F3 
         FILL-IN-SKYLTANT FILL-IN-NSKYLTANT FILL-IN-NISKYLTANT 
      WITH FRAME FRAME-FRI IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-FRI}
  DISPLAY FILL-IN-ADRKAB FILL-IN-KORTKAB FILL-IN-KOPSKAP FILL-IN-B1KAB 
          FILL-IN-B3KAB FILL-IN-B2KAB FILL-IN-F3KAB FILL-IN-F1KAB FILL-IN-F2KAB 
          FILL-IN-MODUL FILL-IN-KOPKAB FILL-IN-NR1KAB FILL-IN-NR2KAB 
          FILL-IN-NR3KAB FILL-IN-T1 FILL-IN-T2 FILL-IN-T3 FILL-IN-LEDKAB 
          FILL-IN-EON 
      WITH FRAME FRAME-KABSKAP IN WINDOW WINDOW-3.
  ENABLE BRW_KABKAB FBTN_ANTKAB BTN_UPPKAB FBTN_KORTKAB FBTN_MTRLKAB BTN_NERKAB 
         BTN_BORTKAB BTN_TYP1KAB BTN_TYP2KAB BTN_TYP3KAB BRW_SKAPKAB 
         BTN_PLUSKAB BTN_MINKAB BTN_KSKAPKAB FILL-IN-B1KAB FILL-IN-B3KAB 
         FILL-IN-B2KAB FILL-IN-F3KAB FILL-IN-F1KAB FILL-IN-F2KAB FILL-IN-MODUL 
         FILL-IN-KOPKAB FILL-IN-NR1KAB FILL-IN-NR2KAB FILL-IN-NR3KAB 
         FILL-IN-LEDKAB 
      WITH FRAME FRAME-KABSKAP IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KABSKAP}
  ENABLE BRW_KOMBKOPUPP FBTN_UPPKOPUPP FBTN_VISAKOPUPP FBTN_OUPPKOPUPP 
         FBTN_SKRIVKOPUPP FBTN_KOMBKOPUPP FBTN_BKOMBKOPUPP FBTN_VISAKOPUPP-2 
         FBTN_SKRIVKOPUPP-2 BTN_FAVOUPP BRW_UPPKOPUPP BRW_FAVOUPPLAG 
         BTN_NYKOPUPP BTN_ANDRAKOPUPP BTN_BORTKOPUPP 
      WITH FRAME FRAME-KOPUPP IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KOPUPP}
  DISPLAY FILL-IN-PROCLINUPP FILL-IN-TOTMETERLINUPP FILL-IN-UPPMETERLINUPP 
          FILL-IN-LINKABLINUPP FILL-IN-ENRLINUPP FILL-IN-BENLINUPP 
          FILL-IN-METERLINUPP FILL-IN-UPPLINUPP FILL-IN-SUMMALINUPP 
      WITH FRAME FRAME-LINUPP IN WINDOW WINDOW-3.
  ENABLE BRW_LINLINUPP BTN_UPLINUPP BTN_VISALINUPP FILL-IN-PROCLINUPP 
         BTN_MINLINUPP BTN_SKRIVLINUPP FILL-IN-TOTMETERLINUPP BTN_NOLLLINUPP 
         BRW_UPPLINUPP FILL-IN-UPPMETERLINUPP BTN_NYLINUPP BTN_ANDRALINUPP 
         BTN_BORTLINUPP BTN_KOPPLINUPP BTN_AVBLINUPP FILL-IN-ENRLINUPP 
         FILL-IN-BENLINUPP FILL-IN-METERLINUPP FILL-IN-UPPLINUPP 
         FILL-IN-SUMMALINUPP 
      WITH FRAME FRAME-LINUPP IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-LINUPP}
  DISPLAY CMB_VALANDKONST 
      WITH FRAME FRAME-ANDKONST IN WINDOW WINDOW-3.
  ENABLE CMB_VALANDKONST BRW_ANDKONST BRW_KONANDKONST BRW_KALAKONS 
         BTN_BYTANGANDKONST BTN_STANGANDKONST 
      WITH FRAME FRAME-ANDKONST IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANDKONST}
  ENABLE BRW_ANDRAK BTN_STANG 
      WITH FRAME FRAME-ANDRAK IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANDRAK}
  ENABLE BRW_KALK BTN_NYKOD BTN_BORT2 BTN_GRUNDUPP 
      WITH FRAME FRAME-KALK IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KALK}
  DISPLAY CMB_VAL TOG_VISAKONST RAD_KOMP TOG_RATT TOG_MARKAV TOG_KOMP 
          TOG_VISAGRUPP FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 FILL-IN-B4 FILL-IN-B5 
          FILL-IN-B6 
      WITH FRAME FRAME-KONST IN WINDOW WINDOW-3.
  ENABLE CMB_VAL TOG_VISAKONST RAD_KOMP TOG_RATT BTN_NER BTN_UPP TOG_MARKAV 
         TOG_KOMP TOG_VISAGRUPP BRW_KON BRW_KB2 BRW_B2 BRW_KB3 BRW_B3 BRW_KB4 
         BRW_B4 BRW_KB5 BRW_B5 BRW_KB6 BRW_B6 FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 
         FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 
      WITH FRAME FRAME-KONST IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KONST}
  DISPLAY CMB_JURP CMB_AVD CMB_OMR CMB_DEL FILL-IN-SUND FILL-IN-KARTR TOG_A 
          TOG_B TOG_C FILL-IN-IDNR1LABEL FILL-IN-IDNR2LABEL FILL-IN-SUNDLABEL 
          FILL-IN-A FILL-IN-B FILL-IN-C TOG_AS 
      WITH FRAME FRAME-ID IN WINDOW WINDOW-3.
  ENABLE CMB_JURP CMB_AVD CMB_OMR BTN_HMT FILL-IN-NR1 BRW_TAB1 FILL-IN-NR2 
         BRW_TAB2 FILL-IN-NR3 BRW_TAB3 BTN_ANL CMB_DEL FILL-IN-IDNR1 
         FILL-IN-IDNR2 FILL-IN-SUND FILL-IN-KARTR BTN_KOPPID TOG_A TOG_B TOG_C 
         FILL-IN-TAB1 FILL-IN-ID1 FILL-IN-IDNR1LABEL FILL-IN-TAB2 FILL-IN-ID2 
         FILL-IN-IDNR2LABEL FILL-IN-SUNDLABEL FILL-IN-A FILL-IN-B FILL-IN-C 
         FILL-IN-TAB3 TOG_AS BTN_BORTKOPPID BTN_NASTKONID 
      WITH FRAME FRAME-ID IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ID}
  ENABLE BRW_BILD BTN_NYA BTN_BORTA BTN_VISA BTN_BILDCOPY 
      WITH FRAME FRAME-BILD IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BILD}
  DISPLAY TOG_ANMLOP 
      WITH FRAME FRAME-ANM IN WINDOW WINDOW-3.
  ENABLE BRW_ANM TOG_ANMLOP BTN_OVER-2 ED_ANM BTN_KOPPANM FILL-IN-TEXT 
      WITH FRAME FRAME-ANM IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANM}
  DISPLAY RAD_MTRL CMB_LEV FILL-IN-ENR FILL-IN-KOD RAD_SOK FILL-IN-SOKALT 
          FILL-IN-ENRSTOLP 
      WITH FRAME FRAME-MTRL IN WINDOW WINDOW-3.
  ENABLE BTN_KUNDLISTA RAD_MTRL BTN_OVER BTN_BACK BTN_LEV CMB_LEV FILL-IN-ENR 
         FILL-IN-KOD FILL-IN-BEN RAD_SOK BTN_NY BTN_ANTAL BTN_SATS BTN_TEXT 
         BTN_PAR BTN_NOLL BTN_ALLTMTRL BTN_SPEC BTN_LINKAB FILL-IN-SOKALT 
         FILL-IN-ENRSTOLP FILL-IN-BENSTOLP IMAGE-4 BRW_HLEV BRW_SATS BRW_STOLP 
         BRW_MTRL BRW_SOKMTRLF 
      WITH FRAME FRAME-MTRL IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MTRL}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixastorlek_UI WINDOW-3 
PROCEDURE fixastorlek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frameh_UI WINDOW-3 
PROCEDURE frameh_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS LOGICAL NO-UNDO.
   IF FRAME FRAME-ATG:HIDDEN = FALSE AND vilkenframe = TRUE THEN DO:
      
      RUN hmtlev_UI IN btnbervalh.
   END.    
   ASSIGN
   FRAME FRAME-ANDRAK:HIDDEN = vilkenframe
   FRAME FRAME-ORDN:HIDDEN = vilkenframe
   FRAME FRAME-FRI:HIDDEN = vilkenframe
   FRAME FRAME-KABSKAP:HIDDEN = vilkenframe
   FRAME FRAME-SKAPMTRL:HIDDEN = vilkenframe
   FRAME FRAME-LIN:HIDDEN = vilkenframe
   FRAME FRAME-LINUPP:HIDDEN = vilkenframe
   FRAME FRAME-KOPUPP:HIDDEN = vilkenframe   
   FRAME FRAME-ATG:HIDDEN = vilkenframe.   
   FRAME FRAME-LISTOR:HIDDEN = vilkenframe.
   IF brwNrandkonst = 28 THEN DO:
      FRAME FRAME-ANDRAK:HIDDEN = TRUE.
      FRAME FRAME-ANDKONST:HIDDEN = vilkenframe.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frame_UI WINDOW-3 
PROCEDURE frame_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS CHARACTER NO-UNDO. 
   /*
   FRAME FRAME-BARABREDD
   */
   RUN radval_UI IN btnbervalh (INPUT 99).
   ASSIGN
   FRAME FRAME-ANDRAK:HIDDEN = TRUE
   FRAME FRAME-ANDKONST:HIDDEN = TRUE
   FRAME FRAME-KONST:HIDDEN = TRUE 
   FRAME FRAME-ANM:HIDDEN = TRUE 
   FRAME FRAME-KALK:HIDDEN = TRUE 
   FRAME FRAME-BILD:HIDDEN = TRUE 
   FRAME FRAME-MTRL:HIDDEN = TRUE 
   FRAME FRAME-ID:HIDDEN = TRUE .
   RUN frameh_UI (INPUT TRUE).
   IF vilkenframe = "ORDN" THEN FRAME FRAME-ORDN:HIDDEN = FALSE.   
   IF vilkenframe = "FRI" THEN FRAME FRAME-FRI:HIDDEN = FALSE.   
   IF vilkenframe = "KSKAP" THEN FRAME FRAME-KABSKAP:HIDDEN = FALSE.   
   IF vilkenframe = "SKAPMAT" THEN FRAME FRAME-SKAPMTRL:HIDDEN = FALSE.   
   IF vilkenframe = "LIN" THEN FRAME FRAME-LIN:HIDDEN = FALSE.   
   IF vilkenframe = "LINUPP" THEN FRAME FRAME-LINUPP:HIDDEN = FALSE.
   IF vilkenframe = "KOPUPP" THEN FRAME FRAME-KOPUPP:HIDDEN = FALSE.      
   IF vilkenframe = "LIST" THEN FRAME FRAME-LISTOR:HIDDEN = FALSE.      
   IF vilkenframe = "ATG" THEN FRAME FRAME-ATG:HIDDEN = FALSE.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fritempskapanvej_UI WINDOW-3 
PROCEDURE fritempskapanvej_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE konlabel_UI WINDOW-3 
PROCEDURE konlabel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f1var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f2var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f3var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f4var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f5var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f6var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f7var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f8var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f9var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER idvar AS CHARACTER NO-UNDO. /* ny input istället för id (från bervalsw.p när man klickar i BRW_VAL) */ 
 
   IF vad = "VAL" THEN DO:
      IF AVAILABLE kon_display THEN DO:
         DISPLAY kon_display.EXTRA1 WITH BROWSE BRW_VAL NO-ERROR.
         kon_display.EXTRA1 = INPUT BROWSE BRW_VAL kon_display.EXTRA1.
         DISPLAY kon_display.EXTRA1 WITH BROWSE BRW_VAL NO-ERROR.
      END.
      ASSIGN
      kon_display.F1:LABEL IN BROWSE BRW_VAL = f1var
      kon_display.F2:LABEL IN BROWSE BRW_VAL = f2var  
      kon_display.F3:LABEL IN BROWSE BRW_VAL = f3var
      kon_display.F4:LABEL IN BROWSE BRW_VAL = f4var      
      kon_display.F5:LABEL IN BROWSE BRW_VAL = f5var      
      kon_display.F6:LABEL IN BROWSE BRW_VAL = f6var.
      ASSIGN
      andrakon_val.F1:LABEL IN BROWSE BRW_ANDRAK = f1var
      andrakon_val.F2:LABEL IN BROWSE BRW_ANDRAK = f2var  
      andrakon_val.F3:LABEL IN BROWSE BRW_ANDRAK = f3var
      andrakon_val.F4:LABEL IN BROWSE BRW_ANDRAK = f4var      
      andrakon_val.F5:LABEL IN BROWSE BRW_ANDRAK = f5var      
      andrakon_val.F6:LABEL IN BROWSE BRW_ANDRAK = f6var.
       ASSIGN
      andrakon_val.F1:LABEL IN BROWSE BRW_ANDKONST = f1var
      andrakon_val.F2:LABEL IN BROWSE BRW_ANDKONST = f2var  
      andrakon_val.F3:LABEL IN BROWSE BRW_ANDKONST = f3var
      andrakon_val.F4:LABEL IN BROWSE BRW_ANDKONST = f4var      
      andrakon_val.F5:LABEL IN BROWSE BRW_ANDKONST = f5var      
      andrakon_val.F6:LABEL IN BROWSE BRW_ANDKONST = f6var.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         kon_display.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "cGRAN" OR Guru.Konstanter:globforetag = "cGKAL" THEN DO:
         kon_display.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      END.
      ELSE DO:
         /*kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Id".*/ /* GAMMAL */
         kon_display.ID2:LABEL IN BROWSE BRW_VAL = idvar. /* istället för "id" i BRW_VAL */
      END. 
      FILL-IN-IDNR2LABEL:SCREEN-VALUE IN FRAME FRAME-ID = kon_display.ID2:LABEL.
   END.
   IF vad = "VALORD" THEN DO:
      ASSIGN
      kon_val.F1:LABEL IN BROWSE BRW_ORD = f1var
      kon_val.F2:LABEL IN BROWSE BRW_ORD = f2var  
      kon_val.F3:LABEL IN BROWSE BRW_ORD = f3var
      kon_val.F4:LABEL IN BROWSE BRW_ORD = f4var      
      kon_val.F5:LABEL IN BROWSE BRW_ORD = f5var      
      kon_val.F6:LABEL IN BROWSE BRW_ORD = f6var.
      ASSIGN
      kon_val.F1:LABEL IN BROWSE BRW_ORDID = f1var
      kon_val.F2:LABEL IN BROWSE BRW_ORDID = f2var  
      kon_val.F3:LABEL IN BROWSE BRW_ORDID = f3var
      kon_val.F4:LABEL IN BROWSE BRW_ORDID = f4var      
      kon_val.F5:LABEL IN BROWSE BRW_ORDID = f5var      
      kon_val.F6:LABEL IN BROWSE BRW_ORDID = f6var.
      ASSIGN
      idkon_val.F1:LABEL IN BROWSE BRW_ORDIDVAL = f1var
      idkon_val.F2:LABEL IN BROWSE BRW_ORDIDVAL = f2var  
      idkon_val.F3:LABEL IN BROWSE BRW_ORDIDVAL = f3var
      idkon_val.F4:LABEL IN BROWSE BRW_ORDIDVAL = f4var      
      idkon_val.F5:LABEL IN BROWSE BRW_ORDIDVAL = f5var      
      idkon_val.F6:LABEL IN BROWSE BRW_ORDIDVAL = f6var.

      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         ASSIGN
         kon_val.ID2:LABEL IN BROWSE BRW_ORD = "Objektnr"
         kon_val.ID2:LABEL IN BROWSE BRW_ORDID = "Objektnr"
         idkon_val.ID2:LABEL IN BROWSE BRW_ORDIDVAL = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:varforetypval[32] = 1 THEN DO:
         ASSIGN
         kon_val.ID2:LABEL IN BROWSE BRW_ORD = "Byggnr"
         kon_val.ID2:LABEL IN BROWSE BRW_ORDID = "Byggnr"
         idkon_val.ID2:LABEL IN BROWSE BRW_ORDIDVAL = "Byggnr".
      END.
      ELSE DO:
         kon_val.ID2:LABEL IN BROWSE BRW_ORD = idvar.
         kon_val.ID2:LABEL IN BROWSE BRW_ORDID = idvar.
         idkon_val.ID2:LABEL IN BROWSE BRW_ORDIDVAL = idvar.
      END.
      /*
      ELSE DO:
         ASSIGN
         kon_val.ID2:LABEL IN BROWSE BRW_ORD = "Id"
         kon_val.ID2:LABEL IN BROWSE BRW_ORDID = "Id"
         idkon_val.ID2:LABEL IN BROWSE BRW_ORDIDVAL = "Id".
      END.
      */ 
   END.
   IF vad = "ORD" THEN DO:
      ASSIGN
      kon_id.LINNR:LABEL IN BROWSE BRW_FRAN = f1var 
      kon_id.NATNR:LABEL IN BROWSE BRW_FRAN = f2var  
      kon_id.FRI1:LABEL IN BROWSE BRW_FRAN = f3var
      kon_id.FRI2:LABEL IN BROWSE BRW_FRAN = f4var.        
   END.                                      
   IF vad = "FRI" THEN DO:
      ASSIGN                                                  
      fri_temp.BENAMNING:LABEL IN BROWSE BRW_KABFRI = f1var  
      fri_temp.KABADR:LABEL IN BROWSE BRW_KABFRI = f2var  
      fri_temp.METER:LABEL IN BROWSE BRW_KABFRI = f3var.   
   END.
   IF vad = "KSKAP" THEN DO:
      ASSIGN       
      kon_val.F2:LABEL IN BROWSE BRW_KABKAB = f1var 
      kon_val.F3:LABEL IN BROWSE BRW_KABKAB = f2var
      kon_val.F4:LABEL IN BROWSE BRW_KABKAB = f3var     
      kon_val.F5:LABEL IN BROWSE BRW_KABKAB = f4var     
      kon_val.F6:LABEL IN BROWSE BRW_KABKAB = f5var
      kon_val.TYP:LABEL IN BROWSE BRW_KABKAB = f6var.
   END.
   IF vad = "KSKAP2" THEN DO:
      ASSIGN       
      kon_val.F1:LABEL IN BROWSE BRW_KABSM = f1var
      kon_val.F2:LABEL IN BROWSE BRW_KABSM = f2var  
      kon_val.F3:LABEL IN BROWSE BRW_KABSM = f3var
      kon_val.F4:LABEL IN BROWSE BRW_KABSM = f4var      
      kon_val.F5:LABEL IN BROWSE BRW_KABSM = f5var      
      kon_val.F6:LABEL IN BROWSE BRW_KABSM = f6var.
      
   END.
   IF vad = "MTRL" THEN DO:
      ASSIGN       
      list_mtrl.ENR:LABEL IN BROWSE BRW_MTRLSM = f1var
      mtrltemp.ENR:LABEL IN BROWSE BRW_HLEVSM = f1var.
      soktamtrlfavo.ENR:LABEL IN BROWSE BRW_SOKMTRLFSM = f1var.             
   END.
   IF vad = "LIN" THEN DO:
      ASSIGN
      kopp_lina.ENR:LABEL IN BROWSE BRW_LIN =  f1var 
      kopp_lina.LEDARE:LABEL IN BROWSE BRW_LIN = f2var.
   END.
   IF vad = "LIN2" THEN DO:
      ASSIGN
      kon_id.LINNR:LABEL IN BROWSE BRW_FRANLIN = f1var
      kon_id.NATNR:LABEL IN BROWSE BRW_FRANLIN = f2var
      kon_id.FRI1:LABEL IN BROWSE BRW_FRANLIN = f3var
      kon_id.FRI2:LABEL IN BROWSE BRW_FRANLIN = f4var.
   END.    
   IF vad = "LIN3" THEN DO:
      ASSIGN
      kon_id2.LINNR:LABEL IN BROWSE BRW_TILLLIN = f1var
      kon_id2.NATNR:LABEL IN BROWSE BRW_TILLLIN = f2var
      kon_id2.FRI1:LABEL IN BROWSE BRW_TILLLIN = f3var
      kon_id2.FRI2:LABEL IN BROWSE BRW_TILLLIN = f4var.
   END. 
   IF vad = "LINUPP" THEN DO:
      ASSIGN
      lin_tab2.ENR:LABEL IN BROWSE BRW_LINLINUPP = f1var.                  
   END.
   IF vad = "KOPUPP" THEN DO:
      ASSIGN
      kombtemp.F1:LABEL IN BROWSE BRW_KOMBKOPUPP    = f1var
      kombtemp.FORNR:LABEL IN BROWSE BRW_KOMBKOPUPP = f2var 
      kombtemp.LINNR:LABEL IN BROWSE BRW_KOMBKOPUPP = f3var
      kombtemp.NATNR:LABEL IN BROWSE BRW_KOMBKOPUPP = f4var     
      kombtemp.FRI1:LABEL IN BROWSE BRW_KOMBKOPUPP  = f5var   
      kombtemp.FRI2:LABEL IN BROWSE BRW_KOMBKOPUPP  = f6var
      kombtemp.ID:LABEL IN BROWSE BRW_KOMBKOPUPP    = f7var
      kombtemp.UPPLAG:LABEL IN BROWSE BRW_KOMBKOPUPP = f8var
      kombtemp.KOMB:LABEL IN BROWSE BRW_KOMBKOPUPP = f9var.
      berupptemp.ORT:LABEL IN BROWSE BRW_UPPKOPUPP = "Ort".
      IF Guru.Konstanter:varforetypval[32] = 1 THEN DO:
         kombtemp.FORNR:VISIBLE IN BROWSE BRW_KOMBKOPUPP = FALSE.
         kombtemp.LINNR:VISIBLE IN BROWSE BRW_KOMBKOPUPP = FALSE.
         kombtemp.NATNR:VISIBLE IN BROWSE BRW_KOMBKOPUPP = FALSE.
      END.
      ELSE DO:
         kombtemp.FORNR:VISIBLE IN BROWSE BRW_KOMBKOPUPP = TRUE.
         kombtemp.LINNR:VISIBLE IN BROWSE BRW_KOMBKOPUPP = TRUE.
         kombtemp.NATNR:VISIBLE IN BROWSE BRW_KOMBKOPUPP = TRUE.
         
      END.      

   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Movetotop_UI WINDOW-3 
PROCEDURE Movetotop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MBTN_ATERGA:HIDDEN IN FRAME FRAME-BARABREDD = FALSE. 
   RUN PlaceraKnapp_UI IN btnbervalh.
   MBTN_ATERGA:TOOLTIP = "Återgå".

   {WIN_M_SLUT.I}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nolabel_UI WINDOW-3 
PROCEDURE nolabel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   {nolabedispl.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdbrwkon_UI WINDOW-3 
PROCEDURE rowdbrwkon_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   IF AVAILABLE konsttemp  THEN DO:  
      IF konsttemp.BENAMNING BEGINS "-------" THEN DO:      
         konsttemp.BENAMNING:FGCOLOR IN BROWSE BRW_KON = 12. 
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdbrwunder_UI WINDOW-3 
PROCEDURE rowdbrwunder_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 2 THEN DO:   
      IF AVAILABLE konstvaltemp  THEN DO:  
         IF konstvaltemp.BENAMNING BEGINS "-----" THEN DO:      
            konstvaltemp.BENAMNING:FGCOLOR IN BROWSE BRW_B2 = 12. 
         END.
      END.   
   END.
   IF vad = 3 THEN DO:   
      IF AVAILABLE konstvaltemp  THEN DO:  
         IF konstvaltemp.BENAMNING BEGINS "-----" THEN DO:      
            konstvaltemp.BENAMNING:FGCOLOR IN BROWSE BRW_B3 = 12. 
         END.
      END.   
   END.
   IF vad = 12 THEN DO:   
      IF AVAILABLE konstvaltemp  THEN DO:  
         IF konstvaltemp.BENAMNING BEGINS "-----" THEN DO:      
            konstvaltemp.BENAMNING:FGCOLOR IN BROWSE BRW_KB2 = 12. 
         END.
      END.   
   END.
   IF vad = 13 THEN DO:   
      IF AVAILABLE konstvaltemp  THEN DO:  
         IF konstvaltemp.BENAMNING BEGINS "-----" THEN DO:      
            konstvaltemp.BENAMNING:FGCOLOR IN BROWSE BRW_KB3 = 12. 
         END.
      END.   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI WINDOW-3 
PROCEDURE rowdispmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE VARIABLE repeatvar AS INTEGER NO-UNDO.
   /*
   IF brwh:NAME = "BRW_HLEV" THEN RUN rowdispmtrl_UI IN huvprogh (INPUT 1). 
   IF brwh:NAME = "BRW_BER" THEN RUN rowdispmtrl_UI (INPUT 2).      
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI IN huvprogh (INPUT 3). 
   IF brwh:NAME = "BRW_MTRLSM" THEN RUN rowdispmtrl_UI IN huvprogh (INPUT 4).  
   IF brwh:NAME = "BRW_STOLP" THEN RUN rowdispmtrl_UI IN huvprogh (INPUT 5). 
   IF brwh:NAME = "BRW_STOLPSM" THEN RUN rowdispmtrl_UI IN huvprogh (INPUT 6). 
   */
  
   IF vad = 1 THEN DO:
     /*HLEV*/
      IF AVAILABLE mtrltemp THEN DO:     
             
         IF mtrltemp.KUND = TRUE THEN DO: 
            /*nettopris gul fortum turkos övriga*/
            ASSIGN
            
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28].            
         END.
         ELSE IF mtrltemp.KUND = ? THEN DO: 
            /*HAR TVÅ FÄRGER PÅ PRISER*/
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38].            
         END.
      END.
   END.
   IF vad = 2 THEN DO:
      /*BYTXTILLY*/
      IF AVAILABLE mtrltemp THEN DO:
         IF mtrltemp.KUND = TRUE THEN DO: 
            /*nettopris gul fortum turkos övriga*/
            ASSIGN
            soktamtrlfavo.ENR:BGCOLOR IN BROWSE BRW_SOKMTRLFSM = Guru.Konstanter:varforetypval[28]
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEVSM = Guru.Konstanter:varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEVSM = Guru.Konstanter:varforetypval[28].
         END.
         ELSE IF mtrltemp.KUND = ? THEN DO: 
            /*nettopris gul fortum turkos övriga*/
            ASSIGN
            soktamtrlfavo.ENR:BGCOLOR IN BROWSE BRW_SOKMTRLFSM = Guru.Konstanter:varforetypval[38]
            soktamtrlfavo.NPRIS:BGCOLOR IN BROWSE BRW_SOKMTRLFSM = Guru.Konstanter:varforetypval[38]
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEVSM = Guru.Konstanter:varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEVSM = Guru.Konstanter:varforetypval[38].
         END.
      END.
   END.
   IF vad = 3 THEN DO:  
      /*VALD MTRL*/
      IF AVAILABLE list_mtrl THEN DO:
         FIND FIRST berstolptemp WHERE /*berstolptemp.LEVKOD = list_mtrl.LEVKOD AND*/ berstolptemp.ENR = list_mtrl.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE berstolptemp THEN DO:                        
            /*STOLPAR turkosa fortum  gula övriga*/
            list_mtrl.BENAMNING:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[31].
            list_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[31].            
         END.
         IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:   
            REPEAT:       
               /*nettopris beredning inköp*/  
               FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl.LEVKOD AND kmtrltemp.ENR = list_mtrl.ENR AND kmtrltemp.KALKNR = 0
               AND kmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
               IF AVAILABLE kmtrltemp THEN DO:      
                  /*STOLPAR turkosa fortum  gula övriga*/
                   IF AVAILABLE berstolptemp THEN berstolptemp.KUND = kmtrltemp.KUND.
                   list_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].
                   list_mtrl.PRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].               
               END.
               ELSE DO:
                  FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl.LEVKOD AND kmtrltemp.ENR = list_mtrl.ENR AND kmtrltemp.KALKNR = 0
                  AND kmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
                  IF AVAILABLE kmtrltemp THEN DO:      
                     /*STOLPAR turkosa fortum  gula övriga*/
                      IF AVAILABLE berstolptemp THEN berstolptemp.KUND = kmtrltemp.KUND.
                      list_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38].
                      list_mtrl.PRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38].               
                  END.
               END.
               IF AVAILABLE kmtrltemp THEN LEAVE.
               IF  repeatvar < 1 THEN repeatvar = 1.
               ELSE LEAVE.
               RUN enrhmt_UI IN nettoh (INPUT list_mtrl.LEVKOD,INPUT list_mtrl.ENR, OUTPUT TABLE kmtrltemp APPEND).
            END. 
         END.     
         IF Guru.Konstanter:varforetypval[37] NE 0 THEN DO:
            /*KOMMENTARER*/
            FIND FIRST kmtrltemp WHERE /*kmtrltemp.LEVKOD = list_mtrl.LEVKOD AND*/  kmtrltemp.ENR = list_mtrl.ENR AND kmtrltemp.KALKNR = 0
            AND kmtrltemp.OFFERT = TRUE  NO-LOCK NO-ERROR.
            IF AVAILABLE kmtrltemp THEN DO:                  
               list_mtrl.BENAMNING:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[37].
               list_mtrl.ENHET:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[37].
               IF AVAILABLE berstolptemp THEN DO:                        
                  /*STOLPAR turkosa fortum  gula övriga*/
                  list_mtrl.BENAMNING:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[31].
               END.         
            END.
         END.
      END.
      
   END.   
  
   IF vad = 4 THEN DO: 
      /*VALD SKÅP MTRL*/
      IF AVAILABLE list_mtrl THEN DO:         
         FIND FIRST berstolptemp WHERE /*berstolptemp.LEVKOD = list_mtrl.LEVKOD AND*/ berstolptemp.ENR = list_mtrl.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE berstolptemp THEN DO:        
            /*STOLPAR turkosa fortum  gula övriga*/
             list_mtrl.BENAMNING:BGCOLOR IN BROWSE BRW_MTRLSM = Guru.Konstanter:varforetypval[31].
             list_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRLSM = Guru.Konstanter:varforetypval[31].            
         END.         
         IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
            /*nettopris beredning inköp*/     
            REPEAT:    
               FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl.LEVKOD AND kmtrltemp.ENR = list_mtrl.ENR AND kmtrltemp.KALKNR = 0
               AND kmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
               IF AVAILABLE kmtrltemp THEN DO:                            
                  /*STOLPAR turkosa fortum  gula övriga*/
                  IF AVAILABLE berstolptemp THEN berstolptemp.KUND = kmtrltemp.KUND.
                  list_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRLSM = Guru.Konstanter:varforetypval[28].
                  list_mtrl.PRIS:BGCOLOR IN BROWSE BRW_MTRLSM = Guru.Konstanter:varforetypval[28].               
               END.
               ELSE DO:
                  FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl.LEVKOD AND kmtrltemp.ENR = list_mtrl.ENR AND kmtrltemp.KALKNR = 0
                  AND kmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
                  IF AVAILABLE kmtrltemp THEN DO:                            
                     /*STOLPAR turkosa fortum  gula övriga*/
                     IF AVAILABLE berstolptemp THEN berstolptemp.KUND = kmtrltemp.KUND.
                     list_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRLSM = Guru.Konstanter:varforetypval[38].
                     list_mtrl.PRIS:BGCOLOR IN BROWSE BRW_MTRLSM = Guru.Konstanter:varforetypval[38].               
                  END.
               END.
               IF AVAILABLE kmtrltemp THEN LEAVE.
               IF  repeatvar < 1 THEN repeatvar = 1.
               ELSE LEAVE.
               RUN enrhmt_UI IN nettoh (INPUT list_mtrl.LEVKOD,INPUT list_mtrl.ENR, OUTPUT TABLE kmtrltemp APPEND).
            END.   
            IF Guru.Konstanter:varforetypval[37] NE 0 THEN DO:
               FIND FIRST kmtrltemp WHERE /*kmtrltemp.LEVKOD = list_mtrl.LEVKOD AND*/  kmtrltemp.ENR = list_mtrl.ENR AND kmtrltemp.KALKNR = 0
               AND kmtrltemp.OFFERT = TRUE  NO-LOCK NO-ERROR.
               IF AVAILABLE kmtrltemp THEN DO:                  
                  list_mtrl.BENAMNING:BGCOLOR IN BROWSE BRW_MTRLSM = Guru.Konstanter:varforetypval[37].
                  list_mtrl.ENHET:BGCOLOR IN BROWSE BRW_MTRLSM = Guru.Konstanter:varforetypval[37].
                  IF AVAILABLE berstolptemp THEN DO:                        
                     /*STOLPAR turkosa fortum  gula övriga*/
                     list_mtrl.BENAMNING:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[31].
                  END.         
               END.
            END.
         END.
      END.
      
   END.   
   IF vad = 5 THEN DO: 
      /*STLPAR/TRAFO*/   
            
      IF AVAILABLE berstolptemp THEN DO:                           
         IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
            /*nettopris beredning STOLPAR*/ 
                         
            FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = berstolptemp.LEVKOD AND kmtrltemp.ENR = berstolptemp.ENR AND kmtrltemp.KALKNR = 0
            AND kmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
            IF AVAILABLE kmtrltemp THEN DO:                                           
               IF AVAILABLE berstolptemp THEN berstolptemp.KUND = kmtrltemp.KUND.
               ASSIGN
               berstolptemp.ENR:BGCOLOR IN BROWSE BRW_STOLP = Guru.Konstanter:varforetypval[28]
               berstolptemp.PRIS:BGCOLOR IN BROWSE BRW_STOLP = Guru.Konstanter:varforetypval[28].                                         
            END.
            ELSE DO:
               FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = berstolptemp.LEVKOD AND kmtrltemp.ENR = berstolptemp.ENR AND kmtrltemp.KALKNR = 0
               AND kmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
               IF AVAILABLE kmtrltemp THEN DO:                                   
                  IF AVAILABLE berstolptemp THEN berstolptemp.KUND = kmtrltemp.KUND.        
                  ASSIGN
                  berstolptemp.ENR:BGCOLOR IN BROWSE BRW_STOLP = Guru.Konstanter:varforetypval[38]
                  berstolptemp.PRIS:BGCOLOR IN BROWSE BRW_STOLP = Guru.Konstanter:varforetypval[38].                                         
               END.
            END.
         END.
      END.
   END.   
   IF vad = 6 THEN DO:   
      /*STOLPAR/TARFO I SKÅP*/          
      IF AVAILABLE berstolptemp THEN DO:                           
         IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
            /*nettopris beredning STOLPAR*/                     
            FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = berstolptemp.LEVKOD AND kmtrltemp.ENR = berstolptemp.ENR AND kmtrltemp.KALKNR = 0
            AND kmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
            IF AVAILABLE kmtrltemp THEN DO:
               IF AVAILABLE berstolptemp THEN berstolptemp.KUND = kmtrltemp.KUND.                                    
               ASSIGN
               berstolptemp.ENR:BGCOLOR IN BROWSE BRW_STOLPSM = Guru.Konstanter:varforetypval[28]
               berstolptemp.PRIS:BGCOLOR IN BROWSE BRW_STOLPSM = Guru.Konstanter:varforetypval[28].                            
            END.
            ELSE DO:
               FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = berstolptemp.LEVKOD AND kmtrltemp.ENR = berstolptemp.ENR AND kmtrltemp.KALKNR = 0
               AND kmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
               IF AVAILABLE kmtrltemp THEN DO:                                   
                  IF AVAILABLE berstolptemp THEN berstolptemp.KUND = kmtrltemp.KUND. 
                  ASSIGN
                  berstolptemp.ENR:BGCOLOR IN BROWSE BRW_STOLPSM = Guru.Konstanter:varforetypval[38]
                  berstolptemp.PRIS:BGCOLOR IN BROWSE BRW_STOLPSM = Guru.Konstanter:varforetypval[38].                            
               END.
            END.
         END.
      END.
   END.   
   IF vad = 7 THEN DO:
     /*UTSÖKTA*/
      IF AVAILABLE soktamtrlfavo THEN DO:     
             
         IF soktamtrlfavo.KUND = TRUE THEN DO: 
            /*nettopris gul fortum turkos övriga*/
            ASSIGN
            soktamtrlfavo.ENR:BGCOLOR IN BROWSE BRW_SOKMTRLF = Guru.Konstanter:varforetypval[28]
            soktamtrlfavo.NPRIS:BGCOLOR IN BROWSE BRW_SOKMTRLF = Guru.Konstanter:varforetypval[28].
            ASSIGN
            soktamtrlfavo.ENR:BGCOLOR IN BROWSE BRW_SOKMTRLFSM = Guru.Konstanter:varforetypval[28]
            soktamtrlfavo.NPRIS:BGCOLOR IN BROWSE BRW_SOKMTRLFSM = Guru.Konstanter:varforetypval[28].
                        
         END.
         ELSE IF soktamtrlfavo.KUND = ? THEN DO: 
            /*HAR TVÅ FÄRGER PÅ PRISER*/
            ASSIGN
            soktamtrlfavo.ENR:BGCOLOR IN BROWSE BRW_SOKMTRLF = Guru.Konstanter:varforetypval[38]
            soktamtrlfavo.NPRIS:BGCOLOR IN BROWSE BRW_SOKMTRLF = Guru.Konstanter:varforetypval[38].
            ASSIGN
            soktamtrlfavo.ENR:BGCOLOR IN BROWSE BRW_SOKMTRLFSM = Guru.Konstanter:varforetypval[38]
            soktamtrlfavo.NPRIS:BGCOLOR IN BROWSE BRW_SOKMTRLFSM = Guru.Konstanter:varforetypval[38].
                        
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowlbrwmtrl_UI WINDOW-3 
PROCEDURE rowlbrwmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS CHARACTER NO-UNDO.
   IF vad = "MTRL" THEN DO:
      IF entrymtrlantal = TRUE THEN DO:
         IF AVAILABLE list_mtrl THEN DO:
            DISPLAY list_mtrl.ANTAL WITH BROWSE BRW_MTRL NO-ERROR.
            list_mtrl.ANTAL = INPUT BROWSE BRW_MTRL list_mtrl.ANTAL.
            DISPLAY list_mtrl.ANTAL WITH BROWSE BRW_MTRL NO-ERROR.
         END.
      END.
   END.
   IF vad = "SMTRL" THEN DO:
      IF entrymtrlantalskap THEN DO:
         IF AVAILABLE list_mtrl THEN DO:
            DISPLAY list_mtrl.ANTAL WITH BROWSE BRW_MTRLSM NO-ERROR.
            list_mtrl.ANTAL = INPUT BROWSE BRW_MTRLSM list_mtrl.ANTAL.
            DISPLAY list_mtrl.ANTAL WITH BROWSE BRW_MTRLSM NO-ERROR.
         END.      
      END.
   END.
   IF vad = "SMTRL2" THEN DO:
      IF FRAME FRAME-SKAPMTRL:HIDDEN = FALSE THEN DO:
         IF VALID-HANDLE(btnkskapmtrlh) THEN DO:
            IF entrymtrlantalskap THEN DO:
               RUN leavefill_UI IN btnkskapmtrlh.
               DISPLAY list_mtrl.ANTAL WITH BROWSE BRW_MTRLSM NO-ERROR.
            END.
         END.
      END.      
   END.
   IF vad = "LINOR" THEN DO:
      IF FRAME FRAME-LIN:HIDDEN = FALSE THEN DO:
         IF VALID-HANDLE(btnklinh) THEN DO:
            RUN leavefill_UI IN btnklinh.
         END.
      END.      
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skaphide_UI WINDOW-3 
PROCEDURE skaphide_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER gomaw AS LOGICAL NO-UNDO.
   
   
   
   
   
   
   
   IF gomaw = FALSE THEN DO:
      IF BTN_LEVSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         ENABLE BTN_LEVSM WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
      END. 
      IF CMB_LEVSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         ENABLE CMB_LEVSM WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
      END. 
      IF FILL-IN-KODSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         ENABLE FILL-IN-KODSM WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
      END. 
      IF FILL-IN-ENRSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         ENABLE FILL-IN-ENRSM WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
      END.  
      IF BRW_STOLPSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         ENABLE BRW_STOLPSM WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
      END. 
      IF BRW_SATSSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         ENABLE BRW_SATSSM WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
      END. 
      IF BRW_HLEVSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         ENABLE BRW_HLEVSM WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
      END.  
      IF BRW_SOKMTRLFSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         ENABLE BRW_SOKMTRLFSM WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
      END.            
      ENABLE BRW_KABSM RAD_MTRLSM BTN_OVERSM BTN_BACKSM FILL-IN-BENSM RAD_SOKSM BTN_NYSM 
         BTN_ANTALSM BTN_SATSSM BTN_TEXTSM BTN_NOLLSM BTN_ALLTMTRLSM BTN_SPECSM 
         BTN_LINKABSM FILL-IN-B2SM FILL-IN-B3SM FILL-IN-F2SM FILL-IN-F3SM 
         FILL-IN-SOKALTSM RECT-10 IMAGE-6  
         BRW_MTRLSM 
      WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
   END.
   
   IF gomaw = TRUE THEN DO:
      IF BTN_LEVSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         DISABLE BTN_LEVSM WITH FRAME FRAME-SKAPMTRL.
      END. 
      IF CMB_LEVSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         DISABLE CMB_LEVSM WITH FRAME FRAME-SKAPMTRL.
      END. 
      IF FILL-IN-KODSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         DISABLE FILL-IN-KODSM WITH FRAME FRAME-SKAPMTRL.
      END. 
      IF FILL-IN-ENRSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         DISABLE FILL-IN-ENRSM WITH FRAME FRAME-SKAPMTRL.
      END.  
      IF BRW_STOLPSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         DISABLE BRW_STOLPSM WITH FRAME FRAME-SKAPMTRL.
      END. 
      IF BRW_SATSSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         DISABLE BRW_SATSSM WITH FRAME FRAME-SKAPMTRL.
      END. 
      IF BRW_HLEVSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         DISABLE BRW_HLEVSM WITH FRAME FRAME-SKAPMTRL.
      END.  
      IF BRW_SOKMTRLFSM:VISIBLE IN FRAME FRAME-SKAPMTRL = TRUE THEN DO:
         DISABLE BRW_SOKMTRLFSM WITH FRAME FRAME-SKAPMTRL.
      END.  
      DISABLE BRW_KABSM RAD_MTRLSM BTN_OVERSM BTN_BACKSM  FILL-IN-BENSM RAD_SOKSM BTN_NYSM 
         BTN_ANTALSM BTN_SATSSM BTN_TEXTSM BTN_NOLLSM BTN_ALLTMTRLSM BTN_SPECSM 
         BTN_LINKABSM FILL-IN-B2SM FILL-IN-B3SM FILL-IN-F2SM FILL-IN-F3SM 
         FILL-IN-SOKALTSM RECT-10 IMAGE-6  
         BRW_MTRLSM 
      WITH FRAME FRAME-SKAPMTRL.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttcopyhmt_UI WINDOW-3 
PROCEDURE ttcopyhmt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE tthandle.
   DEFINE VARIABLE komfav AS CHARACTER NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttcopqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttfavqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.
   ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.  
   tthandle:EMPTY-TEMP-TABLE ().
   komcop = "FOR EACH " + ttbuffcopyh:TABLE + " NO-LOCK.".
   CREATE QUERY ttcopqh.
   ttcopqh:SET-BUFFERS(ttbuffcopyh).
   ttcopqh:QUERY-PREPARE(komcop).
   ttcopqh:QUERY-OPEN().
   
   ttcopqh:GET-FIRST(NO-LOCK).
   DO WHILE ttcopqh:QUERY-OFF-END = FALSE:
      ttbuffh:BUFFER-CREATE().
      ttbuffh:BUFFER-COPY(ttbuffcopyh).
     
      ttcopqh:GET-NEXT(NO-LOCK).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttcopy_UI WINDOW-3 
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
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
   ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.
   ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttjmfPair_UI WINDOW-3 
PROCEDURE ttjmfPair_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER traff AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER pair AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER SparaBuff AS LOGICAL NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER favbuffh AS HANDLE.
   DEFINE VARIABLE compsave AS LOGICAL NO-UNDO.
   DEFINE VARIABLE komfav AS CHARACTER NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttcopqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttfavqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
   komcop = "FOR EACH " + ttbuffcopyh:TABLE + " NO-LOCK.".
   komfav = "FOR EACH " + favbuffh:TABLE + " NO-LOCK.".
   
   
   CREATE QUERY ttfavqh.
   ttfavqh:SET-BUFFERS(favbuffh).
   ttfavqh:QUERY-PREPARE(komfav).
   ttfavqh:QUERY-OPEN().
  
   ttfavqh:GET-FIRST(NO-LOCK).
   compsave = FALSE.
  
   CREATE QUERY ttcopqh.
   ttcopqh:SET-BUFFERS(ttbuffcopyh).
   ttcopqh:QUERY-PREPARE(komcop).
   ttcopqh:QUERY-OPEN().
   ttcopqh:GET-FIRST(NO-LOCK).
   DO WHILE ttcopqh:QUERY-OFF-END = FALSE:
      ttfavqh:GET-FIRST(NO-LOCK).
      compsave = FALSE.
      DO WHILE ttfavqh:QUERY-OFF-END = FALSE:
         IF traff = TRUE THEN DO:
            /*bara dessa fält*/
             /*Anders Olsson Elpool i Umeå AB  29 sep 2020 12:33:38 
             EN IDIOTI MEN JAG FATTAR INTE VARFÖR..... 
             */
            IF ttbuffcopyh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE = favbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE AND 
            ttbuffcopyh:BUFFER-FIELD("ENR"):BUFFER-VALUE  =   favbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE THEN DO: 
               favbuffh:BUFFER-COPY(ttbuffcopyh).
               
            END.          
            IF ttbuffcopyh:BUFFER-COMPARE(favbuffh,"","",pair) THEN DO:   
               compsave = TRUE.       
               ttfavqh:GET-LAST(NO-LOCK).
               ttfavqh:GET-NEXT(NO-LOCK).
            END.
         END.
         ELSE DO:
            /*ej dessa fält*/
             IF favbuffh:BUFFER-COMPARE(ttbuffcopyh,"",pair) THEN DO:
               compsave = TRUE.       
               ttfavqh:GET-LAST(NO-LOCK).
               ttfavqh:GET-NEXT(NO-LOCK).
            END.
         END.           
         ttfavqh:GET-NEXT(NO-LOCK).
      END.  
        
      IF compsave = TRUE THEN DO:
         IF SparaBuff = FALSE THEN  ttbuffcopyh:BUFFER-DELETE().  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttjmf_UI WINDOW-3 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI WINDOW-3 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordnr = ordnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

