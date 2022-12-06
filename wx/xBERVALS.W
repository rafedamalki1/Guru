&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-3

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE apparattemp NO-UNDO LIKE apparattemp.
DEFINE TEMP-TABLE berfortemp NO-UNDO LIKE berfortemp.
DEFINE TEMP-TABLE berlinjetemp NO-UNDO LIKE berlinjetemp.
DEFINE TEMP-TABLE bernattemp NO-UNDO LIKE bernattemp.
DEFINE TEMP-TABLE berskaptemp NO-UNDO LIKE berskaptemp.
DEFINE TEMP-TABLE berstolptemp NO-UNDO LIKE berstolptemp.
DEFINE TEMP-TABLE berupptemp NO-UNDO LIKE berupptemp.
DEFINE TEMP-TABLE bildbertemp NO-UNDO LIKE bildbertemp.
DEFINE TEMP-TABLE fastanmtemp NO-UNDO LIKE fastanmtemp.
DEFINE TEMP-TABLE fri_temp NO-UNDO LIKE fri_temp.
DEFINE TEMP-TABLE kombtemp NO-UNDO LIKE kombtemp.
DEFINE TEMP-TABLE konstgrptemp NO-UNDO LIKE konstgrptemp.
DEFINE TEMP-TABLE konsttemp NO-UNDO LIKE konsttemp.
DEFINE TEMP-TABLE konstvaltemp NO-UNDO LIKE konstvaltemp.
DEFINE TEMP-TABLE kon_id NO-UNDO LIKE kon_id.
DEFINE TEMP-TABLE kon_id2 NO-UNDO LIKE kon_id2.
DEFINE TEMP-TABLE kopp_lina NO-UNDO LIKE kopp_lina.
DEFINE TEMP-TABLE lin_tab2 NO-UNDO LIKE lin_tab2.
DEFINE TEMP-TABLE mtrltemp NO-UNDO LIKE mtrltemp.
DEFINE TEMP-TABLE mtrl_tab NO-UNDO LIKE mtrl_tab.
DEFINE TEMP-TABLE satstemp NO-UNDO LIKE satstemp.
DEFINE TEMP-TABLE kalk_temp NO-UNDO LIKE kalk_temp.
DEFINE TEMP-TABLE list_mtrl NO-UNDO LIKE list_mtrl.
DEFINE TEMP-TABLE kon_val NO-UNDO LIKE kon_val.


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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{WHANDLTEMP.I}
   FIELD WF AS HANDLE EXTENT 200.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{APPARAT.I}
DEFINE NEW SHARED TEMP-TABLE mtrl_tab NO-UNDO   
   FIELD BENAMNING AS CHARACTER LABEL "Benämning".    
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{DEFSOK.I}
{pdf_StartInc.i}
/* {SPECMTRL.I} */
&Scoped-define SHARED SHARED
{BERANN.I}
{MTRLTEMP.I}
{KONVALTEMP.I}
{KONID.I}   
{LISTMTRL.I}   
{KOPPLINA.I}    
{FRITEMP.I}        
{PUNKTTEM.I}  
{SCHAKTTE.I}    
{SKYDDTEM.I}     
{KABTEMP.I}
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
{BERSCHAKTTEMP.I}
{BERKALKTEMP.I}
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
DEFINE NEW SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE xtillyvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE val_row AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE repovar AS LOGICAL NO-UNDO.

/*CONNECTIT VARIABLER*/
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.


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
DEFINE VARIABLE bervalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE rowkonval AS ROWID NO-UNDO.
DEFINE VARIABLE bbnamningvar AS CHARACTER EXTENT 10 NO-UNDO.
/*ID*/

/*MTRL*/
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
/*skapMTRL*/

/*TABELL FÖR KALKYLERING ENLIGT P3*/
DEFINE SHARED VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE kalkvar AS LOGICAL NO-UNDO.
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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-ANM
&Scoped-define BROWSE-NAME BRW_ANM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fastanmtemp apparattemp konstvaltemp ~
bildbertemp kon_id mtrltemp fri_temp kon_val kalk_temp kombtemp kopp_lina ~
lin_tab2 list_mtrl mtrl_tab satstemp berskaptemp berstolptemp berfortemp ~
berlinjetemp bernattemp kon_id2 berupptemp

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
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B2 
&Scoped-define QUERY-STRING-BRW_B2 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B2 OPEN QUERY BRW_B2 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B2 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B2 konstvaltemp


/* Definitions for BROWSE BRW_B3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B3 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B3 
&Scoped-define QUERY-STRING-BRW_B3 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B3 OPEN QUERY BRW_B3 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B3 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B3 konstvaltemp


/* Definitions for BROWSE BRW_B4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B4 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B4 
&Scoped-define QUERY-STRING-BRW_B4 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B4 OPEN QUERY BRW_B4 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B4 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B4 konstvaltemp


/* Definitions for BROWSE BRW_B5                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B5 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B5 
&Scoped-define QUERY-STRING-BRW_B5 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B5 OPEN QUERY BRW_B5 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B5 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B5 konstvaltemp


/* Definitions for BROWSE BRW_B6                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B6 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
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
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_HLEVSM                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEVSM mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEVSM 
&Scoped-define QUERY-STRING-BRW_HLEVSM FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEVSM OPEN QUERY BRW_HLEVSM FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEVSM mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEVSM mtrltemp


/* Definitions for BROWSE BRW_KABFRI                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_KABFRI fri_temp.BENAMNING ~
fri_temp.KABADR fri_temp.METER fri_temp.APPARAT fri_temp.SKAPADR ~
fri_temp.TYP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KABFRI 
&Scoped-define QUERY-STRING-BRW_KABFRI FOR EACH fri_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KABFRI OPEN QUERY BRW_KABFRI FOR EACH fri_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KABFRI fri_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KABFRI fri_temp


/* Definitions for BROWSE BRW_KABKAB                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_KABKAB kon_val.F2 kon_val.F3 kon_val.F4 ~
kon_val.F5 kon_val.F6 kon_val.TYP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KABKAB 
&Scoped-define QUERY-STRING-BRW_KABKAB FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KABKAB OPEN QUERY BRW_KABKAB FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KABKAB kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KABKAB kon_val


/* Definitions for BROWSE BRW_KABSM                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KABSM kon_val.F1 kon_val.F2 kon_val.F3 ~
kon_val.F4 kon_val.F5 kon_val.F6 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KABSM 
&Scoped-define QUERY-STRING-BRW_KABSM FOR EACH kon_val NO-LOCK ~
    BY kon_val.F3 ~
       BY kon_val.F4 ~
        BY kon_val.F5 ~
         BY kon_val.F6
&Scoped-define OPEN-QUERY-BRW_KABSM OPEN QUERY BRW_KABSM FOR EACH kon_val NO-LOCK ~
    BY kon_val.F3 ~
       BY kon_val.F4 ~
        BY kon_val.F5 ~
         BY kon_val.F6.
&Scoped-define TABLES-IN-QUERY-BRW_KABSM kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KABSM kon_val


/* Definitions for BROWSE BRW_KALK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KALK kalk_temp.ARBKOD kalk_temp.LOPNR ~
kalk_temp.BENAMNING kalk_temp.ANTAL kalk_temp.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALK kalk_temp.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KALK kalk_temp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KALK kalk_temp
&Scoped-define QUERY-STRING-BRW_KALK FOR EACH kalk_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALK OPEN QUERY BRW_KALK FOR EACH kalk_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALK kalk_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALK kalk_temp


/* Definitions for BROWSE BRW_KB2                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB2 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB2 
&Scoped-define QUERY-STRING-BRW_KB2 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB2 OPEN QUERY BRW_KB2 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB2 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB2 konstvaltemp


/* Definitions for BROWSE BRW_KB3                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB3 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB3 
&Scoped-define QUERY-STRING-BRW_KB3 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB3 OPEN QUERY BRW_KB3 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB3 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB3 konstvaltemp


/* Definitions for BROWSE BRW_KB4                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB4 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB4 
&Scoped-define QUERY-STRING-BRW_KB4 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB4 OPEN QUERY BRW_KB4 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB4 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB4 konstvaltemp


/* Definitions for BROWSE BRW_KB5                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB5 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB5 
&Scoped-define QUERY-STRING-BRW_KB5 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB5 OPEN QUERY BRW_KB5 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB5 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB5 konstvaltemp


/* Definitions for BROWSE BRW_KB6                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB6 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
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
list_mtrl.ANTAL list_mtrl.ENHET list_mtrl.LINKAB list_mtrl.LEVKOD ~
list_mtrl.PAR2 
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
list_mtrl.ANTAL list_mtrl.ENHET list_mtrl.LINKAB list_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRLSM list_mtrl.ANTAL ~
list_mtrl.LINKAB 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRLSM list_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRLSM list_mtrl
&Scoped-define QUERY-STRING-BRW_MTRLSM FOR EACH list_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRLSM OPEN QUERY BRW_MTRLSM FOR EACH list_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRLSM list_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRLSM list_mtrl


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


/* Definitions for BROWSE BRW_STOLP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_STOLP berstolptemp.ENR ~
berstolptemp.BENAMNING berstolptemp.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STOLP 
&Scoped-define QUERY-STRING-BRW_STOLP FOR EACH berstolptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STOLP OPEN QUERY BRW_STOLP FOR EACH berstolptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STOLP berstolptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STOLP berstolptemp


/* Definitions for BROWSE BRW_STOLPSM                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_STOLPSM berstolptemp.ENR ~
berstolptemp.BENAMNING berstolptemp.PRIS 
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
berupptemp.ADRESS 
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


/* Definitions for FRAME FRAME-ANM                                      */

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
&Scoped-Define ENABLED-OBJECTS BRW_ANM BTN_OVER-2 ED_ANM BTN_KOPPANM ~
FILL-IN-TEXT 

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
       MENU-ITEM m_Visa_informationHLEV LABEL "Visa information via vald Leverantör"
       MENU-ITEM m_Pris_information2 LABEL "Sats information".

DEFINE MENU POPUP-MENU-BRW_HLEVSM 
       MENU-ITEM m_Visa_informationHLEVSM LABEL "Visa information via vald Leverantör".

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

DEFINE MENU POPUP-MENU-BRW_MTRL 
       MENU-ITEM m_Visa_informationBRW_MTRL LABEL "Visa information via vald Leverantör".

DEFINE MENU POPUP-MENU-BRW_MTRLSM 
       MENU-ITEM m_Visa_informationMTRLSM LABEL "Visa information via vald Leverantör".

DEFINE MENU POPUP-MENU-BRW_SATS 
       MENU-ITEM m_Visa_informationSATS LABEL "Visa information via vald Leverantör"
       MENU-ITEM m_Pris_information LABEL "Sats information".

DEFINE MENU POPUP-MENU-BRW_SATSSM 
       MENU-ITEM m_Visa_informationSATSSM LABEL "Visa information Internet".

DEFINE MENU POPUP-MENU-BRW_STOLP 
       MENU-ITEM m_Visa_informationSTOLP LABEL "Visa information via vald Leverantör".

DEFINE MENU POPUP-MENU-BRW_STOLPSM 
       MENU-ITEM m_Visa_informationSTOLPSM LABEL "Visa information via vald Leverantör".

DEFINE MENU POPUP-MENU-BRW_TILLLIN 
       MENU-ITEM m_Visa_information2LIN LABEL "Visa information".


/* Definitions of the field level widgets                               */
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

DEFINE BUTTON MBTN_ANDID  NO-FOCUS
     LABEL "Ordning" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE BUTTON MBTN_ANM  NO-FOCUS
     LABEL "Anmärkning" 
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

DEFINE BUTTON MBTN_FRIKORT  NO-FOCUS
     LABEL "Frikort" 
     SIZE 7.25 BY 2.33
     BGCOLOR 8 FONT 4.

DEFINE BUTTON MBTN_ID  NO-FOCUS
     LABEL "Identitet" 
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

DEFINE VARIABLE CMB_TYP AS CHARACTER FORMAT "X(6)":U 
     LABEL "Typ" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "Servis","In","Ut" 
     DROP-DOWN-LIST
     SIZE 10.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ADR AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kabeladress" 
     VIEW-AS FILL-IN 
     SIZE 45.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANMARK AS CHARACTER FORMAT "X(80)":U 
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

DEFINE VARIABLE FILL-IN-SADR AS CHARACTER FORMAT "X(30)":U 
     LABEL "Skåpadress" 
     VIEW-AS FILL-IN 
     SIZE 48.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SAK AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Säkring (A)" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1 NO-UNDO.

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

DEFINE BUTTON BTN_ANL 
     LABEL "Anl.register" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_HMT 
     LABEL "Hämta" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KOPPID 
     LABEL "Koppla identitet" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

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

DEFINE VARIABLE FILL-IN-IDNR1 AS INTEGER FORMAT ">>>>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR1LABEL AS CHARACTER FORMAT "X(256)":U INITIAL "Nr:" 
      VIEW-AS TEXT 
     SIZE 3.63 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR2 AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR2LABEL AS CHARACTER FORMAT "X(256)":U INITIAL "Nr:" 
      VIEW-AS TEXT 
     SIZE 3.63 BY .63 NO-UNDO.

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

DEFINE VARIABLE FILL-IN-SUND AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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

DEFINE VARIABLE FILL-IN-F1KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F2KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F3KAB AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOPKAB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kopplingstyp" 
      VIEW-AS TEXT 
     SIZE 3 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOPSKAP AS CHARACTER FORMAT "X(256)":U INITIAL "Inget skåp är valt!" 
      VIEW-AS TEXT 
     SIZE 21 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-KORTKAB AS CHARACTER FORMAT "X(256)":U INITIAL "Kabelskåp" 
      VIEW-AS TEXT 
     SIZE 12.5 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEDKAB AS INTEGER FORMAT "->>>>":U INITIAL 0 
     LABEL "Reserv" 
      VIEW-AS TEXT 
     SIZE 8 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR1KAB AS INTEGER FORMAT ">":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 2.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR2KAB AS INTEGER FORMAT ">":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 2.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR3KAB AS INTEGER FORMAT ">":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 2.38 BY .75 NO-UNDO.

DEFINE BUTTON BTN_ANTAL2 
     LABEL "Antal" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT2 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NYKOD 
     LABEL "Ny kod" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NER 
     IMAGE-UP FILE "BILDER\pilner":U
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
"Komplettera skåp", 2
     SIZE 19.75 BY 1.67 NO-UNDO.

DEFINE VARIABLE TOG_RATT AS LOGICAL INITIAL no 
     LABEL "Gå till befintlig konstruktion" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.75 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_VISAGRUPP AS LOGICAL INITIAL no 
     LABEL "Visa endast vald konstruktionsgrupp" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.38 BY .79 NO-UNDO.

DEFINE BUTTON BTN_ANDRAKOPUPP 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTKOPUPP 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NYKOPUPP 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_BKOMBKOPUPP 
     LABEL "Ta bort kombinationer" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_KOMBKOPUPP 
     LABEL "Skapa kombinationer" 
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

DEFINE VARIABLE FILL-IN-LANGDLIN AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
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

DEFINE VARIABLE FILL-IN-METERLINUPP AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Längd" 
      VIEW-AS TEXT 
     SIZE 12 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROCLINUPP AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Procent" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SUMMALINUPP AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "på upplag" 
      VIEW-AS TEXT 
     SIZE 12 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOTMETERLINUPP AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total längd" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPPLINUPP AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Upplag" 
      VIEW-AS TEXT 
     SIZE 3.63 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPPMETERLINUPP AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "till upplag" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

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
     LABEL "Satsinfo." 
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

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

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
"Transf.", 3
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

DEFINE BUTTON BTN_NERORD 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Btn 1" 
     SIZE 5 BY 1.75.

DEFINE BUTTON BTN_UPPORD 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "Btn 1" 
     SIZE 5 BY 1.75.

DEFINE VARIABLE CMB_VALORD AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

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

DEFINE BUTTON BTN_LEVSM 
     LABEL "Åter huvudleverantör" 
     SIZE 20 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_LINKABSM 
     LABEL "Lin/Kab":L 
     SIZE 12 BY 1.

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
     LABEL "Satsinfo." 
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

DEFINE VARIABLE FILL-IN-ENRSM AS CHARACTER FORMAT "X(11)":U 
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
"Transf.", 3
     SIZE 25 BY .79 NO-UNDO.

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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
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
      kon_val SCROLLING.

DEFINE QUERY BRW_KABSM FOR 
      kon_val SCROLLING.

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

DEFINE QUERY BRW_SATS FOR 
      satstemp SCROLLING.

DEFINE QUERY BRW_SATSSM FOR 
      satstemp SCROLLING.

DEFINE QUERY BRW_SKAPKAB FOR 
      berskaptemp SCROLLING.

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
&ANALYZE-RESUME

/* Browse definitions                                                   */
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
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B3 WINDOW-3 _STRUCTURED
  QUERY BRW_B3 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B4 WINDOW-3 _STRUCTURED
  QUERY BRW_B4 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B5 WINDOW-3 _STRUCTURED
  QUERY BRW_B5 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B6 WINDOW-3 _STRUCTURED
  QUERY BRW_B6 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_BILD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BILD WINDOW-3 _STRUCTURED
  QUERY BRW_BILD NO-LOCK DISPLAY
      bildbertemp.NAMN COLUMN-LABEL "Egen text" FORMAT "X(25)":U
            WIDTH 23.63
      bildbertemp.FILNAMN COLUMN-LABEL "Sökväg" FORMAT "X(80)":U
            WIDTH 81.5
  ENABLE
      bildbertemp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-AUTO-VALIDATE NO-COLUMN-SCROLLING SIZE 110.63 BY 8.

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
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 27
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 56.5 BY 10.33
         TITLE "Materiel".

DEFINE BROWSE BRW_HLEVSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEVSM WINDOW-3 _STRUCTURED
  QUERY BRW_HLEVSM NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 20
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 11
         TITLE "Materiellista".

DEFINE BROWSE BRW_KABFRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KABFRI WINDOW-3 _STRUCTURED
  QUERY BRW_KABFRI DISPLAY
      fri_temp.BENAMNING COLUMN-LABEL "Kabeltyp" FORMAT "x(256)":U
            WIDTH 30
      fri_temp.KABADR COLUMN-LABEL "KabelAdress" FORMAT "X(256)":U
            WIDTH 33
      fri_temp.METER FORMAT ">>>9":U
      fri_temp.APPARAT COLUMN-LABEL "Apparat" FORMAT "X(256)":U
            WIDTH 15
      fri_temp.SKAPADR COLUMN-LABEL "SkåpAdress" FORMAT "X(256)":U
            WIDTH 17
      fri_temp.TYP COLUMN-LABEL "Typ" FORMAT "X(8)":U WIDTH 5
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
      kon_val.F6 FORMAT "X(256)":U WIDTH 16.75
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 110 BY 6.38.

DEFINE BROWSE BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALK WINDOW-3 _STRUCTURED
  QUERY BRW_KALK NO-LOCK DISPLAY
      kalk_temp.ARBKOD COLUMN-LABEL "Arbetskod" FORMAT "X(5)":U
      kalk_temp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      kalk_temp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(40)":U
      kalk_temp.ANTAL COLUMN-LABEL "Antal" FORMAT ">>>>9.999":U
      kalk_temp.ENHET COLUMN-LABEL "Enhet" FORMAT "x(5)":U
  ENABLE
      kalk_temp.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 76.5 BY 12
         TITLE "P3 koder".

DEFINE BROWSE BRW_KB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB2 WINDOW-3 _STRUCTURED
  QUERY BRW_KB2 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29 ROW-HEIGHT-CHARS .46.

DEFINE BROWSE BRW_KB3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB3 WINDOW-3 _STRUCTURED
  QUERY BRW_KB3 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB4 WINDOW-3 _STRUCTURED
  QUERY BRW_KB4 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB5 WINDOW-3 _STRUCTURED
  QUERY BRW_KB5 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB6 WINDOW-3 _STRUCTURED
  QUERY BRW_KB6 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 26
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
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

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
      list_mtrl.BENAMNING FORMAT "x(256)":U WIDTH 26
      list_mtrl.ANTAL FORMAT ">>>>>9":U
      list_mtrl.ENHET FORMAT "x(5)":U
      list_mtrl.LINKAB FORMAT "Ja/Nej":U
      list_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U WIDTH 5
      list_mtrl.PAR2 FORMAT "X(1)":U
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
      list_mtrl.BENAMNING FORMAT "x(256)":U WIDTH 20
      list_mtrl.ANTAL FORMAT ">>>>>9":U
      list_mtrl.ENHET FORMAT "x(5)":U
      list_mtrl.LINKAB FORMAT "Ja/Nej":U
      list_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U WIDTH 5
  ENABLE
      list_mtrl.ANTAL
      list_mtrl.LINKAB
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 55.88 BY 13
         TITLE "Vald materiel".

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

DEFINE BROWSE BRW_STOLP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STOLP WINDOW-3 _STRUCTURED
  QUERY BRW_STOLP NO-LOCK DISPLAY
      berstolptemp.ENR FORMAT "X(11)":U
      berstolptemp.BENAMNING FORMAT "x(256)":U WIDTH 31
      berstolptemp.PRIS COLUMN-LABEL "Netto pris" FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 56.5 BY 10.33.

DEFINE BROWSE BRW_STOLPSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STOLPSM WINDOW-3 _STRUCTURED
  QUERY BRW_STOLPSM NO-LOCK DISPLAY
      berstolptemp.ENR FORMAT "X(11)":U
      berstolptemp.BENAMNING FORMAT "x(256)":U WIDTH 25
      berstolptemp.PRIS COLUMN-LABEL "Netto pris" FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 11.

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
      berupptemp.ADRESS COLUMN-LABEL "Adress" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 50 BY 7.38
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-BARABREDD
     BTN_OK AT ROW 1 COL 111.38
     RAD_VAL AT ROW 1 COL 122 NO-LABEL
     MBTN_ATG AT ROW 1 COL 99.88
     BTN_AVB AT ROW 2.33 COL 111.38
     MBTN_ANDID AT ROW 1 COL 72.25
     MBTN_FRIKORT AT ROW 1 COL 56.5
     MBTN_KALK AT ROW 1 COL 64.38
     MBTN_KOPI AT ROW 1 COL 40.75
     MBTN_KSKAP AT ROW 1 COL 48.63
     MBTN_LIN AT ROW 1 COL 80.13
     MBTN_LIST AT ROW 1 COL 95.88
     MBTN_SNABB AT ROW 1 COL 103.63
     BTN_SOLEN AT ROW 3.83 COL 119.75
     BTN_ORDUPP AT ROW 6 COL 120
     BTN_ORDNER AT ROW 8.92 COL 120
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

DEFINE FRAME FRAME-KONST
     CMB_VAL AT ROW 1.08 COL 1.5 NO-LABEL
     TOG_VISAGRUPP AT ROW 1.08 COL 31.63
     RAD_KOMP AT ROW 1.08 COL 76 NO-LABEL
     BTN_NER AT ROW 1.21 COL 57.63
     BTN_UPP AT ROW 1.21 COL 66
     TOG_RATT AT ROW 1.96 COL 95.25
     BRW_KON AT ROW 3.71 COL 1.5
     BRW_KB2 AT ROW 3.71 COL 42.5
     BRW_B2 AT ROW 3.71 COL 42.63
     BRW_KB3 AT ROW 3.71 COL 83.75
     BRW_B3 AT ROW 3.71 COL 83.88
     BRW_B4 AT ROW 11 COL 1.5
     BRW_KB4 AT ROW 11 COL 1.5
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

DEFINE FRAME FRAME-ORDN
     CMB_VALORD AT ROW 2.5 COL 37.5 NO-LABEL
     BRW_FRAN AT ROW 3.83 COL 21.5
     BTN_UPPORD AT ROW 11 COL 92
     BTN_NERORD AT ROW 13.96 COL 92
     "Ändring av ordning" VIEW-AS TEXT
          SIZE 22 BY 1.21 AT ROW 1.13 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-FRI
     CMB_KORTVAL AT ROW 2.79 COL 84.75 COLON-ALIGNED
     RAD_VALFRI AT ROW 3.79 COL 17.63 NO-LABEL
     BRW_APP AT ROW 4.54 COL 17.63
     BRW_MTRLFRI AT ROW 4.54 COL 17.63
     BRW_KABFRI AT ROW 11.88 COL 17.63
     FBTN_VISAFRI AT ROW 8.29 COL 111.38
     FBTN_SKRIVFRI AT ROW 9.42 COL 111.38
     BTN_EXKSKO AT ROW 10.5 COL 111.38
     FBTN_PDFFRI AT ROW 11.58 COL 111.38
     BTN_EXSTOLP AT ROW 12.71 COL 111.38
     BTN_EXMARK AT ROW 13.79 COL 111.38
     BTN_UPPFRI AT ROW 13.42 COL 106.75
     BTN_NERFRI AT ROW 15.92 COL 106.75
     FILL-IN-TYP AT ROW 18.46 COL 15.63 COLON-ALIGNED
     FILL-IN-SADR AT ROW 18.46 COL 55.38 COLON-ALIGNED
     FILL-IN-KABEL AT ROW 19.54 COL 15.63 COLON-ALIGNED
     FILL-IN-METER AT ROW 19.54 COL 71.88 COLON-ALIGNED
     FILL-IN-ADR AT ROW 20.63 COL 15.63 COLON-ALIGNED
     CMB_TYP AT ROW 20.63 COL 71.88 COLON-ALIGNED
     FILL-IN-APPARAT AT ROW 21.71 COL 15.63 COLON-ALIGNED
     FILL-IN-GRUPPNR AT ROW 21.71 COL 71.88 COLON-ALIGNED
     FILL-IN-AR AT ROW 22.79 COL 15.63 COLON-ALIGNED
     FILL-IN-SAK AT ROW 22.79 COL 36.75 COLON-ALIGNED
     FILL-IN-MAX AT ROW 22.79 COL 61.5 COLON-ALIGNED
     FILL-IN-ANMARK AT ROW 23.88 COL 15.63 COLON-ALIGNED
     FBTN_SKAPAFRI AT ROW 25 COL 31.88
     FBTN_ANDFRI AT ROW 25 COL 53.63
     FBTN_BORTFRI AT ROW 25 COL 75.38
     FILL-IN-KORT AT ROW 1.13 COL 1.5 NO-LABEL
     FILL-IN-B2 AT ROW 1.58 COL 15.5 COLON-ALIGNED NO-LABEL
     FILL-IN-B3 AT ROW 1.58 COL 42.13 COLON-ALIGNED NO-LABEL
     FILL-IN-F2 AT ROW 2.75 COL 15.5 COLON-ALIGNED NO-LABEL
     FILL-IN-F3 AT ROW 2.75 COL 42.13 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-KALK
     BRW_KALK AT ROW 1.75 COL 22
     BTN_NYKOD AT ROW 13.88 COL 37.88
     BTN_BORT2 AT ROW 13.88 COL 52.88
     BTN_ANTAL2 AT ROW 13.88 COL 67.88
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
     FILL-IN-IDNR1 AT ROW 6.04 COL 105.75 COLON-ALIGNED NO-LABEL
     FILL-IN-IDNR2 AT ROW 10.42 COL 105.75 COLON-ALIGNED NO-LABEL
     FILL-IN-SUND AT ROW 11.92 COL 105.75 COLON-ALIGNED NO-LABEL
     TOG_A AT ROW 14.42 COL 109.75
     TOG_B AT ROW 14.42 COL 113.5
     TOG_C AT ROW 14.42 COL 117.25
     BTN_KOPPID AT ROW 16.29 COL 108.25
     FILL-IN-TAB1 AT ROW 4.5 COL 3.5 NO-LABEL
     FILL-IN-ID1 AT ROW 4.5 COL 99 NO-LABEL
     FILL-IN-IDNR1LABEL AT ROW 6.42 COL 101.5 COLON-ALIGNED NO-LABEL
     FILL-IN-TAB2 AT ROW 8.88 COL 3.5 NO-LABEL
     FILL-IN-ID2 AT ROW 8.88 COL 99 NO-LABEL
     FILL-IN-IDNR2LABEL AT ROW 10.79 COL 101.5 COLON-ALIGNED NO-LABEL
     FILL-IN-SUNDLABEL AT ROW 12.29 COL 97.5 COLON-ALIGNED NO-LABEL
     FILL-IN-A AT ROW 13.13 COL 107.63 COLON-ALIGNED NO-LABEL
     FILL-IN-B AT ROW 13.13 COL 113.38 NO-LABEL
     FILL-IN-C AT ROW 13.13 COL 115.13 COLON-ALIGNED NO-LABEL
     FILL-IN-TAB3 AT ROW 13.21 COL 3.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.42.

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

DEFINE FRAME FRAME-BILD
     BRW_BILD AT ROW 5.46 COL 5.38
     BTN_NYA AT ROW 14.54 COL 40
     BTN_BORTA AT ROW 14.54 COL 55.88
     BTN_VISA AT ROW 14.54 COL 71.75
     "men systemet klarar även formaten bmp och gif." VIEW-AS TEXT
          SIZE 58 BY .63 AT ROW 3.5 COL 5.5
     "För bästa resultat, se till att bildens bredd inte är större än 620 pixlar." VIEW-AS TEXT
          SIZE 80 BY .63 AT ROW 4.25 COL 5.5
     "Tips vid koppling av bilder" VIEW-AS TEXT
          SIZE 28.5 BY .75 AT ROW 1.46 COL 5.5
     "Det fungerar bäst med formatet jpg eller jpeg," VIEW-AS TEXT
          SIZE 47 BY .63 AT ROW 2.75 COL 5.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.38.

DEFINE FRAME FRAME-ANM
     BRW_ANM AT ROW 2.92 COL 5.38
     BTN_OVER-2 AT ROW 6.13 COL 58.38
     ED_ANM AT ROW 2.92 COL 62.38 NO-LABEL
     BTN_KOPPANM AT ROW 11.08 COL 79.88
     FILL-IN-TEXT AT ROW 1.29 COL 72.38 COLON-ALIGNED NO-LABEL
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
     "Antal meter" VIEW-AS TEXT
          SIZE 11.38 BY .67 AT ROW 14.88 COL 64.13
     "Antal meter fördelat" VIEW-AS TEXT
          SIZE 23.25 BY .67 AT ROW 17.71 COL 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-SKAPMTRL
     BRW_KABSM AT ROW 3.29 COL 1.5
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
     BRW_SATSSM AT ROW 10.58 COL 1.5
     BRW_HLEVSM AT ROW 10.58 COL 1.5
     BRW_STOLPSM AT ROW 10.58 COL 1.5
     BRW_MTRLSM AT ROW 10.58 COL 55.63
     "Skåpmateriel" VIEW-AS TEXT
          SIZE 14 BY 1.21 AT ROW 1.13 COL 1.5
          FONT 17
     RECT-10 AT ROW 22.75 COL 1.5
     IMAGE-6 AT ROW 23.04 COL 2.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-MTRL
     RAD_MTRL AT ROW 1.17 COL 1.75 NO-LABEL
     BTN_OVER AT ROW 5.58 COL 58
     BTN_BACK AT ROW 7.83 COL 58
     BTN_LEV AT ROW 12.54 COL 28.63
     CMB_LEV AT ROW 12.54 COL 26.63 COLON-ALIGNED
     FILL-IN-ENR AT ROW 13.79 COL 26.63 COLON-ALIGNED
     FILL-IN-KOD AT ROW 13.83 COL 26.63 COLON-ALIGNED
     FILL-IN-BEN AT ROW 14.75 COL 26.63 COLON-ALIGNED
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
     BRW_HLEV AT ROW 2.08 COL 1.5
     BRW_SATS AT ROW 2.08 COL 1.5
     BRW_STOLP AT ROW 2.08 COL 1.5
     BRW_MTRL AT ROW 2.08 COL 62
     RECT-22 AT ROW 13.67 COL 1.5
     IMAGE-4 AT ROW 13.83 COL 2.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.38.

DEFINE FRAME FRAME-KABSKAP
     BRW_KABKAB AT ROW 5.58 COL 1.5
     FBTN_ANTKAB AT ROW 5.63 COL 110.13
     BTN_UPPKAB AT ROW 6.75 COL 105.13
     FBTN_KORTKAB AT ROW 6.75 COL 110.13
     FBTN_MTRLKAB AT ROW 7.83 COL 110.13
     BTN_NERKAB AT ROW 9.38 COL 105.13
     BTN_BORTKAB AT ROW 12.75 COL 39.5
     BTN_TYP1KAB AT ROW 15.33 COL 6.63
     BTN_TYP2KAB AT ROW 15.33 COL 26.38
     BTN_TYP3KAB AT ROW 15.33 COL 46.13
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
     FILL-IN-KOPKAB AT ROW 13.71 COL 16.13 COLON-ALIGNED
     FILL-IN-NR1KAB AT ROW 14.33 COL 8 COLON-ALIGNED NO-LABEL
     FILL-IN-NR2KAB AT ROW 14.33 COL 27.63 COLON-ALIGNED NO-LABEL
     FILL-IN-NR3KAB AT ROW 14.33 COL 47.38 COLON-ALIGNED NO-LABEL
     FILL-IN-LEDKAB AT ROW 24.63 COL 12.38 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.

DEFINE FRAME FRAME-KOPUPP
     BRW_KOMBKOPUPP AT ROW 2.67 COL 1.5
     FBTN_UPPKOPUPP AT ROW 8.5 COL 111.38
     FBTN_VISAKOPUPP AT ROW 9.58 COL 111.38
     FBTN_SKRIVKOPUPP AT ROW 10.71 COL 111.38
     FBTN_KOMBKOPUPP AT ROW 11.79 COL 111.5
     FBTN_BKOMBKOPUPP AT ROW 12.92 COL 111.5
     FBTN_VISAKOPUPP-2 AT ROW 14 COL 111.5
     FBTN_SKRIVKOPUPP-2 AT ROW 15.08 COL 111.5
     BRW_UPPKOPUPP AT ROW 17.54 COL 29.88
     BTN_NYKOPUPP AT ROW 25.33 COL 35.38
     BTN_ANDRAKOPUPP AT ROW 25.33 COL 48.38
     BTN_BORTKOPUPP AT ROW 25.33 COL 61.38
     "Koppla upplag samt kombinera konstruktioner" VIEW-AS TEXT
          SIZE 49.5 BY 1.21 AT ROW 1.13 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.83
         SIZE 124.5 BY 25.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: apparattemp T "?" NO-UNDO temp-db apparattemp
      TABLE: berfortemp T "?" NO-UNDO temp-db berfortemp
      TABLE: berlinjetemp T "?" NO-UNDO temp-db berlinjetemp
      TABLE: bernattemp T "?" NO-UNDO temp-db bernattemp
      TABLE: berskaptemp T "?" NO-UNDO temp-db berskaptemp
      TABLE: berstolptemp T "?" NO-UNDO temp-db berstolptemp
      TABLE: berupptemp T "?" NO-UNDO temp-db berupptemp
      TABLE: bildbertemp T "?" NO-UNDO temp-db bildbertemp
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
      TABLE: fri_temp T "?" NO-UNDO temp-db fri_temp
      TABLE: kombtemp T "?" NO-UNDO temp-db kombtemp
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
      TABLE: konsttemp T "?" NO-UNDO temp-db konsttemp
      TABLE: konstvaltemp T "?" NO-UNDO temp-db konstvaltemp
      TABLE: kon_id T "?" NO-UNDO temp-db kon_id
      TABLE: kon_id2 T "?" NO-UNDO temp-db kon_id2
      TABLE: kopp_lina T "?" NO-UNDO temp-db kopp_lina
      TABLE: lin_tab2 T "?" NO-UNDO temp-db lin_tab2
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: mtrl_tab T "?" NO-UNDO temp-db mtrl_tab
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
      TABLE: ? T "?" NO-UNDO temp-db kalk_temp
      TABLE: ? T "?" NO-UNDO temp-db list_mtrl
      TABLE: ? T "?" NO-UNDO temp-db kon_val
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beredning"
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ANM:FRAME = FRAME FRAME-BARABREDD:HANDLE
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
       FRAME FRAME-MTRL:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-ORDN:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-SKAPMTRL:FRAME = FRAME FRAME-BARABREDD:HANDLE.

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
                                                                        */
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

/* SETTINGS FOR FILL-IN FILL-IN-GRUPPNR IN FRAME FRAME-FRI
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-KORT IN FRAME FRAME-FRI
   NO-ENABLE ALIGN-L                                                    */
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
       TOG_B:HIDDEN IN FRAME FRAME-ID           = TRUE.

ASSIGN 
       TOG_C:HIDDEN IN FRAME FRAME-ID           = TRUE.

/* SETTINGS FOR FRAME FRAME-KABSKAP
                                                                        */
/* BROWSE-TAB BRW_KABKAB 1 FRAME-KABSKAP */
/* BROWSE-TAB BRW_SKAPKAB BTN_TYP3KAB FRAME-KABSKAP */
ASSIGN 
       BRW_KABKAB:MAX-DATA-GUESS IN FRAME FRAME-KABSKAP         = 1000.

ASSIGN 
       BRW_SKAPKAB:HIDDEN  IN FRAME FRAME-KABSKAP                = TRUE
       BRW_SKAPKAB:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KABSKAP = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B1KAB IN FRAME FRAME-KABSKAP
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-F1KAB IN FRAME FRAME-KABSKAP
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

/* SETTINGS FOR FRAME FRAME-KALK
                                                                        */
/* BROWSE-TAB BRW_KALK 1 FRAME-KALK */
ASSIGN 
       BRW_KALK:HIDDEN  IN FRAME FRAME-KALK                = TRUE
       BRW_KALK:MAX-DATA-GUESS IN FRAME FRAME-KALK         = 10000.

ASSIGN 
       BTN_ANTAL2:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_BORT2:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_NYKOD:HIDDEN IN FRAME FRAME-KALK           = TRUE.

/* SETTINGS FOR FRAME FRAME-KONST
                                                                        */
/* BROWSE-TAB BRW_KON TOG_RATT FRAME-KONST */
/* BROWSE-TAB BRW_KB2 BRW_KON FRAME-KONST */
/* BROWSE-TAB BRW_B2 BRW_KB2 FRAME-KONST */
/* BROWSE-TAB BRW_KB3 BRW_B2 FRAME-KONST */
/* BROWSE-TAB BRW_B3 BRW_KB3 FRAME-KONST */
/* BROWSE-TAB BRW_B4 BRW_B3 FRAME-KONST */
/* BROWSE-TAB BRW_KB4 BRW_B4 FRAME-KONST */
/* BROWSE-TAB BRW_KB5 BRW_KB4 FRAME-KONST */
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
/* BROWSE-TAB BRW_KOMBKOPUPP TEXT-1 FRAME-KOPUPP */
/* BROWSE-TAB BRW_UPPKOPUPP FBTN_SKRIVKOPUPP-2 FRAME-KOPUPP */
ASSIGN 
       BRW_KOMBKOPUPP:HIDDEN  IN FRAME FRAME-KOPUPP                = TRUE
       BRW_KOMBKOPUPP:POPUP-MENU IN FRAME FRAME-KOPUPP             = MENU POPUP-MENU-BRW_KOMBKOPUPP:HANDLE
       BRW_KOMBKOPUPP:MAX-DATA-GUESS IN FRAME FRAME-KOPUPP         = 1000.

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

/* SETTINGS FOR FRAME FRAME-MTRL
   Custom                                                               */
/* BROWSE-TAB BRW_HLEV IMAGE-4 FRAME-MTRL */
/* BROWSE-TAB BRW_SATS BRW_HLEV FRAME-MTRL */
/* BROWSE-TAB BRW_STOLP BRW_SATS FRAME-MTRL */
/* BROWSE-TAB BRW_MTRL BRW_STOLP FRAME-MTRL */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_HLEV:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_HLEV:HANDLE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 1000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-MTRL       = TRUE.

ASSIGN 
       BRW_MTRL:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_MTRL:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_MTRL:HANDLE
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 10000.

ASSIGN 
       BRW_SATS:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_SATS:HANDLE
       BRW_SATS:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 50000
       BRW_SATS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE
       BRW_SATS:COLUMN-RESIZABLE IN FRAME FRAME-MTRL       = TRUE.

ASSIGN 
       BRW_STOLP:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_STOLP:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_STOLP:HANDLE
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

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEN IN FRAME FRAME-MTRL
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

ASSIGN 
       FILL-IN-KOD:HIDDEN IN FRAME FRAME-MTRL           = TRUE.

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
/* BROWSE-TAB BRW_FRAN CMB_VALORD FRAME-ORDN */
ASSIGN 
       BRW_FRAN:MAX-DATA-GUESS IN FRAME FRAME-ORDN         = 10000.

/* SETTINGS FOR COMBO-BOX CMB_VALORD IN FRAME FRAME-ORDN
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-SKAPMTRL
   Custom                                                               */
/* BROWSE-TAB BRW_KABSM 1 FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_SATSSM IMAGE-6 FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_HLEVSM BRW_SATSSM FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_STOLPSM BRW_HLEVSM FRAME-SKAPMTRL */
/* BROWSE-TAB BRW_MTRLSM BRW_STOLPSM FRAME-SKAPMTRL */
ASSIGN 
       BRW_HLEVSM:HIDDEN  IN FRAME FRAME-SKAPMTRL                = TRUE
       BRW_HLEVSM:POPUP-MENU IN FRAME FRAME-SKAPMTRL             = MENU POPUP-MENU-BRW_HLEVSM:HANDLE
       BRW_HLEVSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 10000
       BRW_HLEVSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_KABSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 1000
       BRW_KABSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_MTRLSM:POPUP-MENU IN FRAME FRAME-SKAPMTRL             = MENU POPUP-MENU-BRW_MTRLSM:HANDLE
       BRW_MTRLSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 10000
       BRW_MTRLSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_SATSSM:HIDDEN  IN FRAME FRAME-SKAPMTRL                = TRUE
       BRW_SATSSM:POPUP-MENU IN FRAME FRAME-SKAPMTRL             = MENU POPUP-MENU-BRW_SATSSM:HANDLE
       BRW_SATSSM:MAX-DATA-GUESS IN FRAME FRAME-SKAPMTRL         = 50000
       BRW_SATSSM:COLUMN-RESIZABLE IN FRAME FRAME-SKAPMTRL       = TRUE.

ASSIGN 
       BRW_STOLPSM:POPUP-MENU IN FRAME FRAME-SKAPMTRL             = MENU POPUP-MENU-BRW_STOLPSM:HANDLE
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

/* SETTINGS FOR FILL-IN FILL-IN-BENSM IN FRAME FRAME-SKAPMTRL
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-BENSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       FILL-IN-ENRSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

ASSIGN 
       FILL-IN-KODSM:HIDDEN IN FRAME FRAME-SKAPMTRL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALTSM IN FRAME FRAME-SKAPMTRL
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALTSM:READ-ONLY IN FRAME FRAME-SKAPMTRL        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANM
/* Query rebuild information for BROWSE BRW_ANM
     _TblList          = "Temp-Tables.fastanmtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.fastanmtemp.ANMARK
"fastanmtemp.ANMARK" "Fasta anmärkningar" "X(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_APP
/* Query rebuild information for BROWSE BRW_APP
     _TblList          = "Temp-Tables.apparattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.apparattemp.BENAMNING
"apparattemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "76" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_APP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B2
/* Query rebuild information for BROWSE BRW_B2
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B3
/* Query rebuild information for BROWSE BRW_B3
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B4
/* Query rebuild information for BROWSE BRW_B4
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B5
/* Query rebuild information for BROWSE BRW_B5
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B6
/* Query rebuild information for BROWSE BRW_B6
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B6 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BILD
/* Query rebuild information for BROWSE BRW_BILD
     _TblList          = "Temp-Tables.bildbertemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bildbertemp.NAMN
"bildbertemp.NAMN" "Egen text" "X(25)" "character" ? ? ? ? ? ? yes ? no no "23.63" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.bildbertemp.FILNAMN
"bildbertemp.FILNAMN" "Sökväg" "X(80)" "character" ? ? ? ? ? ? no ? no no "81.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BILD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRAN
/* Query rebuild information for BROWSE BRW_FRAN
     _TblList          = "Temp-Tables.kon_id"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_id.LINNR|yes,Temp-Tables.kon_id.NATNR|yes,Temp-Tables.kon_id.FRI1|yes,Temp-Tables.kon_id.FRI2|yes"
     _FldNameList[1]   > Temp-Tables.kon_id.LINNR
"kon_id.LINNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_id.NATNR
"kon_id.NATNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_id.FRI1
"kon_id.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_id.FRI2
"kon_id.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRANLIN
/* Query rebuild information for BROWSE BRW_FRANLIN
     _TblList          = "Temp-Tables.kon_id"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_id.LINNR|yes,Temp-Tables.kon_id.NATNR|yes,Temp-Tables.kon_id.FRI1|yes,Temp-Tables.kon_id.FRI2|yes"
     _FldNameList[1]   > Temp-Tables.kon_id.LINNR
"kon_id.LINNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_id.NATNR
"kon_id.NATNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_id.FRI1
"kon_id.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_id.FRI2
"kon_id.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kon_id.FRI3
"kon_id.FRI3" ? ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRANLIN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "27" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
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
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEVSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KABFRI
/* Query rebuild information for BROWSE BRW_KABFRI
     _TblList          = "Temp-Tables.fri_temp"
     _FldNameList[1]   > Temp-Tables.fri_temp.BENAMNING
"fri_temp.BENAMNING" "Kabeltyp" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.fri_temp.KABADR
"fri_temp.KABADR" "KabelAdress" "X(256)" "character" ? ? ? ? ? ? no ? no no "33" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.fri_temp.METER
"fri_temp.METER" ? ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.fri_temp.APPARAT
"fri_temp.APPARAT" "Apparat" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.fri_temp.SKAPADR
"fri_temp.SKAPADR" "SkåpAdress" "X(256)" "character" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.fri_temp.TYP
"fri_temp.TYP" "Typ" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KABFRI */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KABKAB
/* Query rebuild information for BROWSE BRW_KABKAB
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.F2
"kon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_val.F3
"kon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_val.F4
"kon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_val.F5
"kon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kon_val.F6
"kon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[6]   = Temp-Tables.kon_val.TYP
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KABKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KABSM
/* Query rebuild information for BROWSE BRW_KABSM
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_val.F3|yes,Temp-Tables.kon_val.F4|yes,Temp-Tables.kon_val.F5|yes,Temp-Tables.kon_val.F6|yes"
     _FldNameList[1]   > Temp-Tables.kon_val.F1
"kon_val.F1" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.75" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_val.F2
"kon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_val.F3
"kon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_val.F4
"kon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kon_val.F5
"kon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "17.5" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.kon_val.F6
"kon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "16.75" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KABSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALK
/* Query rebuild information for BROWSE BRW_KALK
     _TblList          = "Temp-Tables.kalk_temp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kalk_temp.ARBKOD
"kalk_temp.ARBKOD" "Arbetskod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kalk_temp.LOPNR
"kalk_temp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kalk_temp.BENAMNING
"kalk_temp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kalk_temp.ANTAL
"kalk_temp.ANTAL" "Antal" ">>>>9.999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kalk_temp.ENHET
"kalk_temp.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB2
/* Query rebuild information for BROWSE BRW_KB2
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB3
/* Query rebuild information for BROWSE BRW_KB3
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB4
/* Query rebuild information for BROWSE BRW_KB4
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB5
/* Query rebuild information for BROWSE BRW_KB5
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB6
/* Query rebuild information for BROWSE BRW_KB6
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB6 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOMBKOPUPP
/* Query rebuild information for BROWSE BRW_KOMBKOPUPP
     _TblList          = "Temp-Tables.kombtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kombtemp.F1
"kombtemp.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kombtemp.FORNR
"kombtemp.FORNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kombtemp.LINNR
"kombtemp.LINNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kombtemp.NATNR
"kombtemp.NATNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "14.5" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kombtemp.FRI1
"kombtemp.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.kombtemp.FRI2
"kombtemp.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.kombtemp.ID
"kombtemp.ID" ? ? "logical" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.kombtemp.ID2
"kombtemp.ID2" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.kombtemp.UPPLAG
"kombtemp.UPPLAG" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.kombtemp.KOMB
"kombtemp.KOMB" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOMBKOPUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LIN
/* Query rebuild information for BROWSE BRW_LIN
     _TblList          = "Temp-Tables.kopp_lina"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.kopp_lina.ENR
     _FldNameList[2]   > Temp-Tables.kopp_lina.BENAMNING
"kopp_lina.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "32" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kopp_lina.METER
"kopp_lina.METER" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kopp_lina.LEDARE
"kopp_lina.LEDARE" "Antal" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
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
"lin_tab2.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LINLINUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.list_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.list_mtrl.ENR
     _FldNameList[2]   > Temp-Tables.list_mtrl.BENAMNING
"list_mtrl.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.list_mtrl.ANTAL
"list_mtrl.ANTAL" ? ">>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.list_mtrl.ENHET
     _FldNameList[5]   = Temp-Tables.list_mtrl.LINKAB
     _FldNameList[6]   > Temp-Tables.list_mtrl.LEVKOD
"list_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.list_mtrl.PAR2
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRLFRI
/* Query rebuild information for BROWSE BRW_MTRLFRI
     _TblList          = "Temp-Tables.mtrl_tab"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.mtrl_tab.BENAMNING
"mtrl_tab.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "84.88" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRLFRI */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRLSM
/* Query rebuild information for BROWSE BRW_MTRLSM
     _TblList          = "Temp-Tables.list_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.list_mtrl.ENR
"list_mtrl.ENR" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.list_mtrl.BENAMNING
"list_mtrl.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.list_mtrl.ANTAL
"list_mtrl.ANTAL" ? ">>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.list_mtrl.ENHET
     _FldNameList[5]   > Temp-Tables.list_mtrl.LINKAB
"list_mtrl.LINKAB" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.list_mtrl.LEVKOD
"list_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRLSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATS
/* Query rebuild information for BROWSE BRW_SATS
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   > Temp-Tables.satstemp.BENAMNING
"satstemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "42" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATSSM
/* Query rebuild information for BROWSE BRW_SATSSM
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   > Temp-Tables.satstemp.BENAMNING
"satstemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATSSM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SKAPKAB
/* Query rebuild information for BROWSE BRW_SKAPKAB
     _TblList          = "Temp-Tables.berskaptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.berskaptemp.KOD
     _FldNameList[2]   > Temp-Tables.berskaptemp.BENAMNING
"berskaptemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SKAPKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STOLP
/* Query rebuild information for BROWSE BRW_STOLP
     _TblList          = "Temp-Tables.berstolptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.berstolptemp.ENR
     _FldNameList[2]   > Temp-Tables.berstolptemp.BENAMNING
"berstolptemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "31" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.berstolptemp.PRIS
"berstolptemp.PRIS" "Netto pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STOLP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STOLPSM
/* Query rebuild information for BROWSE BRW_STOLPSM
     _TblList          = "Temp-Tables.berstolptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.berstolptemp.ENR
     _FldNameList[2]   > Temp-Tables.berstolptemp.BENAMNING
"berstolptemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.berstolptemp.PRIS
"berstolptemp.PRIS" "Netto pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
"berfortemp.OMRADE" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.berfortemp.AVDNR
"berfortemp.AVDNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.berfortemp.VIJUDID
"berfortemp.VIJUDID" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TAB1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TAB2
/* Query rebuild information for BROWSE BRW_TAB2
     _TblList          = "Temp-Tables.berlinjetemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.berlinjetemp.LINNR
     _FldNameList[2]   > Temp-Tables.berlinjetemp.NAMN
"berlinjetemp.NAMN" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "49.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TAB2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TAB3
/* Query rebuild information for BROWSE BRW_TAB3
     _TblList          = "Temp-Tables.bernattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.bernattemp.NATNR
     _FldNameList[2]   > Temp-Tables.bernattemp.NAMN
"bernattemp.NAMN" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "49.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TAB3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TILLLIN
/* Query rebuild information for BROWSE BRW_TILLLIN
     _TblList          = "Temp-Tables.kon_id2"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_id2.LINNR|yes,Temp-Tables.kon_id2.NATNR|yes,Temp-Tables.kon_id2.FRI1|yes,Temp-Tables.kon_id2.FRI2|yes"
     _FldNameList[1]   > Temp-Tables.kon_id2.LINNR
"kon_id2.LINNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_id2.NATNR
"kon_id2.NATNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_id2.FRI1
"kon_id2.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_id2.FRI2
"kon_id2.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kon_id2.FRI3
"kon_id2.FRI3" ? ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TILLLIN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPPKOPUPP
/* Query rebuild information for BROWSE BRW_UPPKOPUPP
     _TblList          = "Temp-Tables.berupptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.berupptemp.UPPLAG
"berupptemp.UPPLAG" "Upplag" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.berupptemp.ADRESS
"berupptemp.ADRESS" "Adress" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UPPKOPUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPPLINUPP
/* Query rebuild information for BROWSE BRW_UPPLINUPP
     _TblList          = "Temp-Tables.berupptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.berupptemp.UPPLAG|yes"
     _FldNameList[1]   > Temp-Tables.berupptemp.UPPLAG
"berupptemp.UPPLAG" "Upplag" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.berupptemp.ADRESS
"berupptemp.ADRESS" "Adress" "X(256)" "character" ? ? ? ? ? ? no ? no no "46" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UPPLINUPP */
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

 

&Scoped-define BROWSE-NAME BRW_ANM

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
   APPLY 'CHOOSE' TO BTN_AVB IN FRAME FRAME-BARABREDD.
        
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
   WINDOW-3:TITLE = "".
   {BERTITLE.I}
   RUN enable_UI. 
   RUN DYNFRAMESIZEFM.P PERSISTENT SET framesizeh (INPUT WINDOW-3:HANDLE, INPUT TABLE frametemp,INPUT TABLE widgettemp,INPUT BRW_VAL:HANDLE).
   RUN btnstart_UI.
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
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE
      berstolptemp.PRIS:VISIBLE IN BROWSE BRW_STOLP = FALSE. 
   END.   
   FILL-IN-SKAPMTRL:HIDDEN = TRUE.
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}   
  
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
   IF TRIM(PROVERSION) BEGINS "9.1c" THEN DO:
      IF VALID-HANDLE(btnbervalh) THEN DO:
         FRAME FRAME-KONST:HIDDEN = FALSE.
         /*
         APPLY "CHOOSE" TO MBTN_KONST IN FRAME FRAME-BARABREDD.
         */
      END.
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
      /*
      IF TRIM(PROVERSION) BEGINS "9.1c" THEN. 
      ELSE 
      */
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
      ordningnr = 0.   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,WINDOW-3:HANDLE).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,CMB_KORTVAL:HANDLE IN FRAME FRAME-FRI).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,RAD_VALFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_SKAPAFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_MTRLFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_APP:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_ANDFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_BORTFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_VISAFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_SKRIVFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_EXKSKO:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_PDFFRI:HANDLE IN FRAME FRAME-FRI).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_KABFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_EXSTOLP:HANDLE IN FRAME FRAME-FRI).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_EXMARK:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_UPPFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_NERFRI:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-TYP:HANDLE IN FRAME FRAME-FRI).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-SADR:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-KABEL:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-METER:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-ADR:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,CMB_TYP:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-APPARAT:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-GRUPPNR:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-AR:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-SAK:HANDLE IN FRAME FRAME-FRI).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-MAX:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-ANMARK:HANDLE IN FRAME FRAME-FRI).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-B2:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-B3:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-F2:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-F3:HANDLE IN FRAME FRAME-FRI). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-KORT:HANDLE IN FRAME FRAME-FRI). 
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
      ordningnr = 0.    
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,WINDOW-3:HANDLE).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BRW_KOMBKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      
      RUN whandle_UI (INPUT ordningnr,FBTN_KOMBKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_BKOMBKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_UPPKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_VISAKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_SKRIVKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_UPPKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_NYKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_ANDRAKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_BORTKOPUPP:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_VISAKOPUPP-2:HANDLE IN FRAME FRAME-KOPUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_SKRIVKOPUPP-2:HANDLE IN FRAME FRAME-KOPUPP).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_KonstruktionerKOPUPP:HANDLE IN MENU POPUP-MENU-BRW_KOMBKOPUPP).          
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
      ordningnr = 0.   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,WINDOW-3:HANDLE).
      ordningnr = ordningnr + 1.                 
      RUN whandle_UI (INPUT ordningnr,BRW_KABSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,RAD_MTRLSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BRW_STOLPSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BRW_SATSSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BRW_HLEVSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BRW_MTRLSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_OVERSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_BACKSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,CMB_LEVSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_LEVSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,FILL-IN-KODSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,FILL-IN-ENRSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_NYSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_ANTALSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_SATSSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_TEXTSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,FILL-IN-BENSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_NOLLSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_ALLTMTRLSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_SPECSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_LINKABSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,RAD_SOKSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,FILL-IN-B2SM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,FILL-IN-B3SM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,FILL-IN-F2SM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,FILL-IN-F3SM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,FILL-IN-SOKALTSM:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,RECT-10:HANDLE IN FRAME FRAME-SKAPMTRL). 
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,IMAGE-6:HANDLE IN FRAME FRAME-SKAPMTRL).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,list_mtrl.ANTAL:HANDLE IN BROWSE BRW_MTRLSM).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,mtrltemp.NPRIS:HANDLE IN BROWSE BRW_HLEVSM).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,list_mtrl.LINKAB:HANDLE IN BROWSE BRW_MTRLSM).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationHLEVSM:HANDLE IN MENU POPUP-MENU-BRW_HLEVSM).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationMTRLSM:HANDLE IN MENU POPUP-MENU-BRW_MTRLSM).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationSATSSM:HANDLE IN MENU POPUP-MENU-BRW_SATSSM).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationSTOLPSM:HANDLE IN MENU POPUP-MENU-BRW_STOLPSM).
      RUN SKAPMATRLM.P PERSISTENT SET btnkskapmtrlh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
   END.  
   RUN main_UI IN btnkskapmtrlh. 
   RUN frame_UI (INPUT "SKAPMAT").
  
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
      ordningnr = 0.   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,WINDOW-3:HANDLE).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BRW_KABKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FBTN_ANTKAB:HANDLE IN FRAME FRAME-KABSKAP).  
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_UPPKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FBTN_KORTKAB:HANDLE IN FRAME FRAME-KABSKAP).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FBTN_MTRLKAB:HANDLE IN FRAME FRAME-KABSKAP).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_NERKAB :HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_BORTKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_TYP1KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_TYP2KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_TYP3KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BRW_SKAPKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_PLUSKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_MINKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_KSKAPKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-KORTKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-B1KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-B3KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-B2KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-F3KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-F1KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-F2KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-KOPKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NR1KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NR2KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NR3KAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-LEDKAB:HANDLE IN FRAME FRAME-KABSKAP).   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,FILL-IN-KOPSKAP:HANDLE IN FRAME FRAME-KABSKAP).   
                                                    
      RUN KSKAPM.P PERSISTENT SET btnkskaph (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
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
      ordningnr = 0.   
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,WINDOW-3:HANDLE).   
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_LINLINUPP:HANDLE IN FRAME FRAME-LINUPP).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_UPLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_VISALINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-PROCLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_MINLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_SKRIVLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-TOTMETERLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_NOLLLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_UPPLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-UPPMETERLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_NYLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_ANDRALINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_BORTLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_KOPPLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_AVBLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-ENRLINUPP:HANDLE IN FRAME FRAME-LINUPP).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-BENLINUPP:HANDLE IN FRAME FRAME-LINUPP).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-METERLINUPP:HANDLE IN FRAME FRAME-LINUPP).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-UPPLINUPP:HANDLE IN FRAME FRAME-LINUPP).   
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-SUMMALINUPP:HANDLE IN FRAME FRAME-LINUPP).
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
      ordningnr = 0.   
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,WINDOW-3:HANDLE).   
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,CMB_VALLIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,CMB_VAL2LIN :HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_FRANLIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_TILLLIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_LINUPP:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_VISALIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_SCHAKT:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FBTN_KORTLIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_VISA-2LIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BRW_LIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-LANGDLIN :HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_UPLIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-ANTALLIN :HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_MINLIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_SKAPALIN:HANDLE IN FRAME FRAME-LIN).  
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_ANDLIN:HANDLE IN FRAME FRAME-LIN).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,BTN_BORTLIN:HANDLE IN FRAME FRAME-LIN).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-LINKAB:HANDLE IN FRAME FRAME-LIN).   
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationLIN:HANDLE IN MENU POPUP-MENU-BRW_FRANLIN).
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_information2LIN:HANDLE IN MENU POPUP-MENU-BRW_TILLLIN). 
      ordningnr = ordningnr + 1. 
      RUN whandle_UI (INPUT ordningnr,kopp_lina.METER:HANDLE IN BROWSE BRW_LIN).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,kopp_lina.LEDARE:HANDLE IN BROWSE BRW_LIN).
      ordningnr = ordningnr + 1.
      RUN whandle_UI (INPUT ordningnr,BTN_AVBLIN:HANDLE IN FRAME FRAME-LIN).   
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
   TOG_SPANN = TOG_SPANN:CHECKED
   TOG_UPP   = TOG_UPP:CHECKED
   TOG_ID    = TOG_ID:CHECKED
   TOG_ANM   = TOG_ANM:CHECKED
   TOG_TEXT  = TOG_TEXT:CHECKED
   TOG_EGEN  = TOG_EGEN:CHECKED
   TOG_P3    = TOG_P3:CHECKED
   TOG_KON   = TOG_KON:CHECKED
   TOG_KONVAL = TOG_KONVAL:CHECKED.
   
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
      ordningnr = 0.   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,WINDOW-3:HANDLE).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,CMB_VALORD:HANDLE IN FRAME FRAME-ORDN).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BRW_FRAN:HANDLE IN FRAME FRAME-ORDN).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_UPPORD:HANDLE IN FRAME FRAME-ORDN).
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,BTN_NERORD:HANDLE IN FRAME FRAME-ORDN).
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
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk
   list_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk
   berfortemp.OMRADE:LABEL IN BROWSE BRW_TAB1 = Guru.Konstanter:gomrk
   berfortemp.AVDNR:LABEL IN BROWSE BRW_TAB1= Guru.Konstanter:gavdk
   berfortemp.VIJUDID:LABEL IN BROWSE BRW_TAB1 = Guru.Konstanter:gjuk.
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,WINDOW-3:HANDLE).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,RAD_VAL:HANDLE IN FRAME FRAME-BARABREDD).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_LIST:HANDLE IN FRAME FRAME-BARABREDD).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_KONST:HANDLE IN FRAME FRAME-BARABREDD).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_ANDID:HANDLE IN FRAME FRAME-BARABREDD).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_LIN:HANDLE IN FRAME FRAME-BARABREDD).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_UPPL:HANDLE IN FRAME FRAME-BARABREDD).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_KOPI:HANDLE IN FRAME FRAME-BARABREDD).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_FRIKORT:HANDLE IN FRAME FRAME-BARABREDD).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_KSKAP:HANDLE IN FRAME FRAME-BARABREDD).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_SNABB:HANDLE IN FRAME FRAME-BARABREDD).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_OK:HANDLE IN FRAME FRAME-BARABREDD).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_ID:HANDLE IN FRAME FRAME-BARABREDD).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_AVB:HANDLE IN FRAME FRAME-BARABREDD).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_VAL:HANDLE IN FRAME FRAME-BARABREDD).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_SOLEN:HANDLE IN FRAME FRAME-BARABREDD).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_ORDUPP:HANDLE IN FRAME FRAME-BARABREDD).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_ORDNER:HANDLE IN FRAME FRAME-BARABREDD).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_ANM:HANDLE IN FRAME FRAME-BARABREDD).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_DOK:HANDLE IN FRAME FRAME-BARABREDD).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_KALK:HANDLE IN FRAME FRAME-BARABREDD).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_MTRL:HANDLE IN FRAME FRAME-BARABREDD).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,RECT-MENY:HANDLE IN FRAME FRAME-BARABREDD).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,RAD_MTRL:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_SATS:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_STOLP:HANDLE IN FRAME FRAME-MTRL).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_HLEV:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_MTRL:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_OVER:HANDLE IN FRAME FRAME-MTRL).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_BACK:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_LEV:HANDLE IN FRAME FRAME-MTRL).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,CMB_LEV:HANDLE IN FRAME FRAME-MTRL).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-ENR:HANDLE IN FRAME FRAME-MTRL).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-KOD:HANDLE IN FRAME FRAME-MTRL).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-BEN:HANDLE IN FRAME FRAME-MTRL).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_NY:HANDLE IN FRAME FRAME-MTRL).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_ANTAL:HANDLE IN FRAME FRAME-MTRL).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_SATS:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_TEXT:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_PAR:HANDLE IN FRAME FRAME-MTRL).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,RAD_SOK:HANDLE IN FRAME FRAME-MTRL).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_NOLL:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_ALLTMTRL:HANDLE IN FRAME FRAME-MTRL).    
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_SPEC:HANDLE IN FRAME FRAME-MTRL).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_LINKAB:HANDLE IN FRAME FRAME-MTRL).      
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-SOKALT:HANDLE IN FRAME FRAME-MTRL).  
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,RECT-22:HANDLE IN FRAME FRAME-MTRL).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,IMAGE-4:HANDLE IN FRAME FRAME-MTRL).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_KALK:HANDLE IN FRAME FRAME-KALK).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_NYKOD:HANDLE IN FRAME FRAME-KALK).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_BORT2:HANDLE IN FRAME FRAME-KALK).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_ANTAL2:HANDLE IN FRAME FRAME-KALK).      
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_BILD:HANDLE IN FRAME FRAME-BILD).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_NYA:HANDLE IN FRAME FRAME-BILD).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_BORTA:HANDLE IN FRAME FRAME-BILD).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_VISA:HANDLE IN FRAME FRAME-BILD).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_ANM:HANDLE IN FRAME FRAME-ANM).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,ED_ANM:HANDLE IN FRAME FRAME-ANM).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_OVER-2:HANDLE IN FRAME FRAME-ANM).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_KOPPANM:HANDLE IN FRAME FRAME-ANM).      
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-TEXT:HANDLE IN FRAME FRAME-ANM).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,CMB_JURP:HANDLE IN FRAME FRAME-ID).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,CMB_AVD:HANDLE IN FRAME FRAME-ID).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,CMB_OMR:HANDLE IN FRAME FRAME-ID).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_HMT:HANDLE IN FRAME FRAME-ID).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_TAB1:HANDLE IN FRAME FRAME-ID).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-NR1:HANDLE IN FRAME FRAME-ID).      
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-IDNR1:HANDLE IN FRAME FRAME-ID).    
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_TAB2:HANDLE IN FRAME FRAME-ID).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-NR2:HANDLE IN FRAME FRAME-ID).      
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-IDNR2:HANDLE IN FRAME FRAME-ID).    
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-SUND:HANDLE IN FRAME FRAME-ID).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_TAB3:HANDLE IN FRAME FRAME-ID).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,TOG_A:HANDLE IN FRAME FRAME-ID).            
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,TOG_B:HANDLE IN FRAME FRAME-ID).            
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,TOG_C:HANDLE IN FRAME FRAME-ID).            
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-NR3:HANDLE IN FRAME FRAME-ID).      
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_ANL:HANDLE IN FRAME FRAME-ID).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_KOPPID:HANDLE IN FRAME FRAME-ID).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-TAB1:HANDLE IN FRAME FRAME-ID).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-ID1:HANDLE IN FRAME FRAME-ID).      
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-IDNR1LABEL:HANDLE IN FRAME FRAME-ID).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-TAB2:HANDLE IN FRAME FRAME-ID).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-ID2:HANDLE IN FRAME FRAME-ID).      
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-IDNR2LABEL:HANDLE IN FRAME FRAME-ID).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-SUNDLABEL:HANDLE IN FRAME FRAME-ID).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-A:HANDLE IN FRAME FRAME-ID).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-B:HANDLE IN FRAME FRAME-ID).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-C:HANDLE IN FRAME FRAME-ID).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-TAB3:HANDLE IN FRAME FRAME-ID).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,RAD_KOMP:HANDLE IN FRAME FRAME-KONST).         
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,CMB_VAL:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_NER:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_UPP:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_KON:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_KB2:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_B2:HANDLE IN FRAME FRAME-KONST).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_KB3:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_B3:HANDLE IN FRAME FRAME-KONST).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_KB4:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_B4:HANDLE IN FRAME FRAME-KONST).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_KB5:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_B5:HANDLE IN FRAME FRAME-KONST).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_KB6:HANDLE IN FRAME FRAME-KONST).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_B6:HANDLE IN FRAME FRAME-KONST).           
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-B1:HANDLE IN FRAME FRAME-KONST).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-B2:HANDLE IN FRAME FRAME-KONST).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-B3:HANDLE IN FRAME FRAME-KONST).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-B4:HANDLE IN FRAME FRAME-KONST).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-B5:HANDLE IN FRAME FRAME-KONST).       
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-B6:HANDLE IN FRAME FRAME-KONST).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlB2:HANDLE IN MENU POPUP-MENU-BRW_B2).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildB2:HANDLE IN MENU POPUP-MENU-BRW_B2).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_AvmarkeraB2:HANDLE IN MENU POPUP-MENU-BRW_B2).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlB3:HANDLE IN MENU POPUP-MENU-BRW_B3).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildB3:HANDLE IN MENU POPUP-MENU-BRW_B3).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_AvmarkeraB3:HANDLE IN MENU POPUP-MENU-BRW_B3).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlB4:HANDLE IN MENU POPUP-MENU-BRW_B4).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildB4:HANDLE IN MENU POPUP-MENU-BRW_B4).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_AvmarkeraB4:HANDLE IN MENU POPUP-MENU-BRW_B4).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlB5:HANDLE IN MENU POPUP-MENU-BRW_B5).
   ordningnr = ordningnr + 1.                
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildB5:HANDLE IN MENU POPUP-MENU-BRW_B5).
   ordningnr = ordningnr + 1.                
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_AvmarkeraB5:HANDLE IN MENU POPUP-MENU-BRW_B5).
   ordningnr = ordningnr + 1.                
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlB6:HANDLE IN MENU POPUP-MENU-BRW_B6).
   ordningnr = ordningnr + 1.                
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildB6:HANDLE IN MENU POPUP-MENU-BRW_B6).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_AvmarkeraB6:HANDLE IN MENU POPUP-MENU-BRW_B6).
   ordningnr = ordningnr + 1.                
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationHLEV:HANDLE IN MENU POPUP-MENU-BRW_HLEV). 
   ordningnr = ordningnr + 1.                
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Pris_information2:HANDLE IN MENU POPUP-MENU-BRW_HLEV).
   ordningnr = ordningnr + 1.                
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlKB2:HANDLE IN MENU POPUP-MENU-BRW_KB2). 
   ordningnr = ordningnr + 1.                
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildKB2:HANDLE IN MENU POPUP-MENU-BRW_KB2). 
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlKB3:HANDLE IN MENU POPUP-MENU-BRW_KB3). 
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildKB3:HANDLE IN MENU POPUP-MENU-BRW_KB3). 
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlKB4:HANDLE IN MENU POPUP-MENU-BRW_KB4). 
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildKB4:HANDLE IN MENU POPUP-MENU-BRW_KB4).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlKB5:HANDLE IN MENU POPUP-MENU-BRW_KB5).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildKB5:HANDLE IN MENU POPUP-MENU-BRW_KB5).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_mtrlKB6:HANDLE IN MENU POPUP-MENU-BRW_KB6).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_bildKB6:HANDLE IN MENU POPUP-MENU-BRW_KB6).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_mtrl:HANDLE IN MENU POPUP-MENU-BRW_KON). 
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_bild:HANDLE IN MENU POPUP-MENU-BRW_KON).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationBRW_MTRL:HANDLE IN MENU POPUP-MENU-BRW_MTRL).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationSATS:HANDLE IN MENU POPUP-MENU-BRW_SATS).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Pris_information:HANDLE IN MENU POPUP-MENU-BRW_SATS). 
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationSTOLP:HANDLE IN MENU POPUP-MENU-BRW_STOLP).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Visa_informationVAL:HANDLE IN MENU POPUP-MENU-BRW_VAL).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-ANM:HANDLE).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BARABREDD:HANDLE). 
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BILD:HANDLE).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-ID:HANDLE).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-KALK:HANDLE).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-KONST:HANDLE).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-MTRL:HANDLE).          
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,list_mtrl.ANTAL:HANDLE IN BROWSE BRW_MTRL).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,FILL-IN-SKAPMTRL:HANDLE IN FRAME FRAME-BARABREDD).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,MBTN_ATG:HANDLE IN FRAME FRAME-BARABREDD).     
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,TOG_RATT:HANDLE IN FRAME FRAME-KONST).        
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,TOG_VISAGRUPP:HANDLE IN FRAME FRAME-KONST).        
   
   
   /*
   ordningnr = ordningnr + 1.  
   RUN whandle_UI (INPUT ordningnr,MENU POPUP-MENU-BRW_B3:HANDLE IN FRAME FRAME-KONST).
   ordningnr = ordningnr + 1.  
   RUN whandle_UI (INPUT ordningnr,MENU POPUP-MENU-BRW_B4:HANDLE IN BROWSE BRW_B4).
   */
   RUN BERVALSW.P PERSISTENT SET btnbervalh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,INPUT TABLE widgettemp).                                         
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
  ENABLE BTN_OK RECT-MENY MBTN_ATG BTN_AVB MBTN_ANDID MBTN_FRIKORT MBTN_KALK 
         MBTN_KOPI MBTN_KSKAP MBTN_LIN MBTN_LIST MBTN_SNABB BTN_SOLEN 
         BTN_ORDUPP BTN_ORDNER MBTN_UPPL MBTN_ANM MBTN_DOK MBTN_ID MBTN_KONST 
         MBTN_MTRL 
      WITH FRAME FRAME-BARABREDD IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BARABREDD}
  DISPLAY RAD_MTRLSM CMB_LEVSM FILL-IN-KODSM FILL-IN-ENRSM RAD_SOKSM 
          FILL-IN-B2SM FILL-IN-B3SM FILL-IN-F2SM FILL-IN-F3SM FILL-IN-SOKALTSM 
      WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
  ENABLE BRW_KABSM RAD_MTRLSM BTN_OVERSM BTN_BACKSM BTN_LEVSM CMB_LEVSM 
         FILL-IN-KODSM FILL-IN-ENRSM FILL-IN-BENSM RAD_SOKSM BTN_NYSM 
         BTN_ANTALSM BTN_SATSSM BTN_TEXTSM BTN_NOLLSM BTN_ALLTMTRLSM BTN_SPECSM 
         BTN_LINKABSM FILL-IN-B2SM FILL-IN-B3SM FILL-IN-F2SM FILL-IN-F3SM 
         FILL-IN-SOKALTSM RECT-10 IMAGE-6 BRW_SATSSM BRW_HLEVSM BRW_STOLPSM 
         BRW_MTRLSM 
      WITH FRAME FRAME-SKAPMTRL IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-SKAPMTRL}
  DISPLAY SEL_UPPATG 
      WITH FRAME FRAME-ATG IN WINDOW WINDOW-3.
  ENABLE SEL_UPPATG BTN_KORATG BTN_AVBATG 
      WITH FRAME FRAME-ATG IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ATG}
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
  DISPLAY CMB_KORTVAL RAD_VALFRI FILL-IN-TYP FILL-IN-SADR FILL-IN-KABEL 
          FILL-IN-METER FILL-IN-ADR CMB_TYP FILL-IN-APPARAT FILL-IN-AR 
          FILL-IN-SAK FILL-IN-MAX FILL-IN-ANMARK FILL-IN-KORT FILL-IN-B2 
          FILL-IN-B3 FILL-IN-F2 FILL-IN-F3 
      WITH FRAME FRAME-FRI IN WINDOW WINDOW-3.
  ENABLE CMB_KORTVAL RAD_VALFRI BRW_APP BRW_MTRLFRI BRW_KABFRI FBTN_VISAFRI 
         FBTN_SKRIVFRI BTN_EXKSKO FBTN_PDFFRI BTN_EXSTOLP BTN_EXMARK BTN_UPPFRI 
         BTN_NERFRI FILL-IN-TYP FILL-IN-SADR FILL-IN-KABEL FILL-IN-METER 
         FILL-IN-ADR CMB_TYP FILL-IN-APPARAT FILL-IN-GRUPPNR FILL-IN-AR 
         FILL-IN-SAK FILL-IN-MAX FILL-IN-ANMARK FBTN_SKAPAFRI FBTN_ANDFRI 
         FBTN_BORTFRI FILL-IN-B2 FILL-IN-B3 FILL-IN-F2 FILL-IN-F3 
      WITH FRAME FRAME-FRI IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-FRI}
  ENABLE BRW_KOMBKOPUPP FBTN_UPPKOPUPP FBTN_VISAKOPUPP FBTN_SKRIVKOPUPP 
         FBTN_KOMBKOPUPP FBTN_BKOMBKOPUPP FBTN_VISAKOPUPP-2 FBTN_SKRIVKOPUPP-2 
         BRW_UPPKOPUPP BTN_NYKOPUPP BTN_ANDRAKOPUPP BTN_BORTKOPUPP 
      WITH FRAME FRAME-KOPUPP IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KOPUPP}
  DISPLAY FILL-IN-KORTKAB FILL-IN-KOPSKAP FILL-IN-B1KAB FILL-IN-B3KAB 
          FILL-IN-B2KAB FILL-IN-F3KAB FILL-IN-F1KAB FILL-IN-F2KAB FILL-IN-KOPKAB 
          FILL-IN-NR1KAB FILL-IN-NR2KAB FILL-IN-NR3KAB FILL-IN-LEDKAB 
      WITH FRAME FRAME-KABSKAP IN WINDOW WINDOW-3.
  ENABLE BRW_KABKAB FBTN_ANTKAB BTN_UPPKAB FBTN_KORTKAB FBTN_MTRLKAB BTN_NERKAB 
         BTN_BORTKAB BTN_TYP1KAB BTN_TYP2KAB BTN_TYP3KAB BRW_SKAPKAB 
         BTN_PLUSKAB BTN_MINKAB BTN_KSKAPKAB FILL-IN-B1KAB FILL-IN-B3KAB 
         FILL-IN-B2KAB FILL-IN-F3KAB FILL-IN-F1KAB FILL-IN-F2KAB FILL-IN-KOPKAB 
         FILL-IN-NR1KAB FILL-IN-NR2KAB FILL-IN-NR3KAB FILL-IN-LEDKAB 
      WITH FRAME FRAME-KABSKAP IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KABSKAP}
  DISPLAY CMB_VALLIN CMB_VAL2LIN FILL-IN-LINKAB 
      WITH FRAME FRAME-LIN IN WINDOW WINDOW-3.
  ENABLE CMB_VALLIN CMB_VAL2LIN BRW_FRANLIN BRW_TILLLIN BTN_LINUPP FBTN_VISALIN 
         FBTN_SCHAKT FBTN_KORTLIN BTN_VISA-2LIN BRW_LIN BTN_SKAPALIN 
         BTN_BORTLIN BTN_AVBLIN 
      WITH FRAME FRAME-LIN IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-LIN}
  DISPLAY CMB_VALORD 
      WITH FRAME FRAME-ORDN IN WINDOW WINDOW-3.
  ENABLE CMB_VALORD BRW_FRAN BTN_UPPORD BTN_NERORD 
      WITH FRAME FRAME-ORDN IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ORDN}
  DISPLAY CMB_VAL TOG_VISAGRUPP RAD_KOMP TOG_RATT FILL-IN-B1 FILL-IN-B2 
          FILL-IN-B3 FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 
      WITH FRAME FRAME-KONST IN WINDOW WINDOW-3.
  ENABLE CMB_VAL TOG_VISAGRUPP RAD_KOMP BTN_NER BTN_UPP TOG_RATT BRW_KB2 BRW_B2 
         BRW_KB3 BRW_B3 BRW_B4 BRW_KB4 BRW_KB5 BRW_B5 BRW_KB6 BRW_B6 FILL-IN-B1 
         FILL-IN-B2 FILL-IN-B3 FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 
      WITH FRAME FRAME-KONST IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KONST}
  DISPLAY CMB_JURP CMB_AVD CMB_OMR FILL-IN-SUND TOG_A TOG_B TOG_C 
          FILL-IN-IDNR1LABEL FILL-IN-IDNR2LABEL FILL-IN-SUNDLABEL FILL-IN-A 
          FILL-IN-B FILL-IN-C 
      WITH FRAME FRAME-ID IN WINDOW WINDOW-3.
  ENABLE CMB_JURP CMB_AVD CMB_OMR BTN_HMT FILL-IN-NR1 BRW_TAB1 FILL-IN-NR2 
         BRW_TAB2 FILL-IN-NR3 BRW_TAB3 BTN_ANL FILL-IN-IDNR1 FILL-IN-IDNR2 
         FILL-IN-SUND TOG_A TOG_B TOG_C BTN_KOPPID FILL-IN-TAB1 FILL-IN-ID1 
         FILL-IN-IDNR1LABEL FILL-IN-TAB2 FILL-IN-ID2 FILL-IN-IDNR2LABEL 
         FILL-IN-SUNDLABEL FILL-IN-A FILL-IN-B FILL-IN-C FILL-IN-TAB3 
      WITH FRAME FRAME-ID IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ID}
  ENABLE BRW_KALK BTN_NYKOD BTN_BORT2 BTN_ANTAL2 
      WITH FRAME FRAME-KALK IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KALK}
  ENABLE BRW_BILD BTN_NYA BTN_BORTA BTN_VISA 
      WITH FRAME FRAME-BILD IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BILD}
  DISPLAY RAD_MTRL CMB_LEV FILL-IN-ENR FILL-IN-KOD RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-MTRL IN WINDOW WINDOW-3.
  ENABLE RAD_MTRL BTN_OVER BTN_BACK BTN_LEV CMB_LEV FILL-IN-ENR FILL-IN-KOD 
         FILL-IN-BEN RAD_SOK BTN_NY BTN_ANTAL BTN_SATS BTN_TEXT BTN_PAR 
         BTN_NOLL BTN_ALLTMTRL BTN_SPEC BTN_LINKAB FILL-IN-SOKALT IMAGE-4 
         BRW_HLEV BRW_SATS BRW_STOLP BRW_MTRL 
      WITH FRAME FRAME-MTRL IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MTRL}
  ENABLE BRW_ANM BTN_OVER-2 ED_ANM BTN_KOPPANM FILL-IN-TEXT 
      WITH FRAME FRAME-ANM IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANM}
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
   ASSIGN
   FRAME FRAME-ORDN:HIDDEN = vilkenframe
   FRAME FRAME-FRI:HIDDEN = vilkenframe
   FRAME FRAME-KABSKAP:HIDDEN = vilkenframe
   FRAME FRAME-SKAPMTRL:HIDDEN = vilkenframe
   FRAME FRAME-LIN:HIDDEN = vilkenframe
   FRAME FRAME-LINUPP:HIDDEN = vilkenframe
   FRAME FRAME-KOPUPP:HIDDEN = vilkenframe   
   FRAME FRAME-ATG:HIDDEN = vilkenframe.   
   FRAME FRAME-LISTOR:HIDDEN = vilkenframe.   
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
   IF TRIM(PROVERSION) BEGINS "9.1c" THEN DO:       
      IF vilkenframe = "KSKAP" THEN.
      ELSE DO:
         FRAME FRAME-KONST:HIDDEN = FALSE.
         FRAME FRAME-KABSKAP:HIDDEN = TRUE.   
         IF VALID-HANDLE(btnkskaph) THEN DO:
            RUN borthandE_UI IN btnkskaph.
            DELETE PROCEDURE btnkskaph NO-ERROR. 
            btnkskaph = ?.
         END.                   
      END.
      IF vilkenframe = "SKAPMAT" THEN. 
      ELSE DO:
         FRAME FRAME-KONST:HIDDEN = FALSE.
         FRAME FRAME-SKAPMTRL:HIDDEN = TRUE.   
         IF VALID-HANDLE(btnkskapmtrlh) THEN DO:
            RUN borthandE_UI IN btnkskapmtrlh.
            DELETE PROCEDURE btnkskapmtrlh NO-ERROR. 
            btnkskapmtrlh = ?.
         END.                   
      END.      
      /*
      IF vilkenframe = "KSKAP" OR vilkenframe = "SKAPMAT" THEN.
      ELSE DO:
         IF VALID-HANDLE(btnkskaph) THEN DO:
            RUN borthandE_UI IN btnkskaph.
            DELETE PROCEDURE btnkskaph NO-ERROR. 
            btnkskaph = ?.
         END.                   
         IF VALID-HANDLE(btnkskapmtrlh) THEN DO:
            RUN borthandE_UI IN btnkskapmtrlh.
            DELETE PROCEDURE btnkskapmtrlh NO-ERROR. 
            btnkskapmtrlh = ?.
         END.                   
      END.      
      */
   END.
   RUN radval_UI IN btnbervalh (INPUT 99).
   ASSIGN
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
   IF vad = "VAL" THEN DO:
      ASSIGN
      kon_val.F1:LABEL IN BROWSE BRW_VAL = f1var
      kon_val.F2:LABEL IN BROWSE BRW_VAL = f2var  
      kon_val.F3:LABEL IN BROWSE BRW_VAL = f3var
      kon_val.F4:LABEL IN BROWSE BRW_VAL = f4var      
      kon_val.F5:LABEL IN BROWSE BRW_VAL = f5var      
      kon_val.F6:LABEL IN BROWSE BRW_VAL = f6var.
      IF Guru.Konstanter:globforetag = "SUND" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      END.
      ELSE DO:
         kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END. 
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
/*
      kombtemp.XKORD:LABEL IN BROWSE BRW_KOMBKOPUPP = f8var.  
      */
   END.
   
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
   {NOLABEL.I}
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
            RUN leavefill_UI IN btnkskapmtrlh.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI WINDOW-3 
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

