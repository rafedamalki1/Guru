/*INKKOPOWID.I*/
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
         

       
DEFINE SHARED VARIABLE inkbervar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE inkomrvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE inkaonrvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE inkdelnrvar AS INTEGER NO-UNDO.

{VARXMLDYNOE10.I}
{MTRLTEMP.I}

{EXTRADATA.I}

{LEVKONTAKTTEMP.I}
{DEPSEKINKTEMP.I}
{INKTEMP.I}
{KUNDNRTEMP.I}
{HUVLEVTEMP.I}
{HOPPSEK2W.I}
{LEVTEMP.I}
{ANMARKTEMP.I}
{PERSONALTEMP.I}
{ANMARKD.I}
{EXECLIN2.I}
{KONTAKTPERSTEMP.I}
/*?r dett r?tt???? Lena*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{DEPATEMP.I}
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER NO-UNDO.
DEFINE VARIABLE str0 AS CHARACTER NO-UNDO.
DEFINE VARIABLE str1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{WHANDLTEMP.I}
  
DEFINE VARIABLE spartidvar AS INTEGER NO-UNDO.
DEFINE VARIABLE Snabbspara AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chSnabbspara AS COMPONENT-HANDLE NO-UNDO.
   
   
DEFINE VARIABLE ordnr         AS INTEGER NO-UNDO.
DEFINE VARIABLE C-WIN         AS HANDLE NO-UNDO.

DEFINE VARIABLE DEFAULT-FRAME AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_MTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BEST AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ADD AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_JMFSALDO AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_GJORDA AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_SNABB AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_OK AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ATER AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_AVB AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-MTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ALLBACKMTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ALLOVERMTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BACKMTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_OVERMTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE RAD_UPPKONST AS HANDLE NO-UNDO.
DEFINE VARIABLE RAD_VALDMTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_MTRLBACK AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BYTLEV AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-BYTLEV AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-BYTLEV-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_LEVTILL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_INKANTAL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_MTRLTABORT AS HANDLE NO-UNDO.


DEFINE VARIABLE BTN_MTRLENDASTMARK AS HANDLE NO-UNDO.

DEFINE VARIABLE CMB_DEPVAL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ATERMTRL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ORGENR AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-GRANSKA AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LEVBORT AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_VISA AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_EHANDEL AS HANDLE NO-UNDO.

DEFINE VARIABLE BTN_MANBEST AS HANDLE NO-UNDO.


DEFINE VARIABLE FRAME-ADRESSER AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ANDRALEVK AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BORTLEVK AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_NYLEVK AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-STATUS AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-BESTNR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-IB AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-DEPKUNDNR AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-DEPFOR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-ALTFOR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-ALTFORTID AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-DATUM AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-DEPLEVADR AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-DEPLPNR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-DEPLORT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-DEPFAX AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-KIKONTAKT AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-KITELE AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-KIMOBIL AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-KIEPOST AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-KTKONTAKT AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-KTTELE AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-KTMOBIL AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-PLKONTAKT AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-PLMOBIL AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-GKONTAKT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-GADR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-GPNR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-GORT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-GTELE AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-GFAX AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-MARK AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-KOM AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_KAB AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-AVIS AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-AVISPERS AS HANDLE NO-UNDO.
DEFINE VARIABLE m_Vlj_Personal AS HANDLE NO-UNDO.
DEFINE VARIABLE m_Vlj_Kundreferens AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_KOPARE AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-PNnummer AS HANDLE NO-UNDO.


DEFINE VARIABLE m_Vlj_Personal2 AS HANDLE NO-UNDO.
DEFINE VARIABLE m_Vlj_Personal3 AS HANDLE NO-UNDO.
DEFINE VARIABLE m_Vlj_Personal4 AS HANDLE NO-UNDO.
DEFINE VARIABLE m_Vlj_extern_person AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-LISTOR   AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_LEV1       AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_LEV2       AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_LEV3       AS HANDLE NO-UNDO.
DEFINE VARIABLE SEL_LISTOR     AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_LEVBYTENR  AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-ENRBYT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-BENBYT AS HANDLE NO-UNDO.
DEFINE VARIABLE RAD_SOK        AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LBYTENR    AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LBORT      AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LBORTALLT AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LVISA      AS HANDLE NO-UNDO.
DEFINE VARIABLE RECT-SOK       AS HANDLE NO-UNDO.
DEFINE VARIABLE IMAGE-SOK      AS HANDLE NO-UNDO.
DEFINE VARIABLE RAD_OPT AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_VEJT AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_VHU AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LEHANDEL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_LMANBEST AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_STATUSKOLL AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ADRESSKOLL AS HANDLE NO-UNDO.
DEFINE VARIABLE EDIT_LINFO AS HANDLE NO-UNDO.
DEFINE VARIABLE TOG_FAVOAD AS HANDLE NO-UNDO.
DEFINE VARIABLE RAD_PROJ AS HANDLE NO-UNDO.


DEFINE VARIABLE  FRAME-BYTENR AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_BYTENRBE AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-ENRBYTBE AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-BENBYTBE AS HANDLE NO-UNDO.
DEFINE VARIABLE RAD_SOKBE AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_BYTENRBE AS HANDLE NO-UNDO.
DEFINE VARIABLE RECT-SOKBE AS HANDLE NO-UNDO.
DEFINE VARIABLE IMAGE-SOKBE AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_ATERBE AS HANDLE NO-UNDO.


DEFINE VARIABLE FILL-IN-STATUSTID AS HANDLE NO-UNDO. 
DEFINE VARIABLE FILL-IN-BESTNRTID    AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-PNnummerTID  AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-IBTID        AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-DEPKUNDNRTID AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-DEPFORTID    AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-DATUMTID     AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-DEPLEVADRTID AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-DEPLPNRTID   AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-DEPLORTTID   AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-DEPFAXTID    AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-KIKONTAKTTID AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-KITELETID    AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-KIMOBILTID   AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-KIEPOSTTID   AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-KTKONTAKTTID AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-KTTELETID    AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-KTMOBILTID   AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-PLKONTAKTTID AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-PLMOBILTID   AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-GKONTAKTTID  AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-GADRTID      AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-GPNRTID      AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-GORTTID      AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-GTELETID     AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-GFAXTID      AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-MARKTID      AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-KOMTID       AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-AVISTID      AS HANDLE NO-UNDO.  
DEFINE VARIABLE FILL-IN-AVISPERSTID  AS HANDLE NO-UNDO.  
DEFINE VARIABLE FRAME-TIDADRESSER    AS HANDLE NO-UNDO. 
DEFINE VARIABLE RAD_ADRESS AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-ALMAN    AS HANDLE NO-UNDO.


DEFINE VARIABLE FRAME-BERVAL AS HANDLE NO-UNDO.
DEFINE VARIABLE FRAME-MTRLVAL AS HANDLE NO-UNDO.

DEFINE VARIABLE FRAME-INKDIREKT AS HANDLE NO-UNDO.
DEFINE VARIABLE CMB_LEVDIREKT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-ENRDIREKT AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-BENDIREKT AS HANDLE NO-UNDO.
DEFINE VARIABLE RAD_SOKDIREKT AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_OVERMTRLDIREKT AS HANDLE NO-UNDO.
DEFINE VARIABLE RECT-SOKDIREKT AS HANDLE NO-UNDO.
DEFINE VARIABLE IMAGE-SOKDIREKT AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_KLAR-alm AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_AVB-alm AS HANDLE NO-UNDO.



DEFINE INPUT PARAMETER huvprogh      AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER frmsh AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR whandltemp.

framesizeh = frmsh.
IF Guru.Konstanter:appcon THEN DO:
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
END.
ELSE DO:
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
END.
FIND FIRST whandltemp NO-LOCK NO-ERROR.
IF AVAILABLE whandltemp THEN DO:
   ordnr = 1.
   RUN whout_UI (OUTPUT C-Win).
   RUN whout_UI (OUTPUT DEFAULT-FRAME).
   RUN whout_UI (OUTPUT BTN_MTRL).
   RUN whout_UI (OUTPUT BTN_BEST).
   RUN whout_UI (OUTPUT BTN_ADD).
   RUN whout_UI (OUTPUT BTN_JMFSALDO).
   RUN whout_UI (OUTPUT BTN_SNABB).
   
   RUN whout_UI (OUTPUT FRAME-MTRL).
   RUN whout_UI (OUTPUT BTN_ALLBACKMTRL).
   RUN whout_UI (OUTPUT BTN_ALLOVERMTRL).
   RUN whout_UI (OUTPUT BTN_BACKMTRL).
   RUN whout_UI (OUTPUT BTN_OVERMTRL).
   RUN whout_UI (OUTPUT RAD_UPPKONST).
   RUN whout_UI (OUTPUT RAD_VALDMTRL).
   RUN whout_UI (OUTPUT BTN_MTRLBACK).
   RUN whout_UI (OUTPUT BTN_BYTLEV).
   RUN whout_UI (OUTPUT FILL-IN-BYTLEV).
   RUN whout_UI (OUTPUT FILL-IN-BYTLEV-2).
   RUN whout_UI (OUTPUT CMB_LEVTILL).
   RUN whout_UI (OUTPUT BTN_INKANTAL).
   RUN whout_UI (OUTPUT BTN_MTRLTABORT).
   RUN whout_UI (OUTPUT BTN_ATERMTRL).
   RUN whout_UI (OUTPUT BTN_ORGENR).
   
   RUN whout_UI (OUTPUT FRAME-GRANSKA).
   RUN whout_UI (OUTPUT BTN_LEVBORT).
   RUN whout_UI (OUTPUT FRAME-ADRESSER).
   RUN whout_UI (OUTPUT BTN_ANDRALEVK).
   RUN whout_UI (OUTPUT BTN_BORTLEVK).
   RUN whout_UI (OUTPUT BTN_NYLEVK).
   RUN whout_UI (OUTPUT FILL-IN-STATUS).
   RUN whout_UI (OUTPUT FILL-IN-BESTNR).
   RUN whout_UI (OUTPUT FILL-IN-IB).
   RUN whout_UI (OUTPUT FILL-IN-DEPKUNDNR). 
   RUN whout_UI (OUTPUT FILL-IN-DEPFOR).
   RUN whout_UI (OUTPUT FILL-IN-DATUM).
   RUN whout_UI (OUTPUT FILL-IN-DEPLEVADR). 
   RUN whout_UI (OUTPUT FILL-IN-DEPLPNR).
   RUN whout_UI (OUTPUT FILL-IN-DEPLORT).
   RUN whout_UI (OUTPUT FILL-IN-DEPFAX).
   RUN whout_UI (OUTPUT FILL-IN-KIKONTAKT). 
   RUN whout_UI (OUTPUT FILL-IN-KITELE).
   RUN whout_UI (OUTPUT FILL-IN-KIMOBIL).
   RUN whout_UI (OUTPUT FILL-IN-KIEPOST).
   RUN whout_UI (OUTPUT FILL-IN-KTKONTAKT). 
   RUN whout_UI (OUTPUT FILL-IN-KTTELE).
   RUN whout_UI (OUTPUT FILL-IN-KTMOBIL).
   RUN whout_UI (OUTPUT FILL-IN-PLKONTAKT). 
   RUN whout_UI (OUTPUT FILL-IN-PLMOBIL).
   RUN whout_UI (OUTPUT FILL-IN-GKONTAKT).
   RUN whout_UI (OUTPUT FILL-IN-GADR).
   RUN whout_UI (OUTPUT FILL-IN-GPNR).
   RUN whout_UI (OUTPUT FILL-IN-GORT).
   RUN whout_UI (OUTPUT FILL-IN-GTELE).
   RUN whout_UI (OUTPUT FILL-IN-GFAX).
   RUN whout_UI (OUTPUT FILL-IN-MARK).
   RUN whout_UI (OUTPUT FILL-IN-KOM).
   RUN whout_UI (OUTPUT TOG_KAB).
   RUN whout_UI (OUTPUT FILL-IN-AVIS).
   RUN whout_UI (OUTPUT FILL-IN-AVISPERS).
   RUN whout_UI (OUTPUT m_Vlj_Personal).
   RUN whout_UI (OUTPUT m_Vlj_Kundreferens).
   
   RUN whout_UI (OUTPUT m_Vlj_Personal2).
   RUN whout_UI (OUTPUT m_Vlj_Personal3).
   RUN whout_UI (OUTPUT m_Vlj_Personal4).
   RUN whout_UI (OUTPUT m_Vlj_extern_person).
   
   RUN whout_UI (OUTPUT BTN_VISA).
   RUN whout_UI (OUTPUT BTN_EHANDEL).
   RUN whout_UI (OUTPUT BTN_MANBEST).
   RUN whout_UI (OUTPUT BTN_OK).
   RUN whout_UI (OUTPUT BTN_AVB).
   RUN whout_UI (OUTPUT FRAME-LISTOR).
   RUN whout_UI (OUTPUT CMB_LEV1).
   RUN whout_UI (OUTPUT CMB_LEV2).
   RUN whout_UI (OUTPUT CMB_LEV3).
   RUN whout_UI (OUTPUT SEL_LISTOR).
   RUN whout_UI (OUTPUT CMB_LEVBYTENR).
   RUN whout_UI (OUTPUT FILL-IN-ENRBYT).
   RUN whout_UI (OUTPUT FILL-IN-BENBYT).
   RUN whout_UI (OUTPUT RAD_SOK).
   RUN whout_UI (OUTPUT BTN_LBYTENR).
   RUN whout_UI (OUTPUT BTN_LVISA).
   RUN whout_UI (OUTPUT RECT-SOK).
   RUN whout_UI (OUTPUT IMAGE-SOK).
   RUN whout_UI (OUTPUT RAD_OPT).
   RUN whout_UI (OUTPUT TOG_VEJT).
   RUN whout_UI (OUTPUT TOG_VHU).
   RUN whout_UI (OUTPUT BTN_LEHANDEL).
   RUN whout_UI (OUTPUT BTN_LMANBEST).
   RUN whout_UI (OUTPUT BTN_ADRESSKOLL).
   RUN whout_UI (OUTPUT EDIT_LINFO).
   RUN whout_UI (OUTPUT BTN_LBORT).
   RUN whout_UI (OUTPUT BTN_LBORTALLT).
   RUN whout_UI (OUTPUT BTN_STATUSKOLL).
   RUN whout_UI (OUTPUT TOG_FAVOAD).
   RUN whout_UI (OUTPUT RAD_PROJ).
   RUN whout_UI (OUTPUT BTN_ATER).
   
   RUN whout_UI (OUTPUT FRAME-BYTENR).
   RUN whout_UI (OUTPUT CMB_BYTENRBE).
   RUN whout_UI (OUTPUT FILL-IN-ENRBYTBE).
   RUN whout_UI (OUTPUT FILL-IN-BENBYTBE).
   RUN whout_UI (OUTPUT RAD_SOKBE).
   RUN whout_UI (OUTPUT BTN_BYTENRBE).
   RUN whout_UI (OUTPUT RECT-SOKBE).
   RUN whout_UI (OUTPUT IMAGE-SOKBE).
   RUN whout_UI (OUTPUT BTN_ATERBE).
   RUN whout_UI (OUTPUT CMB_KOPARE).
   RUN whout_UI (OUTPUT FILL-IN-PNnummer).
    RUN whout_UI (OUTPUT FILL-IN-STATUSTID). 
    RUN whout_UI (OUTPUT FILL-IN-BESTNRTID).  
    RUN whout_UI (OUTPUT FILL-IN-PNnummerTID).  
    RUN whout_UI (OUTPUT FILL-IN-IBTID).  
    RUN whout_UI (OUTPUT FILL-IN-DEPKUNDNRTID).  
    RUN whout_UI (OUTPUT FILL-IN-DEPFORTID).  
    RUN whout_UI (OUTPUT FILL-IN-DATUMTID).  
    RUN whout_UI (OUTPUT  FILL-IN-DEPLEVADRTID).  
    RUN whout_UI (OUTPUT FILL-IN-DEPLPNRTID).  
    RUN whout_UI (OUTPUT FILL-IN-DEPLORTTID).  
    RUN whout_UI (OUTPUT FILL-IN-DEPFAXTID).  
    RUN whout_UI (OUTPUT FILL-IN-KIKONTAKTTID).  
    RUN whout_UI (OUTPUT FILL-IN-KITELETID).  
    RUN whout_UI (OUTPUT FILL-IN-KIMOBILTID).  
    RUN whout_UI (OUTPUT FILL-IN-KIEPOSTTID).  
    RUN whout_UI (OUTPUT FILL-IN-KTKONTAKTTID).  
    RUN whout_UI (OUTPUT FILL-IN-KTTELETID).  
    RUN whout_UI (OUTPUT FILL-IN-KTMOBILTID).  
    RUN whout_UI (OUTPUT FILL-IN-PLKONTAKTTID).  
    RUN whout_UI (OUTPUT FILL-IN-PLMOBILTID).  
    RUN whout_UI (OUTPUT FILL-IN-GKONTAKTTID).  
    RUN whout_UI (OUTPUT FILL-IN-GADRTID).  
    RUN whout_UI (OUTPUT FILL-IN-GPNRTID).  
    RUN whout_UI (OUTPUT FILL-IN-GORTTID).  
    RUN whout_UI (OUTPUT FILL-IN-GTELETID).  
    RUN whout_UI (OUTPUT FILL-IN-GFAXTID).  
    RUN whout_UI (OUTPUT FILL-IN-MARKTID).  
    RUN whout_UI (OUTPUT FILL-IN-KOMTID).  
    RUN whout_UI (OUTPUT FILL-IN-AVISTID).  
    RUN whout_UI (OUTPUT FILL-IN-AVISPERSTID).  
    RUN whout_UI (OUTPUT FRAME-TIDADRESSER).
    RUN whout_UI (OUTPUT RAD_ADRESS).
    RUN whout_UI (OUTPUT FRAME-ALMAN).  
    RUN whout_UI (OUTPUT BTN_GJORDA).  
      
    
    RUN whout_UI (OUTPUT BTN_MTRLENDASTMARK). 
    
       RUN whout_UI (OUTPUT CMB_DEPVAL).
    RUN whout_UI (OUTPUT FRAME-BERVAL).
    RUN whout_UI (OUTPUT FRAME-MTRLVAL). 
  
   RUN whout_UI (OUTPUT FRAME-INKDIREKT).
   RUN whout_UI (OUTPUT CMB_LEVDIREKT).
   RUN whout_UI (OUTPUT FILL-IN-ENRDIREKT).
   RUN whout_UI (OUTPUT FILL-IN-BENDIREKT).
   RUN whout_UI (OUTPUT RAD_SOKDIREKT). 
   RUN whout_UI (OUTPUT BTN_OVERMTRLDIREKT).
   RUN whout_UI (OUTPUT RECT-SOKDIREKT).
   RUN whout_UI (OUTPUT IMAGE-SOKDIREKT).
  
   RUN whout_UI (OUTPUT BTN_KLAR-alm).
   RUN whout_UI (OUTPUT BTN_AVB-alm).
   RUN whout_UI (OUTPUT FILL-IN-ALTFOR).
   RUN whout_UI (OUTPUT FILL-IN-ALTFORTID).
    
END.
IF VALID-HANDLE(C-Win) THEN DO:
   ON CTRL-S OF c-win ANYWHERE DO:
      RUN fbtnsnabb_UI IN THIS-PROCEDURE.
   END.
END.


PROCEDURE spartid_UI :
   /*
   FILE-INFO:FILE-NAME = TRIM(SUBSTRING(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"_UI") + 3)).
   RUN loggskap_UI IN logprogh (INPUT 2,INPUT SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)), INPUT "WCCH7", INPUT "").
   */
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?. 
    
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      spartidvar = extradatatemp.SOKINT[1].      
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.  
   edataapph = ?. 
END PROCEDURE.



PROCEDURE spartidh_UI :
   DEFINE OUTPUT PARAMETER spartidout AS INTEGER NO-UNDO.
   spartidout = spartidvar * 60 * 1000. 
  
END PROCEDURE.

PROCEDURE whout_UI :
   DEFINE OUTPUT PARAMETER wih AS HANDLE NO-UNDO.
   wih = whandltemp.WF[ordnr].
   ordnr = ordnr + 1.
END PROCEDURE.     