   /*GAMBEREV.p*/
/*HÄMTAR konstruktioner berval*/
{KONVALTEMP.I}    
{KONID.I}   
{LISTMTRL.I}   
{KOPPLINA.I}    
{FRITEMP.I}        
/*
{PUNKTTEM.I}
  
{SCHAKTTE.I}    
{SKYDDTEM.I}
     
{KABTEMP.I}
*/
{KALKTEMP2.I}
{BILDBERTEMP.I}

DEFINE VARIABLE bsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE diranv AS CHARACTER NO-UNDO.             
DEFINE TEMP-TABLE ord_temp
  FIELD NUM LIKE BERORD.NUM
  FIELD ORD LIKE BERORD.ORD
  INDEX NUM NUM ASCENDING.

DEFINE BUFFER konbuff FOR kon_val.   
DEFINE INPUT  PARAMETER globanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE INPUT PARAMETER datvar LIKE BERMTRL.DATUM NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR kon_val.
DEFINE OUTPUT PARAMETER TABLE FOR kon_id.
DEFINE OUTPUT PARAMETER TABLE FOR kopp_lina.
DEFINE OUTPUT PARAMETER TABLE FOR fri_temp.
/*
DEFINE OUTPUT PARAMETER TABLE FOR punkt_temp.

DEFINE OUTPUT PARAMETER TABLE FOR schakt_temp.
DEFINE OUTPUT PARAMETER TABLE FOR skydd_temp.

DEFINE OUTPUT PARAMETER TABLE FOR kab_temp.
*/
DEFINE OUTPUT PARAMETER TABLE FOR bildbertemp.
DEFINE OUTPUT PARAMETER TABLE FOR berid2temp.
DEFINE VARIABLE gfore AS CHARACTER NO-UNDO.
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
gfore = FORETAG.FORETAG.
/*VALDA KONSTRUKTIONER*/   
FOR EACH BERORD WHERE BERORD.AONR = valaonr AND
BERORD.OMRADE = valomrade NO-LOCK:
   CREATE ord_temp.
   BUFFER-COPY BERORD TO ord_temp.
   
END.          
DEBUGGER:SET-BREAK().
FOR EACH BERVAL WHERE BERVAL.AONR = valaonr AND BERVAL.OMRADE = valomrade NO-LOCK:
   CREATE kon_val.
   BUFFER-COPY BERVAL TO kon_val.
   ASSIGN 
   kon_val.GRUPP = BERVAL.KONSKOD 
   kon_val.F1 = BERVAL.KTYPKOD.
   IF kon_val.KSKAP = FALSE THEN DO:
      FIND FIRST ord_temp WHERE ord_temp.NUM = kon_val.NUM
      USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE ord_temp THEN DO:
         kon_val.ORD = ord_temp.ORD.
      END.
   END.      
END.          
/*MATERIEL*/
/*
FIND FIRST PARSTOLP NO-LOCK NO-ERROR.
FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND BERMTRL.OMRADE = valomrade AND BERMTRL.INKOP = FALSE AND
BERMTRL.DATUM = datvar NO-LOCK:
   CREATE list_mtrl.
   BUFFER-COPY BERMTRL TO list_mtrl.
   list_mtrl.ENHET = LC(list_mtrl.ENHET).
   IF list_mtrl.PAR > 0 THEN DO:
      IF list_mtrl.PAR = 1 THEN list_mtrl.PAR2 = PARSTOLP.A.
      ELSE IF list_mtrl.PAR = 2 THEN list_mtrl.PAR2 = PARSTOLP.B.
      ELSE list_mtrl.PAR2 = PARSTOLP.C.
   END.            
END.   
*/
/*INDENTIFIKATION*/

DEBUGGER:SET-BREAK().
FOR EACH BERID WHERE BERID.AONR = valaonr AND BERID.OMRADE = valomrade NO-LOCK.
   FIND FIRST kon_val WHERE kon_val.NUM = BERID.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      CREATE kon_id.
      BUFFER-COPY BERID TO kon_id.
      ASSIGN      
      kon_id.GRUPP = kon_val.GRUPP.
   END.         
END. 
FOR EACH BERID2 WHERE BERID2.AONR = valaonr AND BERID2.OMRADE = valomrade NO-LOCK.
   CREATE berid2temp.
   BUFFER-COPY BERID2 TO berid2temp.       
END.

/*LINOR OCH KABLAR*/

FOR EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND
BERLINKAB.OMRADE = valomrade AND BERLINKAB.DATUM = datvar NO-LOCK:
   CREATE kopp_lina.
   BUFFER-COPY BERLINKAB TO kopp_lina.
   
END.

/*FRITT KORT*/
FOR EACH FRIKORT WHERE FRIKORT.AONR = valaonr AND FRIKORT.OMRADE = valomrade NO-LOCK:
   CREATE fri_temp.
   BUFFER-COPY FRIKORT TO fri_temp.
   IF fri_temp.ARTAL = ? THEN fri_temp.ARTAL = YEAR(TODAY).      
END.
/*
FOR EACH KSKYDD WHERE KSKYDD.AONR = valaonr AND KSKYDD.OMRADE = valomrade AND KSKYDD.DATUM = datvar AND KSKYDD.BERED = TRUE NO-LOCK:
   CREATE skydd_temp.
   BUFFER-COPY KSKYDD TO skydd_temp.
   skydd_temp.NY = FALSE.         
END.
*/

FOR EACH EXTRADATA WHERE EXTRADATA.PROGRAM = "BERBILD" AND EXTRADATA.HUVUDINT = INTEGER(valaonr) AND 
EXTRADATA.HUVUDCH = valomrade NO-LOCK:
   CREATE bildbertemp.
   ASSIGN
   bildbertemp.NUM = EXTRADATA.SOKINT[1]
   bildbertemp.NAMN = EXTRADATA.SOKCHAR[1]
   bildbertemp.FILNAMN = EXTRADATA.SOKCHAR[2].
   IF gfore = "KRAF" THEN DO:
      
      IF bildbertemp.FILNAMN BEGINS "c:\users\" THEN DO:
         diranv = Guru.Konstanter:globanv.
         {MOLNETMAPPEXTRA.I}
         bsok = SUBSTRING(bildbertemp.FILNAMN,10,INDEX(bildbertemp.FILNAMN,"\",10) - 10).
         bildbertemp.FILNAMN = REPLACE(bildbertemp.FILNAMN,bsok,diranv).  
      END.   
   END.   
END.


DEBUGGER:SET-BREAK().
FOR EACH kon_val WHERE kon_val.KSKAP = FALSE BY kon_val.NUM:
   IF kon_val.ID = TRUE THEN DO:
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM
      USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE kon_id THEN DO:
         IF kon_id.FRI2 = ? THEN kon_val.ID2 = kon_id.NATNR.
         ELSE kon_val.ID2 = STRING(kon_id.FRI2).    
         FIND FIRST konbuff WHERE konbuff.NUM = kon_val.NUM AND
         konbuff.KSKAP = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE konbuff THEN DO:
            kon_val.EXTRA = "+" + " " + kon_id.FRI3.
         END.
         ELSE DO:
            kon_val.EXTRA = "  " + kon_id.FRI3.
         END.
      END.
   END.
   ELSE DO:
      FIND FIRST konbuff WHERE konbuff.NUM = kon_val.NUM AND
      konbuff.KSKAP = TRUE NO-LOCK NO-ERROR.
      IF AVAILABLE konbuff THEN kon_val.EXTRA = "+".
   END.
   ASSIGN
   kon_val.EXTRA1 = SUBSTRING(kon_val.EXTRA,3)
   kon_val.EXTRA2 = SUBSTRING(kon_val.EXTRA,1,1).
END.
/*   
FOR EACH kab_temp:
   IF kab_temp.KABNR NE 0 THEN DO:
      FIND FIRST kopp_lina WHERE kopp_lina.KABNR = kab_temp.KABNR
      NO-LOCK NO-ERROR.
      IF AVAILABLE kopp_lina THEN kab_temp.METER = kopp_lina.METER.
   END.   
END.   
*/