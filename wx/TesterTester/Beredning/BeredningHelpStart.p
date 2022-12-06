
/*------------------------------------------------------------------------
    File        : BeredningHelpStart.p
    Purpose     : 

    Syntax      :run Modules\Beredning\BeredningHelpStart.p ("KabelAnd").

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/
&Scoped-define SHARED SHARED 
&Scoped-define PUBLIC 
{LISTMTRL.I}
{KONVALTEMP.I}
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
{FRITEMP.I}
{MARKGRUPP.I}
{BEREDTH.i}
{BEREDTT.i}
{BEREDTTTH.i}

DEFINE SHARED VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE BUFFER konsttempbuff FOR konsttemp.
DEFINE VARIABLE gruppkod AS INTEGER NO-UNDO.
DEFINE VARIABLE barea AS CHARACTER NO-UNDO.
DEFINE VARIABLE karea AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  

PROCEDURE kabeland_UI :
   DEFINE INPUT  PARAMETER typkod AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER gruppkodIN AS INTEGER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER numvar AS INTEGER NO-UNDO.
   
   
   DEFINE VARIABLE kabeland   AS Modules.Beredning.KabelAnd NO-UNDO.
   
   DEFINE VARIABLE startkabelandroot AS Guru.Root NO-UNDO.
   gruppkod = gruppkodIN.
   
   EMPTY TEMP-TABLE andrakon_val NO-ERROR. 
   EMPTY TEMP-TABLE konsttempTT NO-ERROR. 
   EMPTY TEMP-TABLE bbenamntempTT NO-ERROR.
   EMPTY TEMP-TABLE konstvaltempTT NO-ERROR. 
   FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = gruppkod NO-LOCK NO-ERROR.
   FIND FIRST kon_val WHERE kon_val.NUM = numvar NO-LOCK NO-ERROR.
   FOR EACH kon_val WHERE kon_val.NUM = numvar:
      IF kon_val.SKAPNUM = 0 /*OR kon_val.SKAPNUM = 99*/ THEN .
      ELSE DO:
         CREATE andrakon_val.
         BUFFER-COPY kon_val TO andrakon_val.   
         andrakon_val.TTRECID = RECID(andrakon_val).
      END.   
   END.
   
   FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = gruppkod NO-LOCK NO-ERROR.
   CREATE bbenamntempTT.
   BUFFER-COPY bbenamntemp TO bbenamntempTT.
   bbenamntempTT.TTRECID = RECID(bbenamntempTT).
   FIND FIRST konsttemp WHERE konsttemp.KONSKOD = gruppkod AND konsttemp.KTYPKOD = typkod NO-LOCK NO-ERROR.
   
   CREATE konsttempTT.
   BUFFER-COPY konsttemp TO konsttempTT.
   konsttempTT.TTRECID = RECID(konsttempTT).
   FOR EACH konstvaltemp WHERE konstvaltemp.KONSKOD = gruppkod AND konstvaltemp.KTYPKOD = konsttemp.KTYPKOD AND konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = bbenamntemp.B2: 
      RUN konsttval_UI.
   END.     
   FOR EACH konstvaltemp WHERE konstvaltemp.KONSKOD = gruppkod AND konstvaltemp.KTYPKOD = konsttemp.KTYPKOD AND konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = bbenamntemp.B3: 
      RUN konsttval_UI.    
   END.
   FOR EACH konstvaltemp WHERE konstvaltemp.KONSKOD = gruppkod AND konstvaltemp.KTYPKOD = konsttemp.KTYPKOD AND konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = bbenamntemp.B4: 
      RUN konsttval_UI.
   END.
   FOR EACH konstvaltemp WHERE konstvaltemp.KONSKOD = gruppkod AND konstvaltemp.KTYPKOD = konsttemp.KTYPKOD AND konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = bbenamntemp.B5: 
      RUN konsttval_UI.
   END.
   FOR EACH konstvaltemp WHERE konstvaltemp.KONSKOD = gruppkod AND konstvaltemp.KTYPKOD = konsttemp.KTYPKOD AND konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = bbenamntemp.B6: 
      RUN konsttval_UI.
   END.
   
   startkabelandroot = NEW Guru.Root().
   startkabelandroot:StartKabelAndDb().
   startkabelandroot:StartKabelAnd(THIS-PROCEDURE).
  
   kabeland = NEW Modules.Beredning.KabelAnd(INPUT startkabelandroot,INPUT  konstgrptemp.BENAMNING, INPUT gruppkod).
   WAIT-FOR kabeland:ShowDialog().
   
   
   FOR EACH andrakon_val WHERE NO-LOCK:
      FIND FIRST kon_val WHERE kon_val.NUM = andrakon_val.NUM AND kon_val.SKAPNUM = andrakon_val.SKAPNUM NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kon_val THEN DO:
         CREATE kon_val.
      END.
      BUFFER-COPY andrakon_val TO kon_val.
   END.
   
   FOR EACH kon_val WHERE kon_val.NUM = numvar:
      IF kon_val.SKAPNUM = 0 /*OR kon_val.SKAPNUM = 99*/ THEN .
      ELSE DO:
         FIND FIRST andrakon_val WHERE andrakon_val.NUM = kon_val.NUM AND andrakon_val.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
         IF NOT AVAILABLE andrakon_val THEN DO:
            FOR EACH list_mtrl WHERE list_mtrl.NUM = kon_val.NUM AND list_mtrl.SKAPNUM = kon_val.SKAPNUM NO-LOCK:
               DELETE list_mtrl.
            END.
            FOR EACH fri_temp WHERE fri_temp.NUM = kon_val.NUM AND  fri_temp.KABNR = kon_val.SKAPNUM NO-LOCK:
               DELETE fri_temp.
            END.
            
            DELETE kon_val.
         END.         
      END.   
   END.
  
   
   
   DELETE OBJECT startkabelandroot NO-ERROR.
   DELETE OBJECT kabeland NO-ERROR.
END PROCEDURE.
PROCEDURE gruppkod_UI :
   DEFINE OUTPUT PARAMETER gruppout AS INTEGER NO-UNDO.
   gruppout = gruppkod.
END PROCEDURE.
PROCEDURE konsttval_UI:
   CREATE konstvaltempTT.
   BUFFER-COPY konstvaltemp TO konstvaltempTT.
   konstvaltempTT.TTRECID = RECID(konstvaltempTT).
   
END PROCEDURE.

PROCEDURE Kabelandtth_UI :
   DEFINE OUTPUT PARAMETER valber AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER valomr AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER KonstTThout AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER bbenamntempTThout AS HANDLE NO-UNDO.
  
   DEFINE OUTPUT PARAMETER TABLE FOR KonstvaltempTT.
   DEFINE OUTPUT PARAMETER KskKon_valTThout AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER Fri_tempTThout AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER List_mtrlTThout AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER Markgrupp_tempTThout AS HANDLE NO-UNDO.
   ASSIGN 
   valber = valaonr 
   valomr = valomrade  
   KonstTThout        = KonstTTh       
   bbenamntempTThout  = bbenamntempTTh 
  
   KskKon_valTThout = KskKon_valTTh.
   Fri_tempTThout = Fri_tempTTh. 
   Markgrupp_tempTThout =  Markgrupp_tempTTh.    
   List_mtrlTThout = List_mtrlTTh.  
   RETURN.
   
END PROCEDURE.
PROCEDURE KonstvaltempHmt_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR Konstvaltemp.
END PROCEDURE.
PROCEDURE stationkab_UI :  
   DEFINE INPUT  PARAMETER kvalkodvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lsphsp AS CHARACTER NO-UNDO.
   FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 14 AND
   konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
   IF AVAILABLE konsttempbuff THEN DO:
      lsphsp = "lsp".      
   END.          
   ELSE lsphsp = "hsp".                              
END PROCEDURE.
 