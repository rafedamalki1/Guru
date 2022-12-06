
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


DEFINE BUFFER konsttempbuff FOR konsttemp.
DEFINE VARIABLE gruppkod AS INTEGER NO-UNDO.
DEFINE VARIABLE barea AS CHARACTER NO-UNDO.
DEFINE VARIABLE karea AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  

PROCEDURE kabeland_UI :
   DEFINE INPUT  PARAMETER typkod AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER rubrikhelp AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER gruppkodIN AS INTEGER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER numvar AS INTEGER NO-UNDO.
   
   DEFINE VARIABLE kabeland   AS Modules.Beredning.KabelAnd NO-UNDO.
   
   DEFINE VARIABLE startkabelandroot AS Guru.Root NO-UNDO.
   gruppkod = gruppkodIN.
   
   EMPTY TEMP-TABLE kskkon_val NO-ERROR. 
   EMPTY TEMP-TABLE konsttempTT NO-ERROR. 
   EMPTY TEMP-TABLE bbenamntempTT NO-ERROR.
   EMPTY TEMP-TABLE konstvaltempTT NO-ERROR. 
   FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = gruppkod NO-LOCK NO-ERROR.
   FIND FIRST kon_val WHERE kon_val.NUM = numvar NO-LOCK NO-ERROR.
   FOR EACH kon_val WHERE kon_val.NUM = numvar:
      IF kon_val.SKAPNUM = 0 /*OR kon_val.SKAPNUM = 99*/ THEN .
      ELSE DO:
         CREATE kskkon_val.
         BUFFER-COPY kon_val TO kskkon_val.   
         kskkon_val.TTRECID = RECID(kskkon_val).
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
   kabeland = NEW Modules.Beredning.KabelAnd(INPUT startkabelandroot,INPUT  konstgrptemp.BENAMNING + " " + rubrikhelp, INPUT gruppkod).
   WAIT-FOR kabeland:ShowDialog().
   RUN kskspara_UI.
   
   
   
   DELETE OBJECT startkabelandroot NO-ERROR.
   DELETE OBJECT kabeland NO-ERROR.
   startkabelandroot = ?.
   kabeland = ?.
END PROCEDURE.
PROCEDURE kskspara_UI :
   DEFINE VARIABLE numvarkabel AS INTEGER NO-UNDO.
   FOR EACH kskkon_val WHERE NO-LOCK:
      FIND FIRST kon_val WHERE kon_val.NUM = kskkon_val.NUM AND kon_val.SKAPNUM = kskkon_val.SKAPNUM NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kon_val THEN DO:
         CREATE kon_val.
         kon_val.typ = 1.
      END.
      BUFFER-COPY kskkon_val EXCEPT TYP TO kon_val.
      numvarkabel = kskkon_val.NUM.
   END.
   
   FOR EACH kon_val WHERE kon_val.NUM = numvarkabel:
      IF kon_val.SKAPNUM = 0 OR kon_val.SKAPNUM = 99 THEN .
      ELSE DO:
         FIND FIRST kskkon_val WHERE kskkon_val.NUM = kon_val.NUM AND kskkon_val.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kskkon_val THEN DO:
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
   /*omgjort så att det ska klassas som lsp om kabeln inte finns. Detta för att gamla lspkablar ska komma ut rätt FORS Lena 20180124*/
   lsphsp = "lsp".
   FIND FIRST konsttempbuff WHERE  konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
   IF AVAILABLE konsttempbuff THEN DO:
      IF konsttempbuff.KONSKOD = 14 THEN  lsphsp = "lsp".
      ELSE IF konsttempbuff.KONSKOD = 25 THEN  lsphsp = "hsp".
      ELSE IF konsttempbuff.KONSKOD = 27 THEN  lsphsp = "hsp".
      ELSE IF Guru.Konstanter:globforetag = "FORS" AND konsttempbuff.KONSKOD = 36 THEN  lsphsp = "hsp".
      ELSE IF Guru.Konstanter:globforetag = "FORS" AND konsttempbuff.KONSKOD = 35 THEN  lsphsp = "hsp".
      ELSE IF Guru.Konstanter:globforetag = "FORS" AND konsttempbuff.KONSKOD = 41 THEN  lsphsp = "hsp".
      ELSE IF Guru.Konstanter:globforetag = "gran" AND konsttempbuff.KONSKOD = 55 THEN  lsphsp = "lsp".
      ELSE IF Guru.Konstanter:globforetag = "gran" AND konsttempbuff.KONSKOD = 54 THEN  lsphsp = "hsp".
      ELSE IF Guru.Konstanter:globforetag = "gran" AND konsttempbuff.KONSKOD = 56 THEN  lsphsp = "hsp".
      ELSE IF Guru.Konstanter:globforetag = "gran" AND konsttempbuff.KONSKOD = 58 THEN  lsphsp = "lsp".
      ELSE IF Guru.Konstanter:globforetag = "gran" AND konsttempbuff.KONSKOD = 59 THEN  lsphsp = "hsp".
      ELSE IF Guru.Konstanter:globforetag = "gran" AND konsttempbuff.KONSKOD = 60 THEN  lsphsp = "hsp".
      ELSE IF LEFT-TRIM(kvalkodvar) BEGINS "12" OR LEFT-TRIM(kvalkodvar) BEGINS "24" THEN lsphsp = "hsp".
         
    END.  
   /*FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 14 AND
   konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
   IF AVAILABLE konsttempbuff THEN DO:
      lsphsp = "lsp".      
   END.          
   ELSE lsphsp = "hsp".*/                              
END PROCEDURE.
PROCEDURE stationkabhl_UI :  
   DEFINE INPUT  PARAMETER kvalkodvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lsphsp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE traff AS INTEGER NO-UNDO.
   lsphsp = "lsp".
   IF Guru.Konstanter:globforetag = "KRIN" THEN DO:
      traff = INDEX(TRIM(kvalkodvar),"MAG",1).
      IF traff NE 0 THEN lsphsp = "MAG".
      ELSE DO:
         traff = INDEX(TRIM(kvalkodvar),"KAP",1).
         IF traff NE 0 THEN lsphsp = "KAP".
         ELSE DO:
            traff = INDEX(TRIM(kvalkodvar),"SM",1).
            IF traff NE 0 THEN lsphsp = "SM".
         END.   
      END.            
   END.    
   IF lsphsp = "lsp" THEN DO:
      FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 25 AND
      konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
      IF AVAILABLE konsttempbuff THEN DO:
         lsphsp = "hsp12".      
      END. 
      ELSE DO:
         FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 27 AND
         konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
         IF AVAILABLE konsttempbuff THEN DO:
            lsphsp = "hsp24".      
         END.
         ELSE DO:
            IF Guru.Konstanter:globforetag = "fors" THEN DO:
               FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 36 AND
               konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
               IF AVAILABLE konsttempbuff THEN DO:
                  lsphsp = "hsp24".      
               END.
               ELSE DO:
                  FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 41 AND
                  konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
                  IF AVAILABLE konsttempbuff THEN DO:
                     lsphsp = "hsp24".      
                  END.
                  ELSE DO:
                     FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 35 AND
                     konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
                     IF AVAILABLE konsttempbuff THEN DO:
                        lsphsp = "hsp12".      
                     END.
                 END.    
               END.
                  
            END.
            IF Guru.Konstanter:globforetag = "gran" THEN DO:
               FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 56 AND
               konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
               IF AVAILABLE konsttempbuff THEN DO:
                  lsphsp = "hsp24".      
               END.
               ELSE DO:
                  FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 54 AND
                  konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
                  IF AVAILABLE konsttempbuff THEN DO:
                     lsphsp = "hsp12".      
                  END.
                  ELSE DO:
                     FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 59 AND
                     konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
                     IF AVAILABLE konsttempbuff THEN DO:
                        lsphsp = "hsp12".      
                     END.
                     ELSE DO:
                        FIND FIRST konsttempbuff WHERE konsttempbuff.KONSKOD = 60 AND
                        konsttempbuff.KTYPKOD =  LEFT-TRIM(kvalkodvar) NO-LOCK NO-ERROR.
                        IF AVAILABLE konsttempbuff THEN DO:
                           lsphsp = "hsp24".      
                        END.                      
                     END.                      
                  END.                      
               END.
                  
            END.
            
         END.
      END.      
      IF NOT AVAILABLE konsttempbuff THEN DO:      
         IF LEFT-TRIM(kvalkodvar) BEGINS "12"  THEN lsphsp = "hsp12".
         IF LEFT-TRIM(kvalkodvar) BEGINS "24" THEN lsphsp = "hsp24".
      END.   
       
   END.            
                              
END PROCEDURE.
 