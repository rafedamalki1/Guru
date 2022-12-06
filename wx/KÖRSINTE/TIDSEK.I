/*TIDSEK.I körs inte*/


DEFINE TEMP-TABLE sektemp 
   FIELD tadmSEK AS LOGICAL EXTENT 50 
   FIELD tidaSEK AS LOGICAL EXTENT 50 
   FIELD tidbSEK AS LOGICAL EXTENT 50 
   FIELD tidoSEK AS LOGICAL EXTENT 50 
   FIELD tidrSEK AS LOGICAL EXTENT 50 
   FIELD tidSEK AS LOGICAL EXTENT 50 
   FIELD tidsSEK AS LOGICAL EXTENT 50 
   FIELD tidtSEK AS LOGICAL EXTENT 50 
   FIELD tadmLOG AS LOGICAL 
   FIELD tidaLOG AS LOGICAL 
   FIELD tidbLOG AS LOGICAL 
   FIELD tidoLOG AS LOGICAL 
   FIELD tidrLOG AS LOGICAL 
   FIELD tidLOG AS LOGICAL 
   FIELD tidsLOG AS LOGICAL 
   FIELD tidtLOG AS LOGICAL.    
xhop = "TID".     
IF tidlogvar = FALSE THEN DO:
   tidlogvar = TRUE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN SEKTIAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT globniv, OUTPUT TABLE sektemp).
   END.
   ELSE DO:
      RUN SEKTIAPP.P  
      (INPUT globniv, OUTPUT TABLE sektemp).
   END.   
FIND FIRST sektemp NO-ERROR.
IF AVAILABLE sektemp THEN DO:
   ASSIGN
   tadmlogvar = TRUE
   Guru.Konstanter:tadmsekvar[1] = sektemp.tadmSEK[1]
   Guru.Konstanter:tadmsekvar[2] = sektemp.tadmSEK[2]
   Guru.Konstanter:tadmsekvar[3] = sektemp.tadmSEK[3]
   Guru.Konstanter:tadmsekvar[4] = sektemp.tadmSEK[4]
   Guru.Konstanter:tadmsekvar[5] = sektemp.tadmSEK[5]
   Guru.Konstanter:tadmsekvar[6] = sektemp.tadmSEK[6]
   Guru.Konstanter:tadmsekvar[7] = sektemp.tadmSEK[7]
   Guru.Konstanter:tadmsekvar[8] = sektemp.tadmSEK[8]
   Guru.Konstanter:tadmsekvar[9] = sektemp.tadmSEK[9]
   Guru.Konstanter:tadmsekvar[10] = sektemp.tadmSEK[10]
   Guru.Konstanter:tadmsekvar[11] = sektemp.tadmSEK[11]
   Guru.Konstanter:tadmsekvar[12] = sektemp.tadmSEK[12]
   Guru.Konstanter:tadmsekvar[13] = sektemp.tadmSEK[13].
   ASSIGN
   tidlogvar = TRUE
   Guru.Konstanter:tidsekvar[1] = sektemp.tidSEK[1]
   Guru.Konstanter:tidsekvar[2] = sektemp.tidSEK[2]
   Guru.Konstanter:tidsekvar[3] = sektemp.tidSEK[3]
   Guru.Konstanter:tidsekvar[4] = sektemp.tidSEK[4]
   Guru.Konstanter:tidsekvar[5] = sektemp.tidSEK[5]
   Guru.Konstanter:tidsekvar[6] = sektemp.tidSEK[6]
   Guru.Konstanter:tidsekvar[7] = sektemp.tidSEK[7]
   Guru.Konstanter:tidsekvar[8] = sektemp.tidSEK[8]
   Guru.Konstanter:tidsekvar[9] = sektemp.tidSEK[9]
   Guru.Konstanter:tidsekvar[10] = sektemp.tidSEK[10]
   Guru.Konstanter:tidsekvar[11] = sektemp.tidSEK[11]
   Guru.Konstanter:tidsekvar[12] = sektemp.tidSEK[12]
   Guru.Konstanter:tidsekvar[13] = sektemp.tidSEK[13].
   ASSIGN   
   tidalogvar = TRUE
   Guru.Konstanter:tidasekvar[1] = sektemp.tidaSEK[1]
   Guru.Konstanter:tidasekvar[2] = sektemp.tidaSEK[2]
   Guru.Konstanter:tidasekvar[3] = sektemp.tidaSEK[3]
   Guru.Konstanter:tidasekvar[4] = sektemp.tidaSEK[4]
   Guru.Konstanter:tidasekvar[5] = sektemp.tidaSEK[5]
   Guru.Konstanter:tidasekvar[6] = sektemp.tidaSEK[6]
   Guru.Konstanter:tidasekvar[7] = sektemp.tidaSEK[7]
   Guru.Konstanter:tidasekvar[8] = sektemp.tidaSEK[8]
   Guru.Konstanter:tidasekvar[9] = sektemp.tidaSEK[9]
   Guru.Konstanter:tidasekvar[10] = sektemp.tidaSEK[10]
   Guru.Konstanter:tidasekvar[11] = sektemp.tidaSEK[11]
   Guru.Konstanter:tidasekvar[12] = sektemp.tidaSEK[12]
   Guru.Konstanter:tidasekvar[13] = sektemp.tidaSEK[13].
   ASSIGN   
   tidblogvar = TRUE
   Guru.Konstanter:tidbsekvar[1] = sektemp.tidbSEK[1]
   Guru.Konstanter:tidbsekvar[2] = sektemp.tidbSEK[2]
   Guru.Konstanter:tidbsekvar[3] = sektemp.tidbSEK[3]
   Guru.Konstanter:tidbsekvar[4] = sektemp.tidbSEK[4]
   Guru.Konstanter:tidbsekvar[5] = sektemp.tidbSEK[5]
   Guru.Konstanter:tidbsekvar[6] = sektemp.tidbSEK[6]
   Guru.Konstanter:tidbsekvar[7] = sektemp.tidbSEK[7]
   Guru.Konstanter:tidbsekvar[8] = sektemp.tidbSEK[8]
   Guru.Konstanter:tidbsekvar[9] = sektemp.tidbSEK[9]
   Guru.Konstanter:tidbsekvar[10] = sektemp.tidbSEK[10]
   Guru.Konstanter:tidbsekvar[11] = sektemp.tidbSEK[11]
   Guru.Konstanter:tidbsekvar[12] = sektemp.tidbSEK[12]
   Guru.Konstanter:tidbsekvar[13] = sektemp.tidbSEK[13].
   ASSIGN   
   tidologvar = TRUE
   Guru.Konstanter:tidosekvar[1] = sektemp.tidoSEK[1]
   Guru.Konstanter:tidosekvar[2] = sektemp.tidoSEK[2]
   Guru.Konstanter:tidosekvar[3] = sektemp.tidoSEK[3]
   Guru.Konstanter:tidosekvar[4] = sektemp.tidoSEK[4]
   Guru.Konstanter:tidosekvar[5] = sektemp.tidoSEK[5]
   Guru.Konstanter:tidosekvar[6] = sektemp.tidoSEK[6]
   Guru.Konstanter:tidosekvar[7] = sektemp.tidoSEK[7]
   Guru.Konstanter:tidosekvar[8] = sektemp.tidoSEK[8]
   Guru.Konstanter:tidosekvar[9] = sektemp.tidoSEK[9]
   Guru.Konstanter:tidosekvar[10] = sektemp.tidoSEK[10]
   Guru.Konstanter:tidosekvar[11] = sektemp.tidoSEK[11]
   Guru.Konstanter:tidosekvar[12] = sektemp.tidoSEK[12]
   Guru.Konstanter:tidosekvar[13] = sektemp.tidoSEK[13].
   ASSIGN   
   tidrlogvar = TRUE
   Guru.Konstanter:tidrsekvar[1] = sektemp.tidrSEK[1]
   Guru.Konstanter:tidrsekvar[2] = sektemp.tidrSEK[2]
   Guru.Konstanter:tidrsekvar[3] = sektemp.tidrSEK[3]
   Guru.Konstanter:tidrsekvar[4] = sektemp.tidrSEK[4]
   Guru.Konstanter:tidrsekvar[5] = sektemp.tidrSEK[5]
   Guru.Konstanter:tidrsekvar[6] = sektemp.tidrSEK[6]
   Guru.Konstanter:tidrsekvar[7] = sektemp.tidrSEK[7]
   Guru.Konstanter:tidrsekvar[8] = sektemp.tidrSEK[8]
   Guru.Konstanter:tidrsekvar[9] = sektemp.tidrSEK[9]
   Guru.Konstanter:tidrsekvar[10] = sektemp.tidrSEK[10]
   Guru.Konstanter:tidrsekvar[11] = sektemp.tidrSEK[11]
   Guru.Konstanter:tidrsekvar[12] = sektemp.tidrSEK[12]
   Guru.Konstanter:tidrsekvar[13] = sektemp.tidrSEK[13].
   ASSIGN   
   tidslogvar = TRUE
   Guru.Konstanter:tidssekvar[1] = sektemp.tidsSEK[1]
   Guru.Konstanter:tidssekvar[2] = sektemp.tidsSEK[2]
   Guru.Konstanter:tidssekvar[3] = sektemp.tidsSEK[3]
   Guru.Konstanter:tidssekvar[4] = sektemp.tidsSEK[4]
   Guru.Konstanter:tidssekvar[5] = sektemp.tidsSEK[5]
   Guru.Konstanter:tidssekvar[6] = sektemp.tidsSEK[6]
   Guru.Konstanter:tidssekvar[7] = sektemp.tidsSEK[7]
   Guru.Konstanter:tidssekvar[8] = sektemp.tidsSEK[8]
   Guru.Konstanter:tidssekvar[9] = sektemp.tidsSEK[9]
   Guru.Konstanter:tidssekvar[10] = sektemp.tidsSEK[10]
   Guru.Konstanter:tidssekvar[11] = sektemp.tidsSEK[11]
   Guru.Konstanter:tidssekvar[12] = sektemp.tidsSEK[12]
   Guru.Konstanter:tidssekvar[13] = sektemp.tidsSEK[13].
   ASSIGN   
   tidtlogvar = TRUE   
   Guru.Konstanter:tidtsekvar[1] = sektemp.tidtSEK[1]
   Guru.Konstanter:tidtsekvar[2] = sektemp.tidtSEK[2]
   Guru.Konstanter:tidtsekvar[3] = sektemp.tidtSEK[3]
   Guru.Konstanter:tidtsekvar[4] = sektemp.tidtSEK[4]
   Guru.Konstanter:tidtsekvar[5] = sektemp.tidtSEK[5]
   Guru.Konstanter:tidtsekvar[6] = sektemp.tidtSEK[6]
   Guru.Konstanter:tidtsekvar[7] = sektemp.tidtSEK[7]
   Guru.Konstanter:tidtsekvar[8] = sektemp.tidtSEK[8]
   Guru.Konstanter:tidtsekvar[9] = sektemp.tidtSEK[9]
   Guru.Konstanter:tidtsekvar[10] = sektemp.tidtSEK[10]
   Guru.Konstanter:tidtsekvar[11] = sektemp.tidtSEK[11]
   Guru.Konstanter:tidtsekvar[12] = sektemp.tidtSEK[12]
   Guru.Konstanter:tidtsekvar[13] = sektemp.tidtSEK[13].
END.   
   /*   
   RUN nextguru_UI.      
   ASSIGN
   Guru.Konstanter:tidsekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:tidsekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:tidsekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:tidsekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:tidsekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:tidsekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:tidsekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:tidsekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:tidsekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:tidsekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:tidsekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:tidsekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:tidsekvar[13] = XSEK.SEK[13].
   */
END.
IF Guru.Konstanter:tidsekvar[1] = TRUE THEN ENABLE BTN_TIDREG WITH FRAME {&FRAME-NAME}.
ELSE MENU-ITEM m_Total_tidregistrering:SENSITIVE IN MENU m_Funktioner2 = FALSE.    
IF Guru.Konstanter:tidsekvar[2] = TRUE THEN ENABLE BTN_OVERT WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_vertidsregistrering:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:tidsekvar[3] = TRUE THEN ENABLE BTN_BERRED WITH FRAME {&FRAME-NAME}.              
ELSE MENU-ITEM m_Beredskapsregistrering:SENSITIVE IN MENU m_Funktioner2 = FALSE.       
IF Guru.Konstanter:tidsekvar[4] = TRUE THEN ENABLE BTN_TJRES WITH FRAME {&FRAME-NAME}.              
ELSE MENU-ITEM m_Tjnsteresor:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:tidsekvar[5] = TRUE THEN ENABLE BTN_VTID WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_Visa_tidsedel:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:tidsekvar[6] = TRUE THEN ENABLE BTN_TILLAND WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_ndringar_och_tillgg:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:tidsekvar[7] = TRUE THEN ENABLE BTN_ADMT WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_Administrera_tidsedlar:SENSITIVE IN MENU m_Funktioner2 = FALSE.  
IF Guru.Konstanter:tidsekvar[8] = TRUE THEN ENABLE BTN_FLEX WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_Administrera_flextid:SENSITIVE IN MENU m_Funktioner2 = FALSE.            
IF Guru.Konstanter:tidsekvar[9] = TRUE THEN ENABLE BTN_STANS WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_Stansa_tiduppifter:SENSITIVE IN MENU m_Funktioner2 = FALSE.            


    
