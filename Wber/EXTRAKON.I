/*EXTRAKON.I*/
ASSIGN
kon_val.EXTRA1 = SUBSTRING(kon_val.EXTRA,3)
kon_val.EXTRA2 = SUBSTRING(kon_val.EXTRA,1,1).
RUN laddaberid_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE,
                                   OUTPUT TABLE beridtemp).
FIND FIRST beridtemp WHERE beridtemp.AONR = kon_val2.BERAONR AND 
beridtemp.OMRADE = kon_val2.OMRADE AND beridtemp.NUM = kon_val2.NUM NO-LOCK NO-ERROR.
IF AVAILABLE beridtemp THEN DO:
   IF beridtemp.XKORD NE ? THEN DO:
      ASSIGN
      kombnr = kombnr + 1
      kon_val.SKAPNUM = kombnr.
      OPEN QUERY idq FOR EACH idbuff WHERE idbuff.AONR = kon_val2.BERAONR AND 
      idbuff.OMRADE = kon_val2.OMRADE AND idbuff.XKORD = beridtemp.XKORD NO-LOCK.
      GET FIRST idq NO-LOCK.
      DO WHILE AVAILABLE(idbuff):
         IF idbuff.NUM = beridtemp.NUM THEN musz = musz.
         ELSE DO:
            RUN laddaberval_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE,
                                                INPUT idbuff.NUM,OUTPUT ktypkodvar,OUTPUT bernumvar).
            ordning = ordning + 1.
            CREATE kon_val. 
            ASSIGN
            kon_val.BERAONR = kon_val2.BERAONR
            kon_val.OMRADE = kon_val2.OMRADE
            kon_val.F1 = ktypkodvar                        
            kon_val.NUM = bernumvar
            kon_val.ORD = kon_val2.ORD
            /*
            kon_val.ORD = ordning
            */
            kon_val.SKAPNUM = kombnr.
            IF idbuff.FRI2 = ? THEN kon_val.ID2 = idbuff.NATNR.
            ELSE kon_val.ID2 = STRING(idbuff.FRI2).
            kon_val.EXTRA = idbuff.FRI3.
            ASSIGN
            kon_val.EXTRA1 = SUBSTRING(kon_val.EXTRA,3)
            kon_val.EXTRA2 = SUBSTRING(kon_val.EXTRA,1,1).
            kon_val.UPPLAG = kon_val2.UPPLAG.
         END.
         GET NEXT idq NO-LOCK.
      END.
      CLOSE QUERY idq.
   END.
   ELSE DO:
      kon_val.SKAPNUM = ?.
   END.
END.
