
/*------------------------------------------------------------------------
    File        : NyaPosterForInlasning.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Jul 09 16:52:51 CEST 2021
    Notes       :
  ----------------------------------------------------------------------*/

 DEFINE VARIABLE startnrIT AS INTEGER NO-UNDO.          
 DEFINE VARIABLE startnrAI AS INTEGER NO-UNDO.
MESSAGE "START"
VIEW-AS ALERT-BOX.

startnrIT = 248.
startnrAI = 27.
RUN NyAd_UI ("R","Skarv","ANLAGGNINGSDEL","5","56",3).    
RUN NyAd_UI ("R","Avslut","ANLAGGNINGSDEL","5","57",3). 
RUN NyAd_UI ("R","Skarv","ANLAGGNINGSDEL","6","66",4). 
      
/*Apparater*/      

startnrAI = startnrAI + 1. 
RUN NyAd_UI ("R","Effektbrytare - inomhus","ANLAGGNINGSDEL","7","100",6).
RUN NyAd_UI ("R","Effektbrytare - utomhus","ANLAGGNINGSDEL","7","101",6).
RUN NyAd_UI ("R","Fr�nskiljare - inomhus","ANLAGGNINGSDEL","7","102",6).
RUN NyAd_UI ("R","Fr�nskiljare - utomhus","ANLAGGNINGSDEL","7","103",6). 
RUN NyAd_UI ("R","Lastfr�nskiljare - inomhus","ANLAGGNINGSDEL","7","104",6).
RUN NyAd_UI ("R","Lastfr�nskiljare - utomhus","ANLAGGNINGSDEL","7","105",6).
RUN NyAd_UI ("R","Lastfr�nskiljare med s�kring - inomhus","ANLAGGNINGSDEL","7","106",6).
RUN NyAd_UI ("R","Lastfr�nskiljare med s�kring - utomhus","ANLAGGNINGSDEL","7","107",6).
RUN NyAd_UI ("R","S�kring  Anv�nds vid fel/felfunktion p� s�kringen","ANLAGGNINGSDEL","7","108",6).

RUN NyAd_UI ("R","Kondensatorbatteri","ANLAGGNINGSDEL","7","115",6).

RUN NyAd_UI ("R","Effektbrytare i luftledning och stolpstation","ANLAGGNINGSDEL","7","120",6).
RUN NyAd_UI ("R","Fr�nskiljare i luftledning och stolpstation","ANLAGGNINGSDEL","7","121",6).
RUN NyAd_UI ("R","Lastfr�nskiljare i luftledning och stolpstation","ANLAGGNINGSDEL","7","122",6).
RUN NyAd_UI ("R","Kondensatorbatteri, serie i luftledning och stolpstation","ANLAGGNINGSDEL","7","123",6).
RUN NyAd_UI ("R","Kondensatorbatteri, shunt i luftledning och stolpstation","ANLAGGNINGSDEL","7","124",6).
RUN NyAd_UI ("R","Oljeisolerad krafttransformator","ANLAGGNINGSDEL","7","128",6).
RUN NyAd_UI ("R","Torrisolerad krafttransformator","ANLAGGNINGSDEL","7","129",6).
RUN NyAd_UI ("R","Shuntreaktor","ANLAGGNINGSDEL","7","130",6).
RUN NyAd_UI ("R","Nollpunktsreaktor","ANLAGGNINGSDEL","7","131",6).
RUN NyAd_UI ("R","Nollpunktsmotst�nd","ANLAGGNINGSDEL","7","132",6).
RUN NyAd_UI ("R","Sp�nningstransformator","ANLAGGNINGSDEL","7","133",6).
RUN NyAd_UI ("R","Str�mtransformator","ANLAGGNINGSDEL","7","134",6).
RUN NyAd_UI ("R","Ventilavledare","ANLAGGNINGSDEL","7","135",6).
RUN NyAd_UI ("R","Isolator","ANLAGGNINGSDEL","7","136",6).
RUN NyAd_UI ("R","Kontrollutrustning","ANLAGGNINGSDEL","7","150",7).
RUN NyAd_UI ("R","Elm�tare","ANLAGGNINGSDEL","7","151",7).    




startnrAI = 24. 
RUN NyAd_UI ("S","Tj�le","FELORSAK","1","18",1).
RUN NyAd_UI ("S","Felaktig f�rl�ggning","FELORSAK","3","46",3).



PROCEDURE NyAd_UI :
   DEFINE INPUT  PARAMETER inkodvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER namnvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER tabvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER inkodtypvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER inposch AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER atypvar AS INTEGER NO-UNDO.
   IF tabvar = "ANLAGGNINGSDEL" THEN DO:
      CREATE INLASTAB.
      ASSIGN
      INLASTAB.INKODID = STRING(startnrIT)  /* r�knare f�r alla INLASTAB*/
      INLASTAB.INKOD = inkodvar     /*gruppering*/
      INLASTAB.NAMN = namnvar.
      ASSIGN 
      INLASTAB.INLASTNR =  INTEGER(INLASTAB.INKODID)
      INLASTAB.TABNAMN = tabvar
      INLASTAB.INKODTYP = inkodtypvar /*undergrupp till INLASTAB.INKOD*/
      INLASTAB.ORDNING = 0
      INLASTAB.INKODPOSCH = inposch.  /*motsvara darwin koppling*/         
      FIND FIRST ANLAGGNINGSDEL WHERE ANLAGGNINGSDEL.INKODID = INLASTAB.INKODID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ANLAGGNINGSDEL THEN DO: 
         CREATE ANLAGGNINGSDEL.
         ASSIGN 
         ANLAGGNINGSDEL.ADELID = startnrAI     /*unikt f�r ANLAGGNINGSDEL*/
         ANLAGGNINGSDEL.ATYPID = atypvar   /*ANLAGGNINGSTYP*/      
         ANLAGGNINGSDEL.INKODID = INLASTAB.INKODID
         ANLAGGNINGSDEL.NAMN = INLASTAB.NAMN
         ANLAGGNINGSDEL.ADELDARWINID = INTEGER(INLASTAB.INKODPOSCH)
         ANLAGGNINGSDEL.GILTIG = TRUE.
      END. 
   END.
   IF tabvar = "FELORSAK" THEN DO:
      CREATE INLASTAB.
      ASSIGN
      INLASTAB.INKODID = STRING(startnrIT)  /* r�knare f�r alla INLASTAB*/
      INLASTAB.INKOD = inkodvar     /*gruppering*/
      INLASTAB.NAMN = namnvar.
      ASSIGN 
      INLASTAB.INLASTNR =  INTEGER(INLASTAB.INKODID)
      INLASTAB.TABNAMN = tabvar
      INLASTAB.INKODTYP = inkodtypvar /*undergrupp till INLASTAB.INKOD*/
      INLASTAB.ORDNING = 0
      INLASTAB.INKODPOSCH = inposch.  /*motsvara darwin koppling*/         
      FIND FIRST FELORSAK WHERE FELORSAK.INKODID = INLASTAB.INKODID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FELORSAK THEN DO: 
         CREATE FELORSAK.
         ASSIGN 
         FELORSAK.FELOID  = startnrAI     /*unilt f�r ANLAGGNINGSDEL*/
         FELORSAK.GRUNDFELID = atypvar   /*GRUNDFELTYP*/      
         FELORSAK.INKODID = INLASTAB.INKODID
         FELORSAK.NAMN = INLASTAB.NAMN
         FELORSAK.FELODARWINID = INTEGER(INLASTAB.INKODPOSCH)
         FELORSAK.GILTIG = TRUE.
      END. 
   END.
   
   
   startnrIT = startnrIT + 1.
   startnrAI = startnrAI + 1. 
   
   
     
END PROCEDURE. 
      
      


/* ***************************  Main Block  *************************** */
