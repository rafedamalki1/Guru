 /* Xklgjamfor.P  */
       
DEFINE TEMP-TABLE kalklop1                              
   FIELD KLOGSUBID AS INTEGER
   FIELD ARBKOD AS CHARACTER 
   FIELD LOPNR AS INTEGER 
   FIELD BENAMNING AS CHARACTER
   FIELD TYPKALK AS INTEGER
   INDEX KLOGSUBID KLOGSUBID ARBKOD LOPNR.

DEFINE TEMP-TABLE kalklop2                              
   FIELD KLOGSUBID AS INTEGER
   FIELD ARBKOD AS CHARACTER 
   FIELD LOPNR AS INTEGER 
   FIELD BENAMNING AS CHARACTER
   FIELD TYPKALK AS INTEGER
   INDEX KLOGSUBID KLOGSUBID ARBKOD LOPNR.
 
 DEFINE TEMP-TABLE kalkloputgatt                              
   FIELD KLOGSUBID AS INTEGER
   FIELD ARBKOD AS CHARACTER 
   FIELD LOPNR AS INTEGER 
   FIELD BENAMNING AS CHARACTER
   FIELD TYPKALK AS INTEGER
   INDEX KLOGSUBID KLOGSUBID ARBKOD LOPNR.
   
  DEFINE TEMP-TABLE kalklopnyben                              
   FIELD KLOGSUBID AS INTEGER
   FIELD ARBKOD AS CHARACTER 
   FIELD LOPNR AS INTEGER 
   FIELD BENAMNING AS CHARACTER
   FIELD NYBENAMNING AS CHARACTER
   FIELD TYPKALK AS INTEGER
   INDEX KLOGSUBID KLOGSUBID ARBKOD LOPNR. 

DEFINE TEMP-TABLE kalklopnykod                              
   FIELD KLOGSUBID AS INTEGER
   FIELD ARBKOD AS CHARACTER 
   FIELD LOPNR AS INTEGER 
   FIELD BENAMNING AS CHARACTER
   FIELD TYPKALK AS INTEGER
   INDEX KLOGSUBID KLOGSUBID ARBKOD LOPNR.
   
      
   /*/*2020*/ 
   FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = 69 NO-LOCK:
      CREATE kalklop1.
      BUFFER-COPY KALKYLLOPPOSTER TO kalklop1.
   END.
   /*2021*/
   FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = 72 NO-LOCK:
      CREATE kalklop2.
      BUFFER-COPY KALKYLLOPPOSTER TO kalklop2.
   END.*/
   
   /*2017*/ 
   FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = 56 NO-LOCK:
      CREATE kalklop1.
      BUFFER-COPY KALKYLLOPPOSTER TO kalklop1.
   END.
   /*2018*/
   FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = 61 NO-LOCK:
      CREATE kalklop2.
      BUFFER-COPY KALKYLLOPPOSTER TO kalklop2.
   END.  
   
   
   FOR EACH kalklop1 NO-LOCK:
      FIND FIRST kalklop2 WHERE kalklop2.ARBKOD = kalklop1.ARBKOD AND kalklop2.LOPNR = kalklop1.LOPNR
      AND kalklop2.BENAMNING = kalklop1.BENAMNING NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalklop2 THEN DO:
         FIND FIRST kalklop2 WHERE kalklop2.ARBKOD = kalklop1.ARBKOD AND kalklop2.LOPNR = kalklop1.LOPNR  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalklop2 THEN DO:         
            CREATE kalkloputgatt.
            BUFFER-COPY kalklop1 TO kalkloputgatt .
         END.
         ELSE DO:
            CREATE kalklopnyben.
            BUFFER-COPY kalklop1 TO kalklopnyben.
            ASSIGN kalklopnyben.nybenamning = kalklop2.BENAMNING.
         END.
                     
      END.
   END.
   FOR EACH kalklop2 NO-LOCK:
      FIND FIRST kalklop1 WHERE kalklop1.ARBKOD = kalklop2.ARBKOD AND kalklop1.LOPNR = kalklop2.LOPNR    NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalklop1 THEN DO:
         CREATE kalklopnykod.
         BUFFER-COPY kalklop2 TO kalklopnykod.                             
      END.
   END.
   OUTPUT TO c:\aa\skillnad20172018.txt.
   put "utgått" SKIP.
   FOR EACH kalkloputgatt WHERE NO-LOCK:
      PUT UNFORMATTED kalkloputgatt.arbkod " " kalkloputgatt.lopnr " " kalkloputgatt.benamning FORMAT "x(50)" SKIP.
   END.
   PUT " " SKIP.
   put "ny benämning" SKIP.
   FOR EACH kalklopnyben WHERE NO-LOCK:
      PUT UNFORMATTED kalklopnyben.arbkod " " kalklopnyben.lopnr " " kalklopnyben.benamning FORMAT "x(50)" " NY BEN " kalklopnyben.nybenamning FORMAT "x(50)" SKIP.
   END.
   PUT " " SKIP.
   put "NY" SKIP.
   FOR EACH kalklopnykod WHERE NO-LOCK:
      PUT UNFORMATTED kalklopnykod.arbkod kalklopnykod.lopnr kalklopnykod.benamning FORMAT "x(50)" SKIP.
   END.
    