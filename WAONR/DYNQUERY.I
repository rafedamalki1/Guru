   /*DYNQUERY.I*/
   {SETUSE.I}
   CREATE WIDGET-POOL "dynTemp" NO-ERROR.
   DEFINE INPUT PARAMETER dynakollvar AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER dynajoin AS LOGICAL NO-UNDO.
   /*tabell i databasen som efterfrågas*/
   CREATE BUFFER orgtabh FOR TABLE orginaltab IN WIDGET-POOL "dynTemp". 
   /*resultattabell */
   CREATE BUFFER nytabh FOR TABLE extratemptabh IN WIDGET-POOL "dynTemp".
   /*dina val*/
   IF dynakollvar = TRUE THEN DO:
      CREATE BUFFER utvaltabh FOR TABLE extratemptabh2 IN WIDGET-POOL "dynTemp". 
      CREATE QUERY uqh IN WIDGET-POOL "dynTemp".
      uqh:SET-BUFFERS(utvaltabh).
      uqh:QUERY-PREPARE(utvaltab).   
      uqh:QUERY-OPEN().
      uqh:GET-FIRST(NO-LOCK).
   END.
   
   /*om du har en joinad utsökning*/
   IF dynajoin = TRUE THEN DO:
      CREATE BUFFER jointabh FOR TABLE extrajointemptabh IN WIDGET-POOL "dynTemp".
   END.
 
   /*query för tabellen som efterfrågas*/
   CREATE QUERY qh IN WIDGET-POOL "dynTemp".
   IF dynajoin = TRUE THEN DO:
      qh:SET-BUFFERS(jointabh,orgtabh).
   END.
   ELSE qh:SET-BUFFERS(orgtabh).
   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).


       
    
