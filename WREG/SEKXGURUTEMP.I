/*SEKXGURUTEMP.I*/
{GURUSKEKMENY.I}
DEFINE TEMP-TABLE seksel NO-UNDO
   FIELD MENY AS CHARACTER  
   FIELD MENYVART AS CHARACTER.  
FUNCTION menyhopp RETURNS CHARACTER
  ( INPUT menyerhop AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   IF menyerhop = "Huvudmeny" THEN RETURN "GURU".
   ELSE IF menyerhop = "Arbetsordermeny" THEN RETURN "AONR".
   ELSE IF menyerhop = "Materielhantering" THEN RETURN "MTRL". 
   ELSE IF menyerhop = "BULA - Depåhantering" THEN RETURN "BULA". 
   ELSE IF menyerhop = "Kalkylmeny" THEN RETURN "KALK".
   ELSE IF menyerhop = "Personalmeny" THEN RETURN "PERS".
   ELSE IF menyerhop = "Tidredovisningsmeny" THEN RETURN "TID".
   ELSE IF menyerhop = "Tidregistrering" THEN RETURN "TIDR".
   ELSE IF menyerhop = "Övertidsregistrering" THEN RETURN "TIDO".
   ELSE IF menyerhop = "Beredskapdregistrering" THEN RETURN "TIDB".
   ELSE IF menyerhop = "Tjänstereseregistrering" THEN RETURN "TIDT".
   ELSE IF menyerhop = "Ändring och tillägg" THEN RETURN "TIDA".
   ELSE IF menyerhop = "Tidsedelmeny" THEN RETURN "TIDS".
   ELSE IF menyerhop = "Register" THEN RETURN "REG".  
   ELSE IF menyerhop = "Faktureringsrutin" THEN RETURN "FAKT". 
   ELSE IF menyerhop = "Plan" THEN RETURN "PLAN".  
   ELSE IF menyerhop = "Administrera tidsedel" THEN RETURN "TADM". 
   ELSE IF menyerhop = "Beredning" THEN RETURN "BERE".     
   ELSE IF menyerhop = "Avbrott/Störning" THEN RETURN "STOR".     
END FUNCTION.


