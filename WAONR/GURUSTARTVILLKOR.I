
/*------------------------------------------------------------------------
    File        : GURUSTARTVILLKOR.I
    Purpose     : 

    Syntax      :KOLLA I StoppVillkor_UI I LOSENKOLLWEB.P

    Description : 

    Author(s)   : 
    Created     : Fri Nov 27 11:44:20 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

 IF Guru.GlobalaVariabler:AppSpringFel = "QUIT" THEN DO:
      MESSAGE "Guru är tillfälligt stängt!" SKIP 
      "Ta kontakt med ansvarig för mer information!"
      VIEW-AS ALERT-BOX.
      QUIT.
   END.  