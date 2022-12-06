/*DEPRIS.I*/ 
      ASSIGN
      /*debpris.PERSONALKOD:LABEL IN BROWSE {&BROWSE-NAME} = "Enhet/!Sign"*/
      debpris.PRISA:LABEL IN BROWSE {&BROWSE-NAME} = CAPS(SUBSTRING(timtyp[1],1,1)) + LC(SUBSTRING(timtyp[1],2))
      debpris.PRISA-2:LABEL IN BROWSE {&BROWSE-NAME}= CAPS(SUBSTRING(timtyp[2],1,1)) + LC(SUBSTRING(timtyp[2],2))  
      debpris.PRISA-3:LABEL IN BROWSE {&BROWSE-NAME} = CAPS(SUBSTRING(timtyp[3],1,1)) + LC(SUBSTRING(timtyp[3],2))
      debpris.PRISA-4:LABEL IN BROWSE {&BROWSE-NAME} = CAPS(SUBSTRING(timtyp[4],1,1)) + LC(SUBSTRING(timtyp[4],2)).
      ASSIGN 
      FILL-IN-DEB-1 = CAPS(SUBSTRING(timtyp[1],1,1)) + LC(SUBSTRING(timtyp[1],2))
      FILL-IN-DEB-2 = CAPS(SUBSTRING(timtyp[2],1,1)) + LC(SUBSTRING(timtyp[2],2))
      FILL-IN-DEB-3 = CAPS(SUBSTRING(timtyp[3],1,1)) + LC(SUBSTRING(timtyp[3],2))
      FILL-IN-DEB-4 = CAPS(SUBSTRING(timtyp[4],1,1)) + LC(SUBSTRING(timtyp[4],2))
      FILL-IN-ENAMN = "Efternamn"
      FILL-IN-PER = "Sign".  
      DISPLAY BRW_DEBPR WITH FRAME DIALOG-1.
      ENABLE BRW_DEBPR WITH FRAME DIALOG-1.
