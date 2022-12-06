/*UTHTMLBILD.I*/
/*FOREBILDER*/
DEBUGGER:SET-BREAK().
webblink = ?.
webblink = SEARCH("http://www2.guruonweb.se/images/" + logga).
IF webblink = ? THEN DO:
   /* SEARCH("http://www2.guruonweb.se/images/elpoollogo.png").
   logga = SEARCH("BILDER\" + link).
   */  
   logga = Guru.Konstanter:gurubilder + logga.
END.
ELSE logga = webblink.

