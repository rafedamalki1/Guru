/*LAND.P*/
&Scoped-define NEW NEW 
{RESDEF.I}
DEFINE OUTPUT PARAMETER TABLE FOR landtemp.
FOR EACH LAND NO-LOCK.
   CREATE landtemp.
   BUFFER-COPY LAND TO landtemp.
END.
