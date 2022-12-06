DEFINE VARIABLE login-name AS CHARACTER FORMAT "x(10)".
DEFINE VARIABLE real-name  AS CHARACTER FORMAT "x(20)".
DEFINE VARIABLE loop       AS INTEGER.

/* username:password:uid:gid:gcos-field:home-dir:login-shell */
DEFINE VARIABLE passwd AS CHARACTER EXTENT 5 INITIAL [
    "kulig::201:120:Clyde Kulig:/users/kulig:",
    "gegetskas::202:120:Neal Gegetskas:/users/geget:",
    "bertrand::203:120:Rich Bertrand:/users/bertr:",
    "lepage::204:120:Gary Lepage:/users/lepag:",
    "wnek::205:120:Jordyn Wnek:/users/nwekj:"
    ].

REPEAT:
   SET login-name.
   real-name = ?.
   DO loop = 1 TO 5:
      IF ENTRY(1, passwd[loop],":") = login-name
      THEN LEAVE.
   END.
   IF loop > 5
   THEN MESSAGE "Sorry, but" login-name "is not in my password file.".
   ELSE real-name = ENTRY(5,passwd[loop], ":").
   DISPLAY real-name.
END.
