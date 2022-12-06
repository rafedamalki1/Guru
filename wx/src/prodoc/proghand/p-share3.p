DEFINE SHARED VARIABLE sharedvar AS CHARACTER.
DEFINE SHARED VARIABLE globvar AS CHARACTER.
DISPLAY sharedvar + " in p-share3.p." FORMAT "x(22)" SKIP
        globvar + " in p-share3.p." FORMAT "x(22)" WITH NO-LABELS.
