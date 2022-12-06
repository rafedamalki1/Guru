DEFINE VARIABLE cur-lang AS CHARACTER.

cur-lang = CURRENT-LANGUAGE.

IF cur-lang = "?"
THEN MESSAGE "Your current language is not set.".
ELSE MESSAGE "Your current language is" cur-lang.
