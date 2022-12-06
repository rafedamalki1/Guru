DEFINE VARIABLE cur-lang AS CHARACTER FORMAT "x(10)" VIEW-AS RADIO-SET 
			    RADIO-BUTTONS czech,   "Czech",
			                  danish,  "Danish",
			                  dutch,   "Dutch",
			                  english, "English",
			                  french,  "French",
			                  german,  "German",
			                  hungar,  "Hungarian",
			                  italian, "Italian",
			                  norweg,  "Norwegian",
			                  polish,  "Polish",
			                  portug,  "Portugese",
			                  swedish, "Swedish".

IF CURRENT-LANGUAGE = "?"
THEN cur-lang = "English".
ELSE cur-lang = CURRENT-LANGUAGE.

UPDATE cur-lang NO-LABELS.

CURRENT-LANGUAGE = cur-lang.
MESSAGE "New language is" CURRENT-LANGUAGE.
