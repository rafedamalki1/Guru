
DEFINE VARIABLE choice AS CHARACTER LABEL "You selected".
DEFINE VARIABLE a AS CHARACTER INITIAL "item2".
UPDATE a VIEW-AS RADIO-SET
	RADIO-BUTTONS
	"item1", "item1", "item2", "item2", "item3", "item3"
	LABEL "Please select an item".
choice = a.
DISPLAY choice WITH FRAME b SIDE-LABELS.
PAUSE.
