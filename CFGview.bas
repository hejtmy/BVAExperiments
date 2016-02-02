' konfiguracni soubor pro zobrazeni soucasne s menu'
phasesno = 1 ' pocet faze'
REDIM phases(0 TO phasesno - 1) AS phase

'------------- faze 0 RF cue'
p = 0
s = 0
phases(p).sectorsno = 1
phases(p).cuesno = 2
phases(p).noarenaframe = 1 'nevykresluje a nevypisuje se zadny arena frame

'cil sector'
phases(p).sectors(s).mode = 0  'RF'
phases(p).sectors(s).shape = 1 'circle'
phases(p).sectors(s).avoid = 0 'preference'
phases(p).sectors(s).c = 0     'zadna stabilni pozice'
phases(p).sectors(s).r0 = 80  ' vzdalenost od stredu'
phases(p).sectors(s).r = 20   ' polomer kruhu'
phases(p).sectors(s).kopiedo = ""
phases(p).sectors(s).beepcount = 1 '1x pipne'
phases(p).sectors(s).keytonext = "g" 'G pro dalsi fazi - neprejde automaticky'
phases(p).sectors(s).keyfound = "" 'F pro oznaceni udane pozice cile'