' test pro AD - 4x cued s fixnim cilem, 4x cued s ruznymi cili, 
' zacvik na obrazovce pocitace s viditelnym cilem
' posledni zmena 19.02.2009 23:42:16'
phasesno = 1 ' pocet faze'
REDIM phases(0 TO phasesno - 1) AS phase
pocetpokusu = 5
'------------- faze 0 '
p = 0
phases(p).sectorsno = 1
phases(p).cuesno = 1
phases(p).keytocues = "c" 'c pro zobrazeni cues v kazde fazi'
phases(p).keytostart = "" 'c pro zobrazeni startu v kazde fazi'
phases(p).repeatmax = pocetpokusu 'celkem probehne faze jen 24x - pak zkonci samo'
phases(p).noarenaframe = 1 'nevykresluje a nevypisuje se zadny arena frame 
phases(p).noroomframe = 1 'nevykresluje a nevypisuje se zadny room frame
phases(p).virtual = 1 ' neukazuje se cil, nezobrazuji se tisky na obrazovce
phases(p).tovisibletarget = 1 ' neukazuje se cil, nezobrazuji se tisky na obrazovce

' --------- CIL ---------------------------'
'cil sector'
s = 0
phases(p).sectors(s).mode = 0  'RF'
phases(p).sectors(s).shape = 1 'circle'
phases(p).sectors(s).avoid = 0 'preference'
phases(p).sectors(s).cno = pocetpokusu   '96 ruznych pozic'
phases(p).sectors(s).c = 0     'zadna stabilni pozice'
phases(p).sectors(s).r0 = 65  ' vzdalenost od stredu'
phases(p).sectors(s).r = 15   ' polomer kruhu'
phases(p).sectors(s).kopiedo = ""
phases(p).sectors(s).beepcount = 1 '1x pipne'
phases(p).sectors(s).keytonext = "g" 'G pro dalsi fazi - neprejde automaticky'
phases(p).sectors(s).keyfound = "" 'F pro oznaceni udane pozice cile
phases(p).sectors(s).keyfoundbeepstop = 2 'f jen ukonci pipani'
phases(p).sectors(s).carr(0)=(1-1)*45+20
phases(p).sectors(s).carr(1)=(6-1)*45+20
phases(p).sectors(s).carr(2)=(4-1)*45+20
phases(p).sectors(s).carr(3)=(1-1)*45+20
phases(p).sectors(s).carr(4)=(7-1)*45+20
																	'- aby to nepreslo hned do dalsi faze, kdyz oznacim cil

' --------- START ---------------------------'
c = 0 'start'
phases(p).cues(c).laser = 0
phases(p).cues(c).laserno = pocetpokusu
phases(p).cues(c).segments = 0 
phases(p).cues(c).segmentsno = 0
phases(p).cues(c).startPoint = 1 'pokazde sviti start'
phases(p).cues(c).startPointno = 0
phases(p).cues(c).kopiez = -1 'cue se nekopiruje'
phases(p).cues(c).laserarr(0)=5
phases(p).cues(c).laserarr(1)=1
phases(p).cues(c).laserarr(2)=8
phases(p).cues(c).laserarr(3)=3
phases(p).cues(c).laserarr(4)=6




