' test pro AD - 4x cued s fixnim cilem, 4x cued s ruznymi cili, 
' zacvik na obrazovce pocitace - jine cile a znacky nez ve finalnim CFGad
' posledni zmena 19.02.2009 23:41:39'
phasesno = 1 ' pocet faze'
REDIM phases(0 TO phasesno - 1) AS phase
pocetpokusu = 6
'------------- faze 0 '
p = 0
phases(p).sectorsno = 1
phases(p).cuesno = 2
phases(p).keytocues = "c" 'c pro zobrazeni cues v kazde fazi'
phases(p).keytostart = "" 'c pro zobrazeni startu v kazde fazi'
phases(p).keytostartafterfound = "t" 't pro zobrazeni startu po nalezeni cile'
phases(p).repeatmax = pocetpokusu 'celkem probehne faze jen 24x - pak zkonci samo'
phases(p).noarenaframe = 1 'nevykresluje a nevypisuje se zadny arena frame 
phases(p).noroomframe = 1 'nevykresluje a nevypisuje se zadny room frame
phases(p).virtual = 1 ' neukazuje se cil, nezobrazuji se tisky na obrazovce

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
phases(p).sectors(s).keyfound = "f" 'F pro oznaceni udane pozice cile
phases(p).sectors(s).keyfoundbeepstop = 2 'f jen ukonci pipani'
 
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

' --------- CUE ---------------------------'
c = 1 'cue'
phases(p).cues(c).laser = 0
phases(p).cues(c).laserno = pocetpokusu
phases(p).cues(c).segments = 1 'pokazde sviti dve carky'
phases(p).cues(c).segmentsno = 0
phases(p).cues(c).startPoint = 0 
phases(p).cues(c).startPointno = 0
phases(p).cues(c).kopiez = -1 'cue se nekopiruje'

uhelzmena = 15
redim startarr(0 to 1) as integer 'pozice startu ve ctverici'
redim laserarr(0 to 1) as integer 'pozice znacek ve ctverici'
' CUED , fixni cil'
posun = 0
laser = 6
startarr(0)=4 
'startarr(1)=7
'startarr(2)=2
'startarr(3)=5
lasercil = -2
for i = 0 to 0
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laser
  phases(p).sectors(s).carr(i+posun) = (laser+lasercil) * 45 + uhelzmena 'cil je vazan ke znacce'
  phases(p).sectors(s).cnamearr(i+posun) = "C1" 
next i

' CUED , ruzny cil'
posun = 1
startarr(0)=1 
'startarr(1)=2
'startarr(2)=5
'startarr(3)=8
laserarr(0)=3 
'laserarr(1)=8
'laserarr(2)=1
'laserarr(3)=6
for i = 0 to 0
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laserarr(i)
  phases(p).sectors(s).carr(i+posun) = (laserarr(i)+lasercil) * 45 + uhelzmena 'cil je vazan ke znacce'
  phases(p).sectors(s).cnamearr(i+posun) = "C2" 
next i

'ALLO fixni cil'
posun = 2
laser = 3
startarr(0)=5 
'startarr(1)=2
'startarr(2)=5
'startarr(3)=3
lasercil = 4 'musi to byt jine nez v realne verzi'
for i = 0 to 0
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laser
  cil = laser + lasercil  'cil je vazan ke znacce'
  if(cil > 8) then cil = cil - 8
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena 
  phases(p).sectors(s).cnamearr(i+posun) = "A1" 
next i


' Allo , ruzny cil'
posun = 3
startarr(0)=7 
'startarr(1)=3
'startarr(2)=2
'startarr(3)=6
laserarr(0)=1 
'laserarr(1)=5
'laserarr(2)=8
'laserarr(3)=2
for i = 0 to 0
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laserarr(i)
  cil = laserarr(i) + lasercil  'cil je vazan ke znacce'
  if(cil > 8) then cil = cil - 8 
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena 
  phases(p).sectors(s).cnamearr(i+posun) = "A2" 
next i


'EGO fixni cil'
posun = 4
start = 7
startcil = -8
for i = 0 to 0
  phases(p).cues(0).laserarr(i+posun) = start  
  phases(p).cues(1).laserarr(i+posun) = 0 ' neni cue'
  cil = start + 4  'zmena ze 4 na 5 16.12.2008'                                          'cil je vazan ke startu'
  if(cil > 8) then cil = cil + startcil
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena
  phases(p).sectors(s).cnamearr(i+posun) = "E1" 
next i

'EGO ruzne cile
posun = 5
startarr(0)=5 
'startarr(1)=2
'startarr(2)=5
'startarr(3)=4
for i = 0 to 0
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = 0 ' neni cue'
  cil = startarr(i) + 4   'zmena ze 4 na 5 16.12.2008'                                         'cil je vazan ke startu'
  if(cil > 8) then cil = cil + startcil
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena
  phases(p).sectors(s).cnamearr(i+posun) = "E2" 
next i




