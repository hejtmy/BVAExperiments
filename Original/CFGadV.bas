' test pro AD - 4x cued s fixnim cilem, 4x cued s ruznymi cili, 
' virtualni verze stejne dlouha jako realna verze - stejne vztahy mezi znackou (startem) a cilem v jinych rotacich 
' vytvoreno 14.01.2010 10:03:02'

phasesno = 1 ' pocet faze'
REDIM phases(0 TO phasesno - 1) AS phase
pocetpokusu = 24
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
redim startarr(0 to 3) as integer 'pozice startu ve ctverici'
redim laserarr(0 to 3) as integer 'pozice znacek ve ctverici'
' CUED , fixni cil'
posun = 0
laser = 8
startarr(0)=3 
startarr(1)=6
startarr(2)=2
startarr(3)=5
for i = 0 to 3
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laser
  phases(p).sectors(s).carr(i+posun) = (laser-1) * 45 + uhelzmena 'cil je vazan ke znacce'
  phases(p).sectors(s).cnamearr(i+posun) = "C1" 
next i

' CUED , ruzny cil'
posun = 4
startarr(0)=1 
startarr(1)=7
startarr(2)=4
startarr(3)=2
laserarr(0)=4 
laserarr(1)=2
laserarr(2)=5
laserarr(3)=7
for i = 0 to 3
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laserarr(i)
  phases(p).sectors(s).carr(i+posun) = (laserarr(i)-1) * 45 + uhelzmena 'cil je vazan ke znacce'
  phases(p).sectors(s).cnamearr(i+posun) = "C2" 
next i

'ALLO fixni cil'
posun = 8
laser = 6
startarr(0)=7 
startarr(1)=4
startarr(2)=8
startarr(3)=2
for i = 0 to 3
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laser
  cil = laser + 2  'cil je vazan ke znacce'
  if(cil > 8) then cil = cil - 8 
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena 
  phases(p).sectors(s).cnamearr(i+posun) = "A1" 
next i


' Allo , ruzny cil'
posun = 12
startarr(0)=2 
startarr(1)=8
startarr(2)=4
startarr(3)=1
laserarr(0)=7 '+5'
laserarr(1)=2 '+2'
laserarr(2)=5 '+1'
laserarr(3)=8 '+7'
for i = 0 to 3
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laserarr(i)
  cil = laserarr(i) + 2  'cil je vazan ke znacce'
  if(cil > 8) then cil = cil - 8 
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena 
  phases(p).sectors(s).cnamearr(i+posun) = "A2" 
next i 


'EGO fixni cil'
posun = 16
start = 8
for i = 0 to 3
  phases(p).cues(0).laserarr(i+posun) = start  
  phases(p).cues(1).laserarr(i+posun) = 0 ' neni cue'
  cil = start + 5  'zmena ze 4 na 5 16.12.2008'                                          'cil je vazan ke startu'
  if(cil > 8) then cil = cil - 8
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena
  phases(p).sectors(s).cnamearr(i+posun) = "E1" 
next i

'EGO ruzne cile
posun = 20
startarr(0)=2 
startarr(1)=7
startarr(2)=4
startarr(3)=6
for i = 0 to 3
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = 0 ' neni cue'
  cil = startarr(i) + 5   'zmena ze 4 na 5 16.12.2008'                                         'cil je vazan ke startu'
  if(cil > 8) then cil = cil - 8
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena
  phases(p).sectors(s).cnamearr(i+posun) = "E2" 
next i








