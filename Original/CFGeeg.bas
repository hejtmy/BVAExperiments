' test pro AD - 4x cued s fixnim cilem, 4x cued s ruznymi cili, 
' 4x ego s fixnim cilem, 4x ego s ruznymi cili, 
' 4x allo s fixnim cilem, 4x allo s ruznymi starty
' tj celkem 24 pokusu'
' realna verze '
' posledni zmena 1.9.2008 14:14:16'

phasesno = 1 ' pocet faze'
REDIM phases(0 TO phasesno - 1) AS phase
pocetpokusu = 48
'------------- faze 0 '
p = 0
phases(p).sectorsno = 1
phases(p).cuesno = 2
phases(p).keytocues = "c" 'c pro zobrazeni cues v kazde fazi'
phases(p).keytostart = "" 'c pro zobrazeni startu v kazde fazi'
phases(p).keytostartafterfound = "t" 't pro zobrazeni startu po nalezeni cile'
phases(p).repeatmax = pocetpokusu 'celkem probehne faze jen 24x - pak zkonci samo'
phases(p).noarenaframe = 1 'nevykresluje a nevypisuje se zadny arena frame 

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
posun = 0

'-----------------------------'
'ALLO fixni cil'
'-----------------------------'
opakovani = 4
goalmarkrelation = 2
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek

'start positions'
startarr(0)=7 
startarr(1)=4
startarr(2)=8
startarr(3)=2

'only one mark position'
laser = 3

for i = 0 to opakovani-1
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laser
  cil = laser + goalmarkrelation  'cil je vazan ke znacce'
  if(cil > 8) then cil = cil - 8 
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena 
  phases(p).sectors(s).cnamearr(i+posun) = "A1"
next i
posun = posun + opakovani

'-----------------------------'
' Allo , ruzny cil'
'-----------------------------'
opakovani = 20
goalmarkrelation = 2
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek
'start position
startarr(0)=2
startarr(1)=6
startarr(2)=4
startarr(3)=6
startarr(4)=1
startarr(5)=3
startarr(6)=7
startarr(7)=6
startarr(8)=4
startarr(9)=1
startarr(10)=7
startarr(11)=1
startarr(12)=5
startarr(13)=2
startarr(14)=4
startarr(15)=3
startarr(16)=4
startarr(17)=8
startarr(18)=5
startarr(19)=2

'mark positions
laserarr(0)=3
laserarr(1)=7
laserarr(2)=3
laserarr(3)=6
laserarr(4)=8
laserarr(5)=4
laserarr(6)=1
laserarr(7)=7
laserarr(8)=4
laserarr(9)=2
laserarr(10)=6
laserarr(11)=8
laserarr(12)=5
laserarr(13)=1
laserarr(14)=2
laserarr(15)=2
laserarr(16)=5
laserarr(17)=7
laserarr(18)=6
laserarr(19)=3
for i = 0 to opakovani-1
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laserarr(i)
  cil = laserarr(i) + goalmarkrelation  'cil je vazan ke znacce'
  if(cil > 8) then cil = cil - 8 
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena 
  phases(p).sectors(s).cnamearr(i+posun) = "A2" 
next i 
posun = posun + opakovani

'-----------------------------'
'EGO fixni cil'
'-----------------------------'
opakovani = 4
goalmarkrelation = 5 'zmena ze 4 na 5 16.12.2008'   
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek

'single start position'
start = 1
for i = 0 to opakovani-1
  phases(p).cues(0).laserarr(i+posun) = start  
  phases(p).cues(1).laserarr(i+posun) = 0 ' neni cue'
  cil = start + goalmarkrelation	'cil je vazan ke startu'
  if(cil > 8) then cil = cil - 8
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena
  phases(p).sectors(s).cnamearr(i+posun) = "E1" 
next i
posun = posun + opakovani

'-----------------------------'
'EGO ruzne cile'
'-----------------------------'
opakovani = 20
goalmarkrelation = 5 'zmena ze 4 na 5 16.12.2008'     
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek
'start positions
startarr(0)=2
startarr(1)=6
startarr(2)=1
startarr(3)=5
startarr(4)=4
startarr(5)=2
startarr(6)=8
startarr(7)=4
startarr(8)=7
startarr(9)=5
startarr(10)=3
startarr(11)=1
startarr(12)=5
startarr(13)=3
startarr(14)=8
startarr(15)=6
startarr(16)=1
startarr(17)=7
startarr(18)=3
startarr(19)=8
for i = 0 to opakovani-1
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = 0 ' neni cue'
  cil = startarr(i) + goalmarkrelation  'cil je vazan ke startu'
  if(cil > 8) then cil = cil - 8
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena
  phases(p).sectors(s).cnamearr(i + posun) = "E2"
next i