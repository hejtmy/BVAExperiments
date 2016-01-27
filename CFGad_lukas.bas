' test pro AD - 4x cued s fixnim cilem, 4x cued s ruznymi cili, 
' 4x ego s fixnim cilem, 4x ego s ruznymi cili, 
' 4x allo s fixnim cilem, 4x allo s ruznymi starty
' tj celkem 24 pokusu'
' realna verze '
' posledni zmena 1.9.2008 14:14:16'

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

'-----------------------------'
' CUED , fixni cil'
'-----------------------------'
opakovani = 8
goalmarkrelation = 7
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek
posun = 0
laser = 3
startarr(0)=1 
startarr(1)=6
startarr(2)=8
startarr(3)=5
startarr(4)=8
startarr(5)=6
startarr(6)=7
startarr(7)=1
for i = 0 to opakovani-1
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laser
  'goal definition
  cil = laser + goalmarkrelation
  if(cil > 8) then cil = cil - 8 
  phases(p).sectors(s).carr(i+posun) = cil * 45 + uhelzmena 'cil je vazan ke znacce' - we substract 1 from the position. because the round numbering starts at 1 - 1=0degrees, 2 = 45 degrees etc.
  phases(p).sectors(s).cnamearr(i+posun) = "C1"
next i
posun = posun + opakovani

'-----------------------------'
' CUED ruzny cil'
'-----------------------------'
opakovani = 8
goalmarkrelation = 7
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek
'start positions'
startarr(0)=1 
startarr(1)=7
startarr(2)=3
startarr(3)=8
startarr(4)=4
startarr(5)=5
startarr(6)=1
startarr(7)=2
'goal positions'
laserarr(0)=5
laserarr(1)=1
laserarr(2)=1
laserarr(3)=5
laserarr(4)=6
laserarr(5)=2
laserarr(6)=4
laserarr(7)=7
for i = 0 to opakovani-1
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = laserarr(i)
  'goal definition
  cil = laserarr(i) + goalmarkrelation
  if(cil > 8) then cil = cil - 8 
  phases(p).sectors(s).carr(i+posun) = cil * 45 + uhelzmena 'cil je vazan ke znacce o jeden skok
  phases(p).sectors(s).cnamearr(i+posun) = "C2" 
next i
posun = posun + opakovani

'-----------------------------'
'ALLO fixni cil'
'-----------------------------'
opakovani = 8
goalmarkrelation = 2
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek

'start positions'
startarr(0)=7 
startarr(1)=4
startarr(2)=8
startarr(3)=2
startarr(4)=8
startarr(5)=5
startarr(6)=6
startarr(7)=1

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
opakovani = 8
goalmarkrelation = 2
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek
'start position
startarr(0)=2 
startarr(1)=6
startarr(2)=3
startarr(3)=4
startarr(4)=7
startarr(5)=1
startarr(6)=5
startarr(7)=8
'mark positions
laserarr(0)=5 
laserarr(1)=1 
laserarr(2)=5
laserarr(3)=8
laserarr(4)=2
laserarr(5)=6
laserarr(6)=7
laserarr(7)=4
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
opakovani = 8
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
opakovani = 8
goalmarkrelation = 5 'zmena ze 4 na 5 16.12.2008'     
redim startarr(0 to opakovani-1) as integer 'pozice startu
redim laserarr(0 to opakovani-1) as integer 'pozice znacek
'start positions
startarr(0)=5
startarr(1)=3
startarr(2)=8
startarr(3)=4
startarr(4)=3
startarr(5)=7
startarr(6)=4
startarr(7)=2
for i = 0 to opakovani-1
  phases(p).cues(0).laserarr(i+posun) = startarr(i)  
  phases(p).cues(1).laserarr(i+posun) = 0 ' neni cue'
  cil = startarr(i) + goalmarkrelation  'cil je vazan ke startu'
  if(cil > 8) then cil = cil - 8
  phases(p).sectors(s).carr(i+posun) = (cil-1) * 45 + uhelzmena
  phases(p).sectors(s).cnamearr(i + posun) = "E2"
next i