' ucici krivka pro allo a ego zvlast
' posledni zmena 28.08.2008 18:15:05'
phasesno = 1 ' pocet faze'
REDIM phases(0 TO phasesno - 1) AS phase
pocetpokusu = 32
'------------- faze 0 '
p = 0
phases(p).sectorsno = 1
phases(p).cuesno = 2
phases(p).cuesset = 1
phases(p).keytocues = "c" 'c pro zobrazeni cues v kazde fazi'
phases(p).keytostart = "" 'c pro zobrazeni cues v kazde fazi'
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

startcil = 2 'vztah startu a cile v ego'
lasercil = 5 'vztah cue a cile v allo'
uhelzmena = 20 'kolik stupnu je cil od 45 stupnovych intervalu'
pocetopakovanicile = 2 
pocetopakovanistartu = 2
redim startarr(2) as integer 'jake dva starty se budou ukazovat v allo'
startarr(1)=1 
startarr(2)=6
redim cuearr(2) as integer 'jake dve cues se budou ukazovat v ego'
'=cisla cues, ktera se prostridaji pro kazdy start v ego'
cuearr(1)=1
cuearr(2)=4

' --------- ALLO 0-15 ---------------------------'
for cil = 1 to 8
  for startindex = 1 to pocetopakovanicile ' start nezavisi na cili'
    pokus = (cil-1)*pocetopakovanicile+startindex-1 'hodnoty 0-15  - index v poli laserarr
  	
  	'START'
		start = startarr(startindex)
    phases(p).cues(0).laserarr(pokus) = start
    
		'CUE'
    laser = cil - lasercil '!!!! ZNACKA vs. CIL' 
    if laser < 1 then laser = laser + 8 'pozice cue 1-8'
    phases(p).cues(1).laserarr(pokus) = laser
    
    'CIL'
    uhel = (cil-1) * 45 + uhelzmena '!!!! UHEL CILE od celych 45 stupnu - 0 je na kruhu (arene) vpravo, 90 je nahore'
    if uhel >= 360 then uhel = uhel - 360
    phases(p).sectors(s).carr(pokus) = uhel 'cil'
    
    'NAME'
    if start - cil = startcil or cil - start = 8-startcil then  'pokud se shoduje i pozice startu je to allo ego' 
      phases(p).sectors(s).cnamearr(pokus) = "AE" 'allo ego'
    else
      phases(p).sectors(s).cnamearr(pokus) = "A"+ltrim$(str$(pokus)) 'allo'
    end if
    
  next startindex
next cil
  
' --------- EGO 16-31 ---------------------------'
for cil = 1 to 8
  for laserindex = 1 to pocetopakovanistartu 'cue nezavisi na cili'
    pokus = 16+(cil-1)*pocetopakovanistartu+laserindex-1 'hodnoty 64-95'
		
		'CUE'
    laser = cuearr(laserindex) 'pozice cue 1-8'
		phases(p).cues(1).laserarr(pokus) = laser
		
    'START'
    start = cil - startcil '!!!! CIL vs START'
    if start < 1 then start = start + 8
    phases(p).cues(0).laserarr(pokus) = start
    
    'CIL'
    uhel = (cil-1) * 45 + uhelzmena '!!!! UHEL CILE od celych 45 stupnu
    if uhel >= 360 then uhel = uhel - 360
    phases(p).sectors(s).carr(pokus) = uhel 'cil'
    
    'NAME'
    if laser - cil = lasercil or cil - laser = lasercil then  'pokud se shoduje i pozice cue je to allo ego'
      phases(p).sectors(s).cnamearr(pokus) = "AE" 'allo ego'
    else
      phases(p).sectors(s).cnamearr(pokus) = "E"+ltrim$(str$(pokus)) 'e'
    end if
    
  next laserindex
next cil

' --------- PREHAZENI ---------------------------'
'prehazim vsechno dohromady  0 - 31'
' ale musim necha 4 ego a 4 allo pohromade'
' takze jsem si v excelu udelal poradi, jake chci a do toho to ted potrebuju prehodit'
dim poradi (0 TO pocetpokusu) AS INTEGER
poradi(0)=27
poradi(1)=17
poradi(2)=18
poradi(3)=30
poradi(4)=4
poradi(5)=10
poradi(6)=1
poradi(7)=15
poradi(8)=23
poradi(9)=20
poradi(10)=22
poradi(11)=25
poradi(12)=7
poradi(13)=3
poradi(14)=11
poradi(15)=0
poradi(16)=28
poradi(17)=21
poradi(18)=31
poradi(19)=26
poradi(20)=2
poradi(21)=14
poradi(22)=6
poradi(23)=13
poradi(24)=16
poradi(25)=24
poradi(26)=19
poradi(27)=29
poradi(28)=12
poradi(29)=5
poradi(30)=9
poradi(31)=8

call prehodcfgto(phases(), p,poradi(),pocetpokusu)


' CALL prehodcfg(15, 7, phases(), p)
' CALL prehodcfg(8, 5, phases(), p)
' CALL prehodcfg(0, 9, phases(), p)
' CALL prehodcfg(10, 1, phases(), p)
' CALL prehodcfg(11, 14, phases(), p)
' CALL prehodcfg(2, 12, phases(), p)
' CALL prehodcfg(13, 3, phases(), p)
' CALL prehodcfg(4, 6, phases(), p)
' 
' CALL prehodcfg(15+16, 7+16, phases(), p)
' CALL prehodcfg(8+16, 5+16, phases(), p)
' CALL prehodcfg(0+16, 9+16, phases(), p)
' CALL prehodcfg(10+16, 1+16, phases(), p)
' CALL prehodcfg(11+16, 14+16, phases(), p)
' CALL prehodcfg(2+16, 12+16, phases(), p)
' CALL prehodcfg(13+16, 3+16, phases(), p)
' CALL prehodcfg(4+16, 6+16, phases(), p)


