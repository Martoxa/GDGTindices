## MBT'5Me index from de Jonge, C., et al., 2014
MBT5<-function(raw){
  (raw$Ia+raw$Ib+raw$Ic)/(raw$Ia+raw$Ib+raw$Ic+raw$IIa+raw$IIb+raw$IIIa)
}
## MBT'6Me index based on Dang, X., et al., 2016
MBT6<-function(raw){
  (raw$Ia+raw$Ib+raw$Ic)/(raw$Ia+raw$Ib+raw$Ic+raw$`IIa'`+raw$`IIb'`+raw$`IIIa'`)
}
## Fractional Cyclization ratio based on Martinez-Sosa, P., et al., 2019
fC<-function(raw){
  ((raw$IIb+raw$Ib+raw$`IIb'`)+2*(raw$Ic))/((raw$IIIa+raw$`IIIa'`+raw$IIa+raw$`IIa'`+raw$Ia)+(raw$IIb+raw$Ib+raw$`IIb'`)+(raw$Ic))*0.5
}
## CBT' index from de Jonge, et al., 2014
CBTp<-function(raw){
  log10((raw$Ic+raw$`IIa'`+raw$`IIb'`+raw$`IIIa'`)/(raw$Ia+raw$IIa+raw$IIIa))
}
## Isomer Ratio based on Dang, X., et al., 2016
IR<-function(raw){
  (raw$`IIa'`+raw$`IIb'`+raw$`IIIa'`)/(raw$IIa+raw$IIb+raw$IIIa+raw$`IIa'`+raw$`IIb'`+raw$`IIIa'`)
}
## IBT index based on Ding, S., et al., 2015
IBT<-function(raw){
  -log10((raw$`IIa'`+raw$`IIIa'`)/(raw$IIa+raw$IIIa))
}
## Community Index based on de Jonge, C., et al., 2019
CI<-function(raw){
  (raw$Ia+raw$Ib)/(raw$Ia+raw$IIa+raw$IIIa+raw$`IIIa'`)
}
## TEX86 index based on Schouten, S., et al., 2002
TEX86<-function(raw){
  (raw$GDGT2+raw$GDGT3+raw$`Cren'`)/(raw$GDGT1+raw$GDGT2+raw$GDGT3+raw$`Cren'`)
}
## BIT index based on Hopmans, E., et al., 2004
BIT<-function(raw){
  (raw$Ia+raw$Ib+raw$Ic+raw$IIa+raw$`IIa'`+raw$IIb+raw$`IIb'`+raw$IIc+raw$`IIc'`+raw$IIIa+raw$`IIIa'`+raw$IIIb+raw$`IIIb'`)/(raw$Ia+raw$Ib+raw$Ic+raw$IIa+raw$`IIa'`+raw$IIb+raw$`IIb'`+raw$IIc+raw$`IIc'`+raw$IIIa+raw$`IIIa'`+raw$IIIb+raw$`IIIb'`+raw$Cren)
}
## Methane Index based on Zhang, Y., et al., 2011
MI<-function(raw){
  (raw$GDGT1 + raw$GDGT2 + raw$GDGT3)/(raw$GDGT1 + raw$GDGT2 + raw$GDGT3 + raw$Cren + raw$`Cren'`)
}
