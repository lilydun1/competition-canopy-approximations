A <- aggregate(output_combined_WD$totPs, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
A <- rename(A, totPs = x)

B <- aggregate(output_combined_WD$absPAR, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
B <- rename(B, absPAR = x)

C <- aggregate(output_combined_WD$absNIR, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
C <- rename(C, absNIR = x)

D <- aggregate(output_combined_WD$absTherm, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
D <- rename(D, absTherm = x)

E <- aggregate(output_combined_WD$netPs, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
E <- rename(E, netPs = x)

F <- aggregate(output_combined_WD$totRf, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
F <- rename(F, totRF = x)

G <- aggregate(output_combined_WD$totLE1, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
G <- rename(G, totLE1 = x)

H <- aggregate(output_combined_WD$totLE2, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
H <- rename(H, totLE2 = x)

I <- aggregate(output_combined_WD$totH, by=list(H=output_combined_WD$H, V=output_combined_WD$V, L=output_combined_WD$L, F=output_combined_WD$F, WD=output_combined_WD$WD),data=output_combined_WD,FUN=mean)
I <- rename(I, totH = x)

absPAR <- B$absPAR
absNIR <- C$absNIR
absTherm <- D$absTherm
netPs <- E$netPs
totRF <- F$totRF
totLE1 <- G$totLE1
totLE2 <- H$totLE2
totH <- I$totH
path <- output_combined_WD$path
mn_outputs_WD <- cbind(path, A, absPAR, absNIR, absTherm, netPs, totRF, totLE1, totLE2, totH)