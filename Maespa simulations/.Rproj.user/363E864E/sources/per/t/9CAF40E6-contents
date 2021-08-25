A <- aggregate(output_combined_A$totPs, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
A <- rename(A, totPs = x)

B <- aggregate(output_combined_A$absPAR, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
B <- rename(B, absPAR = x)

C <- aggregate(output_combined_A$absNIR, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
C <- rename(C, absNIR = x)

D <- aggregate(output_combined_A$absTherm, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
D <- rename(D, absTherm = x)

E <- aggregate(output_combined_A$netPs, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
E <- rename(E, netPs = x)

F <- aggregate(output_combined_A$totRf, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
F <- rename(F, totRF = x)

G <- aggregate(output_combined_A$totLE1, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
G <- rename(G, totLE1 = x)

H <- aggregate(output_combined_A$totLE2, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
H <- rename(H, totLE2 = x)

I <- aggregate(output_combined_A$totH, by=list(H=output_combined_A$H, V=output_combined_A$V, L=output_combined_A$L, F=output_combined_A$F),data=output_combined_A,FUN=mean)
I <- rename(I, totH = x)

absPAR <- B$absPAR
absNIR <- C$absNIR
absTherm <- D$absTherm
netPs <- E$netPs
totRF <- F$totRF
totLE1 <- G$totLE1
totLE2 <- H$totLE2
totH <- I$totH
path <- output_combined_A$path
mn_outputs_A <- cbind(path, A, absPAR, absNIR, absTherm, netPs, totRF, totLE1, totLE2, totH)
