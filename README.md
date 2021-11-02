# Honours

## Description
Honours project on how well canopy approximations used in large-scale vegetation models capture competition. 

## How to guide 
1.  Go into the project Simulations then run the Function R script to load the functions then run the Sims R script to create the virtual tree folders and run the MAESPA simulations. 
2. Go into the project modes and run the Function_M R script to load the functions then run the running R script to load in the virtual stands and create the canopy approxmations. 
3. The running R script is seperated into the different computational experiments. 
4. The Results R script is where the code for the graphs are located. The script is seperated by the different outcomes of the different experiments. 


## Dictionary 
### Maespa simulations
R folder = R scripts 
  * Functions.R = The functions for creating the different virtual stands and running MAESPA. <br />
  * results.R = Graphs just looking at the MAESPA results by themselves. <br />

Sims.R = The Script for using all the functions to create the folders and run MAESPA. <br />
template_A = The template folder that has the files needed to run MAESPA. The files are motified to create the different stands. <br />
simulations folders = The MAESPA stands from the different computational experiments. <br />
  * _c_fla = Changing focal tree LA, used to understand the effects of self-shading. <br />
  * _fla_0.1 = Individual compeition with the focal tree with a 0.1 LA. <br />
  * _fla_27 = Individual compeition with the focal tree with a 27 LA. <br />
  * _stand_fla_0.1 = Whole stand light interception with the focal trees with 0.1 LA. <br />
  * _stand_fla_27 = Whole stand light interception with the focal trees with 0.1 LA. <br />


### models 
Function_M.R = all the functions <br />
Results.R = for all the graphs <br />
running.R = running all the functions and loading in the MAESPA outputs <br />
simulations = copied from maespa simulations <br />
met.dat = for getting the hourly PAR <br />
Different maespa csv = all the maespa outputs as csvs <br />





