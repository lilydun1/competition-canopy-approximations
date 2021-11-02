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
  * _stand_fla_27 = Whole stand light interception with the focal trees with 27 LA. <br />

The syntax of the individual stands folders go like this:
 * H#_V#_L#_F#_fla#_S#
 * H = The average height of the stand. 
 * V = The coefficient of variation of the trunk heights of the stand. 
 * L = The LAI of the stand, so correlates to how many trees there are.
 * fla = The LA of the focal tree/trees.
 * S = The seed or which random distribution the trunk heights were sampled from.

### models 
Function_M.R = The script with all the functions to parameterise the canopy approximations. <br />
Results.R = The script with the graphs and statistical tests for the different experiments. Seperated into the different experiments for all the graphs/results. <br />
running.R = The script on loading in the MAESPA outputs and running all the functions. Seperated into the different experiments. <br />
simulations = Copied folders from the MAESPA simulations. All the stands that get loaded in. <br />
met.dat = For getting the hourly PAR. <br />
Different maespa csv = All the maespa outputs as csvs, named similarly to the simulations folder. <br />





