# Level 2 MF
calculating level2 type2 membership function
A New Method for Calibrating the Fuzzy Sets Used in fsQCA

Mehdi Korjani,

##
A number of methods have been published on how to map data collected from a group of subjects into the FOU of a word. The Interval Approach (IA) was the first such method; it was followed by the Enhanced Interval Approach (EIA) , which makes use of more information from the collected data than does the IA. More recently, the HM Approach (HMA) was developed; it makes use of even more information from the collected data than does the EIA.

The IA, EIA and HMA were all initially developed assuming that data can be collected from a group of subjects, and that such a group is available. Sometimes such a group is not available and only one knowledgeable expert is available, e.g. Prof. Ragin or any other fsQCA scholar. Mendel and Wu have presented a different way to collect data from one person, after which that data can be used to generate even more data for what might be called a “virtual group of subjects.” The HMA is then directly applied to that larger set of data.

J. Mendel, M. Korjani, "A New Method for Calibrating the Fuzzy Sets Used in fsQCA," Information science, 2018

## how to run the program
The program calculate FOUs for three words and provide FOU and center of set of each FOU which can be used for assigning breakpoints in fsQCA and generate S-shape level2 MF,
It also illustrates FOU to see the MFs

### 1- Install R and R Studio
You need to install R (https://www.r-project.org) and R Studio (https://www.rstudio.com/products/rstudio/download/)

### 2- install R package: library(pracma)

    install.packages('pracma')
    
### 3- Download the package on your computer

you can download it https://github.com/korjani/Level2Type2MF/archive/master.zip

or

You can clone the repository in the terminal: git clone git@github.com:korjani/Level2Type2MF.git


### 4- go to the directory folder

### 5- set intervals in oneSubjectSshapeMF.R
define end points for one subject interval

    1- On the scale of l to r, what are the endpoints of an interval of numbers ([aL,bL]) that you associate with the left end-point of the word
    2- On the scale of l to r, what are the endpoints of an interval of numbers ([aR,bR]) that you associate with the right end-point of the word
dataL = c(aL1, aR1, aL2, aR2, aL3, aR3)

dataR = c(bL1, bR1, bL2, bR2, bL3, bR3)

### 6- run the program in R

### 7- Results are stored in: 

    MFsLow: 
    MFsMod
    MFsHig
    CenterLow: Low membership breakpoint (fully out)
    CenterModerate: Moderate membership breakpoint (nor fully in nor out)
    CenterHigh: high membership breakpoint (fully in)
    
    FOUs has nine parameters:
    (p1, p2, p3, p4, p5, p6, p7, p8, p9)
    
    ![Alt text](./FOU.png?raw=true "FOU")
    
    The program also show FOU to make sure that computation
    
    

