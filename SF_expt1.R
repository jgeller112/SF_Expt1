
  
install.packages("qualtRics")
install.packages("tidyverse")
install.packages("afex")
install.packages("emmeans")

library(qualtRics)
library(tidyverse)
library(afex)
library(emmeans)

#Read in each qualtrics file. For SF and Generate conditions, we used two counterbalanced lists so each cue-target pair was presented in the fluent (read) and disfluent conditions (generate or SF font) across participants. 


gen1 <- readSurvey("generate1cb.csv")

# cb 1 
gen2 <- readSurvey("generate2cb.csv") 
# cb 2
sf1 <- readSurvey("sf1cb.csv")# Sans F 1
sf2 <- readSurvey("sf2cb.csv")#Sans F 2 

#In the code below we extract the relevant information from the qualtrics files we loaded in earlier. 



#cb1gen
gen1= gen1 %>% 
  dplyr::select(ResponseId,Finished, Progress, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, Q445, Q447, Q447, Q448, Q449, Q450, Q451, Q452, Q452, Q453, Q454, Q455, Q456, Q457, Q458) # select the columns relevant for

#cb2gen

gen2= gen2 %>% 
  dplyr::select(ResponseId,Finished, Progress, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, Q445, Q447, Q447, Q448, Q449, Q450, Q451, Q452, Q452, Q453, Q454, Q455, Q456, Q457, Q458) 

sf1= sf1 %>% 
  dplyr::select(ResponseId,Finished, Progress, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, Q445, Q447, Q447, Q448, Q449, Q450, Q451, Q452, Q452, Q453, Q454, Q455, Q456, Q457, Q458) 
#cb2gen
sf2 = sf2 %>% 
  dplyr::select(ResponseId,Finished, Progress, Q434, Q435, Q436, Q437, Q438, Q439, Q440, Q441, Q442, Q443, Q444, Q445, Q447, Q447, Q448, Q449, Q450, Q451, Q452, Q452, Q453, Q454, Q455, Q456, Q457, Q458) 
#cb2gen
```

#Per our pre-registration criteria, we only include individuals that finished the experiment. We also include individuals with 99% completion rate (it appears they completed the experiment but did not click off the last page). 


gen1fin <- subset(gen1, gen1$Finished==TRUE | gen1$Progress==99) 
gen2fin <- subset(gen2, gen2$Finished==TRUE | gen2$Progress==99) 
gen2fin1 <- gen2fin[sample(nrow(gen2fin), 58), ] # randomly select 58 
sf1fin <- subset(sf1, sf1$Finished==TRUE | sf1$Progress==99) #
sf2fin <- subset(sf2, sf2$Finished==TRUE | sf2$Progress==99) 
sf2fin1 <- sf2fin[sample(nrow(sf2fin), 58),]
```
#Qualtrics files are imported in wide format. We want them in long format. The 'gather' function will do this for us. 
```{r}
t1=tidyr::gather(gen1fin, "question", "answer", Q434:Q458)

t2=tidyr::gather(gen2fin1, "question", "answer", Q434:Q458)

t3=tidyr::gather(sf1fin, "question", "answer", Q434:Q458)

t4=tidyr::gather(sf2fin1, "question", "answer", Q434:Q458)
```

#Load the words lists for each cb. Each line needs to be repeated 58 number of times (the number of respondents)


cb1gen=read.csv("/Users/gellr/Desktop/SF_Generate_Data/CB1.csv", header=TRUE)

cb1=data.frame(cue=rep(cb1gen$cue1, each=58), target=rep(cb1gen$targ1, each=58), cond=rep(cb1gen$font, each=58))

cb2gen=read.csv("/Users/gellr/Desktop/SF_Generate_Data/CB2.csv", header=TRUE)

cb2=data.frame(cue=rep(cb2gen$cue1, each=58), target=rep(cb2gen$targ1, each=58), cond=rep(cb2gen$font, each=58))

cb1sf=read.csv("/Users/gellr/Desktop/SF_Generate_Data/CB1_SF.csv", header=TRUE)

sfcb1=data.frame(cue=rep(cb1sf$cue1, each=58), target=rep(cb1sf$targ1, each=58), cond=rep(cb1sf$font, each=58))

cb2sf=read.csv("/Users/gellr/Desktop/SF_Generate_Data/CB2_SF.csv", header=TRUE)

sfcb2=data.frame(cue=rep(cb2sf$cue1, each=58), target=rep(cb2sf$targ1, each=58), cond=rep(cb2sf$font, each=58))



Merge each cb list with participant responses


gencb1=cbind(cb1, t1)

gencb2=cbind(cb2, t2)

sfcb1=cbind(sfcb1, t3)

sfcb2=cbind(sfcb2, t4)

# combine each condition list
gen12 <- rbind(gencb1, gencb2) # generate cb 1 and 2 

sf12 <- rbind(sfcb1, sfcb2) # sf cb 1 and 2 

gen12$condition <- "generate"

sf12$dis <- ifelse(sf12$cond=="flu", "fluent", "disfluent")
gen12$dis <- ifelse(gen12$cond=="flu", "fluent", "disfluent")
sf12$condition <- "SF"



# quick and dirty recall analysis
#takes the target word correctly spelled and matched it to response subject entered. It does not allow for incorrecly spelled words, but am working on it. 

sf12$acc<-ifelse(sf12$target==sf12$answer, 1, 0)
sf12[is.na(sf12)] <- 0 # no response to incorrect

gen12$acc <-ifelse(gen12$target==gen12$answer, 1, 0)

gen12[is.na(gen12)] <- 0 # no response to incorrect
```
We can Combine all the lists, but before we run our statistical analysis, we need to remove two cue-target pairs. There was an error in the generate CB 1 list wherein *train-plane* was presented twice during encoding and *rifle-range* was not presented at all. 
```{r}
sfgen<-rbind(gen12, sf12)

sfgen1<- sfgen %>% 
  dplyr::filter(target!="plane", target!="rifle")

## get aggreagte recall per subject, condition, and dis
sfgenagg <- sfgen1 %>% 
  dplyr::group_by(ResponseId, condition, dis) %>%
  dplyr::summarise(accuracy=mean(acc))




a1 <- aov_ez("ResponseId", "accuracy", sfgenagg, 
             within = c("dis"), between = c("condition")) # 2 X 2 Mixed ANOVA

#plot the results

kable(nice(a1))

af1=afex_plot(a1, x = "dis", panel = "condition", 
              error = "within",  mapping = c("color", "fill"))

af2=af1+
  theme_set(theme_light(base_size = 14)) + labs(x="Disfluency", y="Cued Recall Performance")

print(af2)

ls1 <- emmeans(a1, c("dis"), by="condition") # get the simple effects test for signifcant interaction. 

flex1=pairs(ls1)

kable(flex1)



