library(dagitty)
acu_dag1 <- dagitty('dag {
    randomisation [pos="0,1"]
    group [pos="1,2"]
    pk5_obs [pos="2,2"]
    pk5_miss [pos="3,1"]
    
    randomisation -> group -> pk5_obs
    randomisation-> pk5_obs
    group -> pk5_miss
    randomisation-> pk5_miss <- pk5_obs
}') #observed data has something to do with the missingness?

plot(acu_dag1)

acu_dag2 <- dagitty('dag {
    randomisation [pos="0,1"]
    group [pos="1,2"]
    pk5_obs [pos="2,2"]
    pk5_miss [pos="3,1"]
    
    randomisation -> group -> pk5_obs
    randomisation-> pk5_obs
    group -> pk5_miss
    randomisation-> pk5_miss 
}')

plot(acu_dag2) #observed data has nothing to do with the missingness?


random_vital_dag <- dagitty('dag {
    randomisation [pos="0,1"]
    fishoilactive [pos="1,2"]
    vitdactive [pos="0,2"]
    control [pos="2,2"]
    pain_yr4 [pos="2,1"]
    pain_base [pos="0,0"]
    
    randomisation -> fishoilactive -> pain_yr4
    randomisation -> vitdactive -> pain_yr4
    randomisation -> control -> pain_yr4
    randomisation-> pain_yr4
    pain_base -> pain_yr4
}')


MCAR_dag_old <- dagitty('dag {
    R_Y [pos="0,1"]
    X [pos="1,1"]
    Y [pos="2,1"]
    
    X -> Y
    R_Y
}')


MAR_dag_old <- dagitty('dag {
    R_Y [pos="0,1"]
    X [pos="1,1"]
    Y [pos="2,1"]
    
    X -> Y
    X -> R_Y
}')

MNAR_dag_old <- dagitty('dag {
    R_Y [pos="0,1"]
    X [pos="1,1"]
    Y [pos="1,2"]
    
    X -> Y
    Y -> R_Y
}')


dag_outcome_base_miss <- dagitty('dag {
  randomisation         [pos="0,2"]
  treatment             [pos="1.5,2.5"]
  observed_outcome      [pos="3,2"]
  missing_outcome       [pos="3,3"]
  baseline_variable1_obs [pos="1,3.5"]
  baseline_variable2_obs [pos="1,1.5"]
  baseline_variable1_miss [pos="1,4.5"]
  baseline_variable2_miss [pos="1,0.5"]
  
    randomisation -> treatment
    treatment -> observed_outcome 
    observed_outcome -> missing_outcome <- treatment
    baseline_variable1_obs -> observed_outcome <- baseline_variable2_obs
    baseline_variable1_miss -> observed_outcome <- baseline_variable2_miss
    baseline_variable1_obs -> missing_outcome <- baseline_variable2_obs
    baseline_variable1_miss -> missing_outcome <- baseline_variable2_miss
    randomisation -> observed_outcome
    randomisation -> missing_outcome 
}')



MAR_dag1 <- dagitty('dag {
  L         [pos="0,2"]
  Y       [pos="3,2"]
  R_Y       [pos="3,3"]
  X_1      [pos="1,3.5"]
  X_2      [pos="1,1.5"]
  
    L -> Y
    X_1 -> Y <- X_2
    X_1-> R_Y <- X_2
}')
MAR_dag1<-ggdag(MAR_dag1, layout = "circle") +
  theme_dag() + 
  ggtitle("")
MAR_dag1
MAR_dag2 <- dagitty('dag {
  L     [pos="0,2"]
  X_1   [pos="1,3.5"]
  X_2   [pos="1,1.5"]
  Y     [pos="3,2"]
  R_Y   [pos="3,3"]

  L -> Y
  X_1 -> Y
  X_2 -> Y
  X_1 -> R_Y
  L -> R_Y
}')
MAR_dag2<-ggdag(MAR_dag2, layout = "circle") +
  theme_dag() + 
  ggtitle("")
MAR_dag2



MCAR_dag <- dagitty('dag {
 L       [pos="0,2"]
 Y             [pos="3,2"]
 R_Y                    [pos="3,3"]
 X_1 [pos="1,3.5"]
 X_2 [pos="1,1.5"]
  
     L -> Y
     X_1 -> Y <- X_2
     L -> Y
}')
MCAR_dag<-ggdag(MCAR_dag, layout = "circle") +
  theme_dag() + 
  ggtitle("")
MCAR_dag
MNAR_dag1 <- dagitty('dag {
  L         [pos="0,2"]
  Y       [pos="3,2"]
  R_Y       [pos="3,3"]
  X_1      [pos="1,3.5"]
  X_2      [pos="1,1.5"]
  
    X_1 -> Y <- X_2
    L -> Y
    Y -> R_Y
}')
MNAR_dag1<-ggdag(MNAR_dag1, layout = "circle") +
  theme_dag() + 
  ggtitle("")
MNAR_dag1

MNAR_dag2<-dagitty('dag {
  L     [pos="0,2"]
  X_1   [pos="1,3.5"]
  X_2   [pos="1,1.5"]
  Y     [pos="3,2"]
  R_Y   [pos="3,3"]

  L -> Y
  X_1 -> Y
  X_2 -> Y

  Y -> R_Y
  X_2 -> R_Y
}')
MNAR_dag2<-ggdag(MNAR_dag2, layout = "circle") +
  theme_dag() + 
  ggtitle("")
MNAR_dag2




dag_base_miss <- dagitty('dag {
  randomisation         [pos="0,2"]
  treatment             [pos="1.5,2.5"]
  observed_outcome      [pos="3,2"]
  baseline_variable1_obs [pos="1,3.5"]
  baseline_variable2_obs [pos="1,1.5"]
  baseline_variable1_miss [pos="1,4.5"]
  baseline_variable2_miss [pos="1,0.5"]
  
    randomisation -> treatment
    treatment -> observed_outcome 
    baseline_variable1_obs -> observed_outcome <- baseline_variable2_obs
    baseline_variable1_miss -> observed_outcome <- baseline_variable2_miss
    randomisation -> observed_outcome
}')


