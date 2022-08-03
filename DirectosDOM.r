rm(list = ls())

library(survey)
library(srvyr)
library(dplyr)

encuesta <- readRDS("encuestaDOM20N1.rds")
names(encuesta)

encuesta$estrato <- encuesta$'_estrato'
table(encuesta$estrato)
encuesta$upm <- encuesta$'_upm'
table(encuesta$upm)
encuesta$fep <- encuesta$'_fep'
summary(encuesta$fep)
sum(encuesta$fep)

encuesta$pobre <- as.numeric(ifelse(encuesta$pobreza != 3, 1, 0))
encuesta$one <- as.factor(1)

table(encuesta$pobre)

diseno <- encuesta %>% 
  as_survey_design(strata = estrato,
                   ids = upm,
                   weights = fep,
                   nest = TRUE)

options(survey.lonely.psu = "adjust")

summary(diseno)

consulta1 <- diseno %>% 
  summarise(
    totaling = survey_total(ingcorte)
  )


consulta1 <- diseno %>% 
  group_by(id_provincia, areageo2) %>% 
  summarise(
    mediaing = survey_mean(ingcorte,
                           vartype = c("se", "cv", "ci"))
  )


consulta1 %>% as.data.frame()

consulta2 <- diseno %>% 
  group_by(id_provincia, areageo2) %>% 
  summarise(
    proppob = survey_mean(pobre,
                            vartype = c("se", "cv", "ci"), )
    )
 


consulta2 %>% as.data.frame()

    
    
    