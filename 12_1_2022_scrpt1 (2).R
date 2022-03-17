#adding data

library(tidyverse)
library(openxlsx)
data <- "data.xlsx" %>%  
  read.xlsx(sheet="s1") %>% 
  drop_na(.)

#creating a function 

frmse <- function(OMY,PMY)
{mspe <- mean(OMY-PMY)^2
rmse <- sqrt(mspe)
return(rmse)}

frmse(OMY = data$OMY,PMY = data$PMY)
library(tidyverse)
library(openxlsx)
biruk <- "Holeta.xlsx" %>% 
  read.xlsx(sheet="Milky")
#linear regression, designate name for the equation,add independent variables, name the eqauation= Equ1 
#lm(independent variable)
Equ1 <- lm(OMY~PMY,data=biruk)
Equ1
#if I want to other parameters (Standard error. R2 etc put summary(name of equation Equ1))
summary(Equ1)
#to add more variables in the model
Equ2 <- lm(OMY~PMY+ADG+VDMI+MEdt+MEI+MEm+MEg+MEc+MEa,data=biruk)
Equ2
summary(Equ2)
#to see the names of the colmn put str(name of the data)
str(biruk)
#Stepwise (backward) multiple linear regression
#to dowload ölsrr
library(olsrr)
ols_step_backward_p(Equ2)
#forward MLR
ols_step_forward_p(Equ2)

# Creating a summary table
library(dplyr)
library(tidyverse)
library(strex)
library(inti)
library(shiny)
library(openxlsx)
# Table creation 
biruk <- "Holeta (1).xlsx" %>% 
  read.xlsx(sheet = "Milky") 
table1 <- biruk %>% 
  select(Breed, OMY, VDMI, MEI, MEg, MEc, MEa, totlac, PMY) %>% 
  group_by(Breed) %>% 
  summarise_all(
    list(~ mean(., na.rm = TRUE)
         , ~ sd(., na.rm = TRUE)
         ,  ~ min(., na.rm = TRUE)
         ,  ~ max(
           ., na.rm = TRUE)
    )
  ) %>% 
  pivot_longer(!Breed) %>% #change the table by type
  mutate(variable = str_before_last(name, "_")) %>% 
  mutate(smr = str_after_last(name, "_")) %>% 
  select(!name) %>% 
  unite(smr, c("Breed", "smr")) %>% 
  pivot_wider(names_from = "smr", values_from = "value") %>% 
  web_table()

table1
