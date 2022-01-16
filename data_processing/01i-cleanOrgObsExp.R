# Organizes destructive analyses data, except results from leaf
# area of calibration plants and data extracted from pictures

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(zoo)

# Load files --------------------------------------------------------------
analysis_files <- list.files('../data/observations/monitoring/', 
                             pattern = '^analises_ciclo',
                             full.names = TRUE)

# Mass destructive analysis -----------------------------------------------
temp <- list()
temp_nodes <- list()

for (filename in analysis_files){
  
  sheets <- excel_sheets(filename)
  
  harvest <- read_xlsx(filename, sheet = "harvest")
  harvest['type'] <- 'harv'
  destructive <- read_xlsx(filename, sheet = "destructive")
  destructive['type'] <- 'dest'

  if ("nodes" %in% sheets){
    
    nodes <- read_xlsx(filename, sheet = "nodes")
    
    temp_nodes[[filename]] <- nodes
    
  }
  
  if ("height_meas" %in% sheets){
    
    nodes <- read_xlsx(filename, sheet = "nodes")
    
    temp_nodes[[filename]] <- nodes
    
  }
  
  temp[[filename]] <- destructive %>%
    rbind(harvest) 

  }

analysis <- Reduce(rbind, temp)

write.csv(analysis, 
          file = '../data/observations/monitoring/analysis.csv',
          row.names = FALSE)


# Nodes -------------------------------------------------------------------
nodes_calib <- Reduce(rbind, temp_nodes)

write.csv(nodes_calib, 
          file = '../data/observations/monitoring/nodes/n_calib_ids.csv',
          row.names = FALSE)