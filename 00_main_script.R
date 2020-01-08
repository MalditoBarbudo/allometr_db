# New allometries database version script
# 
# This is needed because the db must be improved to better implementation of all
# the data.

## Libraries and data access ####
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(pool)
library(magrittr)
library(stringr)
library(glue)
# library(tidyIFN)

## database connections ####
# original db connections
oracle_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  # user = 'ifn',
  # password = rstudioapi::askForPassword('Password for ifn'),
  user = 'guest',
  password = 'guest',
  dbname = 'oracle_ifn',
  host = '158.109.46.23',
  port = 5433
)

# access4_db <- dbPool(
#   RPostgreSQL::PostgreSQL(),
#   user = 'ifn',
#   password = rstudioapi::askForPassword('Password for ifn'),
#   dbname = 'ifn4_access'
# )

# new db connection
allometr_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'allometr_db',
  host = '158.109.46.23',
  port = 5432
)

## scripts sources ####
source('01_creaf_allometries.R')

source('98_thesauruses.R')
source('99_db_tables_creation.R')

## closing pools ####
poolClose(oracle_db)
# poolClose(access4_db)
poolClose(allometr_db)
