library(GetTDData)
library(tidyr)
library(rio)
df.yield <- get.yield.curve()

nome <- as.character(df.yield[1,"current.date"])
assign(nome, df.yield)

export(get(nome), paste(nome,"xlsx",sep = "."))

# Open the scheduler: START -> All Programs -> Accesories -> System Tools -> Scheduler
# Create a new Task
# under tab Action, create a new action
# choose Start Program
# browse to Rscript.exe which should be placed e.g. here:
#   "C:\Program Files\R\R-3.0.2\bin\x64\Rscript.exe"
# input the name of your file in the parameters field
# input the path where the script is to be found in the Start in field
# go to the Triggers tab
# create new trigger
# choose that task should be done each day, month, ... repeated several times, or whatever you like