args = commandArgs(trailingOnly=TRUE)

name      <- args[1]                # read first argument as string

# print(paste("Processing with name:'", name, sep = ''))
seq(name, by = 25, length.out=4)