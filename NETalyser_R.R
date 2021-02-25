
library(ggplot2)
library(EMCluster)
library(plyr)
library(stats)

##csv/Exel options: 
    #csv.option <- 1 : point for decimal
    #csv.option <- 2 : comma for decimal

csv.option <- 1


##plot themes
theme.scatter <- theme()
theme.facet <- theme_light()
theme.summary <- theme()

## load data
load.IJdata <- function(data.id="data"){
  
  #open and make new variables
  file.select <- file.choose()
  df <- read.csv2(file.select, header = TRUE, sep = "\t")
  df$Int <- as.numeric(as.character(df$Int))
  df$Int.norm <- df$Int
  df$NET.class <- 0
  df$mark.class <- 0
  df$class <- 0
  df$dir <- dirname(file.select)
  df$experiment <- strsplit(basename(file.select), "[.]")[[1]][1]
  df$version <- "preAnalysis"
  df$method <- "none"
  df$mark <- "none"
  df$label <- ""
  #set a 'cumulative' lablel depending on ID variables
  ID.list <- names(df)[grep("ID", names(df))]
  for (ID in ID.list) {
    df$label <- paste(df$label, df[[ID]], sep = "\n")
  }
  
  #assign the dataframes 
  #dataframe for all values larger than the maximum size for single nucleus/NET - if it exists
  if(max(df$Area)>df$max.size[1]){
    df.NA <- df[df$Area>df$max.size,]
    df.NA$class <- 4
    assign(paste0(data.id,".NA"), df.NA, envir = globalenv())
  }
  
  
  #all other values and normalize (norm.int includes also the update.global function)
  assign(data.id, df[df$Area<=df$max.size,], envir = globalenv())
  norm.int(data.id = data.id)
  
  #create directories if not present already
  if(!dir.exists(file.path(df$dir[1], df$version[1]))){
    dir.create(file.path(df$dir[1], df$version[1]))
  }
  if(!dir.exists(file.path(file.path(df$dir[1], df$version[1]), "plots"))){
    dir.create(file.path(file.path(df$dir[1], df$version[1]), "plots"))
  }
  if(!dir.exists(file.path(file.path(df$dir[1], df$version[1]), "mapping"))){
    dir.create(file.path(file.path(df$dir[1], df$version[1]), "mapping"))
  }
  
  #assign global variables and update summary
  set.plot.range(x.min=0, x.max = df$max.size[1], y.min=0, y.max="max", percent.min = -5, percent.max = 100)
  set.plot.color()
  
  #show data
  print(simple.plot(get(data.id)))
}
load.Rdata <- function(data.id ="data"){
  
  #open the Rdata file
  df <- get(load(file.choose()))
  
  #assign the dataframes 
  #dataframe for all values larger than the maximum size for single nucleus/NET - if it exists
  if(max(df$Area)>df$max.size[1]){
    df.NA <- df[df$Area>df$max.size,]
    assign(paste0(data.id,".NA"), df.NA, envir = globalenv())
  }
  
  #all other values and normalize (norm.int includes also the update.global function)
  df <- df[df$Area<=df$max.size,]
  assign(data.id, df, envir = globalenv())
  norm.int(data.id = data.id)
  
  #create directories if not present already
  if(!dir.exists(file.path(df$dir[1], df$version[1]))){
    dir.create(file.path(df$dir[1], df$version[1]))
  }
  if(!dir.exists(file.path(file.path(df$dir[1], df$version[1]), "plots"))){
    dir.create(file.path(file.path(df$dir[1], df$version[1]), "plots"))
  }
  if(!dir.exists(file.path(file.path(df$dir[1], df$version[1]), "mapping"))){
    dir.create(file.path(file.path(df$dir[1], df$version[1]), "mapping"))
  }
  
  #assign global variables and update summary
  set.plot.range(x.min=0, x.max = df$max.size[1], y.min=0, y.max="max", percent.min = -5, percent.max = 100)
  set.plot.color()
  
  #show data
  print(simple.plot(get(data.id)))
  
}

## set thresholds and mark data points

EMC.threshold <- function(Area=TRUE, Int=TRUE, ignore.low=TRUE, data.id=analysis.name){
  
  #get the dataframe and reset the NET classification
  df <- get(data.id)
  df$NET.class <- 0
  
  #set the thresholds
  if(Area==TRUE && Int==TRUE){
    area.class <- init.EM(as.matrix(df$Area), nclass = 2, method = "em.EM")
    area.up <- which(area.class$Mu == max(area.class$Mu))
    area.low <- which(area.class$Mu == min(area.class$Mu))
    int.class <- init.EM(as.matrix(df$Int.norm), nclass = 2, method = "em.EM")
    int.up <- which(int.class$Mu == max(int.class$Mu))
    int.low <- which(int.class$Mu == min(int.class$Mu)) 
    df$NET.class[which(area.class$class==area.up & int.class$class==int.up)] <- 1
    if(ignore.low == TRUE){
      df$NET.class[df$Area < area.class$Mu[area.low] ] <- 0
      df$NET.class[df$Int.norm < int.class$Mu[int.low] ] <- 0
    }
    df$version <- format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S")
    df$method <- paste0("EMC, area(", round(as.numeric(min(df$Area[df$NET.class == 1], na.rm = TRUE)),2)," to " , round(as.numeric(max(df$Area[df$NET.class == 1], na.rm = TRUE)), 2),"), intensity(", round(as.numeric(min(df$Int.norm[df$NET.class == 1], na.rm = TRUE)), 2)," to " , round(as.numeric(max(df$Int.norm[df$NET.class == 1], na.rm = TRUE)), 2), ")")
  }
  if(Area==TRUE && Int==FALSE){
    area.class <- init.EM(as.matrix(df$Area), nclass = 2, method = "em.EM")
    area.up <- which(area.class$Mu == max(area.class$Mu))
    area.low <- which(area.class$Mu == min(area.class$Mu))
    df$NET.class[which(area.class$class==area.up)] <- 1
    if(ignore.low == TRUE){
      df$NET.class[df$Area < area.class$Mu[area.low] ] <- 0
    }
    df$version <- format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S")
    df$method <- paste0("EMC, area(", round(as.numeric(min(df$Area[df$NET.class == 1], na.rm = TRUE)),2)," to " , round(as.numeric(max(df$Area[df$NET.class == 1], na.rm = TRUE)), 2),")")
  }
  if(Area==FALSE && Int==TRUE){
    int.class <- init.EM(as.matrix(df$Int.norm), nclass = 2, method = "em.EM")
    int.up <- which(int.class$Mu == max(int.class$Mu))
    int.low <- which(int.class$Mu == min(int.class$Mu)) 
    df$NET.class[which(int.class$class==int.up)] <- 1
    if(ignore.low == TRUE){
      df$NET.class[df$Int.norm < int.class$Mu[int.low] ] <- 0
    }
    df$version <- format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S")
    df$method <- paste0("EMC, intensity(", round(as.numeric(min(df$Int.norm[df$NET.class == 1], na.rm = TRUE)), 2)," to " , round(as.numeric(max(df$Int.norm[df$NET.class == 1], na.rm = TRUE)), 2), ")")
  }
  
  #update dataframe 
  df$class <- (df$NET.class*2) + df$mark.class
  assign(data.id, df, envir = globalenv())
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
  
  #show data
  print(simple.plot(df))
}
Man.threshold <- function(Area.NET = NULL, Int.NET = NULL, data.id=analysis.name){
  
  #get the dataframe and reset the NET classification
  df <- get(data.id)
  df$NET.class <- 0
  
  #set the threshold
  if(!is.null(Area.NET) && !is.null(Int.NET)){
    df$NET.class[(df$Area >= Area.NET) & (df$Int.norm >= Int.NET)] <- 1
    df$version <- format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S")
    df$method <- paste0("manual, area(",Area.NET," to max), intensity(",Int.NET," to max)")
  }
  if(!is.null(Area.NET) && is.null(Int.NET)){
    df$NET.class[(df$Area >= Area.NET)] <- 1
    df$version <- format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S")
    df$method <- paste0("manual, area(",Area.NET," to max)")
  }
  if(is.null(Area.NET) && !is.null(Int.NET)){
    df$NET.class[(df$Int.norm >= Int.NET)] <- 1
    df$version <- format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S")
    df$method <- paste0("manual, intensity(",Int.NET," to max)")
  }
  
  #update dataframe 
  df$class <- (df$NET.class*2) + df$mark.class
  assign(data.id, df, envir = globalenv())
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
  
  #show data
  print(simple.plot(df))
}
mark.data <- function(area.max = "Max", area.min = "Min", int.max = "Max", int.min = "Min", data.id = analysis.name){
  
  #get the dataframe, reset the mark
  df <- get(data.id)
  df$mark.class <- 0
  
  #determine limits if not explicitly set
  if (area.max=="Max") {
    area.max <- max(df$Area, na.rm = TRUE)
  }
  if (area.min=="Min") {
    area.min <- min(df$Area, na.rm = TRUE)
  }
  if (int.max=="Max") {
    int.max <- max(df$Int.norm, na.rm = TRUE)
  }
  if (int.min=="Min") {
    int.min <- min(df$Int.norm, na.rm = TRUE)
  }
  
  #assign mark to cells
  df$mark.class[df$Area <= area.max & df$Area >= area.min & df$Int.norm <= int.max & df$Int.norm >= int.min] <- 1
  df$mark <- paste0("Area(",area.min,"-",area.max,"), Intensity(",int.min,"-",int.max,")")
  
  #update dataframe 
  df$class <- (df$NET.class*2) + df$mark.class
  assign(data.id, df, envir = globalenv())
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
  
  #show data
  print(simple.plot(df))
  
}

## export data to map on original images
map.data <- function(data.id=analysis.name){
  
  #get the data and prepare for export
  df <- get(data.id)
    #if .NA dataframe exists
    if(exists(paste0(data.id,".NA"))){
      df.NA <- get(paste0(data.id,".NA"))
      df.ex <- rbind2(df, df.NA)
    }else{
      df.ex <- df
    }
  
  df.ex <- df.ex[order(df.ex$file),]
  df.ex <- df.ex[,c("file","x.cord","y.cord","class")]
  
  #create directory if not present
  if(!dir.exists(file.path(save.dir(df),"mapping"))){
    dir.create(file.path(save.dir(df),"mapping"), recursive = TRUE)
  }
  
  #write the file
  write.table(df.ex, file=file.path(save.dir(df),"mapping", "mapping.txt"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
}

## show and save functions
show.data <- function(data.id=analysis.name){
  
  #get the dataframe
  df <- get(data.id)
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
  
  #show data
  print(simple.plot(df))
}
show.summary <- function(include=TRUE, data.id=analysis.name){
  #get the dataframe
  df <- get(data.id)
  
  #assign global variables and update summary
  update.global(include, data.id)
  
  #plot summary
  print(summary.plot(get(paste0(data.id,".sum"))))
}
save.data <- function(data.id=analysis.name){
  
  #get the dataframes
  df <- get(data.id)
  #if .NA dataframe exists
  if(exists(paste0(data.id,".NA"))){
    df.NA <- get(paste0(data.id,".NA"))
  }
  
  #determine whether the directoires for saves are already here
  if(!dir.exists(save.dir(df))){
    dir.create(save.dir(df), recursive = TRUE)
  }
  if(!dir.exists(file.path(save.dir(df),"plots"))){
    dir.create(file.path(save.dir(df),"plots"), recursive = TRUE)
  }
  if(!dir.exists(file.path(save.dir(df),"mapping"))){
    dir.create(file.path(save.dir(df),"mapping"), recursive = TRUE)
  }
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
  
  #show and save the cumulative plot
  print(simple.plot(df))
  pdf(file.path(save.dir(df),"data_plot.pdf"), width=15, height=10)
  print(simple.plot(df))
  dev.off()
  
  #save the raw data
    #if .NA dataframe exists join and order the data frames
    if(exists(paste0(data.id,".NA"))){
      df.full <- rbind2(df, df.NA)
    }else{
      df.full <- df
    }
    df.full <- df.full[order(df.full$file),]
  
    #save Rdata and .csv
    save(df.full, file = file.path(save.dir(df),"data.Rdata"))
    if (csv.option == 1) {
      write.csv(df.full, file = file.path(save.dir(df),"data.csv"))
    }
    if (csv.option == 2) {
      write.csv2(df.full, file = file.path(save.dir(df),"data.csv"))
    }
    
    
}
save.summary <- function(include=TRUE, data.id=analysis.name){
  
  #get the dataframe
  df <- get(data.id)
  
  #determine whether the directoires for saves are already here
  if(!dir.exists(save.dir(df))){
    dir.create(save.dir(df), recursive = TRUE)
  }
  
  #assign global variables and update summary
  update.global(include, data.id)
  
  #plot and save the summary
  print(summary.plot(get(paste0(data.id,".sum"))))
  pdf(file.path(save.dir(df),"summary_plot.pdf"), width=15, height=10)
  print(summary.plot(get(paste0(data.id,".sum"))))
  dev.off()
  
  #save the summary for all replicates and assign
  df.sum <- get(paste0(data.id,".sum"))
  save(df.sum, file = file.path(save.dir(df),"summary.Rdata"))
  if (csv.option == 1) {
    write.csv(df.sum, file = file.path(save.dir(df),"summary.csv"))
  }
  if (csv.option == 2) {
    write.csv2(df.sum, file = file.path(save.dir(df),"summary.csv"))
  }
  
  #save the summary for with means of replicates and sd values and assign
  df.mean <- get(paste0(data.id,".mean"))
  save(df.mean, file = file.path(save.dir(df),"mean_summary.Rdata"))
  if (csv.option == 1) {
    write.csv(df.mean, file = file.path(save.dir(df),"mean_summary.csv"))
  }
  if (csv.option == 2) {
    write.csv2(df.mean, file = file.path(save.dir(df),"mean_summary.csv"))
  }
  
}

## scatter plot functions - save included
plot.scatter <- function(individual=TRUE, facets=c("ID1", "ID2"), replicates=FALSE, save="scatter", data.id=analysis.name){
  
  #get the dataframe
  df <- get(data.id)
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
  
  #the cumulative plot
   gg <- simple.plot(df)
  
  #plot individual timepoints
  if(individual == TRUE){
    gg <- list(gg, individual.plot(replicates, df))
  }
  
  #plot facets
  #get the collumns with IDs 
  ID.list <- names(df)[grep("ID", names(df))]
  if (length(facets)==2 &&  !FALSE %in% (facets %in% ID.list) ){
    gg <- list(gg, facet.plot(replicates, facets, df))
  }
  
  print(gg)
  
  #save the plots id save != ""
  if (is.character(save) && save!=""){
    #check if directory is present
    if(!dir.exists(file.path(save.dir(df),"plots"))){
      dir.create(file.path(save.dir(df),"plots"), recursive = TRUE)
    }
    #save the file
    pdf(file.path(save.dir(df),"plots", paste0(save,".pdf")), width=15, height=10)
    print(gg)
    dev.off()
  }
  
  
} 

## data manipulations functions
reset.threshold <- function(data.id=analysis.name){
  
  #get the dataframe and reset the thresholds
  df <- get(data.id)
  df$NET.class <- 0
  df$version <- "preAnalysis"
  df$method <- "none"
  df$class <- (df$NET.class*2) + df$mark.class
  assign(data.id, df, envir = globalenv())
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
  
  #show the (new) plot
  print(simple.plot(df))
}
reset.mark <- function(data.id=analysis.name){
  
  #get the dataframe and reset marks
  df <- get(data.id)
  df$mark.class <- 0
  df$mark <- "none"
  df$class <- (df$NET.class*2) + df$mark.class
  assign(data.id, df, envir = globalenv())
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
  
  #show the (new) plot
  print(simple.plot(df))
}
close.data <- function(data.id = analysis.name){
  r.list <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))[grep(data.id, names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame))))]
  rm(list= r.list, envir = .GlobalEnv)
  analysis.version <<- ""
  data.name <<- ""
  analysis.name <<- ""
  reset.NET.val()
  reset.mark.val()
}
duplicate.data <- function(new.name = "data", data.id=analysis.name){
  
  #get and assign the dataframes
  df <- get(data.id)
  assign(new.name, df, envir = globalenv())
  
  #if .NA dataframe exists
  if(exists(paste0(data.id,".NA"))){
    df.NA <- get(paste0(data.id,".NA"))
    assign(paste0(new.name,".NA"), df.NA, envir = globalenv())
  }
  
  #assign global variables and update summary
  update.global(TRUE, new.name)
  
  #show data
  print(simple.plot(df))
}

## set parameters
norm.int <- function(min.int = 0, max.int = 1, reset=FALSE, data.id=analysis.name){
  
  #get the dataframe
  df <- get(data.id)
  
  #normalize intensity data
  if(reset!=TRUE){
    df$Int.norm <- min.int + ((df$Int-min(df$Int, na.rm = TRUE))*(max.int-min.int)/(max(df$Int, na.rm = TRUE)-min(df$Int, na.rm = TRUE)))
  }
  if(reset==TRUE){
    df$Int.norm <- df$Int
  }
  
  #assign the new dataframe
  assign(data.id, df, envir = globalenv())
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
}
set.plot.label <- function(IDs = c("ID2", "ID1"), digits = 3, data.id = analysis.name){
  
  #get the dataframe
  df <- get(data.id)
  
  #reset 'label' and set new one
  df$label <- ""
  for (ID in IDs) {
    if (is.integer(df[[ID]][1])) {
      df$label <- paste(df$label, formatC(df[[ID]], width = digits, format = "d", flag = "0"), sep = "\n")
    }else{
      df$label <- paste(df$label, df[[ID]], sep = "\n")
    }
  }
  
  #assign the the new dataframe
  assign(data.id, df, envir = globalenv())
  
  #assign global variables and update summary
  update.global(TRUE, data.id)
}
set.plot.range <- function(x.min=x.range[1], x.max=x.range[2], y.min=y.range[1], y.max=y.range[2], percent.min = p.range[1], percent.max = p.range[2], data.id = analysis.name){
  
  #get the dataframe
  df <- get(data.id)
  
  #get the variables that are not explicitly given
  if (x.max == "max") {
    x.max <- max(df$Area, na.rm = TRUE)
  }
  if (y.max == "max") {
    y.max <- max(df$Int.norm, na.rm = TRUE) + 0.05
  }
  if (x.min == "min") {
    x.min <- min(df$Area, na.rm = TRUE)
  }
  if (y.min == "min") {
    y.min <- min(df$Int.norm, na.rm = TRUE) - 0.05
  }
  
  #assign the new plot labels
  assign("x.range", c(x.min,x.max), envir = globalenv())
  assign("y.range", c(y.min,y.max), envir = globalenv())
  assign("p.range", c(percent.min,percent.max), envir = globalenv())
}
set.plot.color <- function(rest="grey30", NET="red", mark.rest="green", mark.NET="yellow", size.ex="blue"){
  assign("colors", c(rest, NET, mark.rest, mark.NET, size.ex), envir = globalenv())
}

## sub functions
#plots
simple.plot <- function(df){
  #simple plot
  gg <- ggplot(data=df, aes(x=Area, y=Int.norm, colour=as.factor(class)))+
    geom_point()+
    ylim(y.range)+
    xlim(x.range)+
    ylab("normalized intensity")+
    scale_color_manual(values=c("0"=colors[1], "1"=colors[3], "2"=colors[2], "3"=colors[4]), labels=c("0"="resting", "1"="marked resting", "2"="NET", "3"="marked NET"), name="Cell state")+
    theme(aspect.ratio = 10/15)+
    theme.scatter+
    ggtitle(paste0("IJdata: ", df$experiment[1],"\n","Version: ", df$version[1], "\n", "Thresholding: ", df$method[1], "\n", "Highlighting: ", df$mark[1]))
  return(gg)
}
individual.plot <- function(replicates, df){
  
  #get the collumns with IDs and generate a new collumn with 'collapsed' ID
  ID.list <- names(df)[grep("ID", names(df))]
  df$ID.sum <- ""
  for(ID in ID.list){
    df$ID.sum <- paste(df$ID.sum, df[[ID]], sep = "/")
  }
  
  #make a list for the plots
  gg.list <- list()
  
  #go thriugh all the new 'collapsed' IDs
  for (ID.plot in unique(df$ID.sum)) {
    #make subset for a given collapsed ID
    df.sub <- df[df$ID.sum==ID.plot,]
    #if replicates
    if (replicates == TRUE) {
      replicate.list <- unique(df.sub$file)
      for(replicate in replicate.list){
        #make subset for a given replicate
        df.sub.rep <- df.sub[df.sub$file==replicate,]
        
        gg.list <- list(gg.list, simple.plot(df.sub.rep)+ggtitle(paste0("IJdata: ", df.sub.rep$experiment[1],"\n","Version: ", df.sub.rep$version[1], "\n", "Thresholding: ", df.sub.rep$method[1], "\n", "Highlighting: ", df.sub.rep$mark[1], "\n", "ID: ", df.sub.rep$ID.sum[1], "\n", "Replicate: ", df.sub.rep$file[1])))
      }
    }else{
      gg.list <- list(gg.list, simple.plot(df.sub)+ggtitle(paste0("IJdata: ", df.sub$experiment[1],"\n","Version: ", df.sub$version[1], "\n", "Thresholding: ", df.sub$method[1], "\n", "Highlighting: ", df.sub$mark[1], "\n", "ID: ", df.sub$ID.sum[1])))
    }
  }
  return(gg.list)
}
facet.plot <- function(replicates, facets, df){
  
  df.facet <- df
  
  #get the collumns with IDs 
  ID.list <- names(df)[grep("ID", names(df))]
  #make two new collums for the dimensions of the facet plot
  df.facet$ID.dim1 <- df.facet[[facets[1]]]
  df.facet$ID.dim2 <- df.facet[[facets[2]]]
  #add all ID values that are not in the two that are given with a "/" separator to the second dimension collumn
  ID.coll <- ID.list[!ID.list %in% facets]
  for(ID in ID.coll){
    df.facet$ID.dim2 <- paste(df.facet$ID.dim2, df.facet[[ID]], sep = "/")
  }
  
  #if replicates are TRUE - make a subset for each unique combination of dim1 and dim2 and then modify within these subsets the 'shorter' dimension 
  #with a number based on the unique 'file' value in these subsets - then put everything together again
  if (replicates==TRUE) {
    
    #make new dataframe with all the unique combinations of dim1 and dim2
    dim <- unique(df.facet[,c("ID.dim1","ID.dim2")])
    
    #make new dataframe to which the modified subsets will be concatenated
    df.facet.new <- df.facet[!1,]
    
    for (i in seq_along(1:length(dim$ID.dim2))) {
      dim.1 <- dim$ID.dim1[i]
      dim.2 <- dim$ID.dim2[i]
      df.facet.sub <- df.facet[df.facet$ID.dim1==dim.1 & df.facet$ID.dim2==dim.2,]
      
      #check which dimension is the 'shorter' one
      dim.1.list <- unique(df.facet$ID.dim1) 
      dim.2.list <- unique(df.facet$ID.dim2)
      
      #modify the shorter dimension
      if(length(dim.1.list) > length(dim.2.list)){
        df.facet.sub$ID.dim2 <- paste(df.facet.sub$ID.dim2, match(df.facet.sub$file, unique(df.facet.sub$file)), sep = "-")
      }
      
      if(length(dim.1.list) < length(dim.2.list)){
        df.facet.sub$ID.dim1 <- paste(df.facet.sub$ID.dim1, match(df.facet.sub$file, unique(df.facet.sub$file)), sep = "-")
      }
      
      #add the modified dataframe 
      df.facet.new <- rbind2(df.facet.new, df.facet.sub)
    }
    #assing the 'main' dataframe to the new one
    df.facet <- df.facet.new
    rm(df.facet.new)
  }  
  
  #the facet plot
  gg.facet <- ggplot(data=df.facet, aes(x=Area, y=Int.norm, colour=as.factor(class)))+
    geom_point(size=1)+
    ylim(y.range)+
    xlim(x.range)+
    facet_grid(rows = vars(ID.dim1), cols = vars(ID.dim2))+
    ylab("normalized intensity")+
    scale_color_manual(values=c("0"=colors[1], "1"=colors[3], "2"=colors[2], "3"=colors[4]), labels=c("0"="resting", "1"="marked resting", "2"="NET", "3"="marked NET"), name="Cell state")+
    theme.facet+
    theme(aspect.ratio = 10/15)+
    ggtitle(paste0("IJdata: ", df.facet$experiment[1],"\n","Version: ", df.facet$version[1], "\n", "Thresholding: ", df.facet$method[1], "\n", "Highlighting: ", df.facet$mark[1]))
  return(gg.facet)
}
summary.plot <- function(df.sum){
  gg <- ggplot(data = df.sum, aes(x=label, y=p.NETs, color="A"))+
    #summary plot
    geom_point(size=4)+
    stat_summary(aes(x=label, y=p.NETs), fun.y = function(x) mean(x), fun.ymin = function(x) mean(x) - sd(x), fun.ymax = function(x) mean(x) + sd(x), geom = "crossbar", fatten=2.5, width = 0.2, color="black")+
    geom_point(aes(x=label, y=p.NA, color="B"), size=4)+
    ylim(p.range)+
    scale_color_manual(values=c("A"=colors[2], "B"=colors[5]), labels=c("A"="NETs", "B"="> NET size"), name="Percent:")+
    ggtitle(paste0("IJdata: ", df.sum$experiment[1],"\n","Version: ", df.sum$version[1], "\n", "Thresholding: ", df.sum$method[1], "\n", "Excluding: ", df.sum$mark[1], "\n", "Include > NET size as NETs: ", df.sum$NA.include[1]))+
    ylab("percent")+
    xlab("conditions")+
    theme.summary
  return(gg)
}

#global variables and dataframes
read.mark.val <- function(df){
  if (sum(df$mark.class)>0) {
    mark.Area.min <<- as.numeric(min(df$Area[df$mark.class == 1], na.rm = TRUE))
    mark.Area.max <<- as.numeric(max(df$Area[df$mark.class == 1], na.rm = TRUE))
    mark.Int.min <<- as.numeric(min(df$Int.norm[df$mark.class == 1], na.rm = TRUE))
    mark.Int.max <<- as.numeric(max(df$Int.norm[df$mark.class == 1], na.rm = TRUE))
  }else{
    mark.Area.min <<- 0
    mark.Area.max <<- 0
    mark.Int.min <<- 0
    mark.Int.max <<- 0
  }
}
read.NET.val <- function(df){
  if (sum(df$NET.class)>0) {
    NET.Area.min <<- as.numeric(min(df$Area[df$NET.class == 1], na.rm = TRUE))
    NET.Area.max <<- as.numeric(max(df$Area[df$NET.class == 1], na.rm = TRUE))
    NET.Int.min <<- as.numeric(min(df$Int.norm[df$NET.class == 1], na.rm = TRUE))
    NET.Int.max <<- as.numeric(max(df$Int.norm[df$NET.class == 1], na.rm = TRUE))
  }else{
    NET.Area.min <<- 0
    NET.Area.max <<- 0
    NET.Int.min <<- 0
    NET.Int.max <<- 0
  }
}
reset.mark.val <- function(){
  mark.Area.min <<- 0
  mark.Area.max <<- 0
  mark.Int.min <<- 0
  mark.Int.max <<- 0
}
reset.NET.val <- function(){
  NET.Area.min <<- 0
  NET.Area.max <<- 0
  NET.Int.min <<- 0
  NET.Int.max <<- 0
}
reset.directory <- function(data.id=analysis.name){
  
  #select a file in the new base directory - will not be opened. Can be the IJ file if whole folder moved or the Rdata file if only this was moved or any file else
  file.select <- file.choose()
  
  #for the main dataframe
  #get the dataframes
    df <- get(data.id)
    #set the new directory in the data frames
    df$dir <- dirname(file.select)
    #assign the new data frames
    assign(data.id, df, envir = globalenv())
  #if .NA exists
  if(exists(paste0(data.id,".NA"))){
    df.NA <- get(paste0(data.id,".NA"))
    df.NA$dir <- dirname(file.select)
    assign(paste0(data.id,".NA"), df.NA, envir = globalenv())
  }
    
  #assign global variables and update summary
  update.global(TRUE, data.id)
}
update.global <- function(include, data.id){
  #get the dataframes and set global variables from data.id
  df <- get(data.id)
  analysis.version <<- df$version[1]
  data.name <<- df$experiment[1]
  analysis.name <<- data.id
  read.NET.val(df)
  read.mark.val(df)
    #if .NA exists
    if(exists(paste0(data.id,".NA"))){
      df.NA <- get(paste0(data.id,".NA"))
      df.full <- rbind2(df, df.NA)
    }else{
      df.full <- df
    }
  df.full <- df.full[order(df.full$file),]
  
  #make the summary for all replicates and assign
  df.sum <- summary.fun1(include, df.full)
  assign(paste0(data.id,".sum"), df.sum, envir = globalenv())
  
  #make the summary for with means of replicates and sd values and assign
  df.mean <- summary.fun2(df.sum)
  assign(paste0(data.id,".mean"), df.mean , envir = globalenv())
}

#summary
summary.fun1 <- function(include, df){
  
  #set collumn with the include variable
  df$include <- include
  
  #get the collumns with ID values 
  ID.list <- names(df)[grep("ID", names(df))]
  
  #summary if NA detections included as NETs - NETs that are marked cells are excluded
  if (include==TRUE) {
    df.sum <- ddply (df, c("file",ID.list), summarise,
                     cells = length(class),
                     NETs = length(class[class==2]),
                     NAs = length(class[class==4]),
                     p.NETs = ((NETs + NAs)/cells)*100,
                     p.NA = (NAs/cells)*100,
                     NA.include = include[1],
                     label = label[1],
                     version = version[1],
                     method = method[1],
                     experiment = experiment[1],
                     mark = mark[1]
    )  
  }
  
  #summary if NA detections NOT included as NETs - NETs that are marked cells are excluded
  if (include==FALSE) {
    df.sum <- ddply (df, c("file",ID.list), summarise,
                     cells = length(class[!class==4]),
                     NETs = length(class[class==2]),
                     NAs = length(class[class==4]),
                     p.NETs = (NETs/cells)*100,
                     p.NA = (NAs/length(class))*100,
                     NA.include = include[1],
                     label = label[1],
                     version = version[1],
                     method = method[1],
                     experiment = experiment[1],
                     mark = mark[1]
    )  
  }
  return(df.sum)
}
summary.fun2 <- function(df){
  
  #get the collumns with ID values 
  ID.list <- names(df)[grep("ID", names(df))]
  
  #make mean with averages and sd values
  df.mean <- ddply(df, ID.list, summarise,
                       cells = sum(cells, na.rm = TRUE),
                       n = length(file),
                       NETs = sum(NETs, na.rm = TRUE),
                       NAs = sum(NAs, na.rm = TRUE),
                       mean.p.NETs = mean(p.NETs, na.rm = TRUE),
                       sd.p.NET = sd(p.NETs, na.rm = TRUE),
                       mean.p.NA = mean(p.NA, na.rm = TRUE),
                       NA.include = NA.include[1],
                       version = version[1],
                       method = method[1],
                       experiment = experiment[1],
                       mark = mark[1]
  )
  return(df.mean)
}

#determine which directory needed for saves
save.dir <- function(df){
  
  base.dir <- df$dir[1]
  if(df$mark[1] == "none"){
    full.dir <- file.path(base.dir, df$version[1])
  }else{
    full.dir <- file.path(base.dir, df$version[1], df$mark[1])
  }
  return(full.dir)
}









