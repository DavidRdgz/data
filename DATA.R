source("features.R")

DATA <- function(my.df = 0)
{
        data <- list(
            raw.data = my.df,
            feature.data = 0,
            train.index = 0,
            test.index = 0
       )

        ## Set the name for the class
        class(data) <- append(class(data),"DATA")
        return(data)
}

add.label <- function(df, label){
	data.frame(df, list(l = rep(strsplit(label, "_")[[1]][5], nrow(df))))
}

set_raw_data <- function(...) UseMethod("set_raw_data")
set_raw_data.DATA<- function(...){ 
    args <- list(...)
    object <- args[["object"]] 
    if ("directory" %in% names(args) ){
        setwd(args[["directory"]])
        dir_files <- list.files()
        n.files <- length(dir_files)
       
        tmp.list <- list()
        gesture.tmp <- c() 
        counter <- 1
        while( counter <= n.files){
			tmp1 <- read.csv(gsub("/", "_", dir_files[[counter]]), header = FALSE)
            label <- paste0(strsplit(dir_files[[counter]], "_")[[1]][5], "_",counter)
            tmp.list[[label]] <- as.matrix(tmp1)
			counter = counter + 1
        }
        setwd("../")
    } else if ("file" %in% names(args)){
	    tmp <- read.csv(gsub("/","_",args[["file"]]), header = FALSE)
        label <- strsplit(args[["file"]], "_")[[1]][5]
        tmp.list[[label]] <- as.matrix(tmp1)

        gesture.tmp <- strsplit(args[["file"]], "_")[[1]][5]
    }
    object$raw.data <- tmp.list
    return(object)
}

get_gestures <- function(...) UseMethod("get_gestures")
get_gestures.DATA <- function(...){
    args <- list(...)
    object <- args[["object"]]
    names(object$raw.data)
}

set_feature_data <- function(...) UseMethod("set_feature_data")
set_feature_data.DATA <- function(...){
    args <- list(...)
    object <- args[["object"]]
    window <- args[["window"]]
    slide <- args[["slide"]]

    tmp.list <- list() 
    for (i in names(object$raw.data)){
        tmp.list[[i]] <- feature_matrix(object$raw.data[[i]], window, slide) 
    }
    object$feature.data <- tmp.list
    return(object)
}

plot_data <- function(...) UseMethod("plot_data")
plot_data.DATA <- function(...){
    args <- list(...)
    object  <- args[["object"]]
    type <- args[["type"]]
    number <- args[["number"]]
    cols <- args[["columns"]]
   
    if (type == "raw"){
        data <- object$raw.data[[number]]
    } else if (type == "feature"){
        data <- object$feature.data[[number]]
    }

    data <- data.frame(data[,cols], list( time = 1:nrow(data)))
    p <- ggplot(melt(data, id = "time"), aes(x=time, y = value, color = variable))
    p <- p + geom_line()
    p <- p + facet_grid(variable ~ . , scales = "free_y")
    p <- p + theme_bw()
    p <- p +  theme(axis.title.x = element_text(size = 12, vjust = .25), 
                    strip.text.x = element_blank(),
                    strip.background = element_blank(),
                    strip.text.y = element_text(colour = "black", angle = 0, size = 15),
                    legend.position="none")
    return(p)
}
