#fread file, truncate nRows
df.clean <- function(my.df, nRows){
	tmp <- read.csv(gsub("/","_",my.df))
	tmp[1:nRows,]
}

#return possible slides of a window for df
p.slides <- function(nRows,nSlide, nWindow){
	tmp <- seq(1, nRows, nSlide) + nWindow
	c(1, tmp[tmp < nRows - nWindow])
}

n.sign.change <- function(vector){
	sign.change = 0
	for (i in 2:(length(vector)-1)){
		cur.step <- vector[i+1] - vector[i] 
		bac.step <- vector[i] - vector[i-1]

		if (!(cur.step * bac.step >= 0)){
			sign.change =sign.change + 1	 	
		}
	}
	sign.change
}

n.zero.crossings <- function(vector){
	tmp <- vector - mean(vector)
	zero.crossings = 0
	for (i in 1:(length(vector)-1)){
		if (!(tmp[i+1]*tmp[i] >=0)){
			zero.crossings = zero.crossings + 1
		}
	}
	zero.crossings
}

mean.abs <- function(vector){
	mean(abs(vector))
}

signal.length <- function(vector){
	tmp.sum = 0
	for (i in 1:(length(vector)-1)){
		tmp.sum = tmp.sum + vector[i+1] - vector[i]
		#add square root, but it's for everyone
	}
	tmp.sum
}

# ---------------
#
# turn window into feature vector
# 
#      [ sig1 sig2 .... sig17 ]
#                ||
#                \/
# [ feats1..4 featss2..4 ... feats17..4 ]

window.to.feature <- function(window){

	tmp.slope.sign.change <- apply(window, 2, n.sign.change)
	tmp.zero.crossings <- apply(window, 2, n.zero.crossings)
	tmp.mean.abs <- apply(window, 2, mean.abs)
	tmp.signal.length <- apply(window, 2, signal.length)

	tmp <-rbind(tmp.slope.sign.change,
				tmp.zero.crossings,
				tmp.mean.abs,
				tmp.signal.length)	

	c(matrix(tmp, 1, nrow(tmp)*ncol(tmp), byrow = FALSE))
}

# -----------------
#
# turns signal data frame into feature matrix 
# rows equal windows
# columns certain signal/tdf.
#
# [ df ] -> | f1 |
#           | f2 |
#		    | .. |
#			| fk |

feature_matrix <- function(df, nWindow, nSlide){

	s.window <- seq(1, nrow(df)-nWindow, nSlide)
	n.window <- length(s.window)
	if (n.window > 0){

		tmp <- window.to.feature(df[s.window[1]:(s.window[1]+nWindow),])

		counter = 2
		while( counter <= n.window){
			tmp <- rbind(tmp,  window.to.feature(df[s.window[counter]:(s.window[counter]+nWindow),]))
			counter = counter + 1
		}
	}
	tmp
}

# -------------------
# 
# Simple take the df, and label (probably from file) 
# and paste.
#
# [ df ] -> [ df | l ] 

add.label <- function(df, label){
	data.frame(df, list(l = rep(strsplit(label, "_")[[1]][5], nrow(df))))
}

# -------------------
# 
# [ df ] -> | df | l1 | 
#			| df | l2 |
#			     . 
#			| df | lk |

train.feature.matrix <- function(maxRows, nSlide, nWindow, dataPath){
	setwd(dataPath)
	dir_files <- list.files()
	n.files <- length(dir_files) 
	if (n.files > 0){

		tmp <- df.to.feature.matrix(df.clean(dir_files[[1]], maxRows), nSlide, nWindow)	
		tmp <- add.label(tmp, dir_files[[1]])
		counter = 2
		while( counter <= n.files) {
			tmp1 <- df.to.feature.matrix(df.clean(dir_files[[counter]], maxRows), nSlide, nWindow)
			tmp1 <- add.label(tmp1, dir_files[[counter]]) 
			tmp <- rbind(tmp, tmp1)
			counter = counter + 1
		}
	}
	setwd("../")
	tmp
}

# -------------------
#
# [ df | l ] ->       l1 l2 . lk
#                | df  0 1  . 0 |
#				 |      .       |
#				 | df  1 0  . 0 |

re.label <- function(df){
	labels <- colnames(df)
	eqn <- as.formula(paste(paste(labels[1:length(labels)-1], collapse = "+"), " ~ l"))

	dcast(df, eqn, function(x)  length(x))
}

