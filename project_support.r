
library(testthat)
library(arm)
library(rethinking)
library(foreign)
library(tictoc)
library(sandwich) # for estfun
library(lmtest) # for estfun

dir_init <- function(path, verbose=FALSE, overwrite=TRUE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(dir.exists(path)){
    if(overwrite){
      if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
      }
      if(dir.exists(path)) unlink(path, recursive=TRUE)
      dir.create(path)
    }
  } else {
    if(verbose){
      print(paste('folder ', path, ' created.', sep=""))
    }
    dir.create(path)
  }
}

randomize_rows <- function(data, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  o <- sample(1:nrow(data))
  data <- data[o,]
  return(data)
}

randomize_ids <- function(ids, reserved, seed = NA, nchars = 5) {
  if(all(is.na(ids)) | length(ids) == 0) stop("not valid ids")
  ids <- as.character(ids)
  id_list <- sort(unique(ids))
  new_ids <- id_maker(n = length(id_list), seed = seed, nchars = nchars)
  out <- new_ids[match(ids, id_list)]
  expect_identical(table(table(ids)), table(table(out)))
  expect_identical(which(is.na(ids)), which(is.na(out)))
  return(out)
}

id_maker <- function(n, reserved = "", seed = NA, nchars = NA){
  my_let <- letters 
  my_num <- 0:9 
  if(is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(as.POSIXlt(Sys.time())))
  if(!is.na(seed) & is.numeric(seed)) set.seed(seed)
  output <- replicate(n, paste(sample(c(my_let, my_num), nchars, replace=TRUE), 
    collapse=''))
  rejected <- duplicated(output) | output %in% reserved |
    substr(output, 1, 1) %in% my_num
  while (any(rejected)) {
    output <- output[-which(rejected)]
    remaining <- n - length(output)
    output <- c(output, replicate(remaining, paste(sample(c(my_let, my_num), nchars, 
      replace=TRUE), collapse="")))
    rejected <- duplicated(output) | output %in% reserved |
      substr(output, 1, 1) %in% my_num
  }
  output
}

robust.se <- function(model, cluster){
	require(sandwich)
	require(lmtest)
	
	M <- length(unique(cluster))
	N <- length(cluster)           
	K <- model$rank
	dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
	uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
	rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
	se <- sqrt(diag(rcse.cov))
	# rcse.se <- coeftest(model, rcse.cov)
	
	return(se)
}


display.er <- function (object, ...) 
{
	.local <- function(object, digits = 3, detail = FALSE, lev.elasticity = TRUE, cluster=NA) 
	{
		call <- object$call
		require(arm)
		summ <- summary(object)
		
		if (detail) {
			# have to manually calculate the t and p values
			coef <- summ$coef[, , drop = FALSE]
			if(!is.na(cluster)){
				coef[,2] <- robust.se(object, cluster)
				deef <- summ$df[2]
				coef[,3] <-  coef[,1]/coef[,2]  # new t-stats
				coef[,4] <- (1-pt(abs(coef[,3]), df=deef))*2 # new p-values
			}
			
		} else {
			coef <- summ$coef[, 1:2, drop = FALSE]	
			if(!is.na(cluster)[1]){
				coef[,2] <- robust.se(object, cluster)
			}
		}
		dimnames(coef)[[2]][1:2] <- c("est", "se")
		if(!is.na(cluster)[1]) dimnames(coef)[[2]][1:2] <- c("est", "ro.se")
		
		if(lev.elasticity){
		#	datamatrix <- object$model
		#	if(dim(datamatrix)[2] > dim(coef)[1]){
		#		covar.names <- attr(object$terms, "term.labels")
		## big problem - for some reason the current "model" object doesn't include the interaction covariates!....that would imply I can't do any interaction effects models in R until this is fixed...
		#	}
			means <- colMeans(object$model)
			x.means <- means[-1]
			y.mean <- means[1]
			
			coef[-1,1] <- coef[-1,1]*x.means/y.mean
			coef[-1,2] <- coef[-1,2]*x.means/y.mean
			if(detail){
				coef[,3] <-  coef[,1]/coef[,2]  # new t-stats
				coef[,4] <- (1-pt(abs(coef[,3]), df=deef))*2 # new p-values
			}
			dimnames(coef)[[2]][1:2] <- c("elas.est", "elas.se")
			if(!is.na(cluster)[1]) dimnames(coef)[[2]][1:2] <- c("elas.est", "elas.ro.se")
			
			coef <- coef[-1,]
		}
		
		n <- summ$df[1] + summ$df[2]
		k <- summ$df[1]
		print(call)
		pfround(coef, digits)
		cat("---\n")
		cat(paste("n = ", n, ", k = ", k, "\nresidual sd = ", pfround(summ$sigma, digits), ", R-Squared = ", pfround(summ$r.squared, 2), "\n", sep = ""))
	}
	.local(object, ...)
}


display.cr <- function (object, ...){
    .local <- function(object, digits = 3, detail = FALSE, lev.elasticity = TRUE, cluster=NA) 
    {
    call <- object$call
    require(arm)
    summ <- summary(object)

    if (detail) {
    # have to manually calculate the t and p values
    coef <- summ$coef[, , drop = FALSE]
    if(!is.na(cluster)){
    coef[,2] <- robust.se(object, cluster)
    deef <- summ$df[2]
    coef[,3] <-  coef[,1]/coef[,2]  # new t-stats
    coef[,4] <- (1-pt(abs(coef[,3]), df=deef))*2 # new p-values
    }

    } else {
    coef <- summ$coef[, 1:2, drop = FALSE]
    if(!is.na(cluster)[1]){
    coef[,2] <- robust.se(object, cluster)
    }
    }
    dimnames(coef)[[2]][1:2] <- c("pcor", "se")
    if(!is.na(cluster)[1]) dimnames(coef)[[2]][1:2] <- c("pcor", "ro.se")

    if(lev.elasticity){
    # datamatrix <- object$model
    # if(dim(datamatrix)[2] > dim(coef)[1]){
    # covar.names <- attr(object$terms, "term.labels")
    # big problem - for some reason the current "model" object doesn't include the interaction covariates!....that would imply I can't do any interaction effects models in R until this is fixed...
    # }
    sdds <- apply(object$model, 2, function(z) sdd(as.numeric(z)))
    if(any(is.na(sdds))) cat("Extreme Warning: Don't trust the numbers for the categorical predictors!\n")
    x.sdds <- sdds[-1]
    y.sdds <- sdds[1]

    coef[-1,1] <- coef[-1,1]*x.sdds/y.sdds
    coef[-1,2] <- coef[-1,2]*x.sdds/y.sdds
    if(detail){
    coef[,3] <-  coef[,1]/coef[,2]  # new t-stats
    coef[,4] <- (1-pt(abs(coef[,3]), df=deef))*2 # new p-values
    }
    dimnames(coef)[[2]][1:2] <- c("pcor.est", "pcor.se")
    if(!is.na(cluster)[1]) dimnames(coef)[[2]][1:2] <- c("pcor.est", "pcor.ro.se")

    coef <- coef[-1,]
    }

    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    print(call)
    pfround(coef, digits)
    cat(" - \n")
    cat(paste("n = ", n, ", k = ", k, "\nresidual sd = ", pfround(summ$sigma, digits), ", R-Squared = ", pfround(summ$r.squared, 2), "\n", sep = ""))
    }
    .local(object, ...)
}


sdd = function(data){
    x <- na.omit(data)
    sqrt(sum((x-mean(x))^2)/length(x))
}
