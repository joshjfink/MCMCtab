# Produce means of coefficients (more measures forthcoming)
central <- function(sims, central="mean", ...) {
	if(central == "mean") {
		t(apply(sims,2,mean))
	}else {
	if(central == "median"){
		t(apply(sims,2,median))
	}}
}

# Produce credible intervals of coefficients
intervals <- function(sims, ci_method="hpd", alpha=.9,...) {
	ints <- if(ci_method=="hpd"){
		HPDinterval(sims, alpha) 
	} else {
	if(ci_method=="quantile"){
		bounds <- c((1-alpha)/2, (.5-alpha/2)) 
		t(apply(sims, 2, quantile, bounds[1], bounds[2])) 
	}}
	return(ints)
}

# Format coefs (Could replace with gsub)
gen_pretty <- function(input, digits=3, ...){
	RoundOut <- formatC(input, format='f', digits=digits)
	return(ifelse(RoundOut <1, str_replace_all(RoundOut, "0[[:punct:]]", "."), RoundOut))
}

# Function to format posterior coefficient means for table
pretty_betas <- function(mc_sims, ...){
	beta <- central(mc_sims)
	gen_pretty(beta)
}

# Beautify credible intervals
pretty_creds <- function(mc_sims, new_order=NULL, ...){
	new_order <- ifelse(new_order==NULL, colnames(b_coda_perm), new_order)
	cis <- gen_pretty(intervals(mc_sims[,new_order]))
	pretty_cis <- paste("(",  cis[,1],", ", cis[,2], ")", sep = "")
	return(matrix(pretty_cis))
}

# Function for STAN post mean coefs and sig stars
MCMCstars <- function(mc_sims, new_order, ...){
	pos001 <- HPDinterval(mc_sims[,new_order], .999)>0 
	pos01 <- HPDinterval(mc_sims[,new_order], .99)>0 
	pos05 <- HPDinterval(mc_sims[,new_order], .95)>0 
	pos10 <- HPDinterval(mc_sims[,new_order], .9)>0 
	sig_stars <- ifelse(pos10[,1]==pos10[,2],"^", "") 
	sig_stars <- ifelse(pos05[,1]==pos05[,2],"*", sig_stars) 
	sig_stars <- ifelse(pos01[,1]==pos01[,2],"**", sig_stars) 
	sig_stars <- ifelse(pos001[,1]==pos001[,2],"***", sig_stars)
	stred_betas <-  as.matrix(paste(pretty_betas(mc_sims[,new_order]),sig_stars, sep=""))
	row.names(stred_betas) <- c(new_order)
	return(stred_betas)
}

# Fcn to create grid to intersect posterior means and CIs
BlendCoefCI <- function(mc_sims, vlabels, new_order, ...){
	PostMeans <- MCMCstars(mc_sims=mc_sims, new_order=new_order)
	PostCIs <- pretty_creds(mc_sims, new_order=new_order)
	# Empty grid to intersect posterior means and CIs
	TabGrid <- matrix(NA, nrow=nrow(PostMeans)*2)
	TabGrid[seq(1, nrow(TabGrid), 2),] <- PostMeans
	TabGrid[seq(0, nrow(TabGrid), 2),] <- PostCIs
	return(TabGrid)
}

# Function to cbind models together
# -- models = list of models| vlabels = sincle array | new_orders = list of orders
ModBind <- function(listMCsims, vlabels, listNewOrders, ...){
	mods <- matrix(nrow=length(vlabels)*2, ncol=length(listMCsims))
	for(i in 1:length(listMCsims)){
		mods[,i] <- BlendCoefCI(listMCsims[[i]], vlabels, listNewOrders[[i]]) 
	}
	# Use vlabels for every other line in table 
	FullLabs <- matrix(NA, nrow=length(vlabels)*2)
	FullLabs[seq(1, nrow(FullLabs), 2),] <- paste(vlabels)
	FullLabs[seq(0, nrow(FullLabs), 2),] <- paste("")
	rownames(mods) <- FullLabs
	return(mods)
}

# Function used from another page to insert super headings in the middle of the table
insertrow <- function(existingDF, newrow, r, ...) {
 existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
 existingDF[r,] <- newrow
 return(existingDF)
}

# Indent for Subheadings/
indentvar <- function(existingDF, varnum, ...){
	Tables[varnum,] <- paste("\\quad", Tables[varnum,])
	return(Tables)
}

# Generate Latex tables (build function for multiple models first)
MCMCtab <- function(listMCsims, listNewOrders, vlabels, clabels=NULL, title="", caption="", central="mean", ci_method="hpd", alpha=.9, digits=3, file=""){
	mbout <- ModBind(listMCsims, vlabels, listNewOrders) 
	tmat <- cbind(Variable = rownames(mbout), mbout)
	colnames(tmat) <- clabels; rownames(tmat)=NULL
	# Set column allignment and seperation (still need to automate the '36pt' resizing/give option for customizing)
	col_sep <- "36pt}}l"
	rep(col_sep <- paste(col_sep, "c", sep="", length(listMCsims)))
	col_align_sep <- paste(col_sep, "}%", sep="")
	# Stargazer table
	Tables <- stargazer(tmat, title=title, 
		column.sep.width = col_align_sep, 
		notes=caption, type="latex", header=FALSE)
	# This transformation will allow for the insertion of subheadings in the future 
	Tables <- as.data.frame(Tables)
	Tables$Tables <- as.character(Tables$Tables)
	# Return results and print to a file 
	# return(Tables)
		# write.table(Tables,file=file,sep="",row.names= FALSE,na="", quote =FALSE, col.names = FALSE))
}


	
# sink(file=NULL, ) 
# # does NOT work, since it closes the last sink, nothing more.

# sink(file=NULL) 

# # Warning message: In sink(file = NULL) : no sink to remove
# # What does work though is:

# f = file()
# sink(file=f) ## silence upcoming output using anonymous file connection
# ... your code here ...
# sink() ## undo silencing
# close(f)

new_fcn <- function(listMCsims, listNewOrders, vlabels, clabels=NULL, title="", caption="", central="mean", ci_method="hpd", alpha=.9, digits=3, file=""){

#Create start of file
	lines = c("\\documentclass[]{article}")
	lines = c(lines,"\t\\usepackage[active,tightpage,pdftex,floats]{preview}",
		"\\usepackage{booktabs}",
		"\\usepackage{caption}",
		"\\begin{document}",
		"\t\\newsavebox{\\tabularBoxgunfw}",  
		"\t\t\\sbox{\\tabularBoxgunfw}{",
		""
	)
	
	for (i in 1:length(var_list)) {
		lines = c(lines, paste(c(
				var_list[i],
				" & ",
				ifelse(coef_list[i]>0,"\\phantom{$+$}",""),
				specify_decimal(coef_list[i],k)," ",
				ifelse(confint,paste(c("(",specify_decimal(ci[i,1],k),",",specify_decimal(ci[i,2],k),")"),collapse=""),""),
				ifelse(p_list[i]<.001,"***",ifelse(p_list[i]<.01, "**", ifelse(p_list[i]<.05,"*", ifelse(p_list[i]<.1,"^+","")))),"\\\\"),collapse=""))
	}
# lmer_to_tex <- function(file, models, confint=T, fit=T, sample.size=T, groups=T, decimals=2) {
# 	k= decimals

# 	#Create start of file
# 	lines = c("\\documentclass[]{article}")
# 	lines = c(lines,"\t\\usepackage[active,tightpage,pdftex,floats]{preview}",
# 		"\\usepackage{booktabs}",
# 		"\\usepackage{caption}",
# 		"\\begin{document}",
# 		"\t\\newsavebox{\\tabularBoxgunfw}",  
# 		"\t\t\\sbox{\\tabularBoxgunfw}{",
# 		""
# 	)
	
# 	#MAGIC HAPPENS HERE
# 	#Assume 1 model for now
# 	model <- models[[1]]
	
# 	lines = c(lines,"\\begin{tabular}{lc}",
# 		"& \\textbf{Model 1}\\\\ \\toprule")
# 	summ <- summary(model)
# 	var_list = row.names(summ$coefficients)
# 	coef_list = summ$coefficients
# 	p_list = coef(summ)[,ncol(coef(summ))] #Assumes p value is in the last column
# 	N = nrow(model.frame(model))
# 	N.groups = summ$ngrps[[1]]
# 	AIC = AIC(logLik(model))
# 	ci = confint(model)
	
# 	for (i in 1:length(var_list)) {
# 		lines = c(lines, paste(c(
# 				var_list[i],
# 				" & ",
# 				ifelse(coef_list[i]>0,"\\phantom{$+$}",""),
# 				specify_decimal(coef_list[i],k)," ",
# 				ifelse(confint,paste(c("(",specify_decimal(ci[i,1],k),",",specify_decimal(ci[i,2],k),")"),collapse=""),""),
# 				ifelse(p_list[i]<.001,"***",ifelse(p_list[i]<.01, "**", ifelse(p_list[i]<.05,"*", ifelse(p_list[i]<.1,"^+","")))),"\\\\"),collapse=""))
# 	}
# 	lines = c(lines,"\\midrule")
# 	lines = c(lines,paste(c("\\textbf{AIC} &", specify_decimal(AIC,k),"\\\\"), collapse=""),paste(c("\\textbf{Obs.} &",N,"\\\\"), collapse=""),paste(c("\\textbf{Groups} &",N.groups,"\\\\"), collapse=""),"\\end{tabular}")
	
# 	#Create end of file
# 	lines = c(lines, "","\t\t} %end hbox and savebox",
# 		"\t\\newlength{\\lengunfw}",
# 		"\t\\settowidth{\\lengunfw}{\\usebox{\\tabularBoxgunfw}}",
# 		"\\begin{table}",
# 		"\t\\begin{minipage}[h]{ \\lengunfw } \\usebox{ \\tabularBoxgunfw }",
# 		"\t\t\\begin{center}",
# 		"\t\t\t{ ***: $p<.001$, **: $p<.01$, *: $p<.05$, $^+$: $p<.1$}",
# 		"\t\t\\end{center}",
#  		"\t\\end{minipage}\\end{table}",
#  		"\\end{document}")	
	
# 	#save file
# 	out<-file(file)
# 	writeLines(lines,out)
# 	close(out)
# }

