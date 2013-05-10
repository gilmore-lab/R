moco.RLS.file.convert <- function( dir="~", study="moco-3-pattern" )
# moco.RLS.file.convert( dir="~/", study="moco-3-pattern" )
#	Opens PowerDiva 3.4 RLS.txt data file, drops unnecessary variables,
#		creates new factors based on iCond numbers and study, 
#		then exports a .Rdata and .csv file in the same directory. 
#		
#		Function returns the new, trimmed, data frame.
#
#		Used for Motion Coherence (MOCO) series of SSVEP Studies.
#
#	dir	: 	directory to look for file, default is "~/"
#	study:	"moco-3-pattern, "moco-3-pattern-hilo"
#
#	To use, source("PATH2PROGRAM/moco-RLS-file-convert.R") at R command line.
#	Call using moco.RLS.file.convert(…)
#
#	Released under GPLv3 by Rick O. Gilmore, thatrickgilmore@gmail.com
#
#############################################################################

#############################################################################
#	History
#
#	2013-05-10-19:07	rog wrote
#

#############################################################################
#	Notes
#
#	2013-05-10-19:07	Needs better error checking. Change filename based on iSess?
#

{

fname = file.path( dir, "RLS.txt")
nCond = 9
nCh = 128
nHarm = 12
nDataPts = nHarm * nCh * nCond

# List of variables to drop from exported data frame
drops = c("iTrial", "iCh", "iFr", "AF", "xF1", "xF2", "FK_Cond", "iBin", "SweepVal",
		  "LSB", "RSB", "UserSc", "Thresh", "ThrBin", "Slope", "ThrInRange", "MaxSNR")

#### Reads tab-delimited data file from file name, assumes header, fills in blank variables

msg = sprintf("Reading file %s into data frame.", fname)
cat( msg, "\n", sep="" )

input.data.df = read.delim( fname, header=TRUE, sep="\t", fill=TRUE)
cat("Data frame read.\n")

#### PowerDiva 3.4 duplicates data in RLS.txt output, so take only first half

cat("Trimming duplicate entries.\n")
input.data.trimmed.df = input.data.df[1:nDataPts,]

#### Remove original frame to save memory
rm( input.data.df )

#### Recode iCond variable 

## If moco-3-pattern-adult, rad = iCond 1..3, rot = iCond 4..6, trans = iCond 7..9

msg = sprintf("This is a %s study. Making Speed and Pattern factors.", study)
cat(msg, "\n", sep="")

if( study == 'moco-3-pattern') 
{
	#	iCond	Pattern	Speed
	#	1		rad		2deg/s
	#	2		rot		2deg/s
	#	3		trans	2deg/s
	#	4		rad		4deg/s
	#	5		rot		4deg/s
	#	6		trans	4deg/s	
	#	7		rad		8deg/s
	#	8		rot		8deg/s
	#	9		trans	8deg/s

	Pattern = factor( rep( c(1,2,3), each=(3*nHarm*nCh)) , labels=c("rad", "rot", "trans") )

	Speed = factor( rep( rep( c(1, 2, 3), each=(nHarm*nCh) ), times=3), labels=c("2deg/s", "4deg/s", "8deg/s"), ordered=TRUE)
}

if( study == 'moco-3-pattern-hilo' )
{

	#	iCond	Pattern	Speed
	#	1		rad		4
	#	2		rot		4
	#	3		trans	4
	#	4		rad		1
	#	5		rad		16
	#	6		rot		1	
	#	7		rot		16
	#	8		trans	1
	#	9		trans	16

	pat.cond1 = rep( 1, nCh*nHarm )
	pat.cond2 = rep( 2, nCh*nHarm )
	pat.cond3 = rep( 3, nCh*nHarm )
	pat.cond4 = rep( 1, nCh*nHarm )
	pat.cond5 = rep( 1, nCh*nHarm )
	pat.cond6 = rep( 2, nCh*nHarm )
	pat.cond7 = rep( 2, nCh*nHarm )
	pat.cond8 = rep( 3, nCh*nHarm )
	pat.cond9 = rep( 3, nCh*nHarm )
	
	Pattern = factor( c( pat.cond1, pat.cond2, pat.cond3, pat.cond4, pat.cond5,
						 pat.cond6, pat.cond7, pat.cond8, pat.cond9 ),
						 labels=c("rad", "rot", "trans")
					)

	spd.cond1 = rep( 2, nCh*nHarm )
	spd.cond2 = rep( 2, nCh*nHarm )
	spd.cond3 = rep( 2, nCh*nHarm )
	spd.cond4 = rep( 1, nCh*nHarm )
	spd.cond5 = rep( 3, nCh*nHarm )
	spd.cond6 = rep( 1, nCh*nHarm )
	spd.cond7 = rep( 3, nCh*nHarm )
	spd.cond8 = rep( 1, nCh*nHarm )
	spd.cond9 = rep( 3, nCh*nHarm )

	Speed = factor( c( spd.cond1, spd.cond2, spd.cond3, spd.cond4, spd.cond5,
						 spd.cond6, spd.cond7, spd.cond8, spd.cond9 ),
						 labels=c("1deg/s", "4deg/s", "16deg/s"), ordered=TRUE
					)
}

####	Fix iCh labels to remove hc and -Avg text for readability

Channel = factor( input.data.trimmed.df$iCh, labels=1:nCh )

####	Rename frequency channel for readability

cat("Renaming data frame variables for readability.\n")
Hz = input.data.trimmed.df$AF

####	Create new data frame with core variables
cat("Creating new data frame.\n")

# Copy input.data.trimmed to output frame, but drop unneeded variables
moco.df = input.data.trimmed.df[, !names(input.data.trimmed.df) %in% drops ]

# Add newly defined variables
moco.df$Pattern = Pattern
moco.df$Speed = Speed
moco.df$Channel = Channel
moco.df$Hz = Hz
moco.df$Study = rep( study, nDataPts)

msg = sprintf( "Exporting R data to %s.", paste( fname,".Rdata", sep="") )
cat( msg, sep="" )
save( moco.df, file=paste( fname,".Rdata", sep="") )

msg = sprintf( "Exporting CSV data to %s.", paste(fname, ".csv", sep="") )
cat("\n", msg, sep="" )
write.table( moco.df, file=paste(fname, ".csv", sep=""), sep=",")

return=moco.df

}
