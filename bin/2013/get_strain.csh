#!/bin/csh
# Kathleen Hodgkinson
# UNAVCO 2012-02-23

# Usage: get_strain.csh  $bsm start_date end_date label
# e.g.,   get_strain.csh B012 2009-01-01 2009-02-01 TEST
# This will put the data in a directory called B012.TEST

	########### set variables   ########################

	set bnum=$1
	set startdate=$2         
	set enddate=$3            
	set label=$4

	set work=`pwd`

	set basey=`echo $startdate | awk '{ print substr($1,1,4) }'`      
	set endy=`echo $enddate | awk '{ print substr($1,1,4) }'`      

	mkdir $bnum.$label

	########### wget data     ########################

	@ yr = $basey # initialize
	while ( $yr <= $endy)  
		ftp ftp://bsm.unavco.org/pub/bsm/level2/${bnum}/${bnum}.${yr}.bsm.level2.tar 
		if ( -e $work/$bnum.${yr}.bsm.level2.tar ) then
			tar -xf $work/$bnum.${yr}.bsm.level2.tar
			rm $work/$bnum.${yr}.bsm.level2.tar
			mv $work/*.level2/* .
			rm -r $work/*level2
			gzip -d $work/*.gz
		else
			echo "START YEAR TOO EARLY"
			exit
		endif
		@ yr = ( $yr + 1 )
	end   
	@ yr =  $basey  # initiliaze
	
	while ( $yr <= $endy)
		foreach type ( gauge0 gauge1 gauge2 gauge3 2Ene Eee-Enn Eee+Enn 2Ene.ER2010 Eee-Enn.ER2010 Eee+Enn.ER2010  )
		   if ( -e $work/$bnum.$yr.xml.$type.txt ) then
			if ( $yr == $basey ) then
				cat  $work/$bnum.$yr.xml.$type.txt   > $work/$bnum.$label/${bnum}_${type}.txt 
			else
				awk '{ if (NR >1 ) print $0 }' $work/$bnum.$yr.xml.$type.txt   >> $work/$bnum.$label/${bnum}_${type}.txt 
			endif
			rm $work/$bnum.$yr.xml.$type.txt
		   endif
		end
		@ yr = ( $yr + 1 )
	end

	ftp ftp://bsm.unavco.org/pub/bsm/level2/${bnum}/${bnum}.README.txt
	mv $work/${bnum}.README.txt $work/$bnum.$label/.

	##### use date window ######

	foreach type ( gauge0 gauge1 gauge2 gauge3 2Ene Eee-Enn Eee+Enn 2Ene.ER2010 Eee-Enn.ER2010 Eee+Enn.ER2010  )
	   if ( -e $work/$bnum.$label/${bnum}_${type}.txt ) then
		head -1 $work/$bnum.$label/${bnum}_${type}.txt > $work/$bnum.$label/${bnum}_${type}.tmp
		set count = `awk '/'$startdate'/,/'$enddate'/ { print $0 }' $work/$bnum.$label/${bnum}_${type}.txt | wc -l | awk '{ print $1}' `
		echo  ${bnum}_${type}.txt
	        if ( $count <= 1 ) then
	                set newstart = `awk 'NR==2 { print substr($2,1,10) }'  $work/$bnum.$label/${bnum}_${type}.txt`
                        awk '/'$newstart'/,/'$enddate'/ { print $0 }' $work/$bnum.$label/${bnum}_${type}.txt >> $work/$bnum.$label/${bnum}_${type}.tmp
                else
			awk '/'$startdate'/,/'$enddate'/ { print $0 }' $work/$bnum.$label/${bnum}_${type}.txt >> $work/$bnum.$label/${bnum}_${type}.tmp
		endif
		mv $work/$bnum.$label/${bnum}_${type}.tmp $work/$bnum.$label/${bnum}_${type}.txt		
	   endif
	end


	
