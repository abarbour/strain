#!/bin/csh

	# script to download Level 2 PBO strainmeter data
	#
	# Usage: get_l2.csh  Bnum  start_date   end_date
	# e.g.   get_l2.csh  B073  2009-04-01   2009-05-01

	########### set variables   ########################

	set bnum=$1
	set start=$2          
	set end=$3          
	set t=`echo $end $start | awk '{ print substr($1,1,4)-substr($2,1,4) }'`
	set starty=`echo $start | awk '{ print substr($1,1,4)}' `
	set endy=`echo $end   | awk '{ print substr($1,1,4)}' `

	########### wget data     ########################

	@ yr = $starty
	while ( $yr <= $endy )  
		ftp ftp://bsm.unavco.org/pub/bsm/level2/${bnum}/${bnum}.${yr}.bsm.level2.tar 
		echo "ftp ftp://bsm.unavco.org/pub/bsm/level2/${bnum}/${bnum}.${yr}.bsm.level2.tar "
		tar -xf $bnum.${yr}.bsm.level2.tar
		rm $bnum.${yr}.bsm.level2.tar
		mv *.level2/* .
		rm -r *level2
		gzip -d *.gz
		@ yr = ( $yr + 1 )
	end   

	@ yr = ( $starty ) # initiliaze
	while ( $yr <= $endy )
		foreach var (gage0 gage1 gage2 gage3 2Ene Eee-Enn Eee+Enn )
			if ( $yr == $starty) then
				head -1 $bnum.$yr.xml.$var.txt > $bnum.xml.$var.txt
			endif
			awk '{ if (NR >1 ) print $0 }' $bnum.$yr.xml.$var.txt   >> $bnum.xml.$var.txt 
			rm $bnum.$yr.xml.$var.txt
		end
		@ yr = ( $yr + 1 )
	end


	#########	select dates	#####

	foreach var (gage0 gage1 gage2 gage3 2Ene Eee-Enn Eee+Enn )
		head -1 $bnum.xml.$var.txt > $bnum.$var.txt
		cat  $bnum.xml.$var.txt | awk '  /'${start}'/,/'${end}'/  { print $0 }'  >> $bnum.$var.txt
		rm  $bnum.xml.$var.txt
	end
