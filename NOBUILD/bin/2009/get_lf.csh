#!/bin/csh

	# Script to download low frequency data
	#
        # Usage: get_lf.csh Bnum start_date   end_date  
        # e.g.,  get_lf.csh B073 2009-01-15   2009-04-15 
        # This should produce three ascii files:
	#	B073.ALL_LF.r.txt contains the four channels of 10-minute raw data
	#	B073.ALL_LF.l.txt contains the four channels of 10-minute linearized data (nanostrain)
	#	B073.SOH.txt   contains state of health and environmental channels ( 30 minute interval)

        ########### set variables   #############

        set name=$1
        set start=$2
        set end=$3

        echo $name $start $end 

	########## get data #####################

	ftp ftp://bsm.unavco.org/pub/bsm/lowfreq/${name}/$name.bsm.tar
	tar -xf $name.bsm.tar
	mv $name.bsm/${name}*.gz .
	rm -r $name.bsm
	gzip -d ${name}*.gz

	######### combine SOH data ##############

	echo " Date-Time	BatteryVolts    DownholeDegC    LoggerDegC      PowerBoxDegC    PressureKPa     Rainfallmm      SolarAmps	SystemAmps" > $name.SOH.txt
	paste ${name}BatteryVolts.txt ${name}DownholeDegC.txt ${name}LoggerDegC.txt ${name}PowerBoxDegC.txt ${name}PressureKPa.txt \
	${name}Rainfallmm.txt ${name}SolarAmps.txt  ${name}SystemAmps.txt | \
	awk ' /'$start'/,/'$end'/ { print $1"T"$2,"\t",  $3,"\t", $6,"\t", $9, "\t",$12,"\t", $15,"\t", $18,"\t", $21, "\t", $24 }'  >> $name.soh.txt

	rm ${name}BatteryVolts.txt ${name}DownholeDegC.txt ${name}LoggerDegC.txt ${name}PowerBoxDegC.txt 
	rm ${name}PressureKPa.txt ${name}Rainfallmm.txt ${name}SolarAmps.txt ${name}SystemAmps.txt

	######### combine gauge data  ###########

	echo "Date-Time	CH0	CH1	CH2	CH3" > $name.ALL_LF.r.txt
	paste ${name}CH0.txt ${name}CH1.txt ${name}CH2.txt ${name}CH3.txt | awk '/'$start'/,/'$end'/ { print $1"T"$2,"\t",$3,"\t",$6,"\t",$9,"\t",$12 }' >>  $name.ALL_LF.r.txt

        ####### make ascii linearized data ##########

        # Decide instrument gap
        # first batch have gap = 0.002 others have gap = 0.001, [B001][B003][B004][B005][B006][B007][B009][B010][B011][B012][B018][B022][B024][B035][B081][B082][B086][B087]:

        switch ($name)
                case [B][0][0]?
                        set gap=0.0002
                        breaksw
                case [B][0][1][0128]
                        set gap=0.0002
                        breaksw
                case [B][0][2][24]
                        set gap=0.0002
                        breaksw
                case [B][0][8][1267]
                        set gap=0.0002
                        breaksw
                default:
                        set gap=0.0001
                        breaksw
        endsw

	foreach c (CH0 CH1 CH2 CH3)
		awk '/'$start'/,/'$end'/ {di=($3/1e+8)/(1-($3/1e+8)) ; if (f == 0 && $3 != 999999)   {d0=di;f=1}  ; \
		if (/999999/) printf ("%s %s      999999\n", $1, $2, $3) ; else printf("%s %s     %14.3f \n", $1,$2, ((di-d0)*'$gap'/0.087)*1e+9)  }' ${name}${c}.txt > ${name}.${c}_LF.l.txt 
	end

	echo "Date-Time CH0     CH1     CH2     CH3" > $name.ALL_LF.l.txt
	paste ${name}.CH0_LF.l.txt ${name}.CH1_LF.l.txt ${name}.CH2_LF.l.txt ${name}.CH3_LF.l.txt | awk '{ print $1"T"$2"\t"$3"\t"$6"\t"$9"\t"$12 }' >>  ${name}.ALL_LF.l.txt

        rm ${name}CH?.txt ${name}.CH?_LF.l.txt

