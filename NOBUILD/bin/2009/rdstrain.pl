#!/usr/bin/perl 

#      rdstrain   : Program to read and extract data from PBO strain XML files 
#      Version 1.0.1: 10 January 2005   
#      Version 1.0.1 can read and apply offsets in BSM XML files. It can also read BSM XML files written before 10 Jan 2006 which do not have offsets.

       use Time::Local;
       use Getopt::Std;
       getopts('hiljf:o:r:q:s:e:');

       if ($opt_h ) {
           print "\nUsage: rdstrain  -f xml_file -i  -o a|x -j  -r flag -q flag  -s YYYYMMDDHHMMSS -e YYYYMMDDHHMMSS
                         version 1.0.1: 10 January 2006
                         -f   xml input file 
                         -i   extract XML header information only
                         -l   print strainmeter summary only 
                         -o   strain data output format (a = tab delimted ascii    x = XML)
                              BSM data will be separated into gage data and areal and shear data
                         -j   add offset correction to strain time series (option can only be used for BSM XML files created after Jan 10 2006)
                         -r   if the strain quality flag is anything other than good replace the strain value with flag
                         -q   if the time series correction quality flags are anything other than good replace the strain value with flag
                         -s   start time YYYYMMDDHHMMSS
                         -e   end   time YYYYMMDDHHMMSS 
                              if no start or end time are specified the entire file is  output. \n";
           exit;
       }

#      Initialize

       my @header     = ();
       my %xml        = ();
       my @compenents = ();

#      Check file exits. If it does open XML file, extract header and instrument type.

       open (XFILE, "$opt_f") || die " Cannot open file $opt_f\n";

       while (<XFILE>)         {
	       chomp ;
               $type = (split( /<|>/, $_))[2] if grep { ( /itype/ ) } $_ ;
               last  if grep { ( /<data>/ ) } $_ ;
               push(@header, "$_\n");
       }

#      Determine strainmeter type

       if ($type =~ /BSM/ ) {
           @components =(gage0,gage1,gage2,gage3,"Eee+Enn","Eee-Enn","2Ene");
       } elsif ($type =~ /LSM/)  {
           @components =("lsm");
       } else {
           print "Cannot determine strainmeter type, laser or borehole\n";
           exit;
       } 

#      Option i: Print XML header    
       
       if ($opt_i) {
           print "@header\n";
           exit;
       } 

#      Convert the start and end time to Modified Julian Dates.
#      If no dates are specified default dates of 1 Jan 1970 and 1 Jan 3000 are used

       if ($opt_s) {
           $start       = $opt_s;
           $start_epoch = timegm(0, substr($start,10,2), substr($start,8,2),substr($start,6,2) , substr($start,4,2)-1, substr($start,0,4)); 
       } 
       $start_mjd       = sprintf("%12.6f", 40587 + $start_epoch/86400);

       if ($opt_e) {
           $end         = $opt_e;
           $end_epoch   = timegm(0, substr($end,10,2), substr($end,8,2),substr($end,6,2) , substr($end,4,2)-1, substr($end,0,4));
           $end_mjd     = sprintf("%12.6f", 40587 + $end_epoch/86400);
       } else {
           $end_mjd     = 416787;                     
       }

#      Parse  XML file
       
       $count = 0;
       READ: while (<XFILE>)         {
               chomp ;
               ($key, $value) = (split (/<|>/,$_))[1,2];
               if ($key =~ /MJD/ && $value > $end_mjd ) {
                   delete $xml{$count};
                   last READ;
               }
               last if $key =~ /\/data/;
               if ($key   =~ /strain/) {
		   $value = (split(/=/,$key))[1];
                   $key = "obs";
                   $count++
                }
                $xml{$count}{$key} = $value;
       }
 
       close XFILE;

#      Option l: Print station and data summary and exit

       if ($opt_l) { 

                   $startflag = 0;
                   $aflag = 0;
                   $bflag = 0;
                   $hflsg = 0;

                   foreach $element (  sort { $xml{$a}{MJD} <=> $xml{$b}{MJD} } keys %xml     ) {
                           if ($startflag  == 0 ) {$firste = $element; $startflag++}
                           if ($aflag  == 0 && $xml{$element}{level} =~ /2a/ )  {$l2a    = $element; $aflag++}
                           if ($bflag  == 0 && $xml{$element}{level} =~ /2b/)  {$l2b    = $element; $bflag++}
                           $laste = $element;
                   }

                   print "\n\nStart Date: $xml{$firste}{date}\n";
                   print "End   Date: $xml{$laste}{date}\n";
                   if ($l2b) {
                          print "\nLevel 2b Starts: $xml{$l2b}{date}\n";
                   } else {
                          print "No Level 2b data\n";
                   }
                   if ($l2a) {
                          print "\nLevel 2a Starts: $xml{$l2a}{date}\n";
                   } else {
                          print "No Level 2a data\n";
                   }

                   print "Level 2a Starts: $xml{$l2a}{date}\n" if ($l2a);

                   foreach $line (@header) {
                           $hflag++ if $line =~ /<station_info/;
                           print $line if ($hflag == 1);
                           last if $line =~ /\/station_info/;
                   }
                   exit;
       }

#      Seperate into components, delete any elements outside the specified time period.

       foreach $comp (@components) {
               $ncount = 0;
               %$comp  = ();
               foreach  $element ( keys %xml  ) {
                        $mjday = $xml{$element}{MJD}; 
                        if ( $mjday >= $start_mjd && $mjday <= $end_mjd ) {
                             if ( ($xml{$element}{obs} =~ /$comp/) ||  ($comp eq "Eee+Enn" && $xml{$element}{obs} =~ /Eee\+Enn/ ) ) {
                                  $ncount++;
                                  %{$$comp{$ncount}} = %{$xml{$element}} ;
                                  delete $xml{$element}; 
                             }
                        } else {
                             delete   $xml{$element} ;
                        }
                }
       }
 


#      Options  r q o  : Extract data


       foreach $comp (@components ) {

               open (COMP,">$opt_f.$comp");
               $flag = 0;
               $off_flag = 0;
               if ($opt_o eq "x") { print COMP "@header  	<data>\n"; }

               foreach $element (  sort { $$comp{$a}{MJD} <=> $$comp{$b}{MJD}   }  keys  %$comp ) {

                       $off_flag = 1 if exists $$comp{$element}{s_offset};
                       if ($opt_j) { $$comp{$element}{s} = $$comp{$element}{s}+$$comp{$element}{s_offset} };
                       if ($opt_r) { $$comp{$element}{s} = $opt_r if $$comp{$element}{s_q} ne g };
                       if ($opt_q) {
	                   $$comp{$element}{s} = $opt_q  if ($type =~/BSM/ && $$comp{$element}{apc_q} ne g);
			   $$comp{$element}{s} = $opt_q  if ($type =~/LSM/ && ($$comp{$element}{oaic_q} ne g || $$comp{$element}{oarc_q} ne g ));
                        }
                       if ($opt_o eq x) {
                                  print COMP  "\t\t<obs  strain=$$comp{$element}{obs} > \n"; 
                                  print COMP  "\t\t\t<date> $$comp{$element}{date} </date> \n"; 
                                  print COMP  "\t\t\t<doy> $$comp{$element}{doy} </doy> \n"; 
                                  print COMP  "\t\t\t<MJD> $$comp{$element}{MJD} </MJD> \n"; 
                                  print COMP  "\t\t\t<s> $$comp{$element}{s} </s> \n"; 
                                  print COMP  "\t\t\t<s_q> $$comp{$element}{s_q} </s_q> \n"; 
                                  print COMP  "\t\t\t<s_offset> $$comp{$element}{s_offset} </s_offset> \n" if exists $$comp{$element}{s_offset}; 
                                  print COMP  "\t\t\t<tc> $$comp{$element}{tc} </tc> \n"; 
                                  print COMP  "\t\t\t<dtc> $$comp{$element}{dtc} </dtc> \n"         if exists $$comp{$element}{dtc}; 
                                  print COMP  "\t\t\t<apc> $$comp{$element}{apc} </apc> \n"         if exists $$comp{$element}{apc};
                                  print COMP  "\t\t\t<apc_q> $$comp{$element}{apc_q} </apc_q> \n"   if exists $$comp{$element}{apc_q};
                                  print COMP  "\t\t\t<oaic> $$comp{$element}{oaic} </oaic> \n"      if exists $$comp{$element}{oarc};
                                  print COMP  "\t\t\t<oaic_q>$$comp{$element}{oaic_q} </oaic_q> \n" if exists $$comp{$element}{oaic_q};
                                  print COMP  "\t\t\t<oarc>$$comp{$element}{oarc} </oarc> \n"       if exists $$comp{$element}{oarc};
                                  print COMP  "\t\t\t<oarc_q>$$comp{$element}{oarc_q} </oarc_q> \n" if exists $$comp{$element}{oarc_q};
                                  print COMP  "\t\t\t<level> $$comp{$element}{level} </level> \n"; 
                                  print COMP  "\t\t\t<v> $$comp{$element}{v} </v> \n"; 
                                  print COMP  "\t\t</obs> \n";                               
                         } else {

		                  if ($flag == 0) { 
		                      if ($type =~ /BSM/ ) {  if ($off_flag == 0 ) {
                                                                  print COMP  "strain\tdate\tdoy\tMJD\t${comp}(Microstrain)\tstrain_quality\ttidal_c\tdetrend_c\tatmp_c\tatm_c_quality\tlevel\tversion\n";
                                                               }  else {
                                                                  print COMP  "strain\tdate\tdoy\tMJD\t${comp}(Microstrain)\ts_offset\tstrain_quality\ttide_c\tdetrend_c\tatmp_c\tatmp_c_quality\tlevel\tversion\n";
                                                               }
                                                            }
		                      if ($type =~ /LSM/ ) {print COMP  "strain\tdate\tdoy\tMJD\t${comp}\tstrain_quality\ttidal_c\toaic\toaic_quality\toarc\toarc_quality\tlevel\tv\n";}
		                      $flag = 1;
		                  }

		                  if ($type =~ /BSM/ && $off_flag == 0 ) { 
		                      printf COMP  "%s\t%s\t%d\t%12.6f\t%12.4f\t%s\t%12.4f\t%12.4f\t%12.4f\t%s\t%s\t%s\n",$$comp{$element}{obs},$$comp{$element}{date},
                                                    $$comp{$element}{doy},$$comp{$element}{MJD},$$comp{$element}{s},$$comp{$element}{s_q},$$comp{$element}{tc},
                                                    $$comp{$element}{dtc},$$comp{$element}{apc},$$comp{$element}{apc_q},$$comp{$element}{level},$$comp{$element}{v} ;
                                  } elsif ($type =~ /BSM/ && $off_flag ==1 ) {
### SQUID TEST Increase precsision #####
                                      printf COMP  "%s\t%s\t%d\t%14.8f\t%14.8f\t%14.8f\t%s\t%14.8f\t%14.8f\t%14.8f\t%s\t%s\t%s\n",$$comp{$element}{obs},$$comp{$element}{date},                                                        $$comp{$element}{doy},$$comp{$element}{MJD},$$comp{$element}{s},$$comp{$element}{s_offset},$$comp{$element}{s_q},
#                                      printf COMP  "%s\t%s\t%d\t%12.6f\t%12.4f\t%12.4f\t%s\t%12.4f\t%12.4f\t%12.4f\t%s\t%s\t%s\n",$$comp{$element}{obs},$$comp{$element}{date},                                                        $$comp{$element}{doy},$$comp{$element}{MJD},$$comp{$element}{s},$$comp{$element}{s_offset},$$comp{$element}{s_q},
                                                    $$comp{$element}{tc},$$comp{$element}{dtc},$$comp{$element}{apc},$$comp{$element}{apc_q},$$comp{$element}{level},
                                                    $$comp{$element}{v} ;
		                  } elsif ($type =~ /LSM/ ) { 
				      printf COMP  "%s\t%s\t%d\t%12.6f\t%12.3f\t%s\t%12.3f\t%12.3f\t%s\t%12.3f\t%s\t%s\t%s\n", $$comp{$element}{obs},$$comp{$element}{date},
                                                   $$comp{$element}{doy},$$comp{$element}{MJD},$$comp{$element}{s},$$comp{$element}{s_q},$$comp{$element}{tc},
                                                   $$comp{$element}{oaic},$$comp{$element}{oaic_q},$$comp{$element}{oarc},$$comp{$element}{oarc_q},$$comp{$element}{level},
                                                   $$comp{$element}{v} ; 
			          }
                         }
                       
              }

              if ($opt_o eq x) { print COMP "\t</data>\n</strain_xml>\n"}
              close COMP;
       }

