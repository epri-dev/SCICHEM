# Variable list, region list, beginning month, and ending month are passed to this script from command line
# ARG0 - input directory
# ARG1 - output directory
# ARG2 - variable list
# ARG3 - region list
# ARG4 - YYYY
# ARG5 - RUNNAME
# ARG6 - SCICHEM vertical layer heights

$i0 = 0;
$i1 = 1;

# Get number of layers and heights 
$nlay = 0;
open(INPF, $ARGV[6]) || die "Cannot open input file: SCICHEM vertical layer heights\n";
while ($ll = <INPF>){
  @dum = split(/,/,$ll);
  $topht{$dum[0]} = $dum[1];
  $nlay++;
}
print "number of layers: $nlay\n";
$dx=0.0; $dy=0.0; $x0=0.0; $y0=0.0;

# Get variable list
open(INPF, $ARGV[2]) || die "Cannot open input file: var list\n";
$nvar = 0;
while ($ll = <INPF>){
  chomp($ll);
  @dum = split(/,/,$ll);
  $varlst{$dum[0]} = $dum[1];
  print "$dum[0] => $dum[1]\n";
  $nvar++;
}
close(INPF);

# Get region list
open(INPF, $ARGV[3]) || die "Cannot open input file: region list\n";
$nreg = 0;
while ($ll = <INPF>){
  chomp($ll);
  @dum = split(/,/,$ll);
  $reglst{$dum[0]}{'full'} = $dum[1];
  $reglst{$dum[0]}{'lat'} = $dum[2];
  $reglst{$dum[0]}{'lon'} = $dum[3];
  print "$dum[0] => SW (lat,lon) = $dum[2],$dum[3]\n";
  $nreg++;
}
close(INPF);

$inpdir = $ARGV[0];
$outdir = $ARGV[1];
$YYYY = $ARGV[4];
$RUNNAME = $ARGV[5];

foreach my $imo (1,2,3,4,5,6,7,8,9,10,11,12) {
#for ($imo=$begm;$imo<=$endm;$imo++) {
  $imm = sprintf "%02d",$imo;
  for $var ( sort keys %varlst ){
    $inpf = $inpdir.'/'.$RUNNAME.'.'.$YYYY.$imm.'.'.$var.'.no_laymap.txt';
    print "$inpf\n";
    open(INPF, "$inpf") || die "Cannot open input file: $inpf\n";
      $n = 0;
      while ($ll = <INPF>){
        #skip header line
        if ($n != 0) {
          chomp($ll);
          @dum = split(',',$ll);
          $hr     =  $dum[0];
          $k      =  $dum[1];
          $k = sprintf "%1d",$k;
          $hr = sprintf "%1d",$hr;
          #find match for full name, but store as ID in array 
          for $reg ( sort keys %reglst ){
              if ($reglst{$reg}{'full'} eq $dum[2]) {
                 $region = $reg;
              } 
          }
          $db{$var}{$k}{$imo}{$hr}{$region} = $dum[3];
        }
        $n++;
      }
    close(INPF);
  }#var
}

for $reg ( sort keys %reglst ){
  print "$reg,$nlay\n";
  $outf   = $outdir.$reg.'.amb';
  open(OUTF, ">$outf") || die "Cannot open output file: $outf\n";
    foreach my $imo (1,2,3,4,5,6,7,8,9,10,11,12) {
    for ($hr=1;$hr<=24;$hr++){
      # Line 1
      print OUTF "FFFFFFFF\n";
      # Line 2
      print OUTF "TEST          F\n";
      # Line 3,4
      $iyr = 9999; $idy = 99;
      $ihr = $hr; $imn = 0; $isc = 0;
      printf OUTF "%12d %12d %12d %12d %12d %12d \n",$idy,$imo,$iyr,$ihr,$imn,$isc;
      printf OUTF "%12d %12d %12d %12d %12d %12d \n",$idy,$imo,$iyr,$ihr,$imn,$isc;
      # Line 5
      printf OUTF "%12d %12d %12d %12d %12d %12d \n",$i1,$i1,$nlay,$i0,$nvar,$i0;
      # Line 6
      printf OUTF "%12d %12d %12d %12d %12d %12d \n",$i0,$i0,$i0,$i0,$i0,$i0;
      # Line 7
      printf OUTF "%12d %12d %12d \n",$i0,$i0,$i0;
      # Line 8
      $xlat0 = $reglst{$reg}{'lat'}; $xlon0 = $reglst{$reg}{'lon'};
      printf OUTF "%12.4f %12.4f %12.4f %12.4f %12.4f %12.4f \n",
                  $topht{1},$topht{2},$topht{3},$topht{4},$topht{5},$topht{6};
      printf OUTF "%12.4f %12.4f %12.4f %12.4f %12.4f %12.4f \n",
                  $topht{7},$topht{8},$topht{9},$topht{10},$topht{11},$topht{12};
      printf OUTF "%12.4f %12.4f %12.4f %12.4f %12.4f %12.4f \n",
                  $topht{13},$topht{14},$topht{15},$topht{16},$topht{17},$topht{18};
      printf OUTF "%12.4f %12.4f %12.4f %12.4f %12.4f %12.4f \n",
                  $dx,$dy,$x0,$y0,$xlat0,$xlon0;
      printf OUTF "%12.4f %12.4f %12.4f %12.4f %12.4f %12.4f \n",
                  $i0,$i0,$i0,$i0,$i0;
      # Line 9
      $line9 = '';
      $cnt = 0;
      #write species list
      for $var ( sort keys %varlst ){
        $cnt++;
        if ($cnt != 6){
          $line9 = $line9.sprintf "%-8s ",$var;
        } elsif ($cnt == 6){
          $line9 = $line9.sprintf "%-8s \n",$var;
          $cnt = 0;
        } else {
          print "Error: number of species per line\n";
        }
      }
      #write species list
      for $var ( sort keys %varlst ){
        $cnt++;
        if ($cnt != 6){
          $line9 = $line9.sprintf "%-8s ",$varlst{$var};
        } elsif ($cnt == 6){
          $line9 = $line9.sprintf "%-8s \n",$varlst{$var};
          $cnt = 0;
        } else {
          print "Error: number of species per line\n";
        }
      }
      if ($cnt == 0){
        print OUTF "$line9";
      } else {
        print OUTF "$line9\n";
      }
      # Line 10
      # need regionlst like varlst
      for $var ( sort keys %varlst ){
        $line10 = '';
        $line11 = '';
        $line12 = '';
        for ($k=1;$k<=6;$k++){
          $line10 = $line10.sprintf "%12.5E ",$db{$var}{$k}{$imo}{$hr}{$reg};
        }
        for ($k=7;$k<=12;$k++){
          $line11 = $line11.sprintf "%12.5E ",$db{$var}{$k}{$imo}{$hr}{$reg};
        }
        for ($k=13;$k<=18;$k++){
          $line12 = $line12.sprintf "%12.5E ",$db{$var}{$k}{$imo}{$hr}{$reg};
        }
        print OUTF "$line10\n";
        print OUTF "$line11\n";
        print OUTF "$line12\n";
      }
    }#hr
    }#imo
  close(OUTF);
}#region
