#!/usr/bin/perl

# small utility perl script to generate a human readable grammar from a dypgen
# file...
# Probably full of bugs!



# printf help if number of arguments different from 2
if (scalar(@ARGV) != 2) {
 print "usage: $0 input.dyp output.txt\n";
 exit -1;
}


$dypgen_file = $ARGV[0];
open (DYP_FILE, "<$dypgen_file") || die "Can't open $dypgen_file!\n";

$output_file = $ARGV[1];
open (GRAM_FILE, ">$output_file")  || die  "Can't open $output_file!\n";



## remove OCAML preamble
## the first line should contain a '{' on the first line.
## it removes all lines until a line ending with a '}'
sub remove_caml_preamble {
 $line = <DYP_FILE> ; chop $line ;
 if ($line =~ /^\s*{/) {
   while ($line=<DYP_FILE>) {
     if ($line =~ /}\s*$/) {return 0;}
     }}
 else {return 1};
}



sub process_preamble {
  while ($line=<DYP_FILE>) {

    # end of preamble...
    if ($line =~ /^(%%)|(%parser)|(%lexer)/) {return 0;}

    # starting rule
    elsif ($line =~ /^%start/) { $line =~ s/\s*<.*>\s*/ / ; print GRAM_FILE "$line" }

    # tokens with definition and substitution
    elsif ($line =~ /%token\s+(<.*>\s*)?(\w+)\s+\/\*\s*:=\s*\'(.*)\'\s+->\s*\'(.*)\'\s*\*\/$/) {
      $token = "$2" ;
      $subst = "$4" ;
      $def = $3 ;
      $substitution{$token} = $subst;
      print GRAM_FILE "%token  $token  :=  $def\n" }

    # tokens with token definition
    elsif ($line =~ /%token\s+(<.*>\s*)?(\w+)\s+\/\*\s*:=\s*\'(.*)\'.*\*\/$/) {
      $token = $2 ;
      $def = $3 ;
      print GRAM_FILE "%token  $token  :=  $def\n" }

    # token with substitution
    elsif ($line =~ /%token\s+(<.*>\s*)?(\w+)\s+\/\*\s*->\s*\'(.*)\'.*\*\/$/) {
      $token = "$2" ;
      $subst = "$3" ;
      $substitution{$token} = $subst
    }

    #all the rest is unchanged
    else {print GRAM_FILE "$line";}

  }
}



sub remove_action {
  while ($line=<DYP_FILE>) {
    if ($line =~ /^[^}]*}\s*$/) {return ""}
    elsif ($line =~ /^[^}]*}\s*(\w+)\s*/) {return "$1\n"}
}}

sub remove_comment {
  while ($line=<DYP_FILE>) {
    if ($line =~ /^.* \*\/\s*$/) {return ""}
    elsif ($line =~ /^.*\*\/\s*(\w+)\s*/) {return "$1\n"}
}}




remove_caml_preamble ();

process_preamble ();

while ($line=<DYP_FILE>) {
  if ($line =~ /^(\s*){.*}\s*$/) {$line = "";}
  elsif ($line =~ /^([^{]*){.*}(.*)$/) {$line = "$1  $2\n"}
  elsif ($line =~ /(.*){[^}]*/) { $line = $1 . remove_action();
                                  if ($line =~ /^\s*$/) { next } }
  elsif ($line =~ /(.*)\/\*--/) {$line = $1 . remove_comment();}



  foreach $token (keys(%substitution)) { $line =~ s/(\W)$token(\W)/$1\"$substitution{$token}\"$2/g }
  print GRAM_FILE "$line";
}


close (DYP_FILE) ;
close (GRAM_FILE) ;





print "Grammar generated in $output_file.\n";

exit 0;


