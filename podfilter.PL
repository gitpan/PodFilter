#!/usr/local/bin/perl

use Config;
use File::Basename qw(&basename &dirname);

# List explicitly here the variables you want Configure to
# generate.  Metaconfig only looks for shell variables, so you
# have to mention them as if they were shell variables, not
# %Config entries.  Thus you write
#  $startperl
# to ensure Configure will look for $Config{startperl}.

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
	if ($^O eq 'VMS' or $^O eq 'os2');  # "case-forgiving"

open OUT,">$file" or die "Can't create $file: $!";

print "Extracting $file (with variable substitutions)\n";

# In this section, perl variables will be expanded during extraction.
# You can use $Config{...} to use Configure variables.

print OUT <<"!GROK!THIS!";
$Config{'startperl'}
    eval 'exec perl -S \$0 "\$@"'
	if 0;
!GROK!THIS!

# In the following, perl variables are not expanded during extraction.

print OUT <<'!NO!SUBS!';

#############################################################################
# podfilter -- command to invoke the podfilter function in Pod::Filter
#
# Derived from Tom Christiansen's pod2text script.
# (with extensive modifications)
#
# Copyright (c) 1996 Bradford Appleton. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
#############################################################################

use strict;
use diagnostics;

=head1 NAME

podfilter - print selected sections of pod documentation on standard output

=head1 SYNOPSIS

B<podfilter> [B<-help>] [B<-man>] [B<-select>S< >I<section-spec>]
[I<file>S< >...]

=head1 OPTIONS AND ARGUMENTS

=over 8

=item B<-help>

Print a brief help message and exit.

=item B<-man>

Print the manual page and exit.

=item B<-select>S< >I<section-spec>

Specify a section to include in the output.
See L<Pod::Filter/"SECTION SPECIFICATIONS">
for the format to use for I<section-spec>.
This option may be given multiple times on the command line.

=item I<file>

The pathname of a file from which to select sections of pod
documentation (defaults to standard input).

=back

=head1 DESCRIPTION

B<podfilter> will read the given input files looking for pod
documentation and will print out (in raw pod format) all sections that
match one ore more of the given section specifications. If no section
specifications are given than all pod sections encountered are output.

=cut

use Pod::Filter;
use Pod::Usage;
use Getopt::Long;

## Define options
my %options = (
        "help"   => 0,
        "man"    => 0,
        "select" => [],
);

## Parse options
GetOptions(\%options, "help", "man", "select=s@")  ||  pod2usage(2);
pod2usage(1)  if ($options{help});
pod2usage(VERBOSE => 2)  if ($options{man});

## Dont default to STDIN if connected to a terminal
pod2usage(2) if ((@ARGV == 0) && (-t STDIN));

## Invoke podfilter().
if (@{$options{"select"}} > 0) {
    podfilter({SELECT => $options{"select"}}, @ARGV);
}
else {
    podfilter(@ARGV);
}


!NO!SUBS!

close OUT or die "Can't close $file: $!";
chmod 0755, $file or die "Can't reset permissions for $file: $!\n";
exec("$Config{'eunicefix'} $file") if $Config{'eunicefix'} ne ':';
