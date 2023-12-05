#!/usr/bin/perl

use List::Util qw( min max );

sub applyMapToNumber {
    my ($number, @maps) = @_;
    foreach my $map (@maps) {
        my ($destinationStart, $sourceStart, $range) = @$map;
        my $sourceEnd = $sourceStart + $range;
        if ($number >= $sourceStart && $number < $sourceEnd) {
            return $destinationStart + ($number - $sourceStart);
        }
    }
    return $number;
}

sub applyMapToNumbers {
    my ($numbers_ref, $maps_ref) = @_;
    return map { applyMapToNumber($_, @$maps_ref) } @$numbers_ref;
}

my @maps = ();
my @seeds = ();

while(<>) {
    if ($_ =~ /seeds:/) {
        @seeds = ($_ =~ /(\d+)/g);
        print ("Seeds: ", join(" ", @seeds), "\n");
    } elsif ($_ =~ /\-to\-/) {
        my ($from, $to) = ($_ =~ /(.+)\-to\-(.+) map:/g);
        print $from, " ", $to, "\n";
    } elsif ($_ =~ /(\d+) (\d+) (\d+)/) {
        my @newMap = ($_ =~ /(\d+) (\d+) (\d+)/g);
        push(@maps, [@newMap]);
    } else {
        print("Apply\n");
        @seeds = applyMapToNumbers(\@seeds, \@maps);
        print("-> ",join(" ", @seeds), "\n");
        @maps = ();
    }
}
@seeds = applyMapToNumbers(\@seeds, \@maps);

print("result: ",min(@seeds), "\n");