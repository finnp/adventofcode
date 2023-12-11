#!/usr/bin/perl

use List::Util qw( min max );

sub map_to_range_with_addition {
    my ($destination, $source, $length)  = @_;
    return ($source, $source + $length - 1, $destination - $source);
} 

sub parse_ranges {
    my $str = shift;
    my @numbers = split(/\s+/, $str);
    my @ranges;

    for (my $i = 0; $i < $#numbers; $i += 2) {
        my $start = $numbers[$i];
        my $end = $start + $numbers[$i + 1] - 1;
        push @ranges, [$start, $end];
    }

    return \@ranges;
}

my $inputRanges;
my $outputRanges;

while(<>) {
    if ($_ =~ /seeds:/) {
        $numbers = substr($_,7);
        print($numbers);
        $outputRanges = parse_ranges($numbers);
    } elsif ($_ =~ /\-to\-/) {

        my ($from, $to) = ($_ =~ /(.+)\-to\-(.+) map:/g);
        print "\n\n", $from, " ", $to, "\n";
        $inputRanges = [ @$inputRanges, @$outputRanges ];
        $outputRanges = [];
    } elsif ($_ =~ /(\d+) (\d+) (\d+)/) {
        my @new_map = map_to_range_with_addition ($_ =~ /(\d+) (\d+) (\d+)/g);

        foreach my $range (@$inputRanges) {
            print "Input Range: [", join(", ", @$range), "]\n";
        }

        print "New Map: [", join(", ", @new_map), "]\n";

        my @newInputRanges;

        foreach my $range (@$inputRanges) {
            my $result = modify_range($range, \@new_map);

            if (@{$result->{not_modified}}) {
                push @newInputRanges, @{$result->{not_modified}};
            }
            if (@{$result->{modified}}) {
                push @$outputRanges, @{$result->{modified}};
            }

            print "Modified ranges: ";
            foreach my $range (@{$result->{modified}}) {
                print "[", join(", ", @$range), "],";
            }
            print "\nNot modified ranges: ";
            foreach my $range (@{$result->{not_modified}}) {
                print "[", join(", ", @$range), "],";
            }
            print "\n";
        }

        $inputRanges = \@newInputRanges;

        print "\n";
    } else {
    }
}

print("Result:\n");
$inputRanges = [ @$inputRanges, @$outputRanges ];

my @minima = map { min(@$_) } @$inputRanges;

print "Minimum of minima: ", min(@minima), "\n";

sub modify_range {
    my ($range, $instruction) = @_;
    my ($instr_from, $instr_to, $addition) = @$instruction;

    my $range_from = $range->[0];
    my $range_to = $range->[1];

    my %result = (modified => [], not_modified => []);

    if ($instr_from > $range_to || $instr_to < $range_from) {
        push @{$result{not_modified}}, $range;
    } else {
        if ($range_from < $instr_from) {
            push @{$result{not_modified}}, [$range_from, $instr_from - 1];
        }

        my $overlap_start = $instr_from > $range_from ? $instr_from : $range_from;
        my $overlap_end = $instr_to < $range_to ? $instr_to : $range_to;
        push @{$result{modified}}, [$overlap_start + $addition, $overlap_end + $addition];

        if ($instr_to < $range_to) {
            push @{$result{not_modified}}, [$instr_to + 1, $range_to];
        }
    }

    return \%result;
}
