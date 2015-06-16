package Perinci::Sub::Getopt;

# DATE
# VERSION

use 5.010001;
use strict 'subs', 'vars';
use warnings; # COMMENT

our @EXPORT = qw();
our @EXPORT_OK = qw(get_options);
our %SPEC;

sub import {
    my $pkg = shift;
    my $caller = caller;
    my @imp = @_ ? @_ : @EXPORT;
    for my $imp (@imp) {
        if (grep {$_ eq $imp} (@EXPORT, @EXPORT_OK)) {
            *{"$caller\::$imp"} = \&{$imp};
        } else {
            die "$imp is not exported by ".__PACKAGE__;
        }
    }
}

$SPEC{get_options} = {
    v => 1.1,
    summary => 'Get command-line options with specification '.
        'from Rinci function metadata',
    args => {
        meta => {
            summary => 'Rinci function metadata, must be normalized',
            schema  => 'hash*',
            req => 1,
        },
        args => {
            summary => 'Hash to store resulting options in',
            schema => 'hash*',
        },
    },
};
sub get_options {
    my $argv = shift;

    my $vals;
    my $spec;

    # if next argument is a hashref, it means user wants to store values in this
    # hash. and the spec is a list.
    if (ref($_[0]) eq 'HASH') {
        $vals = shift;
        $spec = {map { $_ => sub { $vals->{ $_[0]->name } = $_[1] } } @_};
    } else {
        $spec = {@_};
    }

    # parse option spec
    my %parsed_spec;
    for my $k (keys %$spec) {
        my $parsed = parse_getopt_long_opt_spec($k)
            or die "Error in option spec: $k\n";
        if (defined $parsed->{max_vals}) {
            die "Cannot repeat while bundling: $k\n";
        }
        $parsed->{_orig} = $k;
        $parsed_spec{$parsed->{opts}[0]} = $parsed;
    }
    my @parsed_spec_opts = sort keys %parsed_spec;

    my $success = 1;

    my $code_find_opt = sub {
        my ($wanted, $short_mode) = @_;
        my @candidates;
      OPT_SPEC:
        for my $opt (@parsed_spec_opts) {
            my $s = $parsed_spec{$opt};
            for my $o0 (@{ $s->{opts} }) {
                for my $o ($s->{is_neg} && length($o0) > 1 ?
                               ($o0, "no$o0", "no-$o0") : ($o0)) {
                    my $is_neg = $o0 ne $o;
                    next if $short_mode && length($o) > 1;
                    if ($o eq $wanted) {
                        # perfect match, we immediately go with this one
                        @candidates = ([$opt, $is_neg]);
                        last OPT_SPEC;
                    } elsif (index($o, $wanted) == 0) {
                        # prefix match, collect candidates first
                        push @candidates, [$opt, $is_neg];
                        next OPT_SPEC;
                    }
                }
            }
        }
        if (!@candidates) {
            warn "Unknown option: $wanted\n";
            $success = 0;
            return (undef, undef);
        } elsif (@candidates > 1) {
            warn "Option $wanted is ambiguous (" .
                join(", ", map {$_->[0]} @candidates) . ")\n";
            $success = 0;
            return (undef, undef, 1);
        }
        return @{ $candidates[0] };
    };

    my $code_set_val = sub {
        my $is_neg = shift;
        my $name   = shift;

        my $parsed   = $parsed_spec{$name};
        my $spec_key = $parsed->{_orig};
        my $handler  = $spec->{$spec_key};
        my $ref      = ref($handler);

        my $val;
        if (@_) {
            $val = shift;
        } else {
            if ($parsed->{is_inc} && $ref eq 'SCALAR') {
                $val = ($$handler // 0) + 1;
            } elsif ($parsed->{is_inc} && $vals) {
                $val = ($vals->{$name} // 0) + 1;
            } elsif ($parsed->{type} && $parsed->{type} eq 'i' ||
                         $parsed->{opttype} && $parsed->{opttype} eq 'i') {
                $val = 0;
            } elsif ($parsed->{type} && $parsed->{type} eq 'f' ||
                         $parsed->{opttype} && $parsed->{opttype} eq 'f') {
                $val = 0;
            } elsif ($parsed->{type} && $parsed->{type} eq 's' ||
                         $parsed->{opttype} && $parsed->{opttype} eq 's') {
                $val = '';
            } else {
                $val = $is_neg ? 0 : 1;
            }
        }

        # type checking
        if ($parsed->{type} && $parsed->{type} eq 'i' ||
                $parsed->{opttype} && $parsed->{opttype} eq 'i') {
            unless ($val =~ /\A[+-]?\d+\z/) {
                warn qq|Value "$val" invalid for option $name (number expected)\n|;
                return 0;
            }
        } elsif ($parsed->{type} && $parsed->{type} eq 'f' ||
                $parsed->{opttype} && $parsed->{opttype} eq 'f') {
            unless ($val =~ /\A[+-]?(\d+(\.\d+)?|\.\d+)([Ee][+-]?\d+)?\z/) {
                warn qq|Value "$val" invalid for option $name (number expected)\n|;
                return 0;
            }
        }

        if ($ref eq 'CODE') {
            my $cb = Getopt::Long::Less::Callback->new(
                name => $name,
            );
            $handler->($cb, $val);
        } elsif ($ref eq 'SCALAR') {
            $$handler = $val;
        } else {
            # no nothing
        }
        1;
    };

    my $i = -1;
    my @remaining;
  ELEM:
    while (++$i < @$argv) {
        if ($argv->[$i] eq '--') {

            push @remaining, @{$argv}[$i+1 .. @$argv-1];
            last ELEM;

        } elsif ($argv->[$i] =~ /\A--(.+?)(?:=(.*))?\z/) {

            my ($used_name, $val_in_opt) = ($1, $2);
            my ($opt, $is_neg, $is_ambig) = $code_find_opt->($used_name);
            unless (defined $opt) {
                push @remaining, $argv->[$i] unless $is_ambig;
                next ELEM;
            }

            my $spec = $parsed_spec{$opt};
            # check whether option requires an argument
            if ($spec->{type} ||
                    $spec->{opttype} &&
                    (defined($val_in_opt) && length($val_in_opt) || ($i+1 < @$argv && $argv->[$i+1] !~ /\A-/))) {
                if (defined($val_in_opt)) {
                    # argument is taken after =
                    if (length $val_in_opt) {
                        unless ($code_set_val->($is_neg, $opt, $val_in_opt)) {
                            $success = 0;
                            next ELEM;
                        }
                    } else {
                        warn "Option $used_name requires an argument\n";
                        $success = 0;
                        next ELEM;
                    }
                } else {
                    if ($i+1 >= @$argv) {
                        # we are the last element
                        warn "Option $used_name requires an argument\n";
                        $success = 0;
                        last ELEM;
                    }
                    # take the next element as argument
                    if ($spec->{type} || $argv->[$i+1] !~ /\A-/) {
                        $i++;
                        unless ($code_set_val->($is_neg, $opt, $argv->[$i])) {
                            $success = 0;
                            next ELEM;
                        }
                    }
                }
            } else {
                unless ($code_set_val->($is_neg, $opt)) {
                    $success = 0;
                    next ELEM;
                }
            }

        } elsif ($argv->[$i] =~ /\A-(.*)/) {

            my $str = $1;
          SHORT_OPT:
            while ($str =~ s/(.)//) {
                my $used_name = $1;
                my ($opt, $is_neg) = $code_find_opt->($1, 'short');
                next SHORT_OPT unless defined $opt;

                my $spec = $parsed_spec{$opt};
                # check whether option requires an argument
                if ($spec->{type} ||
                        $spec->{opttype} &&
                        (length($str) || ($i+1 < @$argv && $argv->[$i+1] !~ /\A-/))) {
                    if (length $str) {
                        # argument is taken from $str
                        if ($code_set_val->($is_neg, $opt, $str)) {
                            next ELEM;
                        } else {
                            $success = 0;
                            next SHORT_OPT;
                        }
                    } else {
                        if ($i+1 >= @$argv) {
                            # we are the last element
                            warn "Option $used_name requires an argument\n";
                            $success = 0;
                            last ELEM;
                        }
                        # take the next element as argument
                        if ($spec->{type} || $argv->[$i+1] !~ /\A-/) {
                            $i++;
                            unless ($code_set_val->($is_neg, $opt, $argv->[$i])) {
                                $success = 0;
                                next ELEM;
                            }
                        }
                    }
                } else {
                    unless ($code_set_val->($is_neg, $opt)) {
                        $success = 0;
                        next SHORT_OPT;
                    }
                }
            }

        } else { # argument

            push @remaining, $argv->[$i];
            next;

        }
    }

  RETURN:
    splice @$argv, 0, ~~@$argv, @remaining; # replace with remaining elements
    return $success;
}

1;
#ABSTRACT:

=head1 DESCRIPTION

# COMMAND: perl devscripts/bench-startup 2>&1

=head1 SEE ALSO

L<Getopt::Long>

=cut
