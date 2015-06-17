package Perinci::Sub::Getopt;

# DATE
# VERSION

use 5.010001;
use strict 'subs', 'vars';
use warnings; # COMMENT

our @EXPORT = qw();
our @EXPORT_OK = qw(get_args_from_argv);
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

$SPEC{get_args_from_argv} = {
    v => 1.1,
    summary => 'Get command-line options with specification '.
        'from Rinci function metadata',
    args => {
        meta => {
            summary => 'Rinci function metadata, must be normalized',
            schema  => 'hash*',
            req => 1,
        },
        argv => {
            summary => 'Input array, defaults to @ARGV',
            schema => ['array*', of=>'str*'],
        },
        args => {
            summary => 'Hash to store resulting options in',
            schema => 'hash*',
        },
    },
};
sub get_args_from_argv {
    my %ga_args = @_;

    my $meta = $ga_args{meta};
    my $args_prop = $meta->{args} // {};
    my $argv = $ga_args{argv} // \@ARGV;
    my $args = $ga_args{args} // {};

    my %optspecs;
    my %opthandlers;

    # XXX add common options

    for my $argname (sort keys %$args_prop) {
        my $argspec = $args_prop->{$argname};
        my $optname = $argname; $optname =~ s/[A-Z0-9.]+/-/g;

        # XXX handle clash
        #while ($optspecs{$optname}) {
        #    $optspecs{$optname} =
        #}
        my $req_arg = 1;
        $req_arg = 0 if $argspec->{schema} &&
            $argspec->{schema}[0] eq 'bool' && $argspec->{schema}[1]{is};
        $optspecs{$optname} = {
            arg => $argname,
            req_arg => $req_arg,
        };
        # XXX add fqarg
        # XXX add is_neg, neg_opts, pos_opts

        # XXX add negated options for bool arg
        # XXX add json & yaml for non-simple arg
        # XXX add base64 for binary arg

        if (my $als = $argspec->{cmdline_aliases}) {
            for my $alname (sort keys %$als) {
                my $alspec = $als->{$alname};
                # XXX handle clash
                my $al_req_arg;
                if ($alspec->{is_flag}) {
                    $al_req_arg = 0;
                } elsif ($alspec->{schema} &&
                             $alspec->{schema}[0] eq 'bool' && $alspec->{schema}[1]{is}) {
                    $al_req_arg = 0;
                } elsif ($alspec->{schema}) {
                    $al_req_arg = 1;
                }
                $optspecs{$alname} = {
                    is_alias => 1,
                    arg => $argname,
                    req_arg => $al_req_arg // $req_arg,
                    is_code => $alspec->{code} ? 1:0,
                };
                $opthandlers{$alname} = $alspec->{code} if $alspec->{code};
            }
            # XXX add noncode_aliases
        }
    }

    my @optnames = sort keys %optspecs;

    my $success = 1;

    my $code_find_opt = sub {
        my ($wanted, $short_mode) = @_;
        my @candidates;
      OPT:
        for my $optname (@optnames) {
            my $optspec = $optspecs{$optname};
            next if $short_mode && length($optname) > 1;
            if ($optname eq $wanted) {
                # perfect match, we immediately go with this one
                @candidates = ($optname);
                last OPT;
            } elsif (index($optname, $wanted) == 0) {
                # prefix match, collect candidates first
                push @candidates, $optname;
            }
        }
        if (!@candidates) {
            warn "Unknown option: $wanted\n";
            $success = 0;
            return undef; # means not found
        } elsif (@candidates > 1) {
            warn "Option $wanted is ambiguous (" .
                join(", ", map {$_->[0]} @candidates) . ")\n";
            $success = 0;
            return ''; # means ambiguous
        }
        return $candidates[0];
    };

    my $code_set_val = sub {
        my $optname = shift;

        my $val = @_ ? $_[0] : 1;

        if ($opthandlers{$optname}) {
            $opthandlers{$optname}->($val);
        } else {
            $args->{$optname} = $val;
        }
    };

    my $i = -1;
    my @remaining;
  ELEM:
    while (++$i < @$argv) {
        if ($argv->[$i] eq '--') {

            push @remaining, @{$argv}[$i+1 .. @$argv-1];
            last ELEM;

        } elsif ($argv->[$i] =~ /\A--(.+?)(?:=(.*))?\z/) {

            my ($used_optname, $val_in_opt) = ($1, $2);
            my $optname = $code_find_opt->($used_optname);
            if (!defined($optname)) {
                $success = 0;
                push @remaining, $argv->[$i];
            } elsif (!length($optname)) {
                $success = 0;
                next ELEM; # ambiguous
            }

            my $optspec = $optspecs{$optname};
            if ($optspec->{req_arg}) {
                if (defined($val_in_opt)) {
                    # argument is taken after =
                    if (length $val_in_opt) {
                        $code_set_val->($optname, $val_in_opt);
                    } else {
                        warn "Option $used_optname requires an argument\n";
                        $success = 0;
                        next ELEM;
                    }
                } else {
                    if ($i+1 >= @$argv) {
                        # we are the last element
                        warn "Option $used_optname requires an argument\n";
                        $success = 0;
                        last ELEM;
                    }
                    # take the next element as argument
                    $i++;
                    $code_set_val->($optname, $argv->[$i]);
                }
            } else {
                $code_set_val->($optname);
            }

        } elsif ($argv->[$i] =~ /\A-(.*)/) {

            my $str = $1;
          SHORT_OPT:
            while ($str =~ s/(.)//) {
                my $used_optname = $1;
                my $optname = $code_find_opt->($1, 'short');
                next SHORT_OPT unless defined($optname) && length($optname);

                my $optspec = $optspecs{$optname};
                if ($optspec->{req_arg}) {
                    if (length $str) {
                        # argument is taken from $str
                        $code_set_val->($optname, $str);
                    } else {
                        if ($i+1 >= @$argv) {
                            # we are the last element
                            warn "Option $used_optname requires an argument\n";
                            $success = 0;
                            last ELEM;
                        }
                        # take the next element as argument
                        $i++;
                        $code_set_val->($optname, $argv->[$i]);
                    }
                } else {
                    $code_set_val->($optname);
                }
            }

        } else { # argument

            push @remaining, $argv->[$i];
            next;

        }
    }

  RETURN:
    splice @$argv, 0, ~~@$argv, @remaining; # replace with remaining elements
    return [
        $success ? 200:500,
        $success ? "OK":"Failed",
        $args,
        {"func.remaining_argv" => $argv},
    ];
}

1;
#ABSTRACT:

=head1 DESCRIPTION

# COMMAND: perl devscripts/bench-startup

=head1 SEE ALSO

=cut
