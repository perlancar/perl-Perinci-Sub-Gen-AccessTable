use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Sub::Spec::Gen::ReadTable qw(gen_read_table_func);
use Test::More 0.96;

sub test_gen {
    my (%args) = @_;

    subtest $args{name} => sub {
        my $res;
        eval {
            $res = gen_read_table_func(
                table_data => $args{table_data},
                table_spec => $args{table_spec},
            );
        };
        my $eval_err = $@;
        diag "died during function: $eval_err" if $eval_err;

        if ($args{dies}) {
            ok($eval_err, "dies");
        }

        if ($args{status}) {
            is($res->[0], $args{status}, "status = $args{status}") or
                do { diag explain $res; return };
        }

        if ($res->[0] == 200) {
            my $func = $res->[2]{code};
            my $spec = $res->[2]{spec};
            is(ref($func), 'CODE', 'func returned');
            is(ref($spec), 'HASH', 'spec returned');
            my $args = $spec->{args};
            for my $a (qw/show_field_names detail fields
                          sort random result_limit result_start
                          q/) {
                ok($args->{$a}, "common arg '$a' generated");
            }
        }

        if ($args{post_test}) {
            $args{post_test}->($res);
        }
    };
}

1;
