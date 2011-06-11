#!perl

use 5.010;
use strict;
use warnings;
use Log::Any '$log';
use Test::More 0.96;

use Sub::Spec::Gen::ReadTable qw(gen_read_table_func);

test_gen(
    name => 'spec generation tests',
    table_data => [],
    table_spec => {
        columns => {
            s  => ['str*' => {}],
            s2 => ['str*' => {column_filterable=>0}],
            s3 => ['str*' => {column_filterable_regex=>0}],
            i  => ['int*' => {}],
            f  => ['float*' => {}],
            a  => ['array*' => {}],
            b  => ['bool*' => {}],
        },
        pk => 'id',
    },
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};
        ok($args->{b}, "boolean filter arg 'b' generated");
        ok($args->{max_i}, "int filter arg 'min_i' generated");
        ok($args->{min_i}, "int filter arg 'max_i' generated");
        ok($args->{min_f}, "float filter arg 'min_f' generated");
        ok($args->{max_f}, "float filter arg 'max_f' generated");
        ok($args->{has_a}, "array filter arg 'has_a' generated");
        ok($args->{lacks_a}, "array filter arg 'lacks_a' generated");
        ok($args->{s}, "string filter arg 's' generated");
        ok($args->{s_contain}, "string filter arg 's_contain' generated");
        ok($args->{s_not_contain}, "string filter arg 's_not_contain' gen'd");
        ok($args->{s_match}, "string filter arg 's_match' generated");
        ok($args->{s_not_match}, "string filter arg 's_not_match' generated");

        ok(!$args->{s2}, "string filter arg 's2' not generated");
        ok(!$args->{s2_contain},
           "string filter arg 's2_contain' not generated");
        ok(!$args->{s2_not_contain},
           "string filter arg 's2_not_contain' not generated");
        ok(!$args->{s2_match}, "string filter arg 's2_match' not generated");
        ok(!$args->{s2_not_match},
           "string filter arg 's2_not_match' not generated");

        ok($args->{s3}, "string filter arg 's3' generated");
        ok($args->{s3_contain}, "string filter arg 's3_contain' generated");
        ok($args->{s3_not_contain},
           "string filter arg 's3_not_contain' generated");
        ok(!$args->{s3_match}, "string filter arg 's3_match' not generated");
        ok(!$args->{s3_not_match},
           "string filter arg 's3_not_match' not generated");
    },
);

# XXX test clash with args

DONE_TESTING:
done_testing();

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
                do { diag explain $res; last };
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

