#!perl

use 5.010;
use strict;
use warnings;
use Log::Any '$log';
use Test::More 0.96;

use Sub::Spec::Gen::ReadTable qw(gen_read_table_func);

# XXX test: pk must be in columns

test_gen(
    name => 'spec generation tests',
    table_data => [
        {s=>'a1', s2=>'', s3=>'a' , i=>1 , f=>0.1, a=>[qw//]   , b=>0},
        {s=>'b1', s2=>'', s3=>'aa', i=>2 , f=>0.2, a=>[qw/b/]  , b=>0},
        {s=>'a2', s2=>'', s3=>'a' , i=>-3, f=>1.2, a=>[qw/a/]  , b=>1},
        {s=>'a3', s2=>'', s3=>'aa', i=>4 , f=>1.1, a=>[qw/a b/], b=>1},
    ],
    table_spec => {
        columns => {
            s  => ['str*'   => {column_index=>0, }],
            s2 => ['str*'   => {column_index=>1, column_filterable=>0}],
            s3 => ['str*'   => {column_index=>2, column_filterable_regex=>0}],
            i  => ['int*'   => {column_index=>3, }],
            f  => ['float*' => {column_index=>4, }],
            a  => ['array*' => {column_index=>5, }],
            b  => ['bool*'  => {column_index=>6, }],
        },
        pk => 's',
    },
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};
        ok($args->{b}, "boolean filter arg 'b' generated");

        ok($args->{i}, "int filter arg 'i' generated");
        ok($args->{max_i}, "int filter arg 'min_i' generated");
        ok($args->{min_i}, "int filter arg 'max_i' generated");

        ok($args->{f}, "float filter arg 'f' generated");
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

