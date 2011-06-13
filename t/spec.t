#!perl

# test spec generation and the generated spec

use 5.010;
use strict;
use warnings;
use FindBin '$Bin';
use lib $Bin, "$Bin/t";

use Test::More 0.96;
require "testlib.pl";

my ($table_data, $table_spec) = gen_test_data();

test_gen(
    name => 'pk must be in columns',
    table_data => [],
    table_spec => {
        columns => {
            a => ['int*' => {column_index=>0, }],
        },
        pk => 'b',
    },
    status => 400,
);

test_gen(
    name => 'pk must exist in table_spec',
    table_data => [],
    table_spec => {
        columns => {
            a => ['int*' => {column_index=>0, }],
        },
    },
    status => 400,
);

test_gen(
    name => 'columns must exist in table_spec',
    table_data => [],
    table_spec => {
    },
    status => 400,
);

test_gen(
    name => 'fields in sort must exist in columns',
    table_data => [],
    table_spec => {
        columns => {
            a => ['int*' => {column_index=>0, }],
        },
    },
    status => 400,
);

test_gen(
    name => 'field clash with argument',
    table_data => [],
    table_spec => {
        columns => {
            random => ['int*'  => {column_index=>0, }],
        },
        pk => 'random',
    },
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};
        ok( $args->{random}, "random arg generated");
        ok(!$args->{min_random}, "min_random arg not generated");
        ok(!$args->{max_random}, "max_random arg not generated");
        ok( $args->{random_field}, "random_field arg generated");
        ok( $args->{min_random_field}, "min_random_field arg generated");
        ok( $args->{max_random_field}, "min_random_field arg generated");
    },
);

test_gen(
    name => 'field clash with argument (fail)',
    table_data => [],
    table_spec => {
        columns => {
            random => ['int*'  => {column_index=>0, }],
            random_field => ['int*'  => {column_index=>0, }],
        },
        pk => 'random',
    },
    status => 400,
);

test_gen(
    name => 'spec generation',
    table_data => [],
    table_spec => $table_spec,
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

test_gen(
    name => 'disable search',
    table_data => [],
    table_spec => $table_spec,
    other_args => {enable_search=>0},
    status => 200,
);

test_gen(
    name => 'default_sort',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {default_sort=>"s"},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};

        my $fres;
        $fres = $func->(detail=>1);
        subtest "default_sort s" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            my @r = map {$_->{s}} @{$fres->[2]};
            is_deeply(\@r, [qw/a1 a2 a3 b1/], "sort result")
                or diag explain \@r;
        };
    },
);

test_gen(
    name => 'default_random',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {default_random=>1},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};

        test_random_order($func, {}, 50, [qw/a1 a2 a3 b1/],
                          "sort result");
    },
);

test_gen(
    name => 'default_fields',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {default_fields=>'s,b'},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};

        my $fres;
        $fres = $func->();
        subtest "default_fields s,b" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            is_deeply($fres->[2], [
                {s=>'a1', b=>0},
                {s=>'b1', b=>0},
                {s=>'a3', b=>1},
                {s=>'a2', b=>1},
            ], "sort result")
                or diag explain $fres->[2];
        };
    },
);

test_gen(
    name => 'default_detail',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {default_detail=>1},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};

        my $fres;
        $fres = $func->();
        subtest "default_detail 1" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            is_deeply($fres->[2], $table_data, "sort result")
                or diag explain $fres->[2];
        };
    },
);

test_gen(
    name => 'default_show_field_names',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {default_show_field_names=>0},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};

        my $fres;
        $fres = $func->(fields=>['s', 'b']);
        subtest "default_show_field_names 0" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            is_deeply($fres->[2],
                      [['a1', 0],
                       ['b1', 0],
                       ['a3', 1],
                       ['a2', 1]],
                      "sort result")
                or diag explain $fres->[2];
        };
    },
);

test_gen(
    name => 'default_filters',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {default_filters=>[['b','b','truth',0]]},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {}, 2, 'default filter: b=0');
        test_query($func, {min_i=>2}, 2,
                   'default filter + filter = default filter not added');
    },
);

test_gen(
    name => 'default_result_limit',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {default_result_limit=>2},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {}, 2, 'default result_limit');
        test_query($func, {result_limit=>3}, 3, 'explicit result_limit');
    },
);

DONE_TESTING:
done_testing();
