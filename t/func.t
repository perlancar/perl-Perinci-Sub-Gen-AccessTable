#!perl

# test the generated function

use 5.010;
use strict;
use warnings;
use FindBin '$Bin';
use lib $Bin, "$Bin/t";

use Test::More 0.96;
require "testlib.pl";

my ($table_data, $table_spec) = gen_test_data();

test_gen(
    name => 'ordering, detail',
    table_data => $table_data,
    table_spec => $table_spec,
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};

        my $fres;

        $fres = $func->(sort=>"x");
        is($fres->[0], 400, "sort on unknown sort fields -> fail");

        $fres = $func->(sort=>"-a");
        is($fres->[0], 400, "sort on unsortable fields -> fail");

        $fres = $func->(sort=>"s", detail=>1);
        subtest "ascending sort" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            my @r = map {$_->{s}} @{$fres->[2]};
            is_deeply(\@r, [qw/a1 a2 a3 b1/], "sort result")
                or diag explain \@r;
        };

        $fres = $func->(sort=>"-s", detail=>1);
        subtest "descending sort" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            my @r = map {$_->{s}} @{$fres->[2]};
            is_deeply(\@r, [qw/b1 a3 a2 a1/], "sort result")
                or diag explain \@r;
        };

        $fres = $func->(sort=>"b, -s", detail=>1);
        subtest "multiple fields sort" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            my @r = map {$_->{s}} @{$fres->[2]};
            is_deeply(\@r, [qw/b1 a1 a3 a2/], "sort result")
                or diag explain \@r;
        };
    },
);

test_gen(
    name => 'random ordering',
    table_data => $table_data,
    table_spec => $table_spec,
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};

        test_random_order($func, {random=>1}, 50, [qw/a1 a2 a3 b1/],
                          "sort result");
    },
);

test_gen(
    name => 'fields, show_field_names',
    table_data => $table_data,
    table_spec => $table_spec,
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};
        my $spec = $res->[2]{spec};
        my $args = $spec->{args};

        my $fres;

        $fres = $func->(fields=>["x"]);
        is($fres->[0], 400, "mention unknown field in fields -> fail");

        $fres = $func->(fields=>"s");
        subtest "single field" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            is_deeply($fres->[2],
                      [{s=>'a1'},
                       {s=>'b1'},
                       {s=>'a3'},
                       {s=>'a2'}],
                      "result")
                or diag explain $fres->[2];
        };

        $fres = $func->(fields=>"s, b");
        subtest "multiple fields" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            is_deeply($fres->[2],
                      [{s=>'a1', b=>0},
                       {s=>'b1', b=>0},
                       {s=>'a3', b=>1},
                       {s=>'a2', b=>1}],
                      "result")
                or diag explain $fres->[2];
        };

        $fres = $func->(fields=>"b, s, b", show_field_names=>0);
        subtest "multiple duplicate fields" => sub {
            is($fres->[0], 200, "status")
                or diag explain $fres;
            is_deeply($fres->[2],
                      [[0, 'a1', 0],
                       [0, 'b1', 0],
                       [1, 'a3', 1],
                       [1, 'a2', 1]],
                      "result")
                or diag explain $fres->[2];
        };

    },
);

($table_data, $table_spec) = gen_test_data(1);

test_gen(
    name => 'filtering, aoa data',
    table_data => $table_data,
    table_spec => $table_spec,
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {b=>1}, 2, 'bool filter: F=1');
        test_query($func, {b=>0}, 2, 'bool filter: F=0');

        test_query($func, {i=>4}, 1, 'int filter: F');
        test_query($func, {min_i=>4}, 1, 'int filter: min_F');
        test_query($func, {max_i=>2}, 3, 'int filter: max_F');

        test_query($func, {f=>0.2}, 1, 'float filter: F');
        test_query($func, {min_f=>0.2}, 3, 'float filter: min_F');
        test_query($func, {max_f=>0.2}, 2, 'float filter: max_F');

        test_query($func, {s=>'a1'}, 1, 'str filter: F');
        test_query($func, {min_s=>'a2'}, 3, 'str filter: min_F');
        test_query($func, {max_s=>'a2'}, 2, 'str filter: max_F');
        test_query($func, {s_contain=>'a'}, 3, 'str filter: F_contain');
        test_query($func, {s_not_contain=>'a'}, 1, 'str filter: F_not_contain');
        test_query($func, {s_match=>'[12]'}, 3, 'str filter: F_match');
        test_query($func, {s_not_match=>'[12]'}, 1, 'str filter: F_not_match');

        test_query($func, {b=>0, min_i=>2}, 1, 'multiple filters');

    },
);

test_gen(
    name => 'paging',
    table_data => $table_data,
    table_spec => $table_spec,
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query(
            $func, {sort=>"s", result_limit=>2},
            sub {
                my ($rows) = @_;
                is(scalar(@$rows), 2, "num of results = 2");
                is($rows->[0], "a1", "row #1");
                is($rows->[1], "a2", "row #2");
            },
            'result_limit after ordering');
        test_query(
            $func, {sort=>"s", result_start=>3, result_limit=>2},
            sub {
                my ($rows) = @_;
                is(scalar(@$rows), 2, "num of results = 2");
                is($rows->[0], "a3", "row #1");
                is($rows->[1], "b1", "row #2");
            },
            'result_start + result_limit');
    },
);

DONE_TESTING:
done_testing();
