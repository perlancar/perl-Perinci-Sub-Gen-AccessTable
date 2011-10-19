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

        test_query($func, {has_a=>[qw/t1/]}, 2, 'array filter: has_F t1');
        test_query($func, {lacks_a=>[qw/t2/]}, 2, 'array filter: lacks_F t2');
        test_query($func, {has_a=>[qw/t1 t2/]}, 1, 'array filter: has_F t1 t2');
        test_query($func, {lacks_a=>[qw/t1 t2/]}, 1, 'ary f: lacks_F t1 t2');

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

test_gen(
    name => 'function table_data',
    table_data => sub { {data=>$table_data} },
    table_spec => $table_spec,
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {b=>1}, 2, 'bool filter: F=1');
    },
);

test_gen(
    name => 'function table_data (filtered=>1)',
    table_data => sub { {data=>$table_data, filtered=>1} },
    table_spec => $table_spec,
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {b=>1}, 4, 'bool filter: F=1');
    },
);

# XXX test sorted=>1
# XXX test columns_selected=>1
# XXX test paged=>1 (though this is also exercised in examples/num-and-words

test_gen(
    name => 'search',
    table_data => $table_data,
    table_spec => $table_spec,
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {q=>"b"}, 1, 'search b');
        test_query($func, {q=>"B"}, 1, 'search B');
    },
);

test_gen(
    name => 'search on array fields',
    table_data => [
        {id=>1, a=>[qw/pine apple/]},
        {id=>2, a=>[qw/pineapple/]},
        {id=>3, a=>[qw//]},
    ],
    table_spec => {
        columns => {
            id => ['int*' => {
                column_index => 0,
            }],
            a => ['array*' => {
                column_index => 1,
            }],
        },
        pk => 'id',
    },
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {q=>'Apple'}, 2, 'search Apple');
        test_query($func, {q=>'pineapple'}, 1, 'search pineapple');
    },
);

test_gen(
    name => 'column_searchable=0',
    table_data => [
        {id=>1, s=>'a', s2=>'d'},
        {id=>2, s=>'b', s2=>'e'},
        {id=>3, s=>'c', s2=>'f'},
    ],
    table_spec => {
        columns => {
            id => ['int*' => {
                column_index => 0,
            }],
            s => ['str*' => {
                column_index => 1,
                column_searchable => 0,
            }],
            s2 => ['str*' => {
                column_index => 2,
            }],
        },
        pk => 'id',
    },
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {q=>'a'}, 0, "doesn't search non-searchable column");
        test_query($func, {q=>'e'}, 1, "search searchable column");
    },
);

test_gen(
    name => 'case sensitive search',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {case_insensitive_search=>0},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {q=>"b"}, 1, 'search b');
        test_query($func, {q=>"B"}, 0, 'search B');
    },
);

test_gen(
    name => 'word search',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {word_search=>1},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {q=>"b"}, 0, 'word search b (1)');
        test_query($func, {q=>"aa"}, 2, 'word search aa (2)');
    },
);

test_gen(
    name => 'custom search',
    table_data => $table_data,
    table_spec => $table_spec,
    other_args => {custom_search=>sub {
                       my ($row, $q, $opts) = shift;
                       $row->{i} % 2;
                   }},
    status => 200,
    post_test => sub {
        my ($res) = @_;
        my $func = $res->[2]{code};

        test_query($func, {q=>"whatever"}, 2, 'search whatever');
    },
);

DONE_TESTING:
done_testing();
