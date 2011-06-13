#!perl

# test the generated function

use 5.010;
use strict;
use warnings;
use FindBin '$Bin';
use lib $Bin, "$Bin/t";

use Test::More 0.96;
require "testlib.pl";

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
    },
);

DONE_TESTING:
done_testing();
