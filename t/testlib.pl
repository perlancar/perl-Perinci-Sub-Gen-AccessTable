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
        my %fargs = (
            table_data => $args{table_data},
            table_spec => $args{table_spec},
        );
        if ($args{other_args}) {
            while (my ($k, $v) = each %{$args{other_args}}) {
                $fargs{$k} = $v;
            }
        }
        eval { $res = gen_read_table_func(%fargs) };
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

sub gen_test_data {
    my $table_data = [
        {s=>'a1', s2=>'', s3=>'a' , i=>1 , f=>0.1, a=>[qw//]   , b=>0},
        {s=>'b1', s2=>'', s3=>'aa', i=>2 , f=>0.2, a=>[qw/b/]  , b=>0},
        {s=>'a3', s2=>'', s3=>'aa', i=>4 , f=>1.1, a=>[qw/a b/], b=>1},
        {s=>'a2', s2=>'', s3=>'a' , i=>-3, f=>1.2, a=>[qw/a/]  , b=>1},
    ];

    my $table_spec = {
        columns => {
            s  => ['str*'   => {column_index=>0, }],
            s2 => ['str*'   => {column_index=>1, column_filterable=>0}],
            s3 => ['str*'   => {column_index=>2, column_filterable_regex=>0}],
            i  => ['int*'   => {column_index=>3, }],
            f  => ['float*' => {column_index=>4, }],
            a  => ['array*' => {column_index=>5, column_sortable=>0, }],
            b  => ['bool*'  => {column_index=>6, }],
        },
        pk => 's',
    };

    return ($table_data, $table_spec);
}

1;
