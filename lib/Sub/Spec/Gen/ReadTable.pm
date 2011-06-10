package Sub::Spec::Gen::ReadTable;
# ABSTRACT: Generate function (and its spec) to read table data

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Sub::Spec::Utils; # temp, for _parse_schema

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(gen_read_table_func);

our %SPEC;

sub _parse_schema {
    Sub::Spec::Utils::_parse_schema(@_);
}

sub _is_aoa {
    my $data = shift;
    ref($data) eq 'ARRAY' && (!@$data || ref($data->[0]) eq 'ARRAY');
}

sub _is_aoh {
    my $data = shift;
    ref($data) eq 'ARRAY' && (!@$data || ref($data->[0]) eq 'HASH');
}

$SPEC{gen_read_table_func} = {
    summary => 'Generate function (and its spec) to read table data',
    description => <<'_',

The generated function acts like a simple single table SQL SELECT query,
featuring filtering, sorting, and paging, but using arguments as the 'query
language'. The generated function is suitable for exposing a table data from an
API function.

The spec is pretty barebones currently. You can decorate with summary and
description afterwards.

_
    args => {
        table_data => ['any*' => {
            summary => 'Data',
            description => <<'_',

Table data is either an AoH or AoA. Or you can also pass a Perl subroutine (see
below).

Passing a subroutine lets you fetch data dynamically. The subroutine will be
called with these arguments ($query, $hints) and is expected to return an AoA or
AoH. $query is a hashref and is the arguments passed to the generated function
(e.g. {random=>1, result_limit=>1, field1_match=>'foo'}). $hints is a hashref
and provides some, well, hints about the query, e.g. 'mentioned_fields' which
lists fields that are mentioned in either filtering arguments or fields or
ordering, 'requested_fields' (fields mentioned in list of fields to be
returned), 'sort_fields' (fields mentioned in sort arguments), 'filter_fields'
(fields mentioned in filter arguments).

The subroutine can do filtering/ordering/paging beforehand for efficiency, e.g.
SELECT-ing from a DBI table using the appropriate columns, ORDER, WHERE, and
LIMIT clauses. Either way, for consistency, the generated function will still
apply filtering/ordering/paging to the data returned by this subroutine, so the
subroutine can choose to pass the complete table data anyway.

_
        }],
        table_spec => ['hash*' => {
            summary => 'Table specification',
            description_fmt => 'org',
            description => <<'_',

A hashref with these required keys: columns, pk. Columns is a hashref of column
specification with column name as keys, while table_pk specifies which column is
to be designated as the primary key. Currently only single-column PK is allowed.

* Column specification

A Sah schema with these required clauses: column_index (an integer starting from
0 that specifies position of column in the data, especially required with AoA
data), column_sortable (a boolean stating whether column can be sorted).

* Resulting function

The resulting function will accept these arguments. The naming of arguments are
designed to be less Perl-/database-centric.

** *show_field_names* => BOOL (default 1)

By default function will return AoH. If this argument is set to 0, then function
will return AoA instead.

** *detail* => BOOL (default 0)

This is a field selection option. By default, function will return PK column
only. If this argument is set to true, then all columns will be returned.

** *fields* => ARRAY

This is a field selection option. If you only want certain fields, specify them
here.

** *result_limit* => INT (default undef)
** *result_start* => INT (default 1)

The result_limit and result_start arguments are paging options, they work like
LIMIT clause in SQL, except that index starts at 1 and not 0.

** *random* => BOOL (default 0)

The random argument is an ordering option. If set to true, order of rows
returned will be shuffled first. This happened before paging.

** *sort* => STR

The sort argument is an ordering option, containing name of field. A - prefix
signifies descending instead of ascending order. Multiple fields are allowed,
separated by comma.

** *q* => STR

A filtering option. By default, all fields will be searched using simple
case-insensitive string search. In the future, a method to customize searching
will be allowed.

** Filter arguments

They will be generated for each column, except when column has
'column_filterable' clause set to false.

Undef values will not match any filter, just like NULL in SQL.

If a field name clashes with a general argument name (e.g. *q* or *sort*) then
it will be suffixed with '_field' (e.g. *q_field* or *sort_field*).

*** "FIELD" boolean argument for each boolean field

*** "has_FIELD" and "lacks_FIELD" array arguments for each set field

*** "min_FIELD"/"max_FIELD" int/float/str arguments for each int/float/str

*** "FIELD" string argument for each str field

*** "FIELD_contains" string argument for each str field (not implemented)

*** "FIELD_match" and "FIELD_not_match" regex argument for each str field

*** "FIELD_starts_with" string argument for each str field (not implemented)

*** "FIELD_ends_with" string argument for each str field (not implemented)

_
        }],
    },
};
sub gen_read_table_func {
    my %args = @_;

    # XXX schema
    my $table_data = $args{table_data}
        or return [400, "Please specify table_data"];
    _is_aoa($table_data) or _is_aoh($table_data) or ref($table_data) eq 'CODE'
        or return [400, "Invalid table_data: must be AoA or AoH or function"];
    my $table_spec = $args{table_spec}
        or return [400, "Please specify table_spec"];
    ref($table_spec) eq 'HASH'
        or return [400, "Invalid table_spec: must be a hash"];

    my $func_spec = {
        summary => "",
        description => "",
        args => {
            show_field_names => ['bool' => {
                arg_category => 'field selection',
                summary => 'Show field names in result (as hash/assoc)',
                description => <<'_',

When off, will return an array of values without field names (array/list).

Default is off, will be turned on by default when 'fields' or 'detail' options
are specified.

_
            }],
            detail => ['bool' => {
                arg_category => 'field selection',
                summary => 'Return detailed data (all fields)',
                default => 0,
            }],
            fields => ['array' => {
                of => 'str*',
                arg_category => 'field selection',
                summary => 'Select fields to return',
                description => <<'_',

When off, will return an array of values without field names (array/list).

Default is off, will be turned on by default when 'fields' or 'detail' options
are specified.

_
            }],
            sort => ['str' => {
                arg_category => 'order',
                summary => 'Order data according to certain fields',
                description => <<'_',

A list of field names separated by comma. Each field can be prefixed with '-' to
specify descending order instead of the default ascending.

_
            }],
            random => ['bool' => {
                arg_category => 'order',
                summary => 'If on, return result in random order',
                default => 0,
            }],
            result_limit => ['int' => {
                arg_category => 'paging',
                summary => 'Only return a certain number of results',
            }],
            result_start => ['int' => {
                arg_category => 'paging',
                summary => 'Only return results from a certain position',
                default => 1,
            }],
            q => ['str' => {
                arg_category => 'filter',
                summary => 'Filter using string matching',
            }],
        },
    };

    my %c2a; # foo -> foo, q -> q_field (clashes with arg name)
    for my $cname (keys %{$table_spec->{columns}}) {
        my $cspec = _parse_schema($table_spec->{columns}{$cname});
        my $a = $cname;
        if (exists $func_spec->{args}{$a}) {
            $a = "${a}_field";
            if (exists $func_spec->{args}{$a}) {
                return [400, "Clash of column name with arg name: $a"];
            }
        }
        $c2a{$cname} = $a;
        my $t = $cspec->{type};
        if ($t eq 'bool') {
            return [400, "Clash of $t filter argument: $a"]
                if $func_spec->{args}{$a};
            $func_spec->{args}{$a} = ['bool' => {
                summary => "Only return results having a true $a value",
                arg_category => 'filter',
                default => 0,
            }];
        }
        if ($t eq 'set') {
            return [400, "Clash of $t filter argument: has_$a"]
                if $func_spec->{args}{"has_$a"};
            $func_spec->{args}{"has_$a"} = ['array' => {
                of => 'str*',
                arg_category => 'filter',
                summary => "Only return results having ".
                    "specified values in $a",
            }];
            return [400, "Clash of $t filter argument: lacks_$a"]
                if $func_spec->{args}{"lacks_$a"};
            $func_spec->{args}{"lacks_$a"} = ['array' => {
                of => 'str*',
                arg_category => 'filter',
                summary => "Only return results not having ".
                    "specified values in $a",
            }];
        }
        if ($t =~ /(?:int|float|str)/) {
            return [400, "Clash of $t filter argument: min_$a"]
                if $func_spec->{args}{"min_$a"};
            $func_spec->{args}{"min_$a"} = [$t => {
                summary => "Only return results having ".
                    "a certain minimum value of $a",
                arg_category => 'filter',
            }];
            return [400, "Clash of $t filter argument: max_$a"]
                if $func_spec->{args}{"max_$a"};
            $func_spec->{args}{"max_$a"} = [$t => {
                summary => "Only return results having ".
                    "a certain maximum value of $a",
                arg_category => 'filter',
            }];
        }
        if ($t eq 'str') {
            return [400, "Clash of $t filter argument: $a"]
                if $func_spec->{args}{$a};
            $func_spec->{args}{$a} = [$t => {
                summary => "Only return results having certain value of $a",
                arg_category => 'filter',
            }];
            return [400, "Clash of $t filter argument: ${a}_match"]
                if $func_spec->{args}{"${a}_match"};
            $func_spec->{args}{"${a}_match"} = [$t => {
                summary => "Only return results with $a matching ".
                    "specified regex",
                arg_category => 'filter',
            }];
            return [400, "Clash of $t filter argument: ${a}_not_match"]
                if $func_spec->{args}{"${a}_not_match"};
            $func_spec->{args}{"${a}_not_match"} = [$t => {
                summary => "Only return results with $a matching ".
                    "specified regex",
                arg_category => 'filter',
            }];
        }
    }

    # normalize args

    my $func = sub {
        my $hints = {};
    };

    [200, "OK", {spec=>$func_spec, code=>$func}];
}

1;
__END__

=head1 SYNOPSIS

In list_countries.pl:

 #!perl
 use strict;
 use warnings;
 use Sub::Spec::CmdLine        qw(run);
 use Sub::Spec::Gen::ReadTable qw(gen_read_table_func);

 our %SPEC;

 my $countries = [
     ['cn', 'China', 'Cina', [qw/panda/]],
     ['id', 'Indonesia', 'Indonesia', [qw/bali tropical/]],
     ['sg', 'Singapore', 'Singapura', [qw/tropical/]],
     ['us', 'United States of America', 'Amerika Serikat', [qw//]],
 ];

 my $res = gen_read_table_func(
     table_data => $countries,
     table_spec => {
         summary => 'List of countries',
         columns => {
             id => ['int*' => {
                 summary => 'ISO 2-letter code for the country',
                 column_index => 0,
                 column_sortable => 1,
             }],
             en_name => ['str*' => {
                 summary => 'English name',
                 column_index => 1,
                 column_sortable => 1,
             }],
             id_name => ['int*' => {
                 summary => 'Indonesian name',
                 column_index => 2,
                 column_sortable => 1,
             }],
             tags => ['set*' => {
                 summary => 'Keywords/tags',
                 column_index => 3,
                 column_sortable => 0,
             }],
         },
         pk => 'id',
     },
 );
 die "Can't generate function: $res->[0] - $res->[1]" unless $res->[0] == 200;
 *list_countries       = $res->[2]{code};
 $SPEC{list_countries} = $res->[2]{spec};

 run(load=>0, module=>'main', sub=>'list_countries');

Now you can do:

 # list all countries, by default only PK column is shown
 $ list_countries.pl --nopretty
 cn
 id
 sg
 us

 # show as json, randomize order
 $ list_countries.pl --json --random
 ["id","us","sg","cn"]

 # only list countries which are tagged as 'tropical', sort by id_name column in
 # descending order, show all columns (--detail)
 $ list_countries.pl --detail --sort -id_name --tags-has '[tropical]'
 .---------------------------------------------.
 | en_name   | id | id_name   | tags           |
 +-----------+----+-----------+----------------+
 | Singapore | sg | Singapura | tropical       |
 | Indonesia | id | Indonesia | bali, tropical |
 '-----------+----+-----------+----------------'

 # show only certain fields, limit number of rows, return in YAML format
 $ list_countries.pl --fields '[id, en_name]' --result-limit 2 --yaml
 ---
 - id: cn
   en_name: China
 - id: id
   en_name: Indonesia


=head1 DESCRIPTION

This module is useful when you want to expose a table data (an array of
hashrefs, an array of arrays, an actual SQL table) as an API function. This
module will generate a function that accepts arguments for specifying fields,
filtering, sorting, and paging; along with its L<Sub::Spec> spec. The resulting
function can then be run via command-line using L<Sub::Spec::CmdLine> (as
demonstrated in Synopsis), or served via REST using L<Sub::Spec::HTTP::Server>,
or consumed normally by Perl programs.

Currently only Perl data (AoH, AoA, subref) are supported.

This module uses L<Log::Any> for logging framework.

This module's functions has L<Sub::Spec> specs.


=head1 FUNCTIONS

None are exported by default, but they are exportable.


=head1 FAQ


=head1 SEE ALSO

L<Sub::Spec>

L<Sub::Spec::CmdLine>

=cut
