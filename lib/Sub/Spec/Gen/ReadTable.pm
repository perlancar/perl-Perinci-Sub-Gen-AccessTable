package Sub::Spec::Gen::ReadTable;
# ABSTRACT: Generate function (and its spec) to read table data

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Sub::Spec::Utils; # temp, for _parse_schema

our %SPEC;

$SPEC{gen_read_table_func} = {
    summary => 'Generate function (and its spec) to read table data',
    description_fmt => 'org',
    description => <<'_',

* Data

Data is either an AoH or AoA. Or you can also pass a Perl subroutine, in which
case it will be called when data is needed and is expected to return an AoH or
AoA.

In the future, a DBI handle can also be passed.

* Table specification

A hashref with these required keys: columns, pk. Columns is a hashref of column
specification with column name as keys, while pk specifies which column is to be
designated as the primary key. Currently only single-column PK is allowed.

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

** filter arguments

They will be generated for each column, except when column has
'column_filterable' clause set to false.

Undef values will not match any filter, just like NULL in SQL.

If a field name clashes with a general argument name (e.g. *q* or *sort*) then
it will be suffixed with '_field' (e.g. *q_field* or *sort_field*).

*** "FIELD" boolean argument for each boolean field

*** "has_FIELD" and "lacks_FIELD" array arguments for each set field

*** "min_FIELD"/"max_FIELD" int/float/str arguments for each int/float/str

*** "FIELD" string argument for each str field

*** "FIELD_contains" string argument for each str field

*** "FIELD_match" and "FIELD_not_match" regex argument for each str field

*** "FIELD_starts_with" string argument for each str field (not implemented)

*** "FIELD_ends_with" string argument for each str field (not implemented)

_
    args => {
        data => ['any*' => {
            summary => 'Data',
        }],
        table_spec => ['hash*' => {
            summary => 'Table specification',
            description => <<'_',

See description for more details on table spec.

_
        }],
    },
};
sub gen_read_table_func {
    my %args = @_;
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
     data => $countries,
     table_spec => {
         summary => 'List of countries',
         table_columns => {
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


=head1 FAQ


=head1 SEE ALSO

L<Sub::Spec>

L<Sub::Spec::CmdLine>

=cut
