package Sub::Spec::Gen::ReadTable;
# ABSTRACT: Generate function (and its spec) to read table data

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Sub::Spec::Utils; # temp, for _parse_schema

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

 # only list countries which are tagged as 'tropical', sort by id_name column in
 # descending order, show all columns (--detail)
 $ list_countries.pl --detail --sort -id_name --has-tags '[tropical]'
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


=head1 FAQ


=head1 SEE ALSO

L<Sub::Spec>

L<Sub::Spec::CmdLine>

=cut
