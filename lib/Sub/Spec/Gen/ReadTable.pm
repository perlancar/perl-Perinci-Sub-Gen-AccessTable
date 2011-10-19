package Sub::Spec::Gen::ReadTable;
# ABSTRACT: Generate function (and its spec) to read table data

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use List::Util qw(shuffle);
use Data::Sah::Util;

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(gen_read_table_func);

our %SPEC;

sub _parse_schema {
    Data::Sah::Util::_parse_schema(@_);
}

sub _is_aoa {
    my $data = shift;
    ref($data) eq 'ARRAY' && (!@$data || ref($data->[0]) eq 'ARRAY');
}

sub _is_aoh {
    my $data = shift;
    ref($data) eq 'ARRAY' && (!@$data || ref($data->[0]) eq 'HASH');
}

sub _gen_spec {
    my ($table_spec, $opts) = @_;

    # XXX schema
    $table_spec->{columns} or
        return [400, "Invalid table_spec: columns not specified"];
    ref($table_spec->{columns}) eq 'HASH' or
        return [400, "Invalid table_spec: columns must be hash"];
    $table_spec->{pk} or
        return [400, "Invalid table_spec: pk not specified"];
    exists($table_spec->{columns}{ $table_spec->{pk} }) or
        return [400, "Invalid table_spec: pk not in columns"];

    # add general arguments

    my $func_spec = {
        summary => "",
        description => "",
        args => {
            show_field_names => ['bool' => {
                arg_category => 'field selection',
                summary => 'Show field names in result (as hash/assoc)',
                description => <<'_',

When off, will return an array of values without field names (array/list). When
on, will return an array of field names and values (hash/associative array).

_
                default => $opts->{default_show_field_names},
            }],
            detail => ['bool' => {
                arg_category => 'field selection',
                summary => 'Return detailed data (all fields)',
                default => $opts->{default_detail} // 0,
            }],
            fields => ['array' => {
                of => 'str*',
                arg_category => 'field selection',
                summary => 'Select fields to return',
                default => $opts->{default_fields},
            }],
            sort => ['str' => {
                arg_category => 'order',
                summary => 'Order data according to certain fields',
                description => <<'_',

A list of field names separated by comma. Each field can be prefixed with '-' to
specify descending order instead of the default ascending.

_
                default => $opts->{default_sort},
            }],
            random => ['bool' => {
                arg_category => 'order',
                summary => 'If on, return result in random order',
                default => $opts->{default_random} // 0,
            }],
            result_limit => ['int' => {
                arg_category => 'paging',
                summary => 'Only return a certain number of results',
                default => $opts->{default_result_limit},
            }],
            result_start => ['int' => {
                arg_category => 'paging',
                summary => 'Only return results from a certain position',
                default => 1,
            }],
        },
    };

    # add search argument
    if ($opts->{enable_search} // 1) {
        $func_spec->{args}{q} = ['str' => {
            arg_category => 'filter',
            summary => 'Search',
        }];
    }

    # add filter arguments for each table column

    my $col_specs = {};
    my $col2arg = {};
    for my $cname (keys %{$table_spec->{columns}}) {
        my $cspec = _parse_schema($table_spec->{columns}{$cname});
        $col_specs->{$cname} = $cspec;
        my $a = $cname;
        if (exists $func_spec->{args}{$a}) {
            $a = "${a}_field";
            if (exists $func_spec->{args}{$a}) {
                return [400, "Clash of column name with arg name: $a"];
            }
        }
        $col2arg->{$cname} = $a;
        my $cf = $cspec->{clause_sets}[0]{column_filterable};
        next if defined($cf) && !$cf;
        my $t = $cspec->{type};
        if ($t eq 'bool') {
            return [400, "Clash of $t filter argument: $a"]
                if $func_spec->{args}{$a};
            $func_spec->{args}{$a} = ['bool' => {
                summary => "Only return results having a true $a value",
                arg_category => "filter for $cname",
                default => 0,
            }];
        }
        if ($t eq 'array') {
            return [400, "Clash of $t filter argument: has_$a"]
                if $func_spec->{args}{"has_$a"};
            $func_spec->{args}{"has_$a"} = ['array' => {
                of => 'str*',
                arg_category => "filter for $cname",
                summary => "Only return results having ".
                    "specified values in $a",
            }];
            return [400, "Clash of $t filter argument: lacks_$a"]
                if $func_spec->{args}{"lacks_$a"};
            $func_spec->{args}{"lacks_$a"} = ['array' => {
                of => 'str*',
                arg_category => "filter for $cname",
                summary => "Only return results not having ".
                    "specified values in $a",
            }];
        }
        if ($t =~ /(?:int|float|str)/) {
            return [400, "Clash of $t filter argument: $a"]
                if $func_spec->{args}{$a};
            $func_spec->{args}{$a} = [$t => {
                summary => "Only return results having certain value of $a",
                arg_category => "filter for $cname",
            }];
            return [400, "Clash of $t filter argument: min_$a"]
                if $func_spec->{args}{"min_$a"};
            $func_spec->{args}{"min_$a"} = [$t => {
                summary => "Only return results having ".
                    "a certain minimum value of $a",
                arg_category => "filter for $cname",
            }];
            return [400, "Clash of $t filter argument: max_$a"]
                if $func_spec->{args}{"max_$a"};
            $func_spec->{args}{"max_$a"} = [$t => {
                summary => "Only return results having ".
                    "a certain maximum value of $a",
                arg_category => "filter for $cname",
            }];
        }
        if ($t eq 'str') {
            return [400, "Clash of $t filter argument: ${a}_contain"]
                if $func_spec->{args}{"${a}_contain"};
            $func_spec->{args}{"${a}_contain"} = [$t => {
                summary => "Only return results with $a containing ".
                    "certain text",
                arg_category => "filter for $cname",
            }];
            return [400, "Clash of $t filter argument: ${a}_not_contain"]
                if $func_spec->{args}{"${a}_not_contain"};
            $func_spec->{args}{"${a}_not_contain"} = [$t => {
                summary => "Only return results with $a not containing ".
                    "certain text",
                arg_category => "filter for $cname",
            }];
            my $cf = $cspec->{clause_sets}[0]{column_filterable_regex};
            unless (defined($cf) && !$cf) {
                return [400, "Clash of $t filter argument: ${a}_match"]
                    if $func_spec->{args}{"${a}_match"};
                $func_spec->{args}{"${a}_match"} = [$t => {
                    summary => "Only return results with $a matching ".
                        "specified regex",
                    arg_category => "filter for $cname",
                }];
                return [400, "Clash of $t filter argument: ${a}_not_match"]
                    if $func_spec->{args}{"${a}_not_match"};
                $func_spec->{args}{"${a}_not_match"} = [$t => {
                    summary => "Only return results with $a matching ".
                        "specified regex",
                    arg_category => "filter for $cname",
                }];
            }
        }
    }

    # normalize arg specs
    #while (my ($k, $v) = each %{$func_spec->{args}}) {
    #    $func_spec->{args}{$k} = _parse_schema($v);
    #}

    [200, "OK", [$func_spec, $col_specs, $col2arg]];
}

sub _parse_query {
    my ($args, $opts, $table_spec, $col_specs, $col2arg) = @_;
    my $query = {args=>$args};

    my @columns = keys %$col_specs;
    my @requested_fields;
    if ($args->{detail}) {
        @requested_fields = @columns;
        $args->{show_field_names} //= 1;
    } elsif ($args->{fields}) {
        @requested_fields = @{ $args->{fields} };
        $args->{show_field_names} //= 1;
    } else {
        @requested_fields = ($table_spec->{pk});
        $args->{show_field_names} //= 0;
    }
    for (@requested_fields) {
        return [400, "Unknown field $_"] unless $_ ~~ @columns;
    }
    $query->{requested_fields} = \@requested_fields;

    my @filter_fields;
    my @filters;
    for my $c (grep {$col_specs->{$_}{type} eq 'bool'} @columns) {
        my $a = $col2arg->{$c};
        my $exists;
        if (defined $args->{$a}) {
            $exists++;
            push @filters, [$a, $c, "truth", $args->{$a}];
        }
        push @filter_fields, $c if $exists && !($c ~~ @filter_fields);
    }
    for my $c (grep {$col_specs->{$_}{type} eq 'array'} @columns) {
        my $a = $col2arg->{$c};
        my $exists;
        if (defined $args->{"has_$a"}) {
            $exists++;
            push @filters, ["has_$a", $c, "~~", $args->{"has_$a"}];
        }
        if (defined $args->{"lacks_$a"}) {
            $exists++;
            push @filters, ["lacks_$a", $c, "!~~", $args->{"lacks_$a"}];
        }
        push @filter_fields, $c if $exists && !($c ~~ @filter_fields);
    }
    for my $c (grep {$col_specs->{$_}{type} =~ /^(int|float|str)$/}
                   @columns) {
        my $t = $col_specs->{$c}{type};
        my $exists;
        my $a = $col2arg->{$c};
        if (defined $args->{$a}) {
            $exists++;
            push @filters, [$a, $c, $t eq 'str' ? "eq" : "==", $args->{$a}];
        }
        if (defined $args->{"min_$a"}) {
            $exists++;
            push @filters, ["min_$a", $c, $t eq 'str' ? 'ge' : '>=',
                            $args->{"min_$a"}];
        }
        if (defined $args->{"max_$a"}) {
            $exists++;
            push @filters, ["max_$a", $c, $t eq 'str' ? 'le' : '<=',
                            $args->{"max_$a"}];
        }
        push @filter_fields, $c if $exists && !($c ~~ @filter_fields);
    }
    for my $c (grep {$col_specs->{$_}{type} =~ /^str$/} @columns) {
        my $a = $col2arg->{$c};
        my $exists;
        if (defined $args->{"${a}_contain"}) {
            $exists++;
            push @filters, ["${a}_contain", $c, 'pos', $args->{"${a}_contain"}];
        }
        if (defined $args->{"${a}_not_contain"}) {
            $exists++;
            push @filters, ["${a}_not_contain", $c, '!pos',
                            $args->{"${a}_not_contain"}];
        }
        if (defined $args->{"${a}_match"}) {
            $exists++;
            push @filters, ["${a}_match", $c, '=~', $args->{"${a}_match"}];
        }
        if (defined $args->{"${a}_not_match"}) {
            $exists++;
            push @filters, ["${a}_not_match", $c, '!~',
                            $args->{"${a}_not_match"}];
        }
        push @filter_fields, $c if $exists && !($c ~~ @filter_fields);
    }
    $query->{filters}       = \@filters;
    $query->{filter_fields} = \@filter_fields;

    my @searchable_fields = grep {
        !defined($col_specs->{$_}{clause_sets}[0]{column_searchable}) ||
            $col_specs->{$_}{clause_sets}[0]{column_searchable}
        } @columns;
    my $search_opts = {ci => $opts->{case_insensitive_search}};
    my $search_re;
    my $q = $args->{q};
    if (defined $q) {
        if ($opts->{word_search}) {
            $search_re = $opts->{case_insensitive_search} ?
                qr/\b$q\b/i : qr/\b$q\b/;
        } else {
            $search_re = $opts->{case_insensitive_search} ?
                qr/$q/i : qr/$q/;
        }
    }
    $query->{q} = $args->{q};
    $query->{search_opts} = $args->{search_opts};
    unless ($opts->{custom_search}) {
        $query->{search_fields} = \@searchable_fields;
        $query->{search_str_fields} = [grep {
            $col_specs->{$_}{type} =~ /^(str)$/
        } @searchable_fields];
        $query->{search_array_fields} = [grep {
            $col_specs->{$_}{type} =~ /^(array)$/
        } @searchable_fields];
        $query->{search_re} = $search_re;
    }

    my @sort_fields;
    my @sorts;
    if (defined $args->{sort}) {
        my @f = split /\s*[,;]\s*/, $args->{sort};
        for my $f (@f) {
            my $desc = $f =~ s/^-//;
            return [400, "Unknown field in sort: $f"]
                unless $f ~~ @columns;
            my $cs = $col_specs->{$f}{clause_sets}[0]{column_sortable};
            return [400, "Field $f is not sortable"]
                unless !defined($cs) || $cs;
            my $t = $col_specs->{$f}{type};
            my $op = $t =~ /^(int|float)$/ ? '<=>' : 'cmp';
            #print "t=$t, op=$op\n";
            push @sorts, [$f, $op, $desc ? -1:1];
            push @sort_fields, $f;
        }
    }
    $query->{random}      = $args->{random};
    $query->{sorts}       = \@sorts;
    $query->{sort_fields} = \@sort_fields;

    my @mentioned_fields =
        keys %{{ map {$_=>1} @requested_fields,
                     @filter_fields, @sort_fields }};
    $query->{mentioned_fields} = \@mentioned_fields;

    $query->{result_limit} = $args->{result_limit};
    $query->{result_start} = $args->{result_start} // 1;

    $log->tracef("parsed query: %s", $query);
    [200, "OK", [$query]];
}

sub _gen_func {
    my ($opts, $table_data, $func_spec, $table_spec, $col_specs, $col2arg) = @_;

    my $func = sub {
        my %args = @_;

        $args{detail}           //= $opts->{default_detail};
        $args{fields}           //= $opts->{default_fields};
        $args{show_field_names} //= $opts->{default_show_field_names};
        $args{sort}             //= $opts->{default_sort};
        $args{random}           //= $opts->{default_random};
        $args{result_limit}     //= $opts->{default_result_limit};

        # XXX schema
        if (defined $args{fields}) {
            $args{fields} = [split /\s*[,;]\s*/, $args{fields}]
                unless ref($args{fields}) eq 'ARRAY';
        }

        my $res = _parse_query(
            \%args, $opts, $table_spec, $col_specs, $col2arg);
        return $res unless $res->[0] == 200;
        my ($query) = @{$res->[2]};

        $query->{filters} = $opts->{default_filters}
            if defined($opts->{default_filters}) && !@{$query->{filters}};

        my @columns = keys %$col_specs;

        # retrieve data
        my $data;
        my $metadata = {};
        if (_is_aoa($table_data) || _is_aoh($table_data)) {
            $data = $table_data;
        } elsif (ref($table_data) eq 'CODE') {
            my $res;
            return [500, "BUG: Data function died: $@"]
                unless eval { $res = $table_data->($query) };
            return [500, "BUG: Result returned from function is not a hash".
                        ", please report to administrator"]
                unless ref($res) eq 'HASH';
            $data = $res->{data};
            return [500, "BUG: 'data' key from result is not AoA/AoH".
                        ", please report to administrator"]
                unless _is_aoa($data) || _is_aoh($data);
            for (qw/filtered sorted paged columns_selected/) {
                $metadata->{$_} = $res->{$_};
            }
        } else {
            # this should be impossible, already checked earlier
            die "BUG: Data is not an array";
        }

        # this will be the final result.
        my @rows;

        no warnings; # silence undef warnings when comparing row values

        $log->tracef("(read_table_func) Filtering ...");
        my $q = $query->{q};
        my $search_re = $query->{search_re};

      ROW:
        for my $row0 (@$data) {
            my $row_h;
            if (ref($row0) eq 'ARRAY') {
                # currently, internally we always use hashref for rows and
                # convert to array/scalar later when returning final data.
                $row_h = {};
                for my $c (keys %$col_specs) {
                    $row_h->{$c} = $row0->[
                        $col_specs->{$c}{clause_sets}[0]{column_index}];
                }
            } elsif (ref($row0) eq 'HASH') {
                $row_h = { %$row0 };
            } else {
                return [500, "BUG: Invalid row, not a hash/array"];
            }

            goto SKIP_FILTER if $metadata->{filtered};

            for my $f (@{$query->{filters}}) {
                my ($a, $c, $op, $opn) = @$f;
                if ($op eq 'truth') {
                    next ROW if $row_h->{$c} xor $opn;
                } elsif ($op eq '~~') {
                    for (@$opn) {
                        next ROW unless $_ ~~ @{$row_h->{$c}};
                    }
                } elsif ($op eq '!~~') {
                    for (@$opn) {
                        next ROW if $_ ~~ @{$row_h->{$c}};
                    }
                } elsif ($op eq 'eq') {
                    next ROW unless $row_h->{$c} eq $opn;
                } elsif ($op eq '==') {
                    next ROW unless $row_h->{$c} == $opn;
                } elsif ($op eq 'ge') {
                    next ROW unless $row_h->{$c} ge $opn;
                } elsif ($op eq '>=') {
                    next ROW unless $row_h->{$c} >= $opn;
                } elsif ($op eq 'le') {
                    next ROW unless $row_h->{$c} le $opn;
                } elsif ($op eq '<=') {
                    next ROW unless $row_h->{$c} <= $opn;
                } elsif ($op eq '=~') {
                    next ROW unless $row_h->{$c} =~ $opn;
                } elsif ($op eq '!~') {
                    next ROW unless $row_h->{$c} !~ $opn;
                } elsif ($op eq 'pos') {
                    next ROW unless index($row_h->{$c}, $opn) >= 0;
                } elsif ($op eq '!pos') {
                    next ROW if index($row_h->{$c}, $opn) >= 0;
                } else {
                    die "BUG: Unknown op $op";
                }
            }

            if (defined $q) {
                if ($opts->{custom_search}) {
                    next ROW unless $opts->{custom_search}->(
                        $row_h, $q, $query->{search_opts});
                } else {
                    my $match;
                    for my $f (@{$query->{search_str_fields}}) {
                        if ($row_h->{$f} =~ $search_re) {
                            $match++; last;
                        }
                    }
                  ARY_FIELD:
                    for my $f (@{$query->{search_array_fields}}) {
                        for my $el (@{$row_h->{$f}}) {
                            if ($el =~ $search_re) {
                                $match++; last ARY_FIELD;
                            }
                        }
                    }
                    next ROW unless $match;
                }
            }

          SKIP_FILTER:

            push @rows, $row_h;
        }

        $log->tracef("(read_table_func) Ordering ...");
        if ($metadata->{sorted}) {
            # do nothing
        } elsif ($query->{random}) {
            @rows = shuffle @rows;
        } elsif (@{$query->{sorts}}) {
            @rows = sort {
                for my $s (@{$query->{sorts}}) {
                    my ($f, $op, $desc) = @$s;
                    my $x;
                    if ($op eq 'cmp') {
                        $x = $a->{$f} cmp $b->{$f};
                    } else {
                        $x = $a->{$f} <=> $b->{$f};
                    }
                    #print "$a->{$f} $op $b->{$f} = $x (desc=$desc)\n";
                    return $x*$desc if $x != 0;

                }
                0;
            } @rows;
        }

        use warnings;

        # perform paging
        $log->tracef("(read_table_func) Paging ...");
        unless ($metadata->{paged}) {
            if ($query->{result_start} > 1) {
                splice @rows, 0, $query->{result_start}-1;
            }
            if (defined $query->{result_limit}) {
                splice @rows, $query->{result_limit};
            }
        }

        # select fields
        $log->tracef("(read_table_func) Selecting fields ...");
        my $pk = $table_spec->{pk};
        goto SKIP_SELECT_COLUMNS if $metadata->{columns_selected};
      ROW2:
        for my $row (@rows) {
            if (!$args{detail} && !$args{fields}) {
                $row = $row->{$pk};
                next ROW2;
            }
            if ($args{show_field_names}) {
                for (@columns) {
                    delete $row->{$_}
                        unless $_ ~~ @{$query->{requested_fields}};
                }
            } else {
                $row = [map {$row->{$_}} @{$query->{requested_fields}}];
            }
        }
      SKIP_SELECT_COLUMNS:

        # return data
        [200, "OK", \@rows];
    };

    [200, "OK", [$func]];
}

$SPEC{gen_read_table_func} = {
    summary => 'Generate function (and its spec) to read table data',
    description_fmt => 'org',
    description => <<'_',

The generated function acts like a simple single table SQL SELECT query,
featuring filtering, sorting, and paging, but using arguments as the 'query
language'. The generated function is suitable for exposing a table data from an
API function.

The generated spec is pretty barebones currently. You can decorate with summary
and description afterwards.

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

A filtering option. By default, all fields except those specified with
column_searchable=0 will be searched using simple case-insensitive string
search. There are a few options to customize this, using these gen arguments:
*word_search*, *case_insensitive_search*, and *custom_search*.

** Filter arguments

They will be generated for each column, except when column has
'column_filterable' clause set to false.

Undef values will not match any filter, just like NULL in SQL.

If a field name clashes with a general argument name (e.g. *q* or *sort*) then
it will be suffixed with '_field' (e.g. *q_field* or *sort_field*).

*** "FIELD" boolean argument for each boolean field

*** "has_FIELD" and "lacks_FIELD" array arguments for each set field

*** "min_FIELD"/"max_FIELD"/"FIELD" arguments for each int/float/str

*** "FIELD_contain" string argument for each str field

*** "FIELD_match" and "FIELD_not_match" regex argument for each str field

Will not be generated if column_filterable_regex clause in column specification
is set to 0.

*** "FIELD_starts_with" string argument for each str field (not implemented)

*** "FIELD_ends_with" string argument for each str field (not implemented)

_
    args => {
        table_data => ['any*' => {
            summary => 'Data',
            description_fmt => 'org',
            description => <<'_',

Table data is either an AoH or AoA. Or you can also pass a Perl subroutine (see
below).

Passing a subroutine lets you fetch data dynamically. The subroutine will be
called with these arguments ($query) and is expected to return a hashref like
this {data => DATA, paged=>BOOL, filtered=>BOOL, sorted=>BOOL,
columns_selected=>BOOL}. DATA is AoA or AoH. If paged is set to 1, data is
assumed to be already paged and won't be paged again; likewise for filtered and
sorted.

$query is a hashref which contains information about the query, e.g. 'args'
(the original arguments passed to the generated function, e.g. {random=>1,
result_limit=>1, field1_match=>'foo'}), 'mentioned_fields' which lists fields
that are mentioned in either filtering arguments or fields or ordering,
'requested_fields' (fields mentioned in list of fields to be returned),
'sort_fields' (fields mentioned in sort arguments), 'filter_fields' (fields
mentioned in filter arguments), etc.

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
data) and these optional clauses: column_sortable (a boolean stating whether
column can be sorted, default is true), column_filterable (a boolean stating
whether column can be mentioned in filter options).

_
        }],
        default_detail => ['bool' => {
            summary => "Supply default 'detail' value for function spec",
        }],
        default_fields => ['str' => {
            summary => "Supply default 'fields' value for function spec",
        }],
        # not yet documented
        #default_filters => ['array' => {
        #    of => 'array*', # XXX filter structure
        #    summary => "Supply default filters",
        #}],
        default_show_field_names => ['bool' => {
            summary => "Supply default 'show_field_names' ".
                "value for function spec",
        }],
        default_sort => ['str' => {
            summary => "Supply default 'sort' value for function spec",
        }],
        default_random => ['bool' => {
            summary => "Supply default 'random' value for function spec",
        }],
        default_result_limit => ['int' => {
            summary => "Supply default 'result_limit' value for function spec",
        }],
        enable_search => ['bool' => {
            summary => "Generated function will support searching (argument q)",
            default => 1,
        }],
        word_search => ['bool' => {
            summary => "Generated function will perform word searching ".
                "instead of string searching",
            description => <<'_',

For example, if search term is 'pine' and column value is 'green pineapple',
search will match if word_search=false, but won't match under word_search.

_
            default => 0,
        }],
        case_insensitive_search => ['bool' => {
            summary => 'Generated function will perform case-insensitive search',
            default => 1,
        }],
        custom_search => ['code' => {
            summary => 'Supply custom searching for generated function',
            description => <<'_',

Code will be supplied ($row, $q, $opts) where $q is the search term (the
argument q) and $row the hashref row value. $opts is {ci=>0|1}. Code should
return true if row matches search term.

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

    my $opts = {
        default_detail           => $args{default_detail},
        default_show_field_names => $args{default_show_field_names},
        default_fields           => $args{default_fields},
        default_sort             => $args{default_sort},
        default_random           => $args{default_random},
        default_result_limit     => $args{default_result_limit},
        default_filters          => $args{default_filters},
        enable_search            => $args{enable_search} // 1,
        custom_search            => $args{custom_search},
        word_search              => $args{word_search},
        case_insensitive_search  => $args{case_insensitive_search} // 1,
    };

    my $res;
    $res = _gen_spec($table_spec, $opts);
    return [$res->[0], "Can't generate spec: $res->[1]"]
        unless $res->[0] == 200;
    my ($func_spec, $col_specs, $col2arg) = @{$res->[2]};

    $res = _gen_func(
        $opts, $table_data, $func_spec, $table_spec, $col_specs, $col2arg);
    return [$res->[0], "Can't generate func: $res->[1]"]
        unless $res->[0] == 200;
    my ($func) = @{$res->[2]};

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
             id => ['str*' => {
                 summary => 'ISO 2-letter code for the country',
                 column_index => 0,
                 column_sortable => 1,
             }],
             en_name => ['str*' => {
                 summary => 'English name',
                 column_index => 1,
                 column_sortable => 1,
             }],
             id_name => ['str*' => {
                 summary => 'Indonesian name',
                 column_index => 2,
                 column_sortable => 1,
             }],
             tags => ['array*' => {
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
hashrefs, an array of arrays, or external data like a SQL table) as an API
function. This module will generate a function that accepts arguments for
specifying fields, filtering, sorting, and paging; along with its L<Sub::Spec>
spec. The resulting function can then be run via command-line using
L<Sub::Spec::CmdLine> (as demonstrated in Synopsis), or served via REST using
L<Sub::Spec::HTTP::Server>, or consumed normally by Perl programs.

This module uses L<Log::Any> for logging framework.

This module's functions has L<Sub::Spec> specs.


=head1 FUNCTIONS

None are exported by default, but they are exportable.


=head1 FAQ


=head1 SEE ALSO

L<Sub::Spec>

L<Sub::Spec::CmdLine>

=cut
