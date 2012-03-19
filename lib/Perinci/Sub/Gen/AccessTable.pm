package Perinci::Sub::Gen::AcccessTable;

use 5.010;
use Log::Any '$log';
use Moo; # we go OO just for the I18N, we don't store attributes, etc

use Data::Sah;
use List::Util qw(shuffle);

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(gen_read_table_func);

with 'SHARYANTO::Role::I18NMany';

# VERSION

our %SPEC;

sub __parse_schema {
    Data::Sah::normalize_schema($_[0]);
}

sub __is_aoa {
    my $data = shift;
    ref($data) eq 'ARRAY' && (!@$data || ref($data->[0]) eq 'ARRAY');
}

sub __is_aoh {
    my $data = shift;
    ref($data) eq 'ARRAY' && (!@$data || ref($data->[0]) eq 'HASH');
}

sub _add_arg {
    my ($self, $func_meta, $arg_name, $langs, $arg_spec) = @_;

    die "BUG: Duplicate arg $arg_name" if $func_meta->{args}{$arg_name};

    for my $prop (qw/summary description/) {
        if ($arg_spec->{$prop}) {
            for my $lang (@$langs) {
                next if $lang eq 'en_US';
                $arg_spec->{"$prop.alt.lang.$lang"} = $self->locl($
            }
        }
    }
                 # translate summary
    # tanslate description
    # create tag cat:LANG:xxx for each cat:xxx

    $func_meta->{args}{$arg_name} = $arg_spec;

}

sub _gen_meta {
    my ($self, $table_spec, $opts) = @_;
    my $langs = $opts->{langs};

    # add general arguments

    my $func_meta = {
        v => 1.1,
        summary => "REPLACE ME",
        description => "REPLACE ME",
        args => {},
    };
    my $fargs = $func_meta->{args};

    $self->_add_arg(
        $func_meta, 'show_field_names', $langs, {
            schema => ['bool' => {
                default => $opts->{default_show_field_names},
            }],
            tags => ['cat:field selection'],
            summary => 'Show field names in result (as hash/associative array)',
            description => <<'_',

When off, will return an array of values without field names (array/list). When
on, will return an array of field names and values (hash/associative array).

_
        });
    $fargs->{detail} = $self->_localize_arg_specs(
        $langs, {

            detail => {
                schema => ['bool' => {
                    default => $opts->{default_detail} // 0,
                }],
                tags => ['field selection'],
                summary => 'Return detailed data (all fields)',
            },
            fields => {
                schema => ['array' => {
                    of => 'str*',
                    default => $opts->{default_fields},
                }],
                tags => ['field selection'],
                summary => 'Select fields to return',
            },
            sort => {
                schema => ['str' => {
                    default => $opts->{default_sort},
                }],
                tags => ['order'],
                summary => 'Order data according to certain fields',
                description => <<'_',

A list of field names separated by comma. Each field can be prefixed with '-' to
specify descending order instead of the default ascending.

_
            },
            random => {
                schema => ['bool' => {
                    default => $opts->{default_random} // 0,
                }],
                tags => ['order'],
                summary => 'If on, return result in random order',
            },
            result_limit => {
                schema => ['int' => {
                    default => $opts->{default_result_limit},
                }],
                tags => ['paging'],
                summary => 'Only return a certain number of results',
            },
            result_start => {
                schema => ['int' => {
                    default => 1,
                }],
                tags => ['paging'],
                summary => 'Only return results from a certain position',
            },
        },
    };
    my $fargs = $func_meta->{args};

    # add search argument
    if ($opts->{enable_search} // 1) {
        $fargs->{q} = {
            schema => ['str' => {
            }],
            tags => ['filter'],
            summary => 'Search',
        };
    }

    # add filter arguments for each table column

    for my $cname (keys %{$table_spec->{columns}}) {
        my $cspec   = $table_spec->{columns}{$cname};
        my $cschema = _parse_schema($cspec->{schema});
        my $ctype   = $cschema->[0];

        next if defined($cspec->{filterable}) && !$cspec->{filterable};

        $fargs->{"$cname.is"} = {
            schema => ['$ctype*' => {
            }],
            tags => ["filter", "filter for $cname"],
            summary => "Only return results having ".
                "specified values in '$cname' field",
        };
        unless ($fargs->{$cname}) {
            $fargs->{$cname} = $fargs->{"$cname.is"};
        }

        if ($ctype eq 'bool') {
            return [400, "Clash of $ctype filter argument: $a"]
                if $fargs->{$a};
            $fargs->{$a} = ['bool' => {
                summary => "Only return results having a true $a value",
                arg_category => "filter for $cname",
                default => 0,
            }];
        }



        if ($ctype eq 'array') {
            $fargs->{"$a.has"} = {
                schema => ['array' => {
                    of => 'str*',
                }],
                tags => ["filter for $cname"],
                summary => "Only return results having ".
                    "specified values in '$a' field",
            };
            $fargs->{"$a.lacks"} = {
                schema => ['array' => {
                    of => 'str*',
                }],
                tags => ["filter for $cname"],
                summary => "Only return results not having ".
                    "specified values in '$a' field",
            };
        }
        if ($ctype =~ /(?:int|float|str)/) {
            return [400, "Clash of $ctype filter argument: $a"]
                if $fargs->{$a};
            $fargs->{$a} = [$ctype => {
                summary => "Only return results having certain value of $a",
                arg_category => "filter for $cname",
            }];
            return [400, "Clash of $ctype filter argument: min_$a"]
                if $fargs->{"min_$a"};
            $fargs->{"min_$a"} = [$ctype => {
                summary => "Only return results having ".
                    "a certain minimum value of $a",
                arg_category => "filter for $cname",
            }];
            return [400, "Clash of $ctype filter argument: max_$a"]
                if $fargs->{"max_$a"};
            $fargs->{"max_$a"} = [$ctype => {
                summary => "Only return results having ".
                    "a certain maximum value of $a",
                arg_category => "filter for $cname",
            }];
        }
        if ($ctype eq 'str') {
            return [400, "Clash of $ctype filter argument: ${a}_contain"]
                if $fargs->{"${a}_contain"};
            $fargs->{"${a}_contain"} = [$ctype => {
                summary => "Only return results with $a containing ".
                    "certain text",
                arg_category => "filter for $cname",
            }];
            return [400, "Clash of $ctype filter argument: ${a}_not_contain"]
                if $fargs->{"${a}_not_contain"};
            $fargs->{"${a}_not_contain"} = [$ctype => {
                summary => "Only return results with $a not containing ".
                    "certain text",
                arg_category => "filter for $cname",
            }];
            my $cf = $cspec->{clause_sets}[0]{column_filterable_regex};
            unless (defined($cf) && !$cf) {
                return [400, "Clash of $ctype filter argument: ${a}_match"]
                    if $fargs->{"${a}_match"};
                $fargs->{"${a}_match"} = [$ctype => {
                    summary => "Only return results with $a matching ".
                        "specified regex",
                    arg_category => "filter for $cname",
                }];
                return [400, "Clash of $ctype filter argument: ${a}_not_match"]
                    if $fargs->{"${a}_not_match"};
                $fargs->{"${a}_not_match"} = [$ctype => {
                    summary => "Only return results with $a matching ".
                        "specified regex",
                    arg_category => "filter for $cname",
                }];
            }
        }
    }

    [200, "OK", $func_meta];
}

sub __parse_query {
    my ($args, $opts, $table_spec) = @_;
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
        my $exists;
        if (defined $args->{"$c.is"}) {
            $exists++;
            push @filters, [$c, "truth", $args->{$c}];
        }
        push @filter_fields, $c if $exists && !($c ~~ @filter_fields);
    }
    for my $c (grep {$col_specs->{$_}{type} eq 'array'} @columns) {
        my $exists;
        if (defined $args->{"$c.has"}) {
            $exists++;
            push @filters, ["$c.has", $c, "~~", $args->{"$c.has"}];
        }
        if (defined $args->{"$c.lacks"}) {
            $exists++;
            push @filters, ["$c.lacks", $c, "!~~", $args->{"$c.lacks"}];
        }
        push @filter_fields, $c if $exists && !($c ~~ @filter_fields);
    }
    for my $c (grep {$col_specs->{$_}{type} =~ /^(int|float|str)$/}
                   @columns) {
        my $exists;
        my $a = $c;
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
        my $a = $c;
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
        !defined($cspec->{searchable}) || $cspec->{searchable}
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
            $ctype =~ /^(str)$/
        } @searchable_fields];
        $query->{search_array_fields} = [grep {
            $ctype =~ /^(array)$/
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
            return [400, "Field $f is not sortable"]
                unless !defined($cspec->{sortable}) || $cspec->{sortable};
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
    my ($self, $table_spec, $opts, $table_data, $func_meta) = @_;

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
        if (__is_aoa($table_data) || __is_aoh($table_data)) {
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
                unless __is_aoa($data) || __is_aoh($data);
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

    [200, "OK", $func];
}

$SPEC{gen_read_table_func} = {
    summary => 'Generate function (and its spec) to read table data',
    description => <<'_',

The generated function acts like a simple single table SQL SELECT query,
featuring filtering, sorting, and paging, but using arguments as the 'query
language'. The generated function is suitable for exposing a table data from an
API function.

The generated spec is pretty barebones currently. You can decorate with summary
and description afterwards.

The resulting function will accept these arguments. The naming of arguments are
designed to be less Perl-/database-centric.

* *show_field_names* => BOOL (default 1)

  By default function will return AoH. If this argument is set to 0, then
  function will return AoA instead.

* *detail* => BOOL (default 0)

  This is a field selection option. By default, function will return PK column
  only. If this argument is set to true, then all columns will be returned.

* *fields* => ARRAY

  This is a field selection option. If you only want certain fields, specify
  them here.

* *result_limit* => INT (default undef)

* *result_start* => INT (default 1)

  The *result_limit* and *result_start* arguments are paging options, they work
  like LIMIT clause in SQL, except that index starts at 1 and not 0.

* *random* => BOOL (default 0)

  The random argument is an ordering option. If set to true, order of rows
  returned will be shuffled first. This happened before paging.

* *sort* => STR

  The sort argument is an ordering option, containing name of field. A - prefix
  signifies descending instead of ascending order. Multiple fields are allowed,
  separated by comma.

* *q* => STR

  A filtering option. By default, all fields except those specified with
  column_searchable=0 will be searched using simple case-insensitive string
  search. There are a few options to customize this, using these gen arguments:
  *word_search*, *case_insensitive_search*, and *custom_search*.

* Filter arguments

  They will be generated for each column, except when column has
  'column_filterable' clause set to false.

  Undef values will not match any filter, just like NULL in SQL.

  + *FIELD.is* boolean argument for each field. Only records with field matching
     value exactly ('==') will be included. If doesn't clash with other function
     arguments, *FIELD* will also be added for this.

  + *FIELD.not* boolean argument for each field. Only records with field not
    maching value ('!=') will be included.

  + *FIELD.has* and *FIELD.lacks* array arguments for each set field. Only
    records with field having or lacking certain value will be included.

  + *FIELD.min* and *FIELD.max* for each int/float/str field. Only records with
    field greater/equal than, or less/equal than a certain value will be
    included.

  + *FIELD.contains* for each str field. Only records with field containing
    certain value (substring) will be included.

  + *FIELD.matches* and *FIELD.not_matches* for each str field. Only records
    with field matching certain value (regex) will be included. Function will
    return 400 if regex is invalid. These arguments will not be generated if
    'filterable_regex' clause in column specification is set to 0.

  + *FIELD.starts_with* for each str field (not yet implemented)

  + *FIELD.ends_with* for each str field (not yet implemented)

_
    args => {
        table_data => {
            req => 1,
            schema => 'any*',
            summary => 'Data',
            description => <<'_',

Table data is either an AoH or AoA. Or you can also pass a Perl subroutine (see
below).

Passing a subroutine lets you fetch data dynamically and from arbitrary source
(e.g. DBI table or other external sources). The subroutine will be called with
these arguments ('$query') and is expected to return a hashref like this {data
=> DATA, paged=>BOOL, filtered=>BOOL, sorted=>BOOL, columns_selected=>BOOL,
randomized=>BOOL}. DATA is AoA or AoH. If paged is set to 1, data is assumed to
be already paged and won't be paged again; likewise for filtered, sorted, and
column_selected. These are useful for example with DBI result, where requested
data is already filtered/sorted/column selected/paged/randomized via appropriate
SQL query. This way, the generated function will not attempt to duplicate the
efforts.

'$query' is a hashref which contains information about the query, e.g. 'args'
(the original arguments passed to the generated function, e.g. {random=>1,
result_limit=>1, field1_match=>'f.+'}), 'mentioned_fields' which lists fields
that are mentioned in either filtering arguments or fields or ordering,
'requested_fields' (fields mentioned in list of fields to be returned),
'sort_fields' (fields mentioned in sort arguments), 'filter_fields' (fields
mentioned in filter arguments).

_
        },
        table_spec => {
            req => 1,
            schema => 'hash*',
            summary => 'Table specification',
            description => <<'_',

A hashref with these required keys: 'columns', 'pk'. 'Columns' is a hashref of
column specification with column name as keys, while 'pk' specifies which column
is to be designated as the primary key. Currently only single-column PK is
allowed.

Column specification. A hashref with these required keys: 'schema' (a Sah
schema), 'index' (an integer starting from 0 that specifies position of column
in the data, especially required with AoA data) and these optional clauses:
'sortable' (a boolean stating whether column can be sorted, default is true),
'filterable' (a boolean stating whether column can be mentioned in filter
options, default is true).

_
        },
        langs => {
            schema => [array => {of=>'str*', default=>['en_US']}],
            summary => 'Choose language for function metadata',
            description => <<'_',

This function can generate metadata containing text from one or more languages.
For example if you set 'langs' to ['en_US', 'id_ID'] then the generated function
metadata might look something like this:

    {
        v => 1.1,
        args => {
            random => {
                summary => 'Random order of results', # English
                "summary.alt.lang.id_ID" => "Acak urutan hasil", # Indonesian
                ...
            },
            ...
        },
        ...
    }

_
        },
        default_detail => {
            schema => 'bool',
            summary => "Supply default 'detail' value for function arg spec",
        },
        default_fields => {
            schema => 'str',
            summary => "Supply default 'fields' value for function arg spec",
        },
        # not yet documented
        #default_filters => {
        #    schema => ['array' => {
        #        of => 'array*', # XXX filter structure
        #    }],
        #    summary => "Supply default filters",
        #},
        default_show_field_names => {
            schema => 'bool',
            summary => "Supply default 'show_field_names' ".
                "value for function arg spec",
        },
        default_sort => {
            schema => 'str',
            summary => "Supply default 'sort' value for function arg spec",
        },
        default_random => {
            schema => 'bool',
            summary => "Supply default 'random' value for function arg spec",
        },
        default_result_limit => {
            schema => 'int',
            summary => "Supply default 'result_limit' value for function arg ".
                "spec",
        },
        enable_search => {
            schema => ['bool' => {
                default => 1,
            }],
            summary => "Decide whether generated function will support ".
                "searching (argument q)",
        },
        word_search => {
            schema => ['bool' => {
                default => 0,
            }],
            summary => "Decide whether generated function will perform ".
                "word searching instead of string searching",
            description => <<'_',

For example, if search term is 'pine' and column value is 'green pineapple',
search will match if word_search=false, but won't match under word_search.

_
        },
        case_insensitive_search => {
            schema => ['bool' => {
                default => 1,
            }],
            summary => 'Decide whether generated function will perform '.
                'case-insensitive search',
        },
        custom_search => {
            schema => 'code',
            summary => 'Supply custom searching for generated function',
            description => <<'_',

Code will be supplied ($row, $q, $opts) where $q is the search term (from the
function argument 'q') and $row the hashref row value. $opts is {ci=>0|1}. Code
should return true if row matches search term.

_
        },
    },
};
sub gen_read_table_func {
    my %args = @_;

    my $self = __PACKAGE__->new;
    $self->_gen_read_table_func(%args);
}

sub _gen_read_table_func {
    my ($self, %args) = @_;

    # XXX schema
    my $table_data = $args{table_data}
        or return [400, "Please specify table_data"];
    __is_aoa($table_data) or __is_aoh($table_data) or ref($table_data) eq 'CODE'
        or return [400, "Invalid table_data: must be AoA or AoH or function"];
    my $table_spec = $args{table_spec}
        or return [400, "Please specify table_spec"];
    ref($table_spec) eq 'HASH'
        or return [400, "Invalid table_spec: must be a hash"];
    $table_spec->{columns} or
        return [400, "Invalid table_spec: columns not specified"];
    ref($table_spec->{columns}) eq 'HASH' or
        return [400, "Invalid table_spec: columns must be hash"];
    $table_spec->{pk} or
        return [400, "Invalid table_spec: pk not specified"];
    exists($table_spec->{columns}{ $table_spec->{pk} }) or
        return [400, "Invalid table_spec: pk not in columns"];

    # duplicate and make each column's schema normalized
    $table_spec = clone($table_spec);
    for my $c (values %{$table_spec->{columns}}) {
        $c->{schema} //= 'any';
        $c->{schema} = __parse_schema($c->{schema});
    }

    my $opts = {
        langs                    => $args{langs} // ['en_US'],
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
    $res = $self->_gen_meta($table_spec, $opts);
    return [$res->[0], "Can't generate meta: $res->[1]"]
        unless $res->[0] == 200;
    my $func_meta = $res->[2];

    $res = $self->_gen_func($table_spec, $opts, $table_data, $func_meta);
    return [$res->[0], "Can't generate func: $res->[1]"]
        unless $res->[0] == 200;
    my $func = $res->[2];

    [200, "OK", {meta=>$func_meta, code=>$func}];
}

1;
# ABSTRACT: Generate function (and its Rinci metadata) to access table data

=head1 SYNOPSIS

In list_countries.pl:

 #!perl
 use strict;
 use warnings;
 use Perinci::CmdLine;
 use Perinci::Sub::Gen::AccessTable qw(gen_read_table_func);

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
             id => {
                 schema => 'str*',
                 summary => 'ISO 2-letter code for the country',
                 index => 0,
                 sortable => 1,
             },
             en_name => {
                 schema => 'str*',
                 summary => 'English name',
                 index => 1,
                 sortable => 1,
             },
             id_name => {
                 schema => 'str*',
                 summary => 'Indonesian name',
                 index => 2,
                 sortable => 1,
             },
             tags => {
                 schema => 'array*',
                 summary => 'Keywords/tags',
                 index => 3,
                 sortable => 0,
             },
         },
         pk => 'id',
     },
 );
 die "Can't generate function: $res->[0] - $res->[1]" unless $res->[0] == 200;
 *list_countries       = $res->[2]{code};
 $SPEC{list_countries} = $res->[2]{meta};

 Perinci::CmdLine->new(url=>'/main/list_countries')->run;

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
specifying fields, filtering, sorting, and paging; along with its L<Rinci>
metadata. The resulting function can then be run via command-line using
L<Perinci::CmdLine> (as demonstrated in Synopsis), or served via HTTP using
L<Perinci::Access::HTTP::Server>, or consumed normally by Perl programs.

Internally, the
This module uses L<Log::Any> for logging.


=head1 CAVEATS

It is often not a good idea to expose your database schema directly as API.


=head1 FAQ


=head1 SEE ALSO

L<Rinci>

L<Perinci::CmdLine>

=cut
