package Perinci::Sub::Gen::AccessTable;

use 5.010001;
use strict;
use warnings;
use experimental 'smartmatch';
use Log::Any '$log';

use List::Util qw(shuffle);
use Locale::TextDomain::UTF8 'Perinci-Sub-Gen-AccessTable';
use Perinci::Object::Metadata;
use Perinci::Sub::Gen;
use Perinci::Sub::Util qw(err);
use SHARYANTO::Locale::Util qw(:locale_h);
use Scalar::Util qw(reftype);
#use SHARYANTO::String::Util qw(trim_blank_lines);

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(gen_read_table_func);

# VERSION

our %SPEC;

sub __parse_schema {
    require Data::Sah;
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

sub __is_filter_arg {
    my ($arg, $func_meta) = @_;
    my $args = $func_meta->{args};
    return 0 unless $args && $args->{$arg};
    my $tags = $args->{$arg}{tags};
    return 0 unless $tags;
    for my $tag (@$tags) {
        next unless ref($tag) eq 'HASH';
        return 1 if $tag->{name} =~ /^category:filtering/;
    }
    0;
}

sub _add_arg {
    my %args = @_;

    my $arg_name  = $args{name};
    my $fname     = $args{name}; $fname =~ s/\..+//;
    my $func_meta = $args{func_meta};
    my $langs     = $args{langs};

    die "BUG: Duplicate arg $arg_name" if $func_meta->{args}{$arg_name};

    my $tag = {name=>"category:$args{cat_name}"};
    my $schema = ref($args{type}) eq 'ARRAY' ? $args{type} :
        [$args{type} => {}];
    $schema->[1] //= {};
    $schema->[1]{default} = $args{default};
    my $arg_spec = {
        schema => $schema,
        tags => [$tag],
    };

    # translation args
    my %xargs = (field => $fname);

    my $orig_locale = setlocale(LC_ALL);

    for my $prop (qw/summary description/) {
        next unless defined $args{$prop};
        #$args{$prop} = trim_blank_lines($args{$prop});
        for my $lang (@$langs) {
            setlocale(LC_ALL, $lang) or warn "Can't setlocale $lang";
            my $isdeflang = $lang eq 'en_US';
            my $k = $prop . ($isdeflang ? '' : ".alt.lang.$lang");
            $arg_spec->{$k} = __x($args{$prop}, %xargs);
        }
    }

    for my $lang (@$langs) {
        for my $prop (qw/summary/) {
            setlocale(LC_ALL, $lang) or warn "Can't setlocale $lang";
            my $isdeflang = $lang eq 'en_US';
            my $k = $prop . ($isdeflang ? '' : ".alt.lang.$lang");
            $tag->{$k} = __x($args{cat_text}, %xargs);
        }
    }

    setlocale(LC_ALL, $orig_locale);
    $func_meta->{args}{$arg_name} = $arg_spec;
}

sub _add_table_desc_to_func_description {
    my ($func_meta, $table_spec, $opts) = @_;
    my $langs = $opts->{langs};

    my $orig_locale = setlocale(LC_ALL);

    for my $lang (@$langs) {
        setlocale(LC_ALL, $lang) or warn "Can't setlocale $lang";
        my $td = __("Data is in table form. Table fields are as follow:");
        $td .= "\n\n";
        my $ff = $table_spec->{fields};
        # reminder: index property is for older spec, will be removed someday
        for my $fn (sort {($ff->{$a}{pos}//$ff->{$a}{index}//0) <=>
                              ($ff->{$b}{pos}//$ff->{$b}{index}//0)}
                        keys %$ff) {
            my $f  = $ff->{$fn};
            my $fo = Perinci::Object::Metadata->new($f);
            my $sum = $fo->langprop("summary", {lang=>$lang});
            $td .=
                join("",
                     "  - *$fn*",
                     $table_spec->{pk} eq $fn ?
                         " (".__x("ID field").")":"",
                     $sum ? ": $sum" : "",
                     "\n\n");
            my $desc = $fo->langprop("description", {lang=>$lang});
            if ($desc) {
                $desc =~ s/^/    /mg;
                $td .= "$desc\n\n";
            }
        }

        my $key = "description" . ($lang eq 'en_US' ? '' : ".alt.lang.$lang");
        $func_meta->{$key} //= "";
        $func_meta->{$key} .= "\n" unless $func_meta->{$key} =~ /\n\z/;
        $func_meta->{$key} .= "\n" unless $func_meta->{$key} !~ /\S/;
        $func_meta->{$key} .= $td;
    }

    setlocale(LC_ALL, $orig_locale);
}

sub _gen_meta {
    my ($table_spec, $opts) = @_;
    my $langs = $opts->{langs};

    # add general arguments

    my $func_meta = {
        v => 1.1,
        summary => $opts->{summary} // $table_spec->{summary} // "REPLACE ME",
        description => $opts->{description} // "REPLACE ME",
        args => {},
    };

    _add_table_desc_to_func_description($func_meta, $table_spec, $opts);

    my $func_args = $func_meta->{args};

    _add_arg(
        func_meta   => $func_meta,
        langs       => $langs,
        name        => 'with_field_names',
        type        => 'bool',
        default     => $opts->{default_with_field_names},
        cat_name    => 'field-selection',
        cat_text    => N__('field selection'),
        summary     => N__('Return field names in each record (as hash/'.
                               'associative array)'),
        description => N__(<<'_',

When enabled, function will return each record as hash/associative array
(field name => value pairs). Otherwise, function will return each record
as list/array (field value, field value, ...).

_
    ));
    _add_arg(
        func_meta   => $func_meta,
        langs       => $langs,
        name        => 'detail',
        type        => 'bool',
        default     => $opts->{default_detail} // 0,
        cat_name    => 'field-selection',
        cat_text    => N__('field selection'),
        summary     => N__('Return array of full records instead of '.
                               'just ID fields'),
        description => N__(<<'_',

By default, only the key (ID) field is returned per result entry.

_
    ));
    _add_arg(
        func_meta   => $func_meta,
        langs       => $langs,
        name        => 'fields',
        type        => ['array*' => {of=>'str*'}],
        default     => $opts->{default_fields},
        cat_name    => 'field-selection',
        cat_text    => N__('field selection'),
        summary     => N__('Select fields to return'),
    );
    _add_arg(
        func_meta   => $func_meta,
        langs       => $langs,
        name        => 'sort',
        type        => 'str',
        default     => $opts->{default_sort},
        cat_name    => 'ordering',
        cat_text    => N__('ordering'),
        summary     => N__('Order records according to certain field(s)'),
        description => N__(<<'_',

A list of field names separated by comma. Each field can be prefixed with '-' to
specify descending order instead of the default ascending.

_
    ));
    _add_arg(
        func_meta   => $func_meta,
        langs       => $langs,
        name        => 'random',
        type        => 'bool',
        default     => $opts->{default_random} // 0,
        cat_name    => 'ordering',
        cat_text    => N__('ordering'),
        summary     => N__('Return records in random order'),
    );
    _add_arg(
        func_meta   => $func_meta,
        langs       => $langs,
        name        => 'result_limit',
        type        => 'int',
        default     => $opts->{default_result_limit},
        cat_name    => 'paging',
        cat_text    => N__('paging'),
        summary     => N__('Only return a certain number of records'),
    );
    _add_arg(
        func_meta   => $func_meta,
        langs       => $langs,
        name        => 'result_start',
        type        => 'int',
        default     => 1,
        cat_name    => 'paging',
        cat_text    => N__('paging'),
        summary     => N__("Only return starting from the n'th record"),
    );
    _add_arg(
        func_meta   => $func_meta,
        langs       => $langs,
        name        => 'q',
        type        => 'str',
        cat_name    => 'filtering',
        cat_text    => N__('filtering'),
        summary     => N__("Search"),
    ) if $opts->{enable_search} // 1;

    # add filter arguments for each table field

    for my $fname (keys %{$table_spec->{fields}}) {
        my $fspec   = $table_spec->{fields}{$fname};
        my $fschema = $fspec->{schema};
        my $ftype   = $fschema->[0];

        next if defined($fspec->{filterable}) && !$fspec->{filterable};

        _add_arg(
            func_meta   => $func_meta,
            langs       => $langs,
            name        => "$fname.is",
            type        => "$ftype*",
            default     => $opts->{"default_$fname.is"},
            cat_name    => "filtering-for-$fname",
            cat_text    => N__("filtering for {field}"),
            summary     => N__("Only return records where the '{field}' field ".
                                   "equals specified value"),
        );
        unless ($func_args->{$fname}) {
            $func_args->{$fname} =
                Data::Clone::clone($func_args->{"$fname.is"});
        }
        _add_arg(
            func_meta   => $func_meta,
            langs       => $langs,
            name        => "$fname.isnt",
            type        => "$ftype*",
            default     => $opts->{"default_$fname.isnt"},
            cat_name    => "filtering-for-$fname",
            cat_text    => N__("filtering for {field}"),
            summary     => N__("Only return records where the '{field}' field ".
                                   "does not equal specified value"),
        );

        # .in & .not_in should be applicable to arrays to, but it is currently
        # implemented with perl's ~~ which can't handle this transparently. as
        # for bool, it's not that important.
        unless ($ftype ~~ [qw/array bool/]) {
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.in",
                type        => ['array*' => {of => "$ftype*"}],
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "is in the specified values"),
            );
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.not_in",
                type        => ['array*' => {of => "$ftype*"}],
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "is not in the specified values"),
            );
        }
        if ($ftype eq 'array') {
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.has",
                type        => [array => {of=>'str*'}],
                default     => $opts->{"default_$fname.has"},
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "is an array/list which contains specified value"),
            );
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.lacks",
                type        => [array => {of=>'str*'}],
                default     => $opts->{"default_$fname.lacks"},
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "is an array/list which does not contain specified value"),
            );
        }
        if ($ftype =~ /^(?:int|float|str)$/) { # XXX all Comparable types
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.min",
                type        => $ftype,
                default     => $opts->{"default_$fname.min"},
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                    "is greater than or equal to specified value"),
            );
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.max",
                type        => $ftype,
                default     => $opts->{"default_$fname.max"},
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "is less than or equal to specified value"),
            );
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.xmin",
                type        => $ftype,
                default     => $opts->{"default_$fname.xmin"},
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "is greater than specified value"),
            );
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.xmax",
                type        => $ftype,
                default     => $opts->{"default_$fname.xmax"},
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "is less than specified value"),
            );
        }
        if ($ftype eq 'str') {
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.contains",
                type        => $ftype,
                default     => $opts->{"default_$fname.contains"},
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "contains specified text"),
            );
            _add_arg(
                func_meta   => $func_meta,
                langs       => $langs,
                name        => "$fname.not_contains",
                type        => $ftype,
                default     => $opts->{"default_$fname.not_contains"},
                cat_name    => "filtering-for-$fname",
                cat_text    => N__("filtering for {field}"),
                summary     => N__("Only return records where the '{field}' field ".
                                       "does not contain specified text"),
            );
            if ($fspec->{filterable_regex}) {
                _add_arg(
                    func_meta   => $func_meta,
                    langs       => $langs,
                    name        => "$fname.matches",
                    type        => $ftype,
                    default     => $opts->{"default_$fname.matches"},
                    cat_name    => "filtering-for-$fname",
                    cat_text    => N__("filtering for {field}"),
                    summary     => N__("Only return records where the '{field}' field ".
                                           "matches specified regular expression pattern"),
                );
                _add_arg(
                    func_meta   => $func_meta,
                    langs       => $langs,
                    name        => "$fname.not_matches",
                    type        => $ftype,
                    default     => $opts->{"default_$fname.not_matches"},
                    cat_name    => "filtering-for-$fname",
                    cat_text    => N__("filtering for {field}"),
                    summary     => N__("Only return records where the '{field}' field " .
                                           "does not match specified regular expression"),
                );
            }
        }
    } # for each fspec

    # custom filters
    my $cff = $opts->{custom_filters} // {};
    while (my ($cfn, $cf) = each %$cff) {
        $func_args->{$cfn} and return [
            400, "Custom filter '$cfn' clashes with another argument"];
        $func_args->{$cfn} = $cf->{meta};
    }

    [200, "OK", $func_meta];
}

sub __parse_query {
    my ($table_spec, $opts, $func_meta, $args) = @_;
    my $query = {args=>$args};

    my $fspecs = $table_spec->{fields};
    # reminder: index property is for older spec, will be removed someday
    my @fields = sort {($fspecs->{$a}{pos}//$fspecs->{$a}{index}) <=>
                           ($fspecs->{$b}{pos}//$fspecs->{$b}{index})}
        keys %$fspecs;

    my @requested_fields;
    if ($args->{detail}) {
        @requested_fields = @fields;
        $args->{with_field_names} //= 1;
    } elsif ($args->{fields}) {
        @requested_fields = @{ $args->{fields} };
        $args->{with_field_names} //= 0;
    } else {
        @requested_fields = ($table_spec->{pk});
        $args->{with_field_names} //= 0;
    }
    for (@requested_fields) {
        return err(400, "Unknown field $_") unless $_ ~~ @fields;
    }
    $query->{requested_fields} = \@requested_fields;

    my @filter_fields;
    my @filters; # ([field, operator, operand...])

    for my $f (grep {$fspecs->{$_}{schema}[0] eq 'bool'} @fields) {
        my $fspec = $fspecs->{$f};
        my $exists;
        if (defined $args->{"$f.is"}) {
            $exists++;
            push @filters, [$f, "truth", $args->{"$f.is"}];
        } elsif (defined $args->{"$f.isnt"}) {
            $exists++;
            push @filters, [$f, "truth", !$args->{"$f.isnt"}];
        } elsif (defined($args->{$f}) && __is_filter_arg($f, $func_meta)) {
            $exists++;
            push @filters, [$f, "truth", $args->{$f}];
        }
        push @filter_fields, $f if $exists && !($f ~~ @filter_fields);
    }

    for my $f (grep {$fspecs->{$_}{schema}[0] eq 'array'} @fields) {
        my $exists;
        if (defined $args->{"$f.has"}) {
            $exists++;
            push @filters, [$f, "~~", $args->{"$f.has"}];
        }
        if (defined $args->{"$f.lacks"}) {
            $exists++;
            push @filters, [$f, "!~~", $args->{"$f.lacks"}];
        }
        push @filter_fields, $f if $exists && !($f ~~ @filter_fields);
    }

    for my $f (grep {!($fspecs->{$_}{schema}[0] ~~ ['array','bool'])} @fields) {
        my $exists;
        if (defined $args->{"$f.in"}) {
            $exists++;
            push @filters, [$f, "in", $args->{"$f.in"}];
        }
        if (defined $args->{"$f.not_in"}) {
            $exists++;
            push @filters, [$f, "not_in", $args->{"$f.not_in"}];
        }
    }

    for my $f (grep {$fspecs->{$_}{schema}[0] =~ /^(int|float|str)$/}
                   @fields) { # XXX all Comparable
        my $exists;
        my $fspec = $fspecs->{$f};
        my $ftype = $fspec->{schema}[0];
        if (defined $args->{"$f.is"}) {
            $exists++;
            push @filters,
                [$f, $ftype eq 'str' ? "eq" : "==", $args->{"$f.is"}];
        } elsif (defined($args->{$f}) && __is_filter_arg($f,  $func_meta)) {
            $exists++;
            push @filters, [$f, $ftype eq 'str' ? "eq":"==", $args->{$f}];
        }
        if (defined $args->{"$f.isnt"}) {
            $exists++;
            push @filters,
                [$f, $ftype eq 'str' ? "ne" : "!=", $args->{"$f.isnt"}];
        } elsif (defined($args->{$f}) && __is_filter_arg($f,  $func_meta)) {
            $exists++;
            push @filters, [$f, $ftype eq 'str' ? "eq":"==", $args->{$f}];
        }
        if (defined $args->{"$f.min"}) {
            $exists++;
            push @filters, [$f, $ftype eq 'str' ? 'ge':'>=', $args->{"$f.min"}];
        }
        if (defined $args->{"$f.max"}) {
            $exists++;
            push @filters, [$f, $ftype eq 'str' ? 'le':'<=', $args->{"$f.max"}];
        }
        if (defined $args->{"$f.xmin"}) {
            $exists++;
            push @filters, [$f, $ftype eq 'str' ? 'gt':'>', $args->{"$f.xmin"}];
        }
        if (defined $args->{"$f.xmax"}) {
            $exists++;
            push @filters, [$f, $ftype eq 'str' ? 'lt':'<', $args->{"$f.xmax"}];
        }
        push @filter_fields, $f if $exists && !($f ~~ @filter_fields);
    }

    for my $f (grep {$fspecs->{$_}{schema}[0] =~ /^str$/} @fields) {
        my $exists;
        if (defined $args->{"$f.contains"}) {
            $exists++;
            push @filters, [$f, 'pos', $args->{"$f.contains"}];
        }
        if (defined $args->{"$f.not_contains"}) {
            $exists++;
            push @filters, [$f, '!pos', $args->{"$f.not_contains"}];
        }
        if (defined $args->{"$f.matches"}) {
            $exists++;
            push @filters, [$f, '=~', $args->{"$f.matches"}];
        }
        if (defined $args->{"$f.not_matches"}) {
            $exists++;
            push @filters, [$f, '!~', $args->{"$f.not_matches"}];
        }
        push @filter_fields, $f if $exists && !($f ~~ @filter_fields);
    }
    $query->{filters}       = \@filters;
    $query->{filter_fields} = \@filter_fields;

    my $cff = $opts->{custom_filters} // {};
    while (my ($cfn, $cf) = each %$cff) {
        next unless defined $args->{$cfn};
        push @filters, [$cf->{fields}, 'call', [$cf->{code}, $args->{$cfn}]];
        for (@{$cf->{fields} // []}) {
            push @filter_fields, $_ if !($_ ~~ @filter_fields);
        }
    }

    my @searchable_fields = grep {
        !defined($fspecs->{$_}{searchable}) || $fspecs->{$_}{searchable}
        } @fields;
    my $ci = $opts->{case_insensitive_search};
    my $search_opts = {ci => $ci};
    my $search_re;
    my $q = $args->{q};
    if (defined $q) {
        if ($opts->{word_search}) {
            $search_re = $ci ? qr/\b$q\b/i : qr/\b$q\b/;
        } else {
            $search_re = $ci ? qr/$q/i : qr/$q/;
        }
    }
    $query->{q} = $args->{q};
    $query->{search_opts} = $args->{search_opts};
    unless ($opts->{custom_search}) {
        $query->{search_fields} = \@searchable_fields;
        $query->{search_str_fields} = [grep {
            $fspecs->{$_}{schema}[0] =~ /^(str)$/
        } @searchable_fields];
        $query->{search_array_fields} = [grep {
            $fspecs->{$_}{schema}[0] =~ /^(array)$/
        } @searchable_fields];
        $query->{search_re} = $search_re;
    }

    my @sort_fields;
    my @sorts;
    if (defined $args->{sort}) {
        my @f = split /\s*[,;]\s*/, $args->{sort};
        for my $f (@f) {
            my $desc = $f =~ s/^-//;
            return err(400, "Unknown field in sort: $f")
                unless $f ~~ @fields;
            my $fspec = $fspecs->{$f};
            my $ftype = $fspec->{schema}[0];
            return err(400, "Field $f is not sortable")
                unless !defined($fspec->{sortable}) || $fspec->{sortable};
            my $op = $ftype =~ /^(int|float)$/ ? '<=>' : 'cmp';
            #print "ftype=$ftype, op=$op\n";
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
    [200, "OK", $query];
}

sub _gen_func {
    my ($table_spec, $opts, $table_data, $func_meta) = @_;

    my $fspecs = $table_spec->{fields};
    my $func_args = $func_meta->{args};
    my $func = sub {
        my %args = @_;
        my $hooks = $opts->{hooks};
        my %hookargs = %args;
        $hookargs{_func_args} = \%args;

        # XXX schema
        while (my ($ak, $av) = each %$func_args) {
            if (ref($av->{schema}) && ref($av->{schema}[1]) &&
                    defined($av->{schema}[1]{default})) {
                $args{$ak} //= $av->{schema}[1]{default};
            }
            # array-ize "string,with,comma"
            if ($ak eq 'fields' && defined($args{$ak})) {
                $args{$ak} = [split /\s*[,;]\s*/, $args{$ak}]
                    unless ref($args{$ak}) eq 'ARRAY';
            }
        }

        for ('before_parse_query') {
            last unless $hooks->{$_};
            $hookargs{_stage} = $_;
            my $hres = $hooks->{$_}->(%hookargs);
            return $hres if ref($hres);
        }
        my $query;
        {
            my $res = __parse_query($table_spec, $opts, $func_meta, \%args);
            for ('after_parse_query') {
                $hookargs{_parse_res} = $res;
                last unless $hooks->{$_};
                $hookargs{_stage} = $_;
                my $hres = $hooks->{$_}->(%hookargs);
                return $hres if ref($hres);
            }
            return $res unless $res->[0] == 200;
            $query = $res->[2];
        }

        # retrieve data
        my $data;
        my $metadata = {};
        for ('before_fetch_data') {
            $hookargs{_query} = $query;
            last unless $hooks->{$_};
            $hookargs{_stage} = $_;
            my $hres = $hooks->{$_}->(%hookargs);
            return $hres if ref($hres);
        }
        if (__is_aoa($table_data) || __is_aoh($table_data)) {
            $data = $table_data;
        } elsif (reftype($table_data) eq 'CODE') {
            my $res;
            return err(500, "BUG: Table data function died: $@")
                unless eval { $res = $table_data->($query) };
            return err(500, "BUG: Result returned from table data function ".
                           "is not a hash") unless ref($res) eq 'HASH';
            $data = $res->{data};
            return err(500, "BUG: 'data' key from table data function ".
                           "is not an AoA/AoH")
                unless __is_aoa($data) || __is_aoh($data);
            for (qw/filtered sorted paged fields_selected/) {
                $metadata->{$_} = $res->{$_};
            }
        } else {
            # this should be impossible, already checked earlier
            die "BUG: 'data' from table data function is not an array";
        }
        for ('after_fetch_data') {
            $hookargs{_data} = $data;
            last unless $hooks->{$_};
            $hookargs{_stage} = $_;
            my $hres = $hooks->{$_}->(%hookargs);
            return $hres if ref($hres);
        }

        # this will be the final result.
        my @r;

        no warnings; # silence undef warnings when comparing record values

        $log->tracef("(read_table_func) Filtering ...");
        my $q = $query->{q};
        my $search_re = $query->{search_re};

      REC:
        for my $r0 (@$data) {
            my $r_h;
            if (ref($r0) eq 'ARRAY') {
                # currently, internally we always use hashref for records and
                # convert to array/scalar later when returning final data.
                $r_h = {};
                for my $f (keys %$fspecs) {
                    # reminder: index property is for older spec, will be
                    # removed someday
                    $r_h->{$f} = $r0->[$fspecs->{$f}{pos}//$fspecs->{$f}{index}];
                }
            } elsif (ref($r0) eq 'HASH') {
                $r_h = { %$r0 };
            } else {
                return err(500, "BUG: Invalid record, not a hash/array");
            }

            goto SKIP_FILTER if $metadata->{filtered};

            for my $filter (@{$query->{filters}}) {
                my ($f, $op, $opn) = @$filter;
                if ($op eq 'truth') {
                    next REC if $r_h->{$f} xor $opn;
                } elsif ($op eq '~~') {
                    for (@$opn) {
                        next REC unless $_ ~~ @{$r_h->{$f}};
                    }
                } elsif ($op eq '!~~') {
                    for (@$opn) {
                        next REC if $_ ~~ @{$r_h->{$f}};
                    }
                } elsif ($op eq 'in') {
                    next REC unless $r_h->{$f} ~~ @$opn;
                } elsif ($op eq 'not_in') {
                    next REC if $r_h->{$f} ~~ @$opn;
                } elsif ($op eq 'eq') { next REC unless $r_h->{$f} eq $opn
                } elsif ($op eq '==') { next REC unless $r_h->{$f} == $opn
                } elsif ($op eq 'ne') { next REC unless $r_h->{$f} ne $opn
                } elsif ($op eq '!=') { next REC unless $r_h->{$f} != $opn
                } elsif ($op eq 'ge') { next REC unless $r_h->{$f} ge $opn
                } elsif ($op eq '>=') { next REC unless $r_h->{$f} >= $opn
                } elsif ($op eq 'gt') { next REC unless $r_h->{$f} gt $opn
                } elsif ($op eq '>' ) { next REC unless $r_h->{$f} >  $opn
                } elsif ($op eq 'le') { next REC unless $r_h->{$f} le $opn
                } elsif ($op eq '<=') { next REC unless $r_h->{$f} <= $opn
                } elsif ($op eq 'lt') { next REC unless $r_h->{$f} lt $opn
                } elsif ($op eq '<' ) { next REC unless $r_h->{$f} <  $opn
                } elsif ($op eq '=~') { next REC unless $r_h->{$f} =~ $opn
                } elsif ($op eq '!~') { next REC unless $r_h->{$f} !~ $opn
                } elsif ($op eq 'pos') {
                    next REC unless index($r_h->{$f}, $opn) >= 0;
                } elsif ($op eq '!pos') {
                    next REC if index($r_h->{$f}, $opn) >= 0;
                } elsif ($op eq 'call') {
                    next REC unless $opn->[0]->($r_h, $opn->[1]);
                } else {
                    die "BUG: Unknown op $op";
                }
            }

            if (defined $q) {
                if ($opts->{custom_search}) {
                    next REC unless $opts->{custom_search}->(
                        $r_h, $q, $query->{search_opts});
                } else {
                    my $match;
                    for my $f (@{$query->{search_str_fields}}) {
                        if ($r_h->{$f} =~ $search_re) {
                            $match++; last;
                        }
                    }
                  ARY_FIELD:
                    for my $f (@{$query->{search_array_fields}}) {
                        for my $el (@{$r_h->{$f}}) {
                            if ($el =~ $search_re) {
                                $match++; last ARY_FIELD;
                            }
                        }
                    }
                    next REC unless $match;
                }
            }

          SKIP_FILTER:

            push @r, $r_h;
        }

        $log->tracef("(read_table_func) Ordering ...");
        if ($metadata->{sorted}) {
            # do nothing
        } elsif ($query->{random}) {
            @r = shuffle @r;
        } elsif (@{$query->{sorts}}) {
            @r = sort {
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
            } @r;
        }

        use warnings;
        use experimental 'smartmatch';

        # perform paging
        $log->tracef("(read_table_func) Paging ...");
        unless ($metadata->{paged}) {
            if ($query->{result_start} > 1) {
                splice @r, 0, $query->{result_start}-1;
            }
            if (defined $query->{result_limit}) {
                splice @r, $query->{result_limit};
            }
        }

        # select fields
        $log->tracef("(read_table_func) Selecting fields ...");
        my $pk = $table_spec->{pk};
        goto SKIP_SELECT_FIELDS if $metadata->{fields_selected};
      REC2:
        for my $r (@r) {
            if (!$args{detail} && !$args{fields}) {
                $r = $r->{$pk};
                next REC2;
            }
            if ($args{with_field_names}) {
                my @f = keys %$fspecs;
                for (@f) {
                    delete $r->{$_}
                        unless $_ ~~ @{$query->{requested_fields}};
                }
            } else {
                $r = [map {$r->{$_}} @{$query->{requested_fields}}];
            }
        }
      SKIP_SELECT_FIELDS:

        my $resmeta = {};
        my $res = [200, "OK", \@r, $resmeta];

        my %rfopts = (
            table_column_orders  => [$query->{requested_fields}],
            # bool needs display hints
            table_column_formats => [{
                map { $_ => [[bool => {type=>'check'}]] }
                    grep { $fspecs->{$_}{schema}[0] eq 'bool' }
                        @{$query->{requested_fields}}
                    }],
        );
        $resmeta->{result_format_options} = {
            text          => \%rfopts,
            "text-pretty" => \%rfopts,
        };

        for ('before_return') {
            $hookargs{_func_res} = $res;
            last unless $hooks->{$_};
            $hookargs{_stage} = $_;
            my $hres = $hooks->{$_}->(%hookargs);
            return $hres if ref($hres);
        }

        $res;
    }; # func;

    [200, "OK", $func];
}

$SPEC{gen_read_table_func} = {
    v => 1.1,
    summary => 'Generate function (and its metadata) to read table data',
    description => <<'_',

The generated function acts like a simple single table SQL SELECT query,
featuring filtering, ordering, and paging, but using arguments as the 'query
language'. The generated function is suitable for exposing a table data from an
API function.

The resulting function returns an array of results/records and accepts these
arguments.

* *with_field_names* => BOOL (default 1)

  If set to 1, function will return records of field values along with field
  names (hashref), e.g. {id=>'ID', country=>'Indonesia', capital=>'Jakarta'}. If
  set to 0, then function will return record containing field values without
  field names (arrayref) instead, e.g.: ['ID', 'Indonesia', 'Jakarta'].

* *detail* => BOOL (default 0)

  This is a field selection option. If set to 0, function will return PK field
  only. If this argument is set to 1, then all fields will be returned (see also
  *fields* to instruct function to return some fields only).

* *fields* => ARRAY

  This is a field selection option. If you only want certain fields, specify
  them here (see also *detail*).

* *result_limit* => INT (default undef)

* *result_start* => INT (default 1)

  The *result_limit* and *result_start* arguments are paging options, they work
  like LIMIT clause in SQL, except that index starts at 1 and not 0. For
  example, to return the first 20 records in the result, set *result_limit* to
  20 . To return the next 20 records, set *result_limit* to 20 and
  *result_start* to 21.

* *random* => BOOL (default 0)

  The random argument is an ordering option. If set to true, order of records
  returned will be shuffled first. This happened before paging.

* *sort* => STR

  The sort argument is an ordering option, containing name of field. A - prefix
  signifies descending instead of ascending order. Multiple fields are allowed,
  separated by comma.

* *q* => STR

  A filtering option. By default, all fields except those specified with
  searchable=0 will be searched using simple case-insensitive string search.
  There are a few options to customize this, using these gen arguments:
  *word_search*, *case_insensitive_search*, and *custom_search*.

* Filter arguments

  They will be generated for each field, except when field has 'filterable'
  clause set to false.

  Undef values will not match any filter, just like NULL in SQL.

  * *FIELD.is* and *FIELD.isnt* arguments for each field. Only records with
     field equalling (or not equalling) value exactly ('==' or 'eq') will be
     included. If doesn't clash with other function arguments, *FIELD* will also
     be added as an alias for *FIELD.is*.

  * *FIELD.has* and *FIELD.lacks* array arguments for each set field. Only
    records with field having or lacking certain value will be included.

  * *FIELD.min* and *FIELD.max* for each int/float/str field. Only records with
    field greater/equal than, or less/equal than a certain value will be
    included.

  * *FIELD.contains* and *FIELD.not_contains* for each str field. Only records
    with field containing (or not containing) certain value (substring) will be
    included.

  * *FIELD.matches* and *FIELD.not_matches* for each str field. Only records
    with field matching (or not matching) certain value (regex) (or will be
    included. Function will return 400 if regex is invalid. These arguments will
    not be generated if 'filterable_regex' clause in field specification is set
    to 0.

_
    args => {
        %Perinci::Sub::Gen::common_args,
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
=> DATA, paged=>BOOL, filtered=>BOOL, sorted=>BOOL, fields_selected=>BOOL}. DATA
is AoA or AoH. If paged is set to 1, data is assumed to be already paged and
won't be paged again; likewise for filtered, sorted, and fields selected. These
are useful for example with DBI result, where requested data is already
filtered/sorted (including randomized)/field selected/paged via appropriate SQL
query. This way, the generated function will not attempt to duplicate the
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

See `SHARYANTO::TableSpec` for more details.

A hashref with these required keys: 'fields', 'pk'. 'fields' is a hashref of
field specification with field name as keys, while 'pk' specifies which field is
to be designated as the primary key. Currently only single-field PK is allowed.

Field specification. A hashref with these required keys: 'schema' (a Sah
schema), 'index' (an integer starting from 0 that specifies position of field in
the record, required with AoA data) and these optional clauses: 'sortable' (a
boolean stating whether field can be sorted, default is true), 'filterable' (a
boolean stating whether field can be mentioned in filter options, default is
true).

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
        default_with_field_names => {
            schema => 'bool',
            summary => "Supply default 'with_field_names' ".
                "value in generated function's metadata",
        },
        default_sort => {
            schema => 'str',
            summary => "Supply default 'sort' ".
                "value in generated function's metadata",
        },
        default_random => {
            schema => 'bool',
            summary => "Supply default 'random' ".
                "value in generated function's metadata",
        },
        default_result_limit => {
            schema => 'int',
            summary => "Supply default 'result_limit' ".
                "value in generated function's metadata",
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

For example, if search term is 'pine' and field value is 'green pineapple',
search will match if word_search=false, but won't match under word_search.

This will not have effect under 'custom_search'.

_
        },
        default_arg_values => {
            schema => 'hash',
            summary => "Specify defaults for generated function's arguments",
            description => <<'_',

Can be used to supply default filters, e.g.

    # limit years for credit card expiration date
    { "year.min" => $curyear, "year.max" => $curyear+10, }

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

Code will be supplied ($r, $q, $opts) where $r is the record (hashref), $q is
the search term (from the function argument 'q'), and $opts is {ci=>0|1}. Code
should return true if record matches search term.

_
        },
        custom_filters => {
            schema => [hash => {of=>['hash*' => {keys=>{
                'code'=>'code*', 'meta'=>'hash*'}}]}],
            summary => 'Supply custom filters',
            description => <<'_',

A hash of filter name and definitions. Filter name will be used as generated
function's argument and must not clash with other arguments. Filter definition
is a hash containing these keys: *meta* (hash, argument metadata), *code*,
*fields* (array, list of table fields related to this field).

Code will be called for each record to be filtered and will be supplied ($r, $v,
$opts) where $v is the filter value (from the function argument) and $r the
hashref record value. $opts is currently empty. Code should return true if
record satisfies the filter.

_
        },
        hooks => {
            schema      => [hash => {of=>'code*'}],
            summary     => 'Supply hooks',
            description => <<'_',

You can instruct the generated function to execute codes in various stages by
using hooks. Currently available hooks are: `before_parse_query`,
`after_parse_query`, `before_fetch_data`, `after_fetch_data`, `before_return`.
Hooks will be passed the function arguments as well as one or more additional
ones. All hooks will get `_stage` (name of stage) and `_func_res` (function
arguments, but as hash reference so you can modify it). `after_parse_query` and
later hooks will also get `_parse_res` (parse result). `before_fetch_data` and
later will also get `_query`. `after_fetch_data` and later will also get
`_data`. `before_return` will also get `_func_res` (the enveloped response to be
returned to user).

Hook should return nothing or a false value on success. It can abort execution
of the generated function if it returns an envelope response (an array). On that
case, the function will return with this return value.

_
        },
    },
};
sub gen_read_table_func {
    require Data::Clone;

    my %args = @_;

    # XXX schema
    my ($uqname, $package);
    my $fqname = $args{name};
    return [400, "Please specify name"] unless $fqname;
    my @caller = caller();
    if ($fqname =~ /(.+)::(.+)/) {
        $package = $1;
        $uqname  = $2;
    } else {
        $package = $args{package} // $caller[0];
        $uqname  = $fqname;
        $fqname  = "$package\::$uqname";
    }
    my $table_data = $args{table_data}
        or return [400, "Please specify table_data"];
    __is_aoa($table_data) or __is_aoh($table_data) or
        reftype($table_data) eq 'CODE'
            or return [400, "Invalid table_data: must be AoA/AoH/function"];
    my $table_spec = $args{table_spec}
        or return [400, "Please specify table_spec"];
    ref($table_spec) eq 'HASH'
        or return [400, "Invalid table_spec: must be a hash"];
    $table_spec->{fields} or
        return [400, "Invalid table_spec: fields not specified"];
    ref($table_spec->{fields}) eq 'HASH' or
        return [400, "Invalid table_spec: fields must be hash"];
    $table_spec->{pk} or
        return [400, "Invalid table_spec: pk not specified"];
    exists($table_spec->{fields}{ $table_spec->{pk} }) or
        return [400, "Invalid table_spec: pk not in fields"];

    # duplicate and make each field's schema normalized
    $table_spec = Data::Clone::clone($table_spec);
    for my $fspec (values %{$table_spec->{fields}}) {
        $fspec->{schema} //= 'any';
        $fspec->{schema} = __parse_schema($fspec->{schema});
    }
    #  make each custom filter's schema normalized
    my $cff = $args{custom_filters} // {};
    while (my ($cfn, $cf) = each %$cff) {
        $cf->{meta} //= {};
        $cf->{meta}{schema} //= 'any';
        $cf->{meta}{schema} = __parse_schema($cf->{meta}{schema});
    }

    my $dav = $args{default_arg_values} // {};
    my $opts = {
        summary                    => $args{summary},
        description                => $args{description},
        langs                      => $args{langs} // ['en_US'],
        default_detail             => $args{default_detail},
        default_with_field_names   => $args{default_with_field_names},
        default_fields             => $args{default_fields},
        default_sort               => $args{default_sort},
        default_random             => $args{default_random},
        default_result_limit       => $args{default_result_limit},
        enable_search              => $args{enable_search} // 1,
        custom_search              => $args{custom_search},
        word_search                => $args{word_search},
        case_insensitive_search    => $args{case_insensitive_search} // 1,
        (map { ("default_$_" => $dav->{$_}) } keys %$dav),
        custom_filters             => $cff,
        hooks                      => $args{hooks} // {},
    };

    my $res;
    $res = _gen_meta($table_spec, $opts);
    return err(500, "Can't generate meta", $res) unless $res->[0] == 200;
    my $func_meta = $res->[2];

    $res = _gen_func($table_spec, $opts, $table_data, $func_meta);
    return err(500, "Can't generate func", $res) unless $res->[0] == 200;
    my $func = $res->[2];

    if ($args{install} // 1) {
        no strict 'refs';
        no warnings;
        $log->tracef("Installing function as %s ...", $fqname);
        *{ $fqname } = $func;
        ${$package . "::SPEC"}{$uqname} = $func_meta;
    }

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
     name        => 'list_countries',
     summary     => 'func summary',     # opt
     description => 'func description', # opt
     table_data  => $countries,
     table_spec  => {
         summary => 'List of countries',
         fields => {
             id => {
                 schema => 'str*',
                 summary => 'ISO 2-letter code for the country',
                 pos => 0,
                 sortable => 1,
             },
             eng_name => {
                 schema => 'str*',
                 summary => 'English name',
                 pos => 1,
                 sortable => 1,
             },
             ind_name => {
                 schema => 'str*',
                 summary => 'Indonesian name',
                 pos => 2,
                 sortable => 1,
             },
             tags => {
                 schema => 'array*',
                 summary => 'Keywords/tags',
                 pos => 3,
                 sortable => 0,
             },
         },
         pk => 'id',
     },
 );
 die "Can't generate function: $res->[0] - $res->[1]" unless $res->[0] == 200;

 Perinci::CmdLine->new(url=>'/main/list_countries')->run;

Now you can do:

 # list all countries, by default only PK field is shown
 $ list_countries.pl --format=text-simple
 cn
 id
 sg
 us

 # show as json, randomize order
 $ list_countries.pl --format=json --random
 ["id","us","sg","cn"]

 # only list countries which are tagged as 'tropical', sort by ind_name field in
 # descending order, show all fields (--detail)
 $ list_countries.pl --detail --sort -ind_name --tags-has '[tropical]'
 .---------------------------------------------.
 | eng_name  | id | ind_name  | tags           |
 +-----------+----+-----------+----------------+
 | Singapore | sg | Singapura | tropical       |
 | Indonesia | id | Indonesia | bali, tropical |
 '-----------+----+-----------+----------------'

 # show only certain fields, limit number of records, return in YAML format
 $ list_countries.pl --fields '[id, eng_name]' --result-limit 2 --format=yaml
 - 200
 - OK
 -
   - id: cn
     eng_name: China
   - id: id
     eng_name: Indonesia


=head1 DESCRIPTION

This module is useful when you want to expose a table data (an array of
hashrefs, an array of arrays, or external data like a SQL table) as an API
function. This module will generate a function (along with its L<Rinci>
metadata) that accepts arguments for specifying fields, filtering, sorting, and
paging. The resulting function can then be run via command-line using
L<Perinci::CmdLine> (as demonstrated in Synopsis), or served via HTTP using
L<Perinci::Access::HTTP::Server>, or consumed normally by Perl programs.

This module uses L<Log::Any> for logging.


=head1 CAVEATS

It is often not a good idea to expose your database schema directly as API.


=head1 FAQ

=head2 I want my function to accept additional arguments.

You can add arguments to the metadata by yourself, e.g.:

 our %SPEC;
 gen_read_table_func(name => 'myfunc', ...);
 $SPEC{myfunc}{args}{add1} = {...};

As for the implementation, you can specify hooks to do things with the extra
arguments.


=head1 SEE ALSO

L<Rinci>

L<Perinci::CmdLine>

=cut
