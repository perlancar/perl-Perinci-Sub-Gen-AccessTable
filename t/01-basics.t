#!perl

use 5.010;
use strict;
use warnings;
use Log::Any '$log';
use Test::More 0.96;

use Array::Functions::Undoable;
use Capture::Tiny qw(capture);
use File::chdir;
use File::Temp qw(tempdir);
use Sub::Spec::Runner;

my $tempdir = tempdir(CLEANUP => 1);
chdir $tempdir; # so it can't be deleted

package Foo;
use 5.010;
our %SPEC;

$SPEC{a} = {deps=>{run_sub=>"Foo::b"}, args=>{alt=>"bool", alt2=>"bool"}};
sub a {
    my %args=@_;
    print "A".($args{alt} ? "x" : "").($args{alt2} ? "y" : "");
    [200, "OK", "apple"];
}
$SPEC{b} = {deps=>{all=>[{run_sub=>"Foo::c"},{run_sub=>"Foo::d"}]}};
sub b {
    my %args=@_;
    print "B".($args{alt} ? "x" : "").($args{alt2} ? "y" : "");
    [200, "OK", "banana"];
}
$SPEC{c} = {deps=>{all=>[{run_sub=>"Foo::d"}, {run_sub=>"Foo::e"}]},
            args=>{alt=>"bool"}};
sub c {
    my %args=@_;
    print "C".($args{alt} ? "x" : "").($args{alt2} ? "y" : "");
    [200, "OK", "cherry"];
}
$SPEC{d} = {deps=>{run_sub=>"Foo::e"}, args=>{}}; # won't supplied with args
sub d {
    my %args=@_;
    print "D".($args{alt} ? "x" : "");
    [200, "OK", "date"];
}
$SPEC{e} = {};
sub e {
    print "E";
    [304, "OK", "eggplant"];
}

# for testing specifying dependency to sub with arg
$SPEC{f} = {deps=>{all=>[
    {run_sub=>['Foo::d', {alt=>0}]},
    {run_sub=>['Foo::d', {alt=>1}]},]},
};
sub f {
    print "F";
    [200, "OK", "farkleberry"];
}

$SPEC{read_ctx} = {deps=>{run_sub=>"Foo::a"}};
sub read_ctx {
    my %args=@_;
    my $ctx=$args{-ctx};
    my $res_a = $ctx->sub_res("Foo::a");
    #use Data::Dump qw(dump); open F, ">>/tmp/ctx"; print F dump($ctx); close F;
    if ($ctx->sub_res("Foo::a")->[2] eq 'avocado' &&
            $ctx->sub_res("Foo::b")->[2] eq 'blueberry') {
        return [200, "OK"];
    } else {
        return [500, "Failed"];
    }
}

# for testing stop_on_sub_errors
$SPEC{i} = {deps=>{run_sub=>"Foo::j"}};
sub i {
    print "I";
    [304, "OK"];
}
$SPEC{j} = {};
sub j {
    print "J";
    [450, "Failed"];
}

$SPEC{circ1} = {deps=>{run_sub=>"Foo::circ2"}};
sub circ1 {
    [200, "OK"];
}
$SPEC{circ2} = {deps=>{run_sub=>"Foo::circ1"}};
sub circ2 {
    [200, "OK"];
}

$SPEC{z} = {deps=>{run_sub=>"nonexisting"}};
sub z {
    [200, "OK"];
}

$SPEC{unmet} = {deps=>{code=>sub{0}}};
sub unmet {
    [200, "OK"];
}

# for testing dry_run
$SPEC{pure1} = {features=>{pure=>1}};
sub pure1 {
    my %args = @_;
    print "pure1";
    [200, "OK"];
}
$SPEC{dry1} = {features=>{dry_run=>1}, deps=>{run_sub=>'Foo::dry2'}};
sub dry1 {
    my %args = @_;
    print "dry1" unless $args{-dry_run};
    [200, "OK"];
}
$SPEC{dry2} = {features=>{dry_run=>1}, deps=>{run_sub=>'Foo::pure1'}};
sub dry2 {
    my %args = @_;
    print "dry2" unless $args{-dry_run};
    [200, "OK"];
}

# for testing undo: undo1
our $DATA = "what is the meaning of life?";
our $ORIG_DATA = $DATA;
$SPEC{rev1} = {summary=>"Double value in \$DATA",
                features=>{reverse=>1}, deps=>{run_sub=>'Foo::undo1'}};
sub rev1 {
    my %args    = @_;
    my $reverse = $args{-reverse};
    print "rev1";
    if ($reverse) { $DATA /= 2 } else { $DATA *= 2 }
    [200, "OK"];
}
$SPEC{undo1} = {summary=>"Replace content of \$DATA with '42'",
                features=>{undo=>1}, deps=>{run_sub=>'Foo::pure1'}};
sub undo1 {
    my %args  = @_;
    my $undo_action = $args{-undo_action};
    my $undo_data   = $args{-undo_data};
    print "undo1";
    if ($undo_action eq 'undo') {
        return [304, "Not modified"] unless $undo_data->[0];
        $DATA = $undo_data->[0];
        return [200, "OK"];
    } else {
        # warning: if done twice, previous undo data is overwritten
        my $save = $DATA;
        $DATA = 42;
        [200, "OK", undef, {undo_data=>[$save]}];
    }
}

$SPEC{naked1} = {result_naked=>1};
sub naked1 { 10 }

package Bar;
sub a { [200, "OK"] }
sub b { [200, "OK"] }

package main;

our %SPEC;
$SPEC{x} = {};
sub x {}

my $r = Sub::Spec::Runner->new;
$r->load_modules(0);
$r->add('Foo::e');
is(scalar(@{$r->_queue}), 1, "add() adds one item to _queue");
is(scalar(keys %{$r->_queue_idx}), 1, "add() adds one item to _queue_idx");
$r->empty();
is(scalar(@{$r->_queue}), 0, "empty() empties _queue");
is(scalar(keys %{$r->_queue_idx}), 0, "empty() empties _queue_idx");

test_run(
    name          => 'normalize subname (add(x) becomes add(main::x))',
    subs          => ['x'],
    test_before_run => sub {
        my ($runner) = @_;
        $runner->_queue->[0]{subname} eq 'main::x';
    }
);
test_run(
    name          => 'normalize subname (add(::x) becomes add(main::x))',
    subs          => ['::x'],
    test_before_run => sub {
        my ($runner) = @_;
        $runner->_queue->[0]{subname} eq 'main::x';
    }
);

test_run(
    name          => 'no subs',
    subs          => [],
    status        => 412,
);
test_run(
    name          => 'single sub',
    subs          => ['Foo::a'],
    status        => 200,
    num_runs      => 5, num_success_runs => 5, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 5, num_failed_subs  => 0,
    num_run_subs  => 5, num_skipped_subs => 0,
    output_re     => qr/^EDCBA$/,
    test_after_run => sub {
        my ($runner) = @_;

        is($runner->_i, 5, "_i");
        is_deeply($runner->last_res, [200, "OK", "apple"], "last_res()")
            or diag explain $runner->last_res;

        my $items = [map {$_->{subname}}
                         @{ $runner->_find_items_and_dependants('Foo::c') }];
        is_deeply($items,
                  ['Foo::c', 'Foo::b', 'Foo::a'],
                  "_find_items_and_dependants 1")
            or diag explain $items;
        my $a = $runner->stash("a");
        ok(!$a, "stash default to undef");
        $a = $runner->stash("a", 1);
        ok(!$a, "stash returns old value");
        $a = $runner->stash("a");
        is($a, 1, "stash can set value");
    },
);

test_run(
    name          => 'single sub (no dependency)',
    subs          => ['Foo::e'],
    status        => 200,
    num_runs      => 1, num_success_runs => 1, num_failed_runs  => 0,
    num_subs      => 1, num_success_subs => 1, num_failed_subs  => 0,
    num_run_subs  => 1, num_skipped_subs => 0,
    output_re     => qr/^E$/,
);

test_run(
    name          => 'multiple subs',
    subs          => ['Foo::d', 'Foo::c'],
    status        => 200,
    num_runs      => 3, num_success_runs => 3, num_failed_runs  => 0,
    num_subs      => 3, num_success_subs => 3, num_failed_subs  => 0,
    num_run_subs  => 3, num_skipped_subs => 0,
    output_re     => qr/^EDC$/,
);

test_run(
    name          => 'common_args',
    subs          => ['Foo::a'],
    common_args   => {alt=>1},
    status        => 200,
    num_runs      => 5, num_success_runs => 5, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 5, num_failed_subs  => 0,
    num_run_subs  => 5, num_skipped_subs => 0,
    output_re     => qr/^EDCxBxAx$/,
);

test_run(
    name          => 'per-sub args (alt2 given to a, '.
        'not to b/c due to implicit add)',
    subs          => ['Foo::a'],
    common_args   => {alt=>1},
    sub_args      => [{alt2=>1}],
    status        => 200,
    output_re     => qr/^EDCxBxAxy$/,
);
test_run(
    name          => 'per-sub args (alt2 given to b)',
    subs          => ['Foo::b'],
    common_args   => {alt=>1},
    sub_args      => [{alt2=>1}],
    status        => 200,
    output_re     => qr/^EDCxBxy$/,
);
test_run(
    name          => 'per-sub args (alt2 given to c)',
    subs          => ['Foo::c'],
    common_args   => {alt=>1},
    sub_args      => [{alt2=>1}],
    status        => 200,
    output_re     => qr/^EDCxy$/,
);

test_run(
    name          => 'unmet dependencies',
    subs          => ['Foo::unmet'],
    add_dies      => 1,
);
test_run(
    name          => 'cant resolve deps (circular)',
    subs          => ['Foo::circ1'],
    status        => 412,
);
test_run(
    name          => 'cant resolve deps (missing dep)',
    subs          => ['Foo::z'],
    add_dies      => 1,
);

test_run(
    name          => 'dep to sub + arg, allow_add_same_sub',
    runner_args   => {allow_add_same_sub=>1},
    subs          => ['Foo::f'],
    status        => 200,
    num_runs      => 4, num_success_runs => 4, num_failed_runs  => 0,

    num_subs      => 3, num_success_subs => 3, num_failed_subs  => 0,
    num_run_subs  => 3, num_skipped_subs => 0,

    num_items     => 4, num_success_items => 4, num_failed_items => 0,
    num_run_items => 4, num_skipped_items => 0,

    output_re     => qr/^EDDxF$/,
);
test_run(
    name          => 'stop_on_sub_errors on',
    subs          => ['Foo::i'],
    status        => 500,
    num_runs      => 1, num_success_runs => 0, num_failed_runs  => 1,
    num_subs      => 2, num_success_subs => 0, num_failed_subs  => 1,
    num_run_subs  => 1, num_skipped_subs => 1,
    output_re     => qr/J/,
);
test_run(
    name          => 'run option: use_last_res',
    subs          => ['Foo::i'],
    run_opts      => {use_last_res=>1},
    status        => 450,
    #output_re     => qr/J/,
);
test_run(
    name          => 'stop_on_sub_errors off',
    subs          => ['Foo::i'],
    stop_on_sub_errors => 0,
    status        => 200,
    num_runs      => 2, num_success_runs => 1, num_failed_runs  => 1,
    num_subs      => 2, num_success_subs => 1, num_failed_subs  => 1,
    num_run_subs  => 2, num_skipped_subs => 0,
    output_re     => qr/JI/,
);
test_run(
    name          => 'stop_on_sub_errors off (all failed)',
    subs          => ['Foo::j'],
    stop_on_sub_errors => 0,
    status        => 500,
    num_runs      => 1, num_success_runs => 0, num_failed_runs  => 1,
    num_subs      => 1, num_success_subs => 0, num_failed_subs  => 1,
    num_run_subs  => 1, num_skipped_subs => 0,
    output_re     => qr/J/,
);

test_run(
    name          => 'pre_run returns false',
    runner_args   => {_pre_run=>sub {0}},
    subs          => ['Foo::a'],
    status        => 412,
);
test_run(
    name          => 'exception in pre_run trapped',
    runner_args   => {_pre_run=>sub {die}},
    subs          => ['Foo::a'],
    status        => 412,
);

test_run(
    name          => 'post_run returns false',
    runner_args   => {_post_run=>sub {0}},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 5, num_success_runs => 5, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 5, num_failed_subs  => 0,
    num_run_subs  => 5, num_skipped_subs => 0,
    #output_re     => qr/EDCBA/,
);
test_run(
    name          => 'exception in post_run trapped',
    runner_args   => {_post_run=>sub {die}},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 5, num_success_runs => 5, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 5, num_failed_subs  => 0,
    num_run_subs  => 5, num_skipped_subs => 0,
    #output_re     => qr/EDCBA/,
);

test_run(
    name          => 'pre_sub',
    runner_args   => {_pre_sub=>sub {
                          my($self, $subname) = @_;
                          $subname eq 'Foo::c' ? 0:1;
                      }},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 2, num_success_runs => 2, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 2, num_failed_subs  => 0,
    num_run_subs  => 2, num_skipped_subs => 3,
);
test_run(
    name          => 'exception in pre_sub trapped',
    runner_args   => {_pre_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') { die }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 2, num_success_runs => 2, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 2, num_failed_subs  => 0,
    num_run_subs  => 2, num_skipped_subs => 3,
);

test_run(
    name          => 'post_sub',
    runner_args   => {_post_sub=>sub {
                          my($self, $subname) = @_;
                          $subname eq 'Foo::c' ? 0:1;
                      }},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 3, num_success_runs => 3, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 3, num_failed_subs  => 0,
    num_run_subs  => 3, num_skipped_subs => 2,
);
test_run(
    name          => 'exception in post_sub trapped',
    runner_args   => {_post_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') { die }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 500,
    num_runs      => 3, num_success_runs => 3, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 3, num_failed_subs  => 0,
    num_run_subs  => 3, num_skipped_subs => 2,
);

test_run(
    name          => 'skip in pre_sub',
    runner_args   => {_pre_sub=>sub {
                          my($self, $subname, $args) = @_;
                          if ($subname eq 'Foo::c') {
                              $self->skip('Foo::a');
                              $self->skip(qr/[cb]/);
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    num_runs      => 2, num_success_runs => 2, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 2, num_failed_subs  => 0,
    num_run_subs  => 2, num_skipped_subs => 3,
);
test_run(
    name          => 'skip in post_sub',
    runner_args   => {_post_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') {
                              $self->skip('Foo::a');
                              $self->skip(qr/[cb]/);
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    num_runs      => 3, num_success_runs => 3, num_failed_runs  => 0,
    num_subs      => 5, num_success_subs => 3, num_failed_subs  => 0,
    num_run_subs  => 3, num_skipped_subs => 2,
);

test_run(
    name          => 'jump',
    runner_args   => {_post_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') {
                              $self->jump('Foo::a');
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    output_re     => qr/^EDCAB$/,
);
test_run(
    name          => 'jump(unknown) -> dies',
    runner_args   => {_post_sub=>sub {
                          my($self, $subname) = @_;
                          $self->jump('xxx');
                          1;
                      }},
    subs          => ['Foo::e'],
    status        => 500,
);

test_run(
    name          => 'skip_branch()',
    runner_args   => {_pre_sub=>sub {
                          my($self, $subname) = @_;
                          if ($subname eq 'Foo::c') {
                              $self->skip_branch('Foo::c');
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    output_re     => qr/^ED$/,
);

my $once;
test_run(
    name          => 'repeat_branch()',
    runner_args   => {_pre_sub=>sub {
                          my($self, $subname, $args) = @_;
                          if ($subname eq 'Foo::c' && !$once++) {
                              $self->repeat_branch('Foo::e');
                          }
                          1;
                      }},
    subs          => ['Foo::a'],
    status        => 200,
    output_re     => qr/^EDCBAED$/,
);

test_run(
    name          => 'result()',
    subs          => ['Foo::d'],
    status        => 200,
    test_after_run => sub {
        my ($runner) = @_;
        is_deeply($runner->result('Foo::e'), [304, "OK", "eggplant"],
                  "result(e)");
        is_deeply($runner->result('Foo::d'), [200, "OK", "date"],
                  "result(d)");
        #eval { $runner->result('Foo::xxx') };
        #ok($@, "result(unknown) -> dies");
    },
);

test_run(
    name          => 'is_done()',
    subs          => ['Foo::d'],
    status        => 200,
    test_after_run => sub {
        my ($runner) = @_;
        ok($runner->is_done('Foo::d'));
        ok($runner->is_done('Foo::e'));
        ok(!$runner->is_done('Foo::c'));
    },
);

test_run(
    name          => 'naked_result',
    subs          => ['Foo::naked1'],
    status        => 200,
    num_runs      => 1, num_success_runs => 1, num_failed_runs  => 0,
    num_subs      => 1, num_success_subs => 1, num_failed_subs  => 0,
    num_run_subs  => 1, num_skipped_subs => 0,
);

test_run(
    name          => 'dry_run: all subs must have required features',
    runner_args   => {dry_run=>1},
    subs          => ['Foo::a'],
    status        => 412,
);
test_run(
    name          => 'dry_run: disabled',
    runner_args   => {dry_run=>0},
    subs          => ['Foo::dry1'],
    status        => 200,
    output_re     => qr/^pure1dry2dry1$/,
);
test_run(
    name          => 'dry_run: enabled',
    runner_args   => {dry_run=>1},
    subs          => ['Foo::dry1'],
    status        => 200,
    output_re     => qr/^pure1$/,
);

test_run(
    name          => 'undo: all subs must have required features (0)',
    runner_args   => {undo=>0},
    subs          => ['Foo::a'],
    status        => 412,
);
test_run(
    name          => 'undo: all subs must have required features (1)',
    runner_args   => {undo=>1},
    subs          => ['Foo::a'],
    status        => 412,
);
test_run(
    name          => 'undo: 0',
    runner_args   => {undo=>0,
                      _post_sub => sub {
                          my ($self, $subname) = @_;
                          if ($subname eq 'Foo::pure1') {
                              is($Foo::DATA, $Foo::ORIG_DATA,
                                 'after pure1, DATA still unchanged');
                          } elsif ($subname eq 'Foo::undo1') {
                              is($Foo::DATA, 42,
                                 'after undo1, DATA becomes 42');
                          } elsif ($subname eq 'Foo::rev1') {
                              is($Foo::DATA, 84,
                                 'after rev1, DATA becomes 84');
                          }
                          1;
                      }},
    subs          => ['Foo::rev1'],
    status        => 200,
    output_re     => qr/^pure1undo1rev1$/,
    test_after_run=> sub {
        ok((-f "$tempdir/Foo.undo1.yaml"), "undo data saved");
    },
);
test_run(
    name          => 'undo: 1',
    runner_args   => {undo=>1,
                      order_before_run=>-1,
                      _post_sub => sub {
                          my ($self, $subname) = @_;
                          if ($subname eq 'Foo::rev1') {
                              is($Foo::DATA, 42,
                                 'after rev1 (-reverse=>1), DATA becomes 42');
                          } elsif ($subname eq 'Foo::undo1') {
                              is($Foo::DATA, $Foo::ORIG_DATA,
                                 'after undo1 (-undo=>1), DATA restored');
                          } elsif ($subname eq 'Foo::pure1') {
                              is($Foo::DATA, $Foo::ORIG_DATA,
                                 'after rev1, DATA unchanged');
                          }
                          1;
                      }},
    subs          => ['Foo::rev1'],
    status        => 200,
    output_re     => qr/^rev1undo1pure1$/,
    test_after_run=> sub {
        ok(!(-f "$tempdir/Foo.undo1.yaml"), "undo data file removed");
    },
);

# test multiple args for one sub, get_sub_undo_data()
my $ary = [qw/a b/];
test_run(
    name          => 'undo file multiple args (run 1)',
    subs          => ['Array::Functions::Undoable::afu'],
    sub_args      => [{op=>"pop", ary=>$ary}],
    runner_args   => {undo=>0},
    run_opts      => {use_last_res=>1},
    status        => 200,
    test_after_run=> sub {
        ok((-f "$tempdir/Array.Functions.Undoable.afu.yaml"),
           "undo data file created");
        is_deeply($ary, [qw/a/], "ary");
    },
);
test_run(
    name          => 'undo file multiple args (run 2)',
    subs          => ['Array::Functions::Undoable::afu'],
    sub_args      => [{op=>"pop", ary=>$ary}],
    runner_args   => {undo=>0},
    run_opts      => {use_last_res=>1},
    status        => 200,
    test_after_run=> sub {
        is_deeply($ary, [qw//], "ary");
    },
);
my $recs;
test_run(
    name          => 'undo file multiple args (run 3)',
    subs          => ['Array::Functions::Undoable::afu'],
    sub_args      => [{op=>"pop", ary=>$ary}],
    runner_args   => {undo=>0},
    run_opts      => {use_last_res=>1},
    status        => 304,
    test_after_run=> sub {
        my ($runner) = @_;
        is_deeply($ary, [qw//], "ary");
        $recs = $runner->get_sub_undo_data("Array::Functions::Undoable::afu");
        is(scalar(@$recs), 2, "recs has 2 records");
        $runner->remove_sub_undo_data("Array::Functions::Undoable::afu");
        ok(!(-f "$tempdir/Array.Functions.Undoable.afu.yaml"),
           "undo data file removed by remove_sub_undo_data()");
    },
);
# manual undo
my $undo_data = [];
for my $rec (reverse @$recs) {
    for my $u (@{$rec->{undo_datas}}) {
        push @$undo_data, @$u;
    }
}
#use Data::Dump; dd $undo_data;
Array::Functions::Undoable::afu(
    op=>'pop', ary=>$ary, -undo_action => 'undo', -undo_data=>$undo_data);
is_deeply($ary, [qw/a b/], "recs can be used to undo");

DONE_TESTING:

if (Test::More->builder->is_passing) {
    diag "all tests successful, deleting undo data dir";
    $CWD = "/";
} else {
    # don't delete test data dir if there are errors
    diag "there are failing tests, not deleting undo data dir $tempdir";
}

# XXX test load_modules=1?

done_testing();

sub test_run {
    my (%args) = @_;

    subtest $args{name} => sub {

        my $runner = Sub::Spec::Runner->new(
            %{$args{runner_args} // {}},
            undo_data_dir => $tempdir,
        );
        $runner->load_modules(0);
        $runner->common_args($args{common_args}) if $args{common_args};
        $runner->stop_on_sub_errors($args{stop_on_sub_errors})
            if defined($args{stop_on_sub_errors});

        eval {
            for my $i (0..@{$args{subs}}-1) {
                $runner->add($args{subs}[$i], $args{sub_args}[$i]);
            }
        };
        my $eval_err = $@;
        diag "died during add(): $eval_err" if $eval_err;

        if ($args{add_dies}) {
            ok($eval_err, "add dies");
        }

        if ($args{test_before_run}) {
            ok($args{test_before_run}->($runner),
               "test_before_run");
        }

        my $res;
        if ($args{status}) {
            my %run_opts;
            if ($args{run_opts}) {
                $run_opts{$_} = $args{run_opts}{$_}
                    for keys %{$args{run_opts}};
            }
            if (defined($args{output_re})) {
                my ($stdout, $stderr) = capture {
                    $res = $runner->run(%run_opts);
                };
                #diag "stderr during run: $stderr" if $stderr;
                like($stdout // "", $args{output_re}, "output_re")
                    or diag("output is $stdout");
            } else {
                $res = $runner->run(%run_opts);
            }

            if ($args{status}) {
                is($res->[0], $args{status}, "return status = $args{status}") or
                    do { diag explain $res; last };
            }
        }

        for (qw(
                   num_success_runs
                   num_failed_runs
                   num_runs

                   num_success_subs
                   num_failed_subs
                   num_subs
                   num_run_subs
                   num_skipped_subs

                   num_success_items
                   num_failed_items
                   num_items
                   num_run_items
                   num_skipped_items
           )) {
            if (defined $args{$_}) {
                is($res->[2]{$_}, $args{$_}, $_);
            }
        }

        if ($args{test_res}) {
            ok($args{test_res}->($res), "test_res");
        }

        if ($args{test_after_run}) {
            $args{test_after_run}->($runner);
        }
    };
}

