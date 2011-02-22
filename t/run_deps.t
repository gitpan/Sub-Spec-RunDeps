#!perl

use 5.010;
use strict;
use warnings;
use Log::Any '$log';
use Test::More 0.96;

use Capture::Tiny qw(capture);
use Sub::Spec::RunDeps qw(run_deps);

package Foo;
use 5.010;
our %SPEC;

$SPEC{a} = {depends=>{sub=>"Foo::b", args=>{alt=>1}}};
sub a {
    my %args=@_;
    print "A".($args{alt} ? "x" : "");
    [200, "OK",
     $args{alt} ? ($args{alt} > 1 ? "apple" : "apricot") :
         "avocado"];
}
$SPEC{b} = {depends=>"Foo::c & Foo::d", args=>{alt=>'int'}};
sub b {
    my %args=@_;
    print "B".($args{alt} ? "x" : "");
    [200, "OK", $args{alt} ? "blueberry" : "banana"];
}
$SPEC{c} = {depends=>{and=>["Foo::d", "Foo::e"]}, args=>{alt=>'int'}};
sub c {
    my %args=@_;
    print "C".($args{alt} ? "x" : "");
    [200, "OK", "cherry"];
}
$SPEC{d} = {depends=>"Foo::e"};
sub d {
    print "D";
    [200, "OK", "date"];
}
$SPEC{e} = {};
sub e {
    print "E";
    [304, "OK", "eggplant"];
}

$SPEC{read_ctx} = {depends=>"Foo::a"};
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

# for testing ignore_errors
$SPEC{i} = {depends=>"Foo::j"};
sub i {
    print "I";
    [304, "OK"];
}
$SPEC{j} = {};
sub j {
    print "J";
    [500, "Failed"];
}

$SPEC{circ1} = {depends=>"circ2"};
sub circ1 {
    [200, "OK"];
}
$SPEC{circ2} = {depends=>"circ1"};
sub circ2 {
    [200, "OK"];
}

$SPEC{z} = {depends=>"nonexisting"};
sub z {
    [200, "OK"];
}

package Bar;
sub a { [200, "OK"] }
sub b { [200, "OK"] }

package main;

test_rdeps(
    name          => 'single item',
    args          => {item=>'Foo::a'},
    status        => 200,
    num_items     => 5, num_failed => 0, num_success => 5,
    output_re     => qr/^EDCBxA$/,
);
test_rdeps(
    name          => 'single item (2)',
    args          => {item=>'Foo::e'},
    status        => 200,
    num_items     => 1, num_failed => 0, num_success => 1,
    output_re     => qr/^E$/,
);
test_rdeps(
    name          => 'multiple items',
    args          => {items=>['Foo::d', {sub=>'Foo::c'}]},
    status        => 200,
    num_items     => 3, num_failed => 0, num_success => 3,
    output_re     => qr/^EDC$/,
);

# a won't be called with alt=>1 since it doesn't specify alt as args
test_rdeps(
    name          => 'common_args',
    args          => {item=>'Foo::a', common_args=>{alt=>1}},
    status        => 200,
    num_items     => 5, num_failed => 0, num_success => 5,
    output_re     => qr/^EDCxBxA$/,
);

test_rdeps(
    name          => 'reverse_order',
    args          => {item=>'Foo::a', reverse_order=>1},
    status        => 200,
    num_items     => 5, num_failed => 0, num_success => 5,
    output_re     => qr/^ABxCDE$/,
);

test_rdeps(
    name          => 'dry_run',
    args          => {item=>'Foo::a', dry_run=>1},
    status        => 200,
    output_re     => qr/^$/,
    check_res     => sub {
        my $res = shift;
        $res->[2]{items} && $res->[2]{deps} && $res->[2]{sched};
    },
);

test_rdeps(
    name          => 'cant resolve deps (circular)',
    args          => {item=>'Foo::circ1'},
    status        => 412,
);
test_rdeps(
    name          => 'cant resolve deps (missing dep)',
    args          => {item=>'Foo::z'},
    status        => 412,
);

test_rdeps(
    name          => 'ignore_errors off',
    args          => {item=>'Foo::i'},
    status        => 500,
);
test_rdeps(
    name          => 'ignore_errors on',
    args          => {item=>'Foo::i', ignore_errors=>1},
    status        => 200,
    num_items     => 2, num_failed => 1, num_success => 1,
);
test_rdeps(
    name          => 'ignore_errors on (all failed)',
    args          => {item=>'Foo::j', ignore_errors=>1},
    status        => 500,
    num_items     => 1, num_failed => 1, num_success => 0,
);

test_rdeps(
    name          => 'ctx: sub_res',
    args          => {item=>'Foo::read_ctx'},
    status        => 200,
    num_items     => 6, num_failed => 0, num_success => 6,
);

test_rdeps(
    name          => 'exclude 1',
    args          => {item=>'Foo::a',
                      exclude=>['Foo::e', {sub=>'Foo::d'}]},
    status        => 200,
    num_items     => 3, num_failed => 0, num_success => 3,
    output_re     => qr/^CBxA$/,
);

# if we exclude an item, all its dependencies are also excluded
test_rdeps(
    name          => 'exclude 2',
    args          => {item=>'Foo::a',
                      exclude=>['Foo::b']},
    status        => 200,
    num_items     => 1, num_failed => 0, num_success => 1,
    output_re     => qr/^A$/,
);

test_rdeps(
    name          => 'specs hashref',
    args          => {item=>'Bar::a',
                      specs=>{Bar=>{a=>{depends=>"Bar::b"}, b=>{}}}},
    status        => 200,
    num_items     => 2, num_failed => 0, num_success => 2,
);
test_rdeps(
    name          => 'specs coderef',
    args          => {item=>'Bar::a',
                      specs=>sub{
                          my %args=@_;
                          if ($args{module} eq 'Bar') {
                              if ($args{sub} eq 'a') {
                                  return {depends=>"Bar::b"};
                              } elsif ($args{sub} eq 'b') {
                                  return {};
                              } else {
                                  return;
                              }
                          } else {
                              return;
                          }
                      }},
    status        => 200,
    num_items     => 2, num_failed => 0, num_success => 2,
);

test_rdeps(
    name          => 'after_item (repeat every 200 item twice)',
    args          => {item=>'Foo::a',
                      after_item => sub {
                          my %args = @_;
                          my $res      = $args{res};
                          my $ref_i    = $args{ref_i};
                          my $item     = $args{item};
                          my $ctx      = $args{ctx};
                          ${$ref_i}-- if $res->[0] == 200 &&
                              !$ctx->stash("repeated_$item->{key}", 1);
                      },
                  },
    status        => 200,
    num_items        => 9, num_failed       => 0, num_success        => 9,
    num_unique_items => 5, num_uniquefailed => 0, num_unique_success => 5,
    output_re     => qr/^EDDCCBxBxAA$/,
);

test_rdeps(
    name          => 'before_item (skip every other step)',
    args          => {item=>'Foo::a',
                      before_item => sub {
                          my %args = @_;
                          my $ref_i    = $args{ref_i};
                          ${$ref_i}++;
                      },
                  },
    status        => 200,
    num_items        => 2, num_failed       => 0, num_success        => 2,
    num_unique_items => 2, num_uniquefailed => 0, num_unique_success => 2,
    output_re     => qr/^DBx$/,
);

test_rdeps(
    name          => 'demo: insert a new item after b',
    args          => {item=>'Foo::a',
                      ignore_errors=>1,
                      after_item => sub {
                          my %args  = @_;
                          my $item  = $args{item};
                          my $items = $args{items};
                          my $ref_i = $args{ref_i};
                          return unless $item->{key} eq 'SUB:Foo::c';
                          push @{$items}, {
                              item => {sub=>'Foo::j'},
                              key  => 'SUB:Foo::j',
                              spec => $Foo::SPEC{j},
                          };
                      },
                  },
    status        => 200,
    num_items        => 6, num_failed       => 1, num_success        => 5,
    num_unique_items => 6, num_uniquefailed => 1, num_unique_success => 5,
    output_re     => qr/^EDCBxAJ$/,
);

# XXX test load

done_testing();

sub test_rdeps {
    my (%args) = @_;

    my ($stdout, $stderr);
    my $res;
    ($stdout, $stderr) = capture {
        $res = run_deps(load=>0, %{$args{args}});
    };

    subtest $args{name} => sub {
        if ($args{status}) {
            is($res->[0], $args{status}, "return status = $args{status}") or
                do { diag explain $res; last };
        }
        if (defined $args{num_items}) {
            is($res->[2]{num_items}, $args{num_items}, "num_items");
        }
        if (defined $args{num_unique_items}) {
            is($res->[2]{num_unique_items}, $args{num_unique_items},
               "num_unique_items");
        }
        if (defined $args{num_failed}) {
            is($res->[2]{num_failed}, $args{num_failed}, "num_failed");
        }
        if (defined $args{num_unique_failed}) {
            is($res->[2]{num_unique_failed}, $args{num_unique_failed},
               "num_unique_failed");
        }
        if (defined $args{num_success}) {
            is($res->[2]{num_success}, $args{num_success}, "num_success");
        }
        if (defined $args{num_unique_success}) {
            is($res->[2]{num_unique_success}, $args{num_unique_success},
               "num_unique_success");
        }

        if ($args{output_re}) {
            like($stdout // "", $args{output_re}, "output_re")
                or diag("output is $stdout");
        }

        if ($args{check_res}) {
            ok($args{check_res}->($res), "check_res");
        }
    };
}

