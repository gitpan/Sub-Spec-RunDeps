package Sub::Spec::RunDeps;
BEGIN {
  $Sub::Spec::RunDeps::VERSION = '0.02';
}
# ABSTRACT: Run subroutine in order of its dependencies

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(run_deps);
our %SPEC;

use Sub::Spec::Utils; # temp, for _parse_schema

sub _parse_schema {
    Sub::Spec::Utils::_parse_schema(@_);
}

my $re_subname = qr/(?:\w+(?:::\w+)*)/;

# XXX code duplication with routine in _collect_dep
sub _item_keys {
    my ($item) = @_;
    my @k;
    if (!ref($item)) {
        if ($item =~ /^ $re_subname $/x) {
            push @k, "SUB:$item";
        } elsif ($item =~ /^ $re_subname (\s*&\s* $re_subname)+ $/x) {
            # parse A & B
            push @k, map {"SUB:$_"} split(/\s*&\s*/, $item);
        } elsif ($item =~ /^ $re_subname (\s*\|\s* $re_subname)+ $/x) {
            # parse A | B
        } else {
            # invalid syntax in item
        }
    } else {
        if ($item->{sub}) {
            push @k, "SUB:$item->{sub}";
        }
        # doesn't recognize any other depend clause yet
    }
    @k;
}

sub _collect_dep {
    my ($deps, $items, $item, $parent_args, $errors) = @_;
    # $deps is hoa for Algorithm::Dependency::Ordered, items store items (key
    # same as $deps), parent_args is args to run_deps, errors collect errors
    # when collecting deps.
    $log->tracef("-> _collect_dep(item=%s)", $item);

    if (!ref($item)) {
        my @i;
        if ($item =~ /^ $re_subname $/x) {
            push @i, {sub=>$item};
        } elsif ($item =~ /^ $re_subname (\s*&\s* $re_subname)+ $/x) {
            # parse A & B
            push @i, map({sub=>$_}, split(/\s*&\s*/, $item));
        } elsif ($item =~ /^ $re_subname (\s*\|\s* $re_subname)+ $/x) {
            # parse A | B
            push @$errors, "Sorry, I cannot handle A|B syntax yet: $item";
            return;
        } else {
            push @$errors, "Invalid item syntax: $item";
            return;
        }
        return map {_collect_dep($deps, $items, $_, $parent_args, $errors)} @i;
    }

    my @res;
    for my $ik (keys %$item) {
        if ($ik eq 'sub') {
            my $sub0 = $item->{sub};
            my ($module, $sub);
            if ($sub0 =~ /(.+)::(.+)/) {
                ($module, $sub) = ($1, $2);
            } else {
                $module = "main";
                $sub = $sub0;
            }
            # get spec
            my $spec;
            my $specs = $parent_args->{specs};
            if ($specs) {
                if (ref($specs) eq 'HASH') {
                    my $ms = $specs->{$module};
                    if ($ms) {
                        $spec = $ms->{$sub};
                    }
                    unless ($spec) {
                        push @$errors, "Can't find spec for $module\::$sub ".
                            "(via specs hash)";
                        return;
                    }
                } else {
                    $spec = $specs->(module=>$module, sub=>$sub);
                    unless ($spec) {
                        push @$errors, "Can't find spec for $module\::$sub ".
                        "(via specs coderef)";
                        return;
                    }
                }
            } else {
                if ($parent_args->{load} // 1) {
                    my $modulep = $module;
                    $modulep =~ s!::!/!g; $modulep .= ".pm";
                    eval { require $modulep } or do {
                        push @$errors, "Cannot load module $module: $@";
                        return;
                    };
                }
                no strict 'refs';
                my $ms = \%{"$module\::SPEC"};
                $spec = $ms->{$sub} if $ms;
                unless ($spec) {
                    push @$errors, "Can't find spec in \$$module\::SPEC{$sub}";
                    return;
                }
            }

            my $k = "SUB:$module\::$sub";
            next if $parent_args->{exclude} && $k ~~ @{$parent_args->{exclude}};

            if (!$deps->{$k}) {
                $deps->{$k}  = [];
                $items->{$k} = {item=>$item, spec=>$spec};
                if ($spec->{depends}) {
                    push @{ $deps->{$k} }, _collect_dep(
                        $deps, $items, $spec->{depends}, $parent_args, $errors);
                }
            }
            push @res, $k;
        } elsif ($ik eq 'or') {
            push @$errors, "Sorry, I cannot handle 'or' depend clause yet";
            return;
        } elsif ($ik eq 'and') {
            push @res, _collect_dep($deps, $items, $_, $parent_args, $errors)
                for @{ $item->{and} };
        } elsif ($ik eq 'args') {
            # ignore, used to pass arguments to sub
        } elsif ($ik =~ /^\w+$/) {
            push @$errors, "Sorry, can't understand depend clause '$ik' yet";
            return;
        } else {
            push @$errors, "Invalid depend clause syntax: $ik";
            return;
        }
    }
    @res;
}

$SPEC{run_deps} = {
    summary     => 'Run subroutine in order of dependencies',
    description => <<'_',

Will build dependency tree first and fail with error 412 if cannot solve
dependencies.

Will stop after a dependency failed. All subroutines must return 200 or 304
status to be considered as not failed.

Each subroutine will be passed arguments from common_args (if any), item args
(if any), and "-ctx" which is the context object. You can call $ctx->sub_res()
to find out the result of other subroutines. You can also call
$ctx->stash($key[, $value]) to get/set value that can be accessed by other
subroutines.

_
    args        => {
        item => ['any' => {
            of          => ['str',
                            ['hash'=>{keys=>{name=>'str', args=>'hash'}}]],
            summary     => 'A single subroutine name/args to execute',
            description => <<'_',

Example:

 'Package::foo'   # subroutine foo in package Package, will be called with no
                  # args/only common_args

 'bar'            # subroutine bar in package main, will be caled with no
                  # args/only common args

 {sub=>'Package::baz', args=>{a=>1, b=>2}} # pass args to sub, in addition to
                                           # common_args

See also 'items' if you want to execute several subroutines in successive order.

_
        }],
        items => ['array' => {
            of       => ['any' => {
                of => ['str',
                       ['hash'=>{keys=>{name=>'str', args=>'hash'}}]],
            }],
            summary => 'Execute several items',
            description => <<'_',

An array of zero or more items. See 'item' for more details.

_
        }],
        exclude => ['array' => {
            of       => ['any' => {
                of => ['str',
                       ['hash'=>{keys=>{name=>'str', args=>'hash'}}]],
            }],
            summary => 'Exclude items',
        }],
        common_args => ['hash' => {
            summary => 'Arguments to pass to every subroutine',
        }],
        load        => ['bool' => {
            summary => 'Whether to require modules',
            default => 1,
        }],
        specs       => ['any' => {
            #of      => ['code*', 'hash*'], # SS::Pod can't handle 'code' yet
            summary => 'Instead of searching for specs in %SPEC of '.
                'appropriate package, search in the specified specs',
            description => <<'_',

'specs' can be a hash (with package name as key) of hash (with function name as
the key) of specs. Or it can be a coderef which will be given %args module =>
..., sub => ... and expected to return a spec.

_
        }],
        reverse_order => ['bool' => {
            summary => 'If set to true, reverse order (dependents run first)',
            default => 0,
        }],
        ignore_errors => ['bool' => {
            summary => 'If set to true, ignore error when executing a '.
                'subroutine and move on to the next',
            default => 0,
        }],
        dry_run => ['bool' => {
            summary => 'If set to true, only form dependency tree and '.
                'return the ordered items',
            default => 0,
        }],
        after_item => ['any' => { # xxx code
            summary => 'A coderef to customize the order of execution',
            description => <<'_',

If set, after_item will be executed after each item, and will be given
%arguments: item, items, ref_i (reference to current index of items), ctx, res
(return value of item).

See also 'before_item'.
_
        }],
        before_item => ['any' => { # xxx code
            summary => 'A coderef to customize the order of execution',
            description => <<'_',

If set, after_item will be executed before each item, and will be given
%arguments: item, items, ref_i (reference to current index of items), ctx.

See also 'after_item'.
_
        }],
    },
};
sub run_deps {
    require Algorithm::Dependency::Ordered;
    require Algorithm::Dependency::Source::HoA;

    my %args = @_;

    my @items;
    if ($args{item}) {
        push @items, $args{item};
    }
    if ($args{items}) {
        push @items, @{ $args{items} };
    }
    return [400, "Please specify one or more items to execute"]
        unless @items;

    if ($args{exclude}) {
        $args{exclude} = [map {_item_keys($_)} @{ $args{exclude} }];
    }

    my $specs = $args{specs};
    my %deps;
    my @errors;
    my %items;
    for my $item (@items) {
        _collect_dep(\%deps, \%items, $item, \%args, \@errors);
    }
    if (@errors) {
        return [412, "Cannot get dependency of one or more items",
            \@errors];
    }

    $log->tracef("deps = %s", \%deps);

    my $ds = Algorithm::Dependency::Source::HoA->new(\%deps);
    my $dep = Algorithm::Dependency::Ordered->new(
        source   => $ds,
        selected => []
    ) or return [500, "Failed to set up dependency algorithm"];
    my $sched = $dep->schedule_all;
    return [412, "Can't resolve dependencies, ".
                "please check for circular depends"]
        unless ref($sched) eq 'ARRAY';

    # generate ordered items
    @items = ();
    for my $k (@$sched) {
        $items{$k}{key} = $k;
        push @items, $items{$k};
    }

    if ($args{reverse_order}) {
        #$log->tracef("Reversing order of execution ...");
        @items = reverse @items;
        $sched = [reverse @$sched];
    }

    if ($args{dry_run}) {
        return [200, "OK", {items=>\@items, sched=>$sched, deps=>\%deps}];
    }

    $log->infof("Total number of items to run: %d", scalar(@items));
    my $num_success = 0;
    my $num_failed  = 0;
    my $ctx = Sub::Spec::RunDeps::ContextObject->new({
        items=>\@items, sched=>$sched, deps=>\%deps, sub_res=>{}, stash=>{}});
    my $i = -1;
    while (1) {
        $i++;
        my $item = $items[$i];
        if ($args{before_item}) {
            $log->infof("Running before_item(), i=%d", $i);
            $args{before_item}->(
                ref_i=>\$i,
                item=>$item,
                items=>\@items,
                ctx=>$ctx
            );
        }
        last if $i >= @items;
        $item = $items[$i];

        my $key = $item->{key};
        $log->infof("Running item #%d (%s)", $i, $key);

        my $res;
        eval {
            my $error;
            if ($key !~ /^SUB:(.+)::(.+)$/) {
                $error = "Don't know how to run item '$key'";
                $log->error($error);
                die "$error\n";
            }
            my ($module, $sub) = ($1, $2);
            no strict 'refs';
            my $fref = \&{"$module\::$sub"};
            if (!$fref) {
                $error = "No subroutine \&$module\::$sub defined";
                $log->error("item #$i: $error");
                die "$error\n";
            }
            my %item_args;
            if ($args{common_args}) {
                for (keys %{ $args{common_args} }) {
                    next unless $item->{spec}{args} &&
                        $item->{spec}{args}{$_};
                    $item_args{$_} = $args{common_args}{$_};
                }
            }
            if ($item->{item}{args}) {
                $item_args{$_} = $item->{item}{args}{$_}
                    for keys %{ $item->{item}{args} };
            }
            $item_args{"-ctx"} = $ctx;
            $res = $fref->(%item_args);
            $ctx->{sub_res}{"$module\::$sub"} = $res;
            if ($res->[0] !~ /^(200|304)$/) {
                $num_failed++;
                $error = "didn't succeed: $res->[0] - $res->[1]";
                $log->error("item #$i: $error");
                die "$error\n";
            } else {
                $num_success++;
            }
        };
        my $eval_err = $@;
        if ($eval_err && !$args{ignore_errors}) {
            chomp($eval_err);
            return [500, "Failed at item #$i/".scalar(@items).
                        " ($key): $eval_err"];
        }
        if ($args{after_item}) {
            $log->infof("Running after_item(), i=%d", $i);
            $args{after_item}->(res=>$res,
                                ref_i=>\$i,
                                item=>$item,
                                items=>\@items,
                                ctx=>$ctx
                            );
            last if $i >= @items;
            $log->tracef("size(items)=%d", scalar(@items));
        }
    }
    my $num_unique_success = 0;
    my $num_unique_failed  = 0;
    my $num_unique_items   = 0;
    for (keys %{$ctx->{sub_res}}) {
        my $sr = $ctx->{sub_res}{$_};
        $num_unique_items++;
        if ($sr->[0] == 200 || $sr->[0] == 304) {
            $num_unique_success++;
        } else {
            $num_unique_failed++;
        }
    }
    my $res = [];
    if ($num_success) {
        if ($num_failed) {
            $res->[0] = 200;
            $res->[1] = "Some items failed";
        } else {
            $res->[0] = 200;
            $res->[1] = "All steps succeeded";
        }
    } else {
        $res->[0] = 500;
        $res->[1] = "All steps failed";
    }
    $res->[2] = {
        num_success => $num_success,
        num_failed  => $num_failed,
        num_items   => ($num_success+$num_failed),
        num_unique_success => $num_unique_success,
        num_unique_failed  => $num_unique_failed ,
        num_unique_items   => $num_unique_items  ,
    };
    $res;
}

package Sub::Spec::RunDeps::ContextObject;
BEGIN {
  $Sub::Spec::RunDeps::ContextObject::VERSION = '0.02';
}

# we use object to abstract the context data. if we run items remotely, we don't
# want to pass the whole context data (which might be big for lots of items)
# over the network for each item.

sub new {
    my ($class, $data) = @_;
    bless $data, $class;
}

sub sub_res {
    my ($self, $sub) = @_;
    $self->{sub_res}{$sub};
}

sub stash {
    my ($self, $key, $value) = @_;
    my $oldval = $self->{stash}{$key};
    if (defined($value)) {
        $self->{stash}{$key} = $value;
    }
    $oldval;
}

1;


=pod

=head1 NAME

Sub::Spec::RunDeps - Run subroutine in order of its dependencies

=head1 VERSION

version 0.02

=head1 SYNOPSIS

In your module:

 package YourModule;

 use 5.010;
 our %SPEC;

 $SPEC{a} = { depends => 'b', ... };
 sub a { my %args = @_; say "a", $args{punctuation}; [200, "OK"] }

 $SPEC{b} = { depends => {subname=>'c'}, ... };
 sub b { my %args = @_; say "b", $args{punctuation}; [200, "OK"] }

 $SPEC{c} = { depends => 'd|e', ... };
 sub c { my %args = @_; say "c", $args{punctuation}; [200, "OK"] }

 $SPEC{d} = { ... };
 sub d { my %args = @_; say "d", $args{punctuation}; [200, "OK"] }

 $SPEC{e} = { ... };
 sub e { my %args = @_; say "e", $args{punctuation}; [200, "OK"] }

 1;

In caller:

 use Sub::Spec::RunDeps qw(run_deps);
 run_deps(item => {sub=>'YourModule::a'}, common_args=>{punctuation=>'!'});

will output:

 d!
 e!
 c!
 b!
 a!

=head1 DESCRIPTION

This module reads the B<depends> clause on subroutine specs and runs subroutine
in order of dependencies. That is, if you tell it to run C<A>, it will try to
satisfy (run) A's dependencies first, and A's dependencies' dependencies, and so
on.

This module uses L<Log::Any> logging framework. Use something like
L<Log::Any::App>, etc to see more logging statements for debugging.

=head1 FUNCTIONS

None of the functions are exported by default, but they are exportable.

=head2 run_deps(%args) -> [STATUSCODE, ERRMSG, RESULT]


Run subroutine in order of dependencies.

Will build dependency tree first and fail with error 412 if cannot solve
dependencies.

Will stop after a dependency failed. All subroutines must return 200 or 304
status to be considered as not failed.

Each subroutine will be passed arguments from common_args (if any), item args
(if any), and "-ctx" which is the context object. You can call $ctx->sub_res()
to find out the result of other subroutines. You can also call
$ctx->stash($key[, $value]) to get/set value that can be accessed by other
subroutines.

Returns a 3-element arrayref. STATUSCODE is 200 on success, or an error code
between 3xx-5xx (just like in HTTP). ERRMSG is a string containing error
message, RESULT is the actual result.

Arguments (C<*> denotes required arguments):

=over 4

=item * B<after_item> => I<>

A coderef to customize the order of execution.

If set, after_item will be executed after each item, and will be given
%arguments: item, items, ref_i (reference to current index of items), ctx, res
(return value of item).

See also 'before_item'.

=item * B<before_item> => I<>

A coderef to customize the order of execution.

If set, after_item will be executed before each item, and will be given
%arguments: item, items, ref_i (reference to current index of items), ctx.

See also 'after_item'.

=item * B<common_args> => I<hash>

Arguments to pass to every subroutine.

=item * B<dry_run> => I<bool> (default C<0>)

If set to true, only form dependency tree and return the ordered items.

=item * B<exclude> => I<array>

Exclude items.

=item * B<ignore_errors> => I<bool> (default C<0>)

If set to true, ignore error when executing a subroutine and move on to the next.

=item * B<item> => I<hash|str>

A single subroutine name/args to execute.

Example:

 'Package::foo'   # subroutine foo in package Package, will be called with no
                  # args/only common_args

 'bar'            # subroutine bar in package main, will be caled with no
                  # args/only common args

 {sub=>'Package::baz', args=>{a=>1, b=>2}} # pass args to sub, in addition to
                                           # common_args

See also 'items' if you want to execute several subroutines in successive order.

=item * B<items> => I<array>

Execute several items.

An array of zero or more items. See 'item' for more details.

=item * B<load> => I<bool> (default C<1>)

Whether to require modules.

=item * B<reverse_order> => I<bool> (default C<0>)

If set to true, reverse order (dependents run first).

=item * B<specs> => I<>

Instead of searching for specs in %SPEC of appropriate package, search in the specified specs.

'specs' can be a hash (with package name as key) of hash (with function name as
the key) of specs. Or it can be a coderef which will be given %args module =>
..., sub => ... and expected to return a spec.

=back

=head1 SEE ALSO

L<Sub::Spec>

L<Sub::Spec::Clause::depends>

=head1 AUTHOR

Steven Haryanto <stevenharyanto@gmail.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2011 by Steven Haryanto.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut


__END__

