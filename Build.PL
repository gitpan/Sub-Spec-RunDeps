
use strict;
use warnings;

use Module::Build 0.3601;


my %module_build_args = (
  'build_requires' => {
    'Module::Build' => '0.3601'
  },
  'configure_requires' => {
    'Module::Build' => '0.3601'
  },
  'dist_abstract' => 'Run subroutine in order of its dependencies',
  'dist_author' => [
    'Steven Haryanto <stevenharyanto@gmail.com>'
  ],
  'dist_name' => 'Sub-Spec-RunDeps',
  'dist_version' => '0.03',
  'license' => 'perl',
  'module_name' => 'Sub::Spec::RunDeps',
  'recommends' => {},
  'recursive_test_files' => 1,
  'requires' => {
    'Algorithm::Dependency' => '0',
    'Log::Any' => '0',
    'Sub::Spec' => '0.04',
    'Test::More' => '0.96',
    'perl' => '5.010000'
  },
  'script_files' => []
);


my $build = Module::Build->new(%module_build_args);

$build->create_build_script;
