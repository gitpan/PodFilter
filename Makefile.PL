use ExtUtils::MakeMaker;

WriteMakefile(
    NAME      => "Pod::Filter",
    DISTNAME  => "PodFilter",
    VERSION   => "0.01",
    SKIP      => [qw(static dynamic)],
    linkext   => {LINKTYPE => ''},
    INSTALLDIRS => 'perl',
    PM        => { "Filter.pm" => '$(INST_LIB)/Pod/Filter.pm' },
    PL_FILES  => { "podfilter.PL" => "podfilter" }
);
