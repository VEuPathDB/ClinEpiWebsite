package ClinEpiWebsite::Model::CannedQuery::NodeMetadataSampleInfo;

@ISA = qw( EbrcWebsiteCommon::Model::CannedQuery );

=pod

=head1 Purpose

This canned query selects various characteristics from the sample table for a given participant.

=head1 Macros

The following macros must be available to execute this query.

=over

=item Id - source id for the participant

=back

=cut

# ========================================================================
# ----------------------------- Declarations -----------------------------
# ========================================================================

use strict;

use FileHandle;

use EbrcWebsiteCommon::Model::CannedQuery;

use Data::Dumper;

# ========================================================================
# ----------------------- Create, Init, and Access -----------------------
# ========================================================================

# --------------------------------- init ---------------------------------

sub init {
  my $Self = shift;
  my $Args = ref $_[0] ? shift : {@_};

  $Self->SUPER::init($Args);

  $Self->setName                 ( $Args->{Name          });
  $Self->setId                   ( $Args->{Id            });
  $Self->setContXAxis            ( $Args->{ContXAxis     });
  $Self->setSampleInfo           ( $Args->{SampleInfo    });
  $Self->setTblPrefix            ( $Args->{TblPrefix     });

  my $contXAxis = $Self->getContXAxis();
  my $sampleInfo = $Self->getSampleInfo();
  my $tblPrefix = $Self->getTblPrefix();
  my $prtcpntTable = $tblPrefix . "Participants";
  my $ioTable = $tblPrefix . "PANIO";
  my $sampleTable = $tblPrefix . "Samples";
  my $ontologyTable = $tblPrefix . "Ontology";
  my $obsTable = $tblPrefix . "Observations";

  $Self->setSql(<<Sql);

select pa.NAME as LEGEND
  , m.ONTOLOGY_TERM_NAME as ID
  , sa.$sampleInfo as STATUS
  , ea.$contXAxis as NAME
from apidbtuning.$sampleTable sa
   , apidbtuning.$prtcpntTable pa
   , apidbtuning.$obsTable ea
   , apidbtuning.$ioTable io
   , apidbtuning.$ioTable pio
   , apidbtuning.$ontologyTable m
where pa.NAME = \'<<Id>>\'
and pa.PAN_ID = io.INPUT_PAN_ID
and io.OUTPUT_PAN_ID = ea.PAN_ID
and ea.PAN_ID = pio.INPUT_PAN_ID
and pio.OUTPUT_PAN_ID = sa.PAN_ID
and sa.$sampleInfo is not null
and m.ONTOLOGY_TERM_SOURCE_ID = \'$sampleInfo\'
order by ea.$contXAxis

Sql

  return $Self;
}


# -------------------------------- access --------------------------------

sub getId                            { $_[0]->{'Id'                         } }
sub setId                            { $_[0]->{'Id'                         } = $_[1]; $_[0] }

sub getName                          { $_[0]->{'Name'                       } }
sub setName                          { $_[0]->{'Name'                       } = $_[1]; $_[0] }

sub getSampleInfo                    { $_[0]->{'SampleInfo'                 } }
sub setSampleInfo                    { $_[0]->{'SampleInfo'                 } = $_[1]; $_[0] }

sub getContXAxis                     { $_[0]->{'ContXAxis'                  } }
sub setContXAxis                     { $_[0]->{'ContXAxis'                  } = $_[1]; $_[0] }

sub getTblPrefix                     { $_[0]->{'TblPrefix'                  } }
sub setTblPrefix                     { $_[0]->{'TblPrefix'                  } = $_[1]; $_[0] }

# ========================================================================
# --------------------------- Support Methods ----------------------------
# ========================================================================

sub prepareDictionary {
  my $Self = shift;
  my $Dict = shift || {};

  $Dict->{Id} = $Self->getId();

  my $Rv = $Dict;

  return $Rv;

}


# ========================================================================
# ---------------------------- End of Package ----------------------------
# ========================================================================

1;
