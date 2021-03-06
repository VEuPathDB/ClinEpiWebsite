package ClinEpiWebsite::Model::CannedQuery::NodeMetadata;

@ISA = qw( EbrcWebsiteCommon::Model::CannedQuery );

=pod

=head1 Purpose

This canned query selects various physical characteristics associated with a given participant.

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

  $Self->setName                 ( $Args->{Name        });
  $Self->setId                   ( $Args->{Id          });
  $Self->setContXAxis            ( $Args->{ContXAxis   });
  $Self->setYAxis                ( $Args->{YAxis       });
  $Self->setTblPrefix            ( $Args->{TblPrefix   });

  my $contXAxis = $Self->getContXAxis();
  my $yAxis = $Self->getYAxis();
  my $tblPrefix = $Self->getTblPrefix();
  my $prtcpntTable = $tblPrefix . "Participants";
  my $ioTable = $tblPrefix . "PANIO";
  my $obsTable = $tblPrefix . "Observations";
  my $ontologyTable = $tblPrefix . "Ontology";

  $Self->setSql(<<Sql);

select m.ONTOLOGY_TERM_NAME as LEGEND
  , ea.$yAxis as VALUE
  , ea.$contXAxis as NAME
  , m.ONTOLOGY_TERM_NAME as YLABEL
  , mt.ONTOLOGY_TERM_NAME as XLABEL
-- profile_file is participant id
from apidbtuning.$prtcpntTable pa
   , apidbtuning.$ioTable io
   , apidbtuning.$obsTable ea
   , apidbtuning.$ontologyTable m
   , apidbtuning.$ontologyTable mt 
where pa.name = \'<<Id>>\'
and pa.pan_id = io.input_pan_id 
and io.OUTPUT_PAN_ID = ea.PAN_ID
and ea.$yAxis is not null
and m.ONTOLOGY_TERM_SOURCE_ID = \'$yAxis\'
and mt.ONTOLOGY_TERM_SOURCE_ID = \'$contXAxis\'
order by $contXAxis 

Sql

  return $Self;
}


# -------------------------------- access --------------------------------

sub getId                            { $_[0]->{'Id'                         } }
sub setId                            { $_[0]->{'Id'                         } = $_[1]; $_[0] }

sub getName                          { $_[0]->{'Name'                       } }
sub setName                          { $_[0]->{'Name'                       } = $_[1]; $_[0] }

sub getContXAxis                     { $_[0]->{'ContXAxis'                  } }
sub setContXAxis                     { $_[0]->{'ContXAxis'                  } = $_[1]; $_[0] }

sub getYAxis                         { $_[0]->{'YAxis'                      } }
sub setYAxis                         { $_[0]->{'YAxis'                      } = $_[1]; $_[0] }

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
