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

  $Self->setName                 ( $Args->{Name      });
  $Self->setId                   ( $Args->{Id        });
  $Self->setContXAxis            ( $Args->{ContXAxis });
  $Self->setYAxis                ( $Args->{YAxis     });

  my $contXAxis = $Self->getContXAxis();
  my $yAxis = $Self->getYAxis();

  $Self->setSql(<<Sql);

select pa.name as LEGEND
  , ea.$yAxis as VALUE
  , ea.$contXAxis as NAME
-- profile_file is participant id
from apidbtuning.participantattributes pa
   , apidbtuning.PANIO io
   , APIDBTUNING.EVENTATTRIBUTES ea
where pa.name = \'<<Id>>\'
and pa.protocol_app_node_id = io.input_pan_id 
and io.OUTPUT_PAN_ID = ea.PROTOCOL_APP_NODE_ID
and ea.$yAxis is not null
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
