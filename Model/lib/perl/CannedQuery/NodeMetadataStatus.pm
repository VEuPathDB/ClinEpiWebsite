package ClinEpiWebsite::Model::CannedQuery::NodeMetadataStatus;

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
  $Self->setStatus               ( $Args->{Status      });
  $Self->setOptStatus            ( $Args->{OptStatus   });
  $Self->setTblPrefix            ( $Args->{TblPrefix   });

  my $contXAxis = $Self->getContXAxis();
  my $status = $Self->getStatus();
  #this to allow a second optional status to return as well
  my $optStatus = $Self->getOptStatus();
  my $tblPrefix = $Self->getTblPrefix();
  my $prtcpntTable = $tblPrefix . "Participants";
  my $ioTable = $tblPrefix . "PANIO";
  my $obsTable = $tblPrefix . "Observations";

if (defined $optStatus) {
  $Self->setSql(<<Sql);

select pa.name as LEGEND
  , ea.$status as STATUS
  , ea.$contXAxis as NAME
  , ea.$optStatus as OPT_STATUS
-- profile_file is participant id
from apidbtuning.$prtcpntTable pa
   , apidbtuning.$ioTable io
   , apidbtuning.$obsTable ea
where pa.name = \'<<Id>>\'
and pa.pan_id = io.input_pan_id 
and io.OUTPUT_PAN_ID = ea.PAN_ID
and ea.$status is not null
order by $contXAxis 

Sql

  return $Self;

} else {
  $Self->setSql(<<Sql);

select pa.name as LEGEND
  , ea.$status as STATUS
  , ea.$contXAxis as NAME
-- profile_file is participant id
from apidbtuning.$prtcpntTable pa
   , apidbtuning.$ioTable io
   , apidbtuning.$obsTable ea
where pa.name = \'<<Id>>\'
and pa.pan_id = io.input_pan_id 
and io.OUTPUT_PAN_ID = ea.PAN_ID
and ea.$status is not null
order by $contXAxis 

Sql

  return $Self;
 }
}

# -------------------------------- access --------------------------------

sub getId                            { $_[0]->{'Id'                         } }
sub setId                            { $_[0]->{'Id'                         } = $_[1]; $_[0] }

sub getName                          { $_[0]->{'Name'                       } }
sub setName                          { $_[0]->{'Name'                       } = $_[1]; $_[0] }

sub getContXAxis                     { $_[0]->{'ContXAxis'                  } }
sub setContXAxis                     { $_[0]->{'ContXAxis'                  } = $_[1]; $_[0] }

sub getStatus                        { $_[0]->{'Status'                     } }
sub setStatus                        { $_[0]->{'Status'                     } = $_[1]; $_[0] }

sub getOptStatus                     { $_[0]->{'OptStatus'                  } }
sub setOptStatus                     { $_[0]->{'OptStatus'                  } = $_[1]; $_[0] }

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
