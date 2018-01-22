package ClinEpiWebsite::View::GraphPackage::Templates::Participant;

use strict;
use vars qw( @ISA );

@ISA = qw( EbrcWebsiteCommon::View::GraphPackage::MixedPlotSet );
use EbrcWebsiteCommon::View::GraphPackage::MixedPlotSet;

use EbrcWebsiteCommon::View::GraphPackage::Util;

use EbrcWebsiteCommon::View::GraphPackage::BarPlot;
use EbrcWebsiteCommon::View::GraphPackage::LinePlot;
use EbrcWebsiteCommon::View::GraphPackage::ScatterPlot;
use EbrcWebsiteCommon::View::GraphPackage::GGScatterPlot;
use EbrcWebsiteCommon::View::GraphPackage::GGLinePlot;
use EbrcWebsiteCommon::View::GraphPackage::GGBarPlot;
use EbrcWebsiteCommon::View::GraphPackage::GGPiePlot;

use Scalar::Util qw /blessed/;
use Data::Dumper;

sub finalProfileAdjustments {} 

sub init {
  my $self = shift;
  $self->SUPER::init(@_);

  my $xAxis = $self->getContXAxis();
  my $yAxis = $self->getYAxis();
  my $eventStart = $self->getEventStart();
  my $eventDur = $self->getEventDur();
  my $status = $self->getStatus();
  my $optStatus =  $self->getOptStatus();
  my $tblPrefix = $self->getDatasetId();
  $tblPrefix =~ s/DS_/D/g;

  my $subtitle = "red lines = +/- 2 sd";
  my $yLabel = "Weight for Height Z-score";
  my @legendLabel = [];
  if (@{$yAxis}) {
    if (scalar @{$yAxis} > 1) {
      #specific for now. i think about it again later since this already needs so much work.
      $yLabel = "Z-score";
    } elsif ($yAxis->[0] eq 'EUPATH_0000732') {
      $yLabel = "Weight";
      $subtitle = "";
    } elsif ($yAxis->[0] eq 'EUPATH_0000719') {
      $yLabel = "Length";
      $subtitle = "";
    } elsif ($yAxis->[0] eq 'EUPATH_0000682') {
      $yLabel = "Head Circum for Age Z-score";
    } elsif ($yAxis->[0] eq 'EUPATH_0000689') {
      $yLabel = "Height for Age Z-score";
    } elsif ($yAxis->[0] eq 'EUPATH_0000733') {
      $yLabel = "Weight for Age Z-score";
    } elsif ($yAxis->[0] eq 'EUPATH_0000662') {
      $yLabel = "BMI for Age Z-score";
    } elsif ($yAxis->[0] ne 'EUPATH_0000734') {
      warn "This option is not yet recognized. Y-axis label will need to be established in Participant.pm template.";
    }
  } else {
    $yLabel = "";
    $subtitle = "";
  }

  my $xLabel = "Age in Days";

  if ($xAxis eq 'EUPATH_0000091') {
    $xLabel = "Date of Visit";
  } elsif ($xAxis ne 'EUPATH_0000644') {
    warn "This option is not yet recognized. X-axis label will need to be established in Participant.pm template.";
  }

  my @nodeMetadata;
  my $count = 0;
  if (defined $yAxis) {
    for my $row (@{$yAxis}) {
      $nodeMetadata[$count] =  ({
                                 Id => $self->getId(), 
                                 contXAxis => $xAxis,  
                                 yAxis => $row,
                                 tblPrefix => $tblPrefix,
                               });
    $legendLabel[$count] = "Weight for Height Z-score";
    if ($row eq 'EUPATH_0000682') {
      $legendLabel[$count] = "Head Circum for Age Z-score";
    } elsif ($row eq 'EUPATH_0000732') {
      $legendLabel[$count] = "Weight";
    } elsif ($row eq 'EUPATH_0000719') {
      $legendLabel[$count] = "Length";
    } elsif ($row eq 'EUPATH_0000689') {
      $legendLabel[$count] = "Height for Age Z-score";
    } elsif ($row eq 'EUPATH_0000733') {
      $legendLabel[$count] = "Weight for Age Z-score";
    } elsif ($row eq 'EUPATH_0000662') {
      $legendLabel[$count] = "BMI for Age Z-score";
    } elsif ($row ne 'EUPATH_0000734') {
      warn "This option is not yet recognized. Legend label will need to be established in Participant.pm template.";
    } 
      $count++;
    }
  } else {
      $nodeMetadata[0] =  ({
                            Id => $self->getId(),
                            tblPrefix => $tblPrefix,
                          });
  }

  my $nodeMetadataEvent;
  if (defined $eventStart) {
    $nodeMetadataEvent = ({ 
                            Id => $self->getId(), 
                            eventStart => $eventStart, 
                            eventDur => $eventDur,
                            tblPrefix => $tblPrefix
                          });
    if ($eventDur eq 'EUPATH_0000665') {
      if ($subtitle eq '') {
        $subtitle = "bars = diarrhea";
      } else {
        $subtitle = $subtitle . "; bars = diarrhea";
      }
    }
  } else {
    $nodeMetadataEvent = ({
                            Id => $self->getId(),
                            tblPrefix => $tblPrefix,
                          });
  }

  my $nodeMetadataStatus;
  if (defined $status) {
    if (defined $optStatus) {
      $nodeMetadataStatus = ({
                               Id => $self->getId(),
                               contXAxis => $xAxis,
                               status => $status,
                               optStatus => $optStatus,
                               tblPrefix => $tblPrefix,
                             });
    } else {
      $nodeMetadataStatus = ({
                               Id => $self->getId(),
                               contXAxis => $xAxis,
                               status => $status,
                               tblPrefix => $tblPrefix,
                             });
    if ($status eq 'EUPATH_0000704') {
      if ($subtitle eq '') {
        $subtitle = "points = micro+";
      } else {
        $subtitle = $subtitle . "; points = micro+";
      }
    }
    }
  } else {
    $nodeMetadataStatus = ({
                               Id => $self->getId(),
                               tblPrefix => $tblPrefix,
                          });
  }

  if (!defined $yAxis && !defined $eventStart && !defined $status) {
    die "No data was provided to plot. Must provide 'yAxis', 'eventStart' or 'status' in arguments.";
  }

#TODO will eventually need for loop in this file (mimic the one in expression.pm) to allow plot parts
  my $participantProfile = EbrcWebsiteCommon::View::GraphPackage::Util::makeNodeMetadataSet(\@nodeMetadata, $nodeMetadataEvent, $nodeMetadataStatus);
  my $line = EbrcWebsiteCommon::View::GraphPackage::GGLinePlot::ParticipantSummary->new(@_);
  
  $line->setProfileSets($participantProfile);
  $line->setXaxisLabel($xLabel);
  $line->setYaxisLabel($yLabel);
  $line->setSubtitle($subtitle);

  my @colorOptions = ( "#56B4E9", "#CC79A7", "#0072B2", "#009E73", "#F0E442", "#999999", "#E69F00");
  $count--;
  #TODO add a check here that count is not outside bounds of colorOptions and let it reuse colorOptions if it is. 
  my @colors = @colorOptions[0..$count];
  if (defined $eventStart) {
    $colors[$count+1] = "#000099";
  }
  if (defined $status) {
    if (defined $eventStart) {
      $colors[$count+2] = "#000099";
    } else {
      $colors[$count+1] = "#000099";
    }
  }

  if (@colors) {
    $line->setColors(\@colors);
  }
  if (@legendLabel) {
    $line->setLegendLabels(\@legendLabel);
  }

  $self->finalProfileAdjustments($line);
  $self->setGraphObjects($line);

  return $self;

}

1;

package ClinEpiWebsite::View::GraphPackage::Templates::Participant::DS_841a9f5259;
use vars qw( @ISA );
@ISA = qw( ClinEpiWebsite::View::GraphPackage::Templates::Participant );
use ClinEpiWebsite::View::GraphPackage::Templates::Participant;

use strict;

#anything to add or override put it here

1;

package ClinEpiWebsite::View::GraphPackage::Templates::Participant::DS_0ad509829e;
use vars qw( @ISA );
@ISA = qw( ClinEpiWebsite::View::GraphPackage::Templates::Participant );
use ClinEpiWebsite::View::GraphPackage::Templates::Participant;

use strict;

sub finalProfileAdjustments{
  my ($self, $profile) = @_;

  my $rAdjustString = << 'RADJUST';
profile.df.full$ELEMENT_NAMES = as.Date(profile.df.full$ELEMENT_NAMES, '%d-%b-%y');
profile.df.full$ELEMENT_NAMES_NUMERIC = NA;
profile.df.full$STATUS[profile.df.full$STATUS == "Blood smear not indicated"] <- NA
profile.df.full = transform(profile.df.full, "COLOR"=ifelse(OPT_STATUS == 'Yes', "1", ifelse((grepl("LAMP not done", STATUS) | grepl("patent", STATUS)), "2", "3")));
profile.df.full = transform(profile.df.full, "FILL"=ifelse((grepl("patent",STATUS) | grepl("malaria",STATUS)), as.character(COLOR), ifelse(grepl("microscopic",STATUS),"3",NA)));
colors <- magma(3, end = .9, begin = .3)
#profile.df.full = transform(profile.df.full, "COLOR"=ifelse(grepl("malaria", STATUS), "6", ifelse(STATUS == 'Blood smear not indicated', "1", ifelse(grepl("LAMP positive", STATUS), "4", ifelse(STATUS == 'Asymptomatic patent parasitemia', "5", ifelse(STATUS == 'Negative blood smear and LAMP not done', "2", "3"))))))
#profile.df.full = transform(profile.df.full, "SHAPE"=ifelse(OPT_STATUS == 'Yes', "full", "half"))
profile.df.full$COLOR = as.factor(profile.df.full$COLOR);
RADJUST

  $profile->addAdjustProfile($rAdjustString);
  $profile->setForceNoLines(1);
  my $xmax = $self->getDefaultXMax() ? $self->getDefaultXMax() : "2016-06-30";
  my $xmin = $self->getDefaultXMin() ? $self->getDefaultXMin() : "2011-08-01";
  $profile->setDefaultXMax($xmax);
  $profile->setDefaultXMin($xmin);
  $profile->setTimeline('TRUE');

}

1;
