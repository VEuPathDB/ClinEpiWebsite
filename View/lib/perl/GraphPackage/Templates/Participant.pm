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

use Scalar::Util qw /blessed/;
use Data::Dumper;

#for now will just do something simple, will call it participant summary [prtcpnt_sum]
# may have to do some radjust to manipulate the df(s) into what we want
# will have to add rpostscript to add segments and glyphs for events

#sub noElemFile { return 1; }

sub init {
  my $self = shift;
  $self->SUPER::init(@_);

  my $xAxis = $self->getContXAxis();
  my $yAxis = $self->getYAxis();
  my $eventStart = $self->getEventStart();
  my $eventDur = $self->getEventDur();
  my $status = $self->getStatus();
  my $optStatus =  $self->getOptStatus();

  my $yLabel = "Weight for Height Z-score";

  if (defined $yAxis) {
    if ($yAxis eq 'EUPATH_0000682') {
      $yLabel = "Head Circum for Age Z-score";
    } elsif ($yAxis eq 'EUPATH_0000689') {
      $yLabel = "Height for Age Z-score";
    } elsif ($yAxis eq 'EUPATH_0000733') {
      $yLabel = "Weight for Age Z-score";
    } elsif ($yAxis eq 'EUPATH_0000662') {
      $yLabel = "BMI for Age Z-score";
    } elsif ($yAxis ne 'EUPATH_0000734') {
      warn "This option is not yet recognized. Y-axis label will need to be established in Participant.pm template.";
    }
  } else {
    $yLabel = "";
  }

  my $xLabel = "Age in Days";

  if ($xAxis eq 'EUPATH_0000091') {
    $xLabel = "Date of Visit";
  } elsif ($xAxis ne 'EUPATH_0000644') {
    warn "This option is not yet recognized. X-axis label will need to be established in Participant.pm template.";
  }

  my $nodeMetadata;
  if (defined $yAxis) {
    $nodeMetadata =  ({
                        Id => $self->getId(), 
                        contXAxis => $xAxis,  
                        yAxis => $yAxis,
                      });
  } else {
    $nodeMetadata =  ({
                        Id => $self->getId(),
                      });
  }

  my $nodeMetadataEvent;
  if (defined $eventStart) {
    $nodeMetadataEvent = ({ 
                            Id => $self->getId(), 
                            eventStart => $eventStart, 
                            eventDur => $eventDur,
                          });
  } else {
    $nodeMetadataEvent = ({
                            Id => $self->getId(),
                          });
  }

  my $nodeMetadataStatus;
  if (defined $status) {
    if (defined $optStatus) {
      $nodeMetadataStatus = ({
                               Id => $self->getId(),
                               contXAxis => $xAxis,
                               status => $status,
                               optStatus => $optStatus
                             });
    } else {
      $nodeMetadataStatus = ({
                               Id => $self->getId(),
                               contXAxis => $xAxis,
                               status => $status
                             });
    }
  } else {
    $nodeMetadataStatus = ({
                               Id => $self->getId(),
                          });
  }

  if (!defined $yAxis && !defined $eventStart && !defined $status) {
    die "No data was provided to plot. Must provide 'yAxis', 'eventStart' or 'status' in arguments.";
  }

#TODO will eventually need for loop here mimic the one in expression.pm to allow plot parts
  my $participantProfile = EbrcWebsiteCommon::View::GraphPackage::Util::makeNodeMetadataSet($nodeMetadata, $nodeMetadataEvent, $nodeMetadataStatus);
  my $line = EbrcWebsiteCommon::View::GraphPackage::GGLinePlot::ParticipantSummary->new(@_);
  
  $line->setProfileSets($participantProfile);
  $line->setXaxisLabel($xLabel);
  $line->setYaxisLabel($yLabel);

  #this will need to be improved / maybe moved somewhere else later. just trying to get it working for now.
  if (!defined $yAxis && !defined $eventStart && defined $status) {
    my $rAdjustString = << 'RADJUST';
      profile.df.full$ELEMENT_NAMES = as.Date(profile.df.full$ELEMENT_NAMES, '%d-%b-%y');
      profile.df.full$ELEMENT_NAMES_NUMERIC = NA;
      profile.df.full = transform(profile.df.full, "COLOR"=ifelse(OPT_STATUS == 'Yes', "red", ifelse(grepl("not", STATUS), "green", "blue")));
      profile.df.full = transform(profile.df.full, "SOLID"=ifelse((grepl("parasitemia",STATUS) | grepl("malaria",STATUS)), "solid", "hollow"));

RADJUST
  
  $line->addAdjustProfile($rAdjustString);
  #$line->setForceNoLines(1);
  }

  $self->setGraphObjects($line);

  return $self;

}

1;
