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
  print STDERR Dumper($yAxis);
  my $eventStart = $self->getEventStart();
  my $eventDur = $self->getEventDur();
  my $status = $self->getStatus();
  my $optStatus =  $self->getOptStatus();

  my $yLabel = "Weight for Height Z-score";
  my @legendLabel = [];
  print STDERR Dumper($yAxis->[0]);
  if (@{$yAxis}) {
    if (scalar @{$yAxis} > 1) {
      #specific for now. i think about it again later since this already needs so much work.
      $yLabel = "Z-score"
    } elsif ($yAxis->[0] eq 'EUPATH_0000719') {
      $yLabel = "Length";
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
  }
  print STDERR Dumper($yLabel);

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
                               });
    @legendLabel[$count] = "WHZ";
    if ($row eq 'EUPATH_0000682') {
      @legendLabel[$count] = "Head Circum for Age Z-score";
    } elsif ($row eq 'EUPATH_0000719') {
      $legendLabel[$count] = "LEN";
    } elsif ($row eq 'EUPATH_0000689') {
      $legendLabel[$count] = "HAZ";
    } elsif ($row eq 'EUPATH_0000733') {
      $legendLabel[$count] = "WAZ";
    } elsif ($row eq 'EUPATH_0000662') {
      $legendLabel[$count] = "BMI for Age Z-score";
    } elsif ($row ne 'EUPATH_0000734') {
      warn "This option is not yet recognized. Legend label will need to be established in Participant.pm template.";
    } 
      $count++;
      print STDERR Dumper($count);
    }
  } else {
      $nodeMetadata[0] =  ({
                            Id => $self->getId(),
                          });
  }
  print STDERR Dumper(\@nodeMetadata);
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
  my $participantProfile = EbrcWebsiteCommon::View::GraphPackage::Util::makeNodeMetadataSet(\@nodeMetadata, $nodeMetadataEvent, $nodeMetadataStatus);
  my $line = EbrcWebsiteCommon::View::GraphPackage::GGLinePlot::ParticipantSummary->new(@_);
  
  $line->setProfileSets($participantProfile);
  $line->setXaxisLabel($xLabel);
  $line->setYaxisLabel($yLabel);

  my @colorOptions = ( "#56B4E9", "#CC79A7", "#0072B2", "#009E73", "#F0E442", "#999999", "#E69F00");
  $count--;
  my @colors = @colorOptions[0..$count];
  if (defined $eventStart) {
    @colors[$count+1] = "#000099";
  }
  if (defined $status) {
    @colors[$count+2] = "#000099";
  }

  #my @colors = map { 
  #  "#" . join "", map { sprintf "%02x", rand(255) } (0..2) 
  #} (@nodeMetadata);
  print STDERR Dumper(\@colors);
  if (@colors) {
    $line->setColors(\@colors);
  }
  if (@legendLabel) {
    $line->setLegendLabels(\@legendLabel);
  }

  #this will need to be improved / maybe moved somewhere else later. just trying to get it working for now.
  if (!defined $yAxis && !defined $eventStart && defined $status) {
    #this is a bit specific right now.. will look at it again later.
    my $rAdjustString = << 'RADJUST';
      profile.df.full$ELEMENT_NAMES = as.Date(profile.df.full$ELEMENT_NAMES, '%d-%b-%y');
      profile.df.full$ELEMENT_NAMES_NUMERIC = NA;
      profile.df.full = transform(profile.df.full, "COLOR"=ifelse(OPT_STATUS == 'Yes', "red", ifelse((grepl("not", STATUS) | grepl("patent", STATUS)), "green", "blue")));
      profile.df.full = transform(profile.df.full, "FILL"=ifelse((grepl("parasitemia",STATUS) | grepl("malaria",STATUS)), as.character(COLOR), NA));
      profile.df.full$FILL = as.factor(profile.df.full$FILL);
      #profile.df.full$VALUE = 1;
RADJUST
  
  $line->addAdjustProfile($rAdjustString);
  $line->setForceNoLines(1);
  my $xmax = $self->getDefaultXMax() ? $self->getDefaultXMax() : "2016-06-30";
  my $xmin = $self->getDefaultXMin() ? $self->getDefaultXMin() : "2011-08-01";
  $line->setDefaultXMax($xmax);
  $line->setDefaultXMin($xmin);
  }

  $self->setGraphObjects($line);

  return $self;

}

1;
