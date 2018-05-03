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
  my $sampleInfo = $self->getSampleInfo();
  my $tblPrefix = $self->getDatasetId();
  $tblPrefix =~ s/DS_/D/g;

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
    }
  } else {
    $nodeMetadataStatus = ({
                               Id => $self->getId(),
                               tblPrefix => $tblPrefix,
                          });
  }

  my @nodeMetadataSampleInfo;
  my $countSampleCols = 0;
  if (defined $sampleInfo) {
    for my $row (@{$sampleInfo}) {
      $nodeMetadataSampleInfo[$countSampleCols] =  ({
                                                     Id => $self->getId(),
                                                     contXAxis => $xAxis,
                                                     sampleInfo => $row,
                                                     tblPrefix => $tblPrefix,
                                                   });
      $countSampleCols++;
    }
  }

  if (!defined $yAxis && !defined $eventStart && !defined $status && !defined $sampleInfo) {
    die "No data was provided to plot. Must provide 'yAxis', 'eventStart', 'status' or 'sampleInfo' in arguments.";
  }

  my $participantProfile = EbrcWebsiteCommon::View::GraphPackage::Util::makeNodeMetadataSet(\@nodeMetadata, $nodeMetadataEvent, $nodeMetadataStatus, \@nodeMetadataSampleInfo);
  my $line = EbrcWebsiteCommon::View::GraphPackage::GGLinePlot::ParticipantSummary->new(@_);
 
  $line->setProfileSets($participantProfile);

  $self->finalProfileAdjustments($line);
  $self->setGraphObjects($line);

  return $self;

}

1;

#maled
package ClinEpiWebsite::View::GraphPackage::Templates::Participant::DS_121f2c2f02;
use vars qw( @ISA );
@ISA = qw( ClinEpiWebsite::View::GraphPackage::Templates::Participant );
use ClinEpiWebsite::View::GraphPackage::Templates::Participant;

use strict;

sub finalProfileAdjustments{
  my ($self, $profile) = @_;

  my $rAdjustString = << 'RADJUST';
profile.df.full$DURATION[profile.df.full$DURATION == '0 day(s)'] <- NA
profile.df.full$ID[profile.df.full$STATUS == 'No'] <- NA
profile.df.full$STATUS <- profile.df.full$ID
profile.df.full$ID <- NULL
#profile.df.full$LEGEND <- as.factor(profile.df.full$YLABEL)

RADJUST
  my $colorValues = "c(\"Length/height-for-age z-score\" = \"#56B4E9\", \"Weight for age z-score\" = \"#CC79A7\", \"Weight for length/height z-score\" = \"#0072B2\", \"Duration of diarrheal episode in days\" = \"#000099\", \"Vibrio\" = \"#FF0000FF\", \"Taenia sp.\" = \"#FF3500FF\", \"A. lumbricoides\" = \"#FF6A00FF\", \"Adenovirus\" = \"#FF9E00FF\", \"Aeromonas\" = \"#FFD300FF\", \"Astrovirus\" = \"#F6FF00FF\", \"Balantidium coli\" = \"#C1FF00FF\", \"C. mesnili\" = \"#8DFF00FF\", \"Cyclospora\" = \"#58FF00FF\", \"E. histolytica\" = \"#23FF00FF\", \"E. nana\" = \"#00FF12FF\", \"E. vermicularis\" = \"#00FF46FF\", \"EAEC\" = \"#00FF7BFF\", \"EIEC\" = \"#00FFB0FF\", \"EPEC\" = \"#00FFE5FF\", \"ETEC\" = \"#00E5FFFF\", \"Entamoeba coli\" = \"#00B0FFFF\", \"H. diminuta\" = \"#007BFFFF\", \"H. nana\" = \"#0046FFFF\", \"Hookworm\" = \"#0012FFFF\", \"I. butschilii\" = \"#2300FFFF\", \"Norovirus\" = \"#5800FFFF\", \"Rotavirus\" = \"#8D00FFFF\", \"S. stercoralis\" = \"#C100FFFF\", \"Salmonella\" = \"#F600FFFF\", \"Schistosoma\" = \"#FF00D3FF\", \"Shigella\" = \"#FF009EFF\", \"T. trichiura\" = \"#FF006AFF\", \"Yersinia enterocolitica\" = \"#FF0035FF\")";
  my $breaks = "c(\"Length/height-for-age z-score\", \"Weight for age z-score\", \"Weight for length/height z-score\")";

  $profile->addAdjustProfile($rAdjustString);
  $profile->setSubtitle("red lines = +/-2 sd; bars = diarrhea; dots = pathogen+");
  $profile->setEventDurLegend("Diarrhea");
  $profile->setStatusLegend("Pathogen +");
  $profile->setColorVals($colorValues);
  $profile->setCustomBreaks($breaks);

}

1;

#icemr prism
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
profile.df.full = transform(profile.df.full, "COLOR"=ifelse(STATUS == "Blood smear not indicated", "0", ifelse(OPT_STATUS == 'Yes', "1", ifelse((grepl("LAMP not done", STATUS) | grepl("patent", STATUS)), "2", "3"))));
profile.df.full = transform(profile.df.full, "FILL"=ifelse((grepl("patent",STATUS) | grepl("malaria",STATUS)), as.character(COLOR), ifelse(grepl("microscopic",STATUS),"3",NA)));
profile.df.full$COLOR = as.factor(profile.df.full$COLOR);
profile.df.full$TOOLTIP = paste0(profile.df.full$STATUS, "| Febrile: ", profile.df.full$OPT_STATUS)
RADJUST

  $profile->addAdjustProfile($rAdjustString);
  $profile->setForceNoLines(1);
  my $xmax = $self->getDefaultXMax() ? $self->getDefaultXMax() : "2016-06-30";
  my $xmin = $self->getDefaultXMin() ? $self->getDefaultXMin() : "2011-08-01";
  $profile->setDefaultXMax($xmax);
  $profile->setDefaultXMin($xmin);
  $profile->setTimeline('TRUE');
  $profile->setXaxisLabel("Date");
  $profile->setColorVals("c(\"0\" = \"black\", \"1\" = \"#CD4071FF\", \"2\" = \"#FA7C5EFF\", \"3\" = \"#FECE91FF\")");
}

1;

#icemr india
package ClinEpiWebsite::View::GraphPackage::Templates::Participant::DS_f6c59e88c1;
use vars qw( @ISA );
@ISA = qw( ClinEpiWebsite::View::GraphPackage::Templates::Participant );
use ClinEpiWebsite::View::GraphPackage::Templates::Participant;

use strict;

sub finalProfileAdjustments{
  my ($self, $profile) = @_;

  my $rAdjustString = << 'RADJUST';
profile.df.full$ELEMENT_NAMES = as.Date(profile.df.full$ELEMENT_NAMES, '%d-%b-%y');
profile.df.full$ELEMENT_NAMES_NUMERIC = NA;
profile.df.full = transform(profile.df.full, "COLOR"=ifelse(test = (STATUS == "Illness other than malaria" | STATUS == "Asymptomatic malaria"), yes = "1", no = ifelse(STATUS == "No illness", "0", "2")))
profile.df.full = transform(profile.df.full, "FILL"= ifelse(STATUS == "Asymptomatic malaria", "1", ifelse(STATUS == "Severe malaria", "2", NA)))
profile.df.full$TOOLTIP = profile.df.full$STATUS

RADJUST

  $profile->addAdjustProfile($rAdjustString);
  $profile->setForceNoLines(1);
  my $xmax = $self->getDefaultXMax() ? $self->getDefaultXMax() : "2015-03-31";
  my $xmin = $self->getDefaultXMin() ? $self->getDefaultXMin() : "2012-12-01";
  $profile->setDefaultXMax($xmax);
  $profile->setDefaultXMin($xmin);
  $profile->setTimeline('TRUE');
  $profile->setXaxisLabel("Date");
  $profile->setColorVals("c(\"0\" = \"black\", \"2\" = \"#B63679FF\", \"1\" = \"#FECE91FF\")");
}

1;
