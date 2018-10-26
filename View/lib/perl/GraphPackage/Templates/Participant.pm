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

sub useWhoStandards {0}

sub init {
  my $self = shift;
  $self->SUPER::init(@_);

  my %WhoProfileSets;

  $WhoProfileSets{'male'}{'EUPATH_0000719'} = 'Length/height for age for boys zscore';
  $WhoProfileSets{'female'}{'EUPATH_0000719'} = 'Length/height for age for girls zscore';
  $WhoProfileSets{'male'}{'EUPATH_0000732'} = 'Weight for age for boys zscore';
  $WhoProfileSets{'female'}{'EUPATH_0000732'} = 'Weight for age for girls zscore';

  my $xAxis = $self->getContXAxis();
  my $yAxis = $self->getYAxis();
  my $eventStart = $self->getEventStart();
  my $eventDur = $self->getEventDur();
  my $status = $self->getStatus();
  my $optStatus =  $self->getOptStatus();
  my $sampleInfo = $self->getSampleInfo();
  my $tblPrefix = $self->getDatasetId();
  $tblPrefix =~ s/DS_/D/g;

  my $profileSets;

  my @nodeMetadata;
  my $count = 0;

  if (defined $yAxis) {
      
      if ($self->useWhoStandards()){
	  
	  my $tablename = 'APIDBTUNING.' . $tblPrefix . 'PARTICIPANTS';
	  $tablename =~ s/[^a-zA-Z0-9.]//g;
	  
	  my $ID = $self->getId();

	  my $sql = "select PATO_0000047
                     from ". $tablename .
                    " where NAME = '" . $ID . "'" ; 
	  
	  my $qh = $self->getQueryHandle();

	  my $sh = $qh->prepare($sql);  

	  $sh->execute();
	  
	  my ($sex) = $sh->fetchrow_array();

	  #if($sex eq 'Female | Male'){
	   #   $sex='male';
	  #}
	  
	  $sex = lc($sex);

	  $sh->finish();


	  if(scalar @{$yAxis} ==1){

	      my $currentWHOProfileSet = $WhoProfileSets{$sex}{$yAxis->[0]};

	      print STDERR Dumper($currentWHOProfileSet);


	      if (defined $currentWHOProfileSet){	  

		  my @profileSetArray = ([$currentWHOProfileSet,'values', '', '', '', '', '', '', '', '', '','SD0'],
					 [$currentWHOProfileSet,'values', '', '', '', '', '', '', '', '', '','SD2'],
					 [$currentWHOProfileSet,'values', '', '', '', '', '', '', '', '', '','SD2neg'],
		      );
		  
		  $profileSets = EbrcWebsiteCommon::View::GraphPackage::Util::makeProfileSets(\@profileSetArray);
		  
	      }
	      
	  } 
	  
      }



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
    if (!defined $eventDur) {
      $nodeMetadataEvent = ({
                             Id => $self->getId(),
                             eventStart => $eventStart,
                             contXAxis => $xAxis,
                             tblPrefix => $tblPrefix
                            });
    } else {
      $nodeMetadataEvent = ({ 
                             Id => $self->getId(), 
                             eventStart => $eventStart, 
                             eventDur => $eventDur,
                             tblPrefix => $tblPrefix
                            });
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
  
  if (defined $profileSets ){
      push @{$participantProfile},@{$profileSets};
  }                                                             
  
  $line->setProfileSets($participantProfile);

  $self->finalProfileAdjustments($line);
  $self->setGraphObjects($line);

  return $self;

}

1;

#maled
package ClinEpiWebsite::View::GraphPackage::Templates::Participant::DS_3dbf92dc05;
use vars qw( @ISA );
@ISA = qw( ClinEpiWebsite::View::GraphPackage::Templates::Participant );
use ClinEpiWebsite::View::GraphPackage::Templates::Participant;

use strict;

sub useWhoStandards {1}

sub finalProfileAdjustments{

  my ($self, $profile) = @_;

  my $rAdjustString = << 'RADJUST';
#profile.df.full$DURATION[profile.df.full$DURATION == '0 day(s)'] <- NA
profile.df.full$ID[profile.df.full$STATUS == 'No'] <- NA
profile.df.full$STATUS <- profile.df.full$ID
profile.df.full$ID <- NULL

profile.df.full$EVENT[profile.df.full$EVENT == "No diarrhea"] <- NA
profile.df.full$EVENT[profile.df.full$EVENT == ""] <- NA

profile.df.full$oldLegend <- as.character(profile.df.full$LEGEND)

profile.df.full <- transform(profile.df.full, "LEGEND" = ifelse(grepl("SD0", profile.df.full$PROFILE_FILE), "WHO Standards, Mean", ifelse(grepl("SD2neg", profile.df.full$PROFILE_FILE), "WHO Standards, -2SD", ifelse(grepl("SD2", profile.df.full$PROFILE_FILE), "WHO Standards, +2SD", oldLegend))))

profile.df.full$oldLegend <- NULL



RADJUST
  my $colorValues = "c(\"WHO Standards, +2SD\" = \"red\",\"WHO Standards, -2SD\" = \"red\",\"WHO Standards, Mean\" = \"black\",\"Recumbent length/height (cm)\" = \"blue\", \"Weight (kg)\" = \"blue\", \"Length/height-for-age z-score\" = \"#56B4E9\", \"Weight-for-age z-score\" = \"#CC79A7\", \"Weight-for-length/height z-score\" = \"#0072B2\", \"Diarrheal episode status\" = \"#000099\", \"Adenovirus, by ELISA\" = \"#FF0000FF\", \"Aeromonas, by bacteriology\" = \"#FF2100FF\", \"Ascaris lumbricoides, by microscopy\" = \"#FF4300FF\", \"Astrovirus, by ELISA\" = \"#FF6400FF\", \"Atypical EPEC, by PCR\" = \"#FF8500FF\", \"Balantidium coli, by microscopy\" = \"#FFA600FF\", \"Campylobacter, by ELISA\" = \"#FFC800FF\", \"Campylobacter, by bacteriology\" = \"#FFE900FF\", \"Chilomastix mesnili, by microscopy\" = \"#F4FF00FF\", \"Cryptosporidium, by ELISA\" = \"#D3FF00FF\", \"Cyclospora, by microscopy\" = \"#B1FF00FF\", \"EAEC aatA and aaiC pos, by PCR\" = \"#90FF00FF\", \"EAEC aatA or aaiC pos, by PCR\" = \"#6FFF00FF\", \"EIEC ipaH pos, by PCR\" = \"#4EFF00FF\", \"EPEC bfpA pos, by PCR\" = \"#2CFF00FF\", \"EPEC eae and bfpA pos, by PCR\" = \"#0BFF00FF\", \"EPEC eae pos, by PCR\" = \"#00FF16FF\", \"ETEC LT neg ST pos, by PCR\" = \"#00FF37FF\", \"ETEC LT or ST pos, by PCR\" = \"#00FF59FF\", \"ETEC LT pos ST neg, by PCR\" = \"#00FF7AFF\", \"Endolimax nana, by microscopy\" = \"#00FF9BFF\", \"Entamoeba coli, by microscopy\" = \"#00FFBCFF\", \"Entamoeba histolytica, by ELISA\" = \"#00FFDEFF\", \"Enterobius vermicularis, by microscopy\" = \"#00FFFFFF\", \"Escherichia coli, by bacteriology\" = \"#00DEFFFF\", \"Giardia, by ELISA\" = \"#00BCFFFF\", \"Hookworm, by microscopy\" = \"#009BFFFF\", \"Hymenolepis diminuta, by microscopy\" = \"#007AFFFF\", \"Hymenolepis nana, by microscopy\" = \"#0059FFFF\", \"Iodamoeba butschlii, by microscopy\" = \"#0037FFFF\", \"Isospora, by microscopy\" = \"#0016FFFF\", \"Norovirus GI, by RT-PCR\" = \"#0B00FFFF\", \"Norovirus GII, by RT-PCR\" = \"#2C00FFFF\", \"Norovirus, by RT-PCR\" = \"#4E00FFFF\", \"Other parasites, by microscopy\" = \"#6F00FFFF\", \"Plesiomonas shigelloides, by bacteriology\" = \"#9000FFFF\", \"Rotavirus, by ELISA\" = \"#B100FFFF\", \"STEC stx1 or stx2 pos, by PCR\" = \"#D300FFFF\", \"Salmonella, by bacteriology\" = \"#F400FFFF\", \"Schistosoma, by microscopy\" = \"#FF00E9FF\", \"Shigella, by bacteriology\" = \"#FF00C8FF\", \"Strongyloides stercoralis, by microscopy\" = \"#FF00A6FF\", \"Taenia, by microscopy\" = \"#FF0085FF\", \"Trichuris trichiura, by microscopy\" = \"#FF0064FF\", \"Vibrio, by bacteriology\" = \"#FF0043FF\", \"Yersinia enterocolitica, by bacteriology\" = \"#FF0021FF\")";
 
  my $breaks = "c(\"WHO Standards, Mean\",\"WHO Standards, +2SD\",\"WHO Standards, -2SD\",\"Length/height for age z-score\", \"Weight for age z-score\", \"Weight for length/height z-score\",\"Weight (kg)\",\"Recumbent length/height (cm)\")";


  $profile->addAdjustProfile($rAdjustString);
  #$profile->setSubtitle("red lines = +/-2 sd; bars = diarrhea; dots = pathogen+");
  $profile->setEventDurLegend("Diarrhea");
  $profile->setStatusLegend("Pathogen+ (check the point for pathogen  information)");
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
profile.df.full = transform(profile.df.full, "COLOR"=ifelse(STATUS == "Blood smear not indicated", "Blood smear not indicated", ifelse(OPT_STATUS == 'Yes', "Febrile", ifelse(grepl("Blood smear positive",STATUS),"Not febrile and BS positive", "Not LAMP positive"))));
profile.df.full = transform(profile.df.full, "FILL"=ifelse(STATUS == "Blood smear not indicated", "None", ifelse(STATUS == "Blood smear indicated but not done", "BS indicated not done", ifelse(STATUS == "Symptomatic malaria", "Symptomatic malaria", ifelse(grepl("Blood smear positive",STATUS),"Blood smear positive", ifelse(grepl("LAMP positive", STATUS), "LAMP positive", "None"))))));
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
  $profile->setColorVals("c(\"Febrile\" = \"#CD4071FF\", \"Blood smear not indicated\" = \"black\", \"Not febrile and BS positive\" = \"#FA7C5EFF\", \"Not LAMP positive\" = \"#FECE91FF\")");
  $profile->setFillVals("c(\"Symptomatic malaria\" = \"#CD4071FF\", \"LAMP positive\" = \"#FECE91FF\", \"BS indicated not done\" = \"gray\", \"Blood smear positive\" = \"#FA7C5EFF\", \"None\" = NA)");
  $profile->setCustomBreaks("c(\"Febrile\", \"Blood smear not indicated\", \"Symptomatic malaria\", \"LAMP positive\", \"BS indicated not done\", \"Blood smear positive\", \"Not LAMP positive\")");
}

1;

#icemr india
package ClinEpiWebsite::View::GraphPackage::Templates::Participant::DS_05ea525fd3;
use vars qw( @ISA );
@ISA = qw( ClinEpiWebsite::View::GraphPackage::Templates::Participant );
use ClinEpiWebsite::View::GraphPackage::Templates::Participant;

use strict;

sub finalProfileAdjustments{
  my ($self, $profile) = @_;

  my $rAdjustString = << 'RADJUST';
profile.df.full$ELEMENT_NAMES = as.Date(profile.df.full$ELEMENT_NAMES, '%d-%b-%y');
profile.df.full$ELEMENT_NAMES_NUMERIC = NA;
profile.df.full = transform(profile.df.full, "COLOR"=ifelse(test = (STATUS == "Illness other than malaria" | STATUS == "Asymptomatic malaria"), yes = "Asymptomatic malaria or other illness", no = ifelse(STATUS == "No illness", "No illness", "Symptomatic malaria")))
profile.df.full = transform(profile.df.full, "FILL"= ifelse(STATUS == "Asymptomatic malaria", "Asymptomatic malaria", ifelse(STATUS == "Severe malaria", "Severe malaria", NA)))
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
  $profile->setColorVals("c(\"No illness\" = \"black\", \"Symptomatic malaria\" = \"#B63679FF\", \"Asymptomatic malaria or other illness\" = \"#FECE91FF\")");
  $profile->setFillVals("c(\"Severe malaria\" = \"#B63679FF\", \"Asymptomatic malaria\" = \"#FECE91FF\")");
  $profile->setCustomBreaks("c(\"No illness\", \"Symptomatic malaria\", \"Asymptomatic malaria or other illness\", \"Severe malaria\", \"Asymptomatic malaria\")");
}

1;
