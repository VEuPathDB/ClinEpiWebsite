package ClinEpiWebsite::View::GraphPackage::Templates::ParticipantTimeline;

use strict;
use vars qw( @ISA );

@ISA = qw( ClinEpiWebsite::View::GraphPackage::Templates::Participant );
use ClinEpiWebsite::View::GraphPackage::Templates::Participant;

use Data::Dumper;

#this is a very specific flavor of participant summary where only status query is run. 
#right now it will only work for icemr .. should try to generalize later.

sub finalProfileAdjustments{
  my ($self, $profile) = @_;

  my $rAdjustString = << 'RADJUST';
  profile.df.full$ELEMENT_NAMES = as.Date(profile.df.full$ELEMENT_NAMES, '%d-%b-%y');
  profile.df.full$ELEMENT_NAMES_NUMERIC = NA;
  profile.df.full = transform(profile.df.full, "COLOR"=ifelse(OPT_STATUS == 'Yes', "red", ifelse((grepl("not", STATUS) | grepl("patent", STATUS)), "green", "blue")));
  profile.df.full = transform(profile.df.full, "FILL"=ifelse((grepl("parasitemia",STATUS) | grepl("malaria",STATUS)), as.character(COLOR), NA));
  profile.df.full$FILL = as.factor(profile.df.full$FILL);
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
