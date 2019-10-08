// check if timeout exists upon disconnect, if so offer to reload
// if it was not a timeout, we say server error and offer link to contact us form

var myShinyAppsVar = setInterval(myCheckForTimeout, 1000);
var myStyleApps = document.createElement('style');
myStyleApps.innerHTML = `
div#ss-connect-dialog {
  width: 30em;
  top: 10em;
  height: 6em;
  border: 1px solid #9999FF;
}
div#shiny-disconnected-overlay, div.modal-backdrop.fade.in  {
  opacity: 0;
}
div#ss-overlay {
  opacity: 0.1;
}
`;
document.head.appendChild(myStyleApps);

function myCheckForTimeout() {
  if(document.body.contains(document.getElementById('shiny-modal-wrapper'))){
    console.log('Timeout!');
    document.getElementById('ss-connect-dialog').style.fontSize = '140%';
    document.getElementById('ss-reload-link').textContent = 'Inactivity timeout, you may reload the analysis.';
    clearInterval(myShinyAppsVar);
  } else {
      //console.log('NO Timeout!');
      if(document.body.contains(document.getElementById('ss-reload-link'))){
        clearInterval(myShinyAppsVar);
	document.getElementById('ss-reload-link').style.display = 'none';
        var a = document.createElement('a');
        var linkText = document.createTextNode("Server error, please contact us with your selections.");
        a.appendChild(linkText);
        a.target = "_blank";
	a.href = "/a/app/contact-us";
        document.getElementById('ss-connect-dialog').appendChild(a);
      }
    }
}


