// check if timeout exists upon disconnect, if so we offer to reload
// if not a timeout, it is a bug and we offer link to contact us

// TODO: get this global var from R
var timeoutSeconds = 15;

var myShinyAppsVar = setInterval(myCheckForTimeout, 1000, timeoutSeconds);

var myStyleApps = document.createElement('style');
myStyleApps.innerHTML = `
div#ss-connect-dialog {
  width: 28em;
  top: 10em;
  height: 6em;
  border: 2px solid blue;
  font-size: 110%;
  background-color: white;
}
div#ss-connect-dialog a, div#ss-connect-dialog a:visited {
 color: blue;
 text-decoration: underline;
}
div#ss-connect-dialog label {
  color: black;
}
div.modal-body {
  display: none;
}
div#shiny-disconnected-overlay, div.modal-backdrop.fade.in  {
  opacity: 0;
}
div#ss-overlay {
  opacity: 0.1;
}
`;
document.head.appendChild(myStyleApps);

function myCheckForTimeout(timeoutSeconds) {
  if(document.body.contains(document.getElementById('shiny-modal-wrapper'))){
    console.log('Timeout!');
    document.querySelector('#ss-connect-dialog label').textContent = 'Timeout due to ' + timeoutSeconds  + 'mn of inactivity';
    document.getElementById('ss-reload-link').textContent = 'Reload the analysis';
    clearInterval(myShinyAppsVar);
  } else {
      //console.log('NO Timeout!');
      if(document.body.contains(document.getElementById('ss-reload-link'))){
        clearInterval(myShinyAppsVar);
	document.getElementById('ss-reload-link').style.display = 'none';
        document.querySelector('#ss-connect-dialog label').textContent = 'You hit a bug: open a new analysis and select different parameters';
        var a = document.createElement('a');
        var linkText = document.createTextNode("Please report this bug");
        a.appendChild(linkText);
        a.target = "_blank";
	a.href = "/a/app/contact-us";
        document.getElementById('ss-connect-dialog').appendChild(a);
      }
    }
}


