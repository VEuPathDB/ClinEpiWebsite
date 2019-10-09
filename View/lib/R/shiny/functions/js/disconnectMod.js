// check if timeout exists upon disconnect
//
var myVar = setInterval(myCheckForTimeout, 1000);
function myCheckForTimeout() {
 if(document.body.contains(document.getElementById('shiny-modal-wrapper'))){
        console.log('Timeout!');
        //document.getElementById('ss-reload-link').style.color = 'red';
        document.getElementById('ss-reload-link').textContent = 'Inactivity timeout, please reload the analysis.';
        clearInterval(myVar);
    } else {
        //console.log('NO Timeout!');
        if(document.body.contains(document.getElementById('ss-reload-link'))){
          clearInterval(myVar);
	  document.getElementById('ss-reload-link').textContent = 'Server error, please contact us with your selections.';
          //document.getElementById('ss-reload-link').style.color = 'blue';
       }
    }
}


