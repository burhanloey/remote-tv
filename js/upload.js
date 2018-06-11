var form = document.getElementsByTagName('form')[0];
var uploadButton = document.getElementById('upload');
var progressbar = document.querySelector('.progress .progress-bar');
var progressbarText = document.querySelector('.progress .progress-bar span');

var nonanimated = progressbar.getAttribute('class');
var animated = nonanimated + ' progress-bar-striped active';

function updateProgressBar(percentComplete) {
    uploadButton.disabled = true;
    progressbar.setAttribute('class', animated);
    progressbar.setAttribute('style', 'width:' + percentComplete + '%');
    progressbar.setAttribute('aria-valuenow', percentComplete);
    progressbarText.innerHTML = percentComplete + '%';
}

function updateProgress(event) {
    if (event.lengthComputable) {
        var rawPercentComplete = event.loaded / event.total * 100;
        var percentComplete = rawPercentComplete.toFixed(0);
        updateProgressBar(percentComplete);
    }
}

function completeUpload(event) {
    uploadButton.disabled = false;
    progressbar.setAttribute('class', nonanimated);
}

function handleResponse(event) {
    progressbarText.innerHTML = this.responseText;
}

function submit() {
    var data = new FormData(form);

    var xhr = new XMLHttpRequest();
    xhr.upload.addEventListener('progress', updateProgress, true);
    xhr.upload.addEventListener('load', completeUpload);
    xhr.addEventListener('load', handleResponse);
    xhr.open('POST', '/play');
    xhr.send(data);
}
