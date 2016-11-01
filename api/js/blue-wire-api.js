export function bluewire(config) {
let bw = {};
bw.postNew = function(body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '' + config.address + '/new', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.setRequestHeader("Content-Type","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

bw.getAppByApplication = function(application, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '' + config.address + '/app/' + encodeURIComponent(application) + '', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

bw.postAppByApplicationHeartbeat = function(application, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '' + config.address + '/app/' + encodeURIComponent(application) + '/heartbeat', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

bw.getAppByApplicationKicks = function(application, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '' + config.address + '/app/' + encodeURIComponent(application) + '/kicks', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

bw.postAppByApplicationKicks = function(application, body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '' + config.address + '/app/' + encodeURIComponent(application) + '/kicks', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.setRequestHeader("Content-Type","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

bw.postAppByApplicationKicksAdd = function(application, body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '' + config.address + '/app/' + encodeURIComponent(application) + '/kicks/add', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.setRequestHeader("Content-Type","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

bw.getConfig = function(onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '' + config.address + '/config', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

return bw;
}