
var currentIndex = -1;
var elemImage = document.getElementById("image");
var elemDownloadLink = document.getElementById("downloadLink");
var buttonNext = document.getElementById("buttonNext");
var buttonPrev = document.getElementById("buttonPrev");
var indicator = document.getElementById("indicator");
var imageDate = document.getElementById("imageDate");
var imageDesc = document.getElementById("imageDesc");

function loadIndex(idx) {
  if(idx == currentIndex) {
    return;
  }
  disableButtons();
  currentIndex = idx;
  indicator.textContent = idx;
  fetch("https://proxy.xgh.io/www.bing.com/HPImageArchive.aspx?format=js&idx=" + idx + "&n=1&mkt=zh-CN&nc=1489309260264&pid=hp&video=1")
  .then(function(resp) {
    return resp.json();
  })
  .then(function(data) {
    var img = data.images[0];
    var uri = img.url;
    var proxy_url = "https://proxy.xgh.io/www.bing.com" + uri;
    var origin_url = "https://www.bing.com" + uri;
    elemImage.src = proxy_url;
    elemDownloadLink.href = origin_url;
    imageDate.textContent = img.enddate;
    imageDesc.textContent = img.copyright;
    enableButtons();
  });
}

function disableButtons() {
  buttonNext.disabled = "disabled";
  buttonPrev.disabled = "disabled";
}

function enableButtons() {
  if(currentIndex < 7) {
    buttonPrev.disabled = "";
  }
  if(currentIndex > 0) {
    buttonNext.disabled = "";
  }
}

function loadPrev() {
  loadIndex(currentIndex + 1);
}

function loadNext() {
  loadIndex(currentIndex - 1);
}

window.onload = function() {
  loadIndex(0);
};

//# sourceMappingURL=data:application/json;charset=utf8;base64,eyJ2ZXJzaW9uIjozLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiIiwic291cmNlcyI6WyJhcHAuanMiXSwic291cmNlc0NvbnRlbnQiOlsiXG52YXIgY3VycmVudEluZGV4ID0gLTE7XG52YXIgZWxlbUltYWdlID0gZG9jdW1lbnQuZ2V0RWxlbWVudEJ5SWQoXCJpbWFnZVwiKTtcbnZhciBlbGVtRG93bmxvYWRMaW5rID0gZG9jdW1lbnQuZ2V0RWxlbWVudEJ5SWQoXCJkb3dubG9hZExpbmtcIik7XG52YXIgYnV0dG9uTmV4dCA9IGRvY3VtZW50LmdldEVsZW1lbnRCeUlkKFwiYnV0dG9uTmV4dFwiKTtcbnZhciBidXR0b25QcmV2ID0gZG9jdW1lbnQuZ2V0RWxlbWVudEJ5SWQoXCJidXR0b25QcmV2XCIpO1xudmFyIGluZGljYXRvciA9IGRvY3VtZW50LmdldEVsZW1lbnRCeUlkKFwiaW5kaWNhdG9yXCIpO1xudmFyIGltYWdlRGF0ZSA9IGRvY3VtZW50LmdldEVsZW1lbnRCeUlkKFwiaW1hZ2VEYXRlXCIpO1xudmFyIGltYWdlRGVzYyA9IGRvY3VtZW50LmdldEVsZW1lbnRCeUlkKFwiaW1hZ2VEZXNjXCIpO1xuXG5mdW5jdGlvbiBsb2FkSW5kZXgoaWR4KSB7XG4gIGlmKGlkeCA9PSBjdXJyZW50SW5kZXgpIHtcbiAgICByZXR1cm47XG4gIH1cbiAgZGlzYWJsZUJ1dHRvbnMoKTtcbiAgY3VycmVudEluZGV4ID0gaWR4O1xuICBpbmRpY2F0b3IudGV4dENvbnRlbnQgPSBpZHg7XG4gIGZldGNoKFwiaHR0cHM6Ly9wcm94eS54Z2guaW8vd3d3LmJpbmcuY29tL0hQSW1hZ2VBcmNoaXZlLmFzcHg/Zm9ybWF0PWpzJmlkeD1cIiArIGlkeCArIFwiJm49MSZta3Q9emgtQ04mbmM9MTQ4OTMwOTI2MDI2NCZwaWQ9aHAmdmlkZW89MVwiKVxuICAudGhlbihmdW5jdGlvbihyZXNwKSB7XG4gICAgcmV0dXJuIHJlc3AuanNvbigpO1xuICB9KVxuICAudGhlbihmdW5jdGlvbihkYXRhKSB7XG4gICAgdmFyIGltZyA9IGRhdGEuaW1hZ2VzWzBdO1xuICAgIHZhciB1cmkgPSBpbWcudXJsO1xuICAgIHZhciBwcm94eV91cmwgPSBcImh0dHBzOi8vcHJveHkueGdoLmlvL3d3dy5iaW5nLmNvbVwiICsgdXJpO1xuICAgIHZhciBvcmlnaW5fdXJsID0gXCJodHRwczovL3d3dy5iaW5nLmNvbVwiICsgdXJpO1xuICAgIGVsZW1JbWFnZS5zcmMgPSBwcm94eV91cmw7XG4gICAgZWxlbURvd25sb2FkTGluay5ocmVmID0gb3JpZ2luX3VybDtcbiAgICBpbWFnZURhdGUudGV4dENvbnRlbnQgPSBpbWcuZW5kZGF0ZTtcbiAgICBpbWFnZURlc2MudGV4dENvbnRlbnQgPSBpbWcuY29weXJpZ2h0O1xuICAgIGVuYWJsZUJ1dHRvbnMoKTtcbiAgfSk7XG59XG5cbmZ1bmN0aW9uIGRpc2FibGVCdXR0b25zKCkge1xuICBidXR0b25OZXh0LmRpc2FibGVkID0gXCJkaXNhYmxlZFwiO1xuICBidXR0b25QcmV2LmRpc2FibGVkID0gXCJkaXNhYmxlZFwiO1xufVxuXG5mdW5jdGlvbiBlbmFibGVCdXR0b25zKCkge1xuICBpZihjdXJyZW50SW5kZXggPCA3KSB7XG4gICAgYnV0dG9uUHJldi5kaXNhYmxlZCA9IFwiXCI7XG4gIH1cbiAgaWYoY3VycmVudEluZGV4ID4gMCkge1xuICAgIGJ1dHRvbk5leHQuZGlzYWJsZWQgPSBcIlwiO1xuICB9XG59XG5cbmZ1bmN0aW9uIGxvYWRQcmV2KCkge1xuICBsb2FkSW5kZXgoY3VycmVudEluZGV4ICsgMSk7XG59XG5cbmZ1bmN0aW9uIGxvYWROZXh0KCkge1xuICBsb2FkSW5kZXgoY3VycmVudEluZGV4IC0gMSk7XG59XG5cbndpbmRvdy5vbmxvYWQgPSBmdW5jdGlvbigpIHtcbiAgbG9hZEluZGV4KDApO1xufTtcbiJdLCJmaWxlIjoiYXBwLmpzIn0=
