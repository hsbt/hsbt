window.onload = function() {
  var source = new EventSource('/live_assets/sse');
  source.addEventListener('reloadCSS', function(e) {
    var sheets = document.querySelectorAll('[rel=stylesheet]');
    var forEach = Array.prototype.forEach;
    forEach.call(sheets, function(sheet){
      var clone = sheet.cloneNode();
      clone.addEventListener('load', function() {
	sheet.parentNode.removeChild(sheet);
      });
      document.head.appendChild(clone);
    });
  });
};
