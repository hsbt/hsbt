$( function() {
  if (stompUrl) {
    var client = Stomp.client(stompUrl);
  }
});

client.connect(null, null, function() {
  client.subscribe('/stomp/status', function(message) {
    var s = $.parseJSON(message.body);
    $.each(s, function() {
      onNewStatus(this);
    });
  });
});

var onNewStatus = function(status) {
  $('#statusTable > tbody > tr:first').before(
    '<tr>' +
      '<td>'+status.creator+'</td>' +
      '<td>'+status.created_at+'</td>' +
      '<td>' +
      '<a href="/customers/retweet/' + status.id + '" ' +
      'data-method="post" rel="nofollow">Retweet</a>' +
      '</td>' +
      '<td>' + status.status_text + '</td>' +
      '</tr>'
  );
};
