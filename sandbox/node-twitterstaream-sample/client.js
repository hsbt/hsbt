var tweet_list = $("#tweets");
  
function load_tweets() {
  $.getJSON("/stream", function(tweets) {
    $.each(tweets, function() {
      $("<li>").html(this.text).prependTo(tweet_list);
    });
    load_tweets();
  });
}

setTimeout(load_tweets, 1000);
