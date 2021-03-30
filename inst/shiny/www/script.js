// script.js
function getCookies(){
  var res = Cookies.get();
  Shiny.setInputValue('cookies', res);
}

// script.js
Shiny.addCustomMessageHandler('cookie-set', function(msg){
  Cookies.set(msg.name, msg.value);
  getCookies();
})

//Shiny.addCustomMessageHandler('cookie-remove', function(msg){
//  Cookies.remove(msg.name);
//  getCookies();
//})

// script.js
$(document).on('shiny:connected', function(ev){
  getCookies();
})