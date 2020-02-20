
$(document).keydown(function(event){
    if (event.which === 83){
      click_silent_button();
    } else if (event.which === 66){
      click_buzz_button();
    }
});


function click_silent_button(){
  if ($("button:contains('Stay Silent')").length > 0) {
    $("button:contains('Stay Silent')").click();
  }
}

function click_buzz_button(){
  if ($("button:contains('Buzz In')").length > 0) {
    $("button:contains('Buzz In')").click();
  }
}