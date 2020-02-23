
$(document).keydown(function(event){
    if (event.which === 83){
      click_button("Stay Silent");
    } else if (event.which === 66){
      click_button("Buzz In");
    } else if (event.which === 13){
      click_button("Submit Answer");
      click_button("Close");
      click_button("Thanks Alex!");
      click_button("Make Wager!");
    }
});


function click_button(button_label){
  const target = "button:contains('" + button_label + "')"; 
  if ($(target).length > 0) {
    if (!$(target).hasClass("disabled")){
       $(target).click();
    }
  }
}

Shiny.addCustomMessageHandler("refocus", function(NULL) {
  $(".answer-div").children().children()[1].focus();
    //document.getElementByClassName("shiny-input-container").focus();
  }
);