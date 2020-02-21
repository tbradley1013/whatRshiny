
$(document).keydown(function(event){
    if (event.which === 83){
      click_button("Stay Silent");
    } else if (event.which === 66){
      click_button("Buzz In");
    } else if (event.which === 13){
      click_button("Submit Answer");
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