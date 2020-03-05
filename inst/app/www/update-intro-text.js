


function update_intro(msg){
  const target = $(".intro-text");
  if (target.length == 1){
    target.html(msg);
  }
}

Shiny.addCustomMessageHandler("update_intro_text", update_intro);