// Toggle enable/disable of a dropdown
//
// This can be done by `{shinyjs::enable/disable}` R package if that package is
// added to the dependecies.
// Parameters
//  `message` should have `input_id` string and `disabled` logical properties.
Shiny.addCustomMessageHandler("toggle_dropdown", function (message) {
  console.log(message);
  const input_id = message.input_id;
  const disabled = message.disabled;

  let el = document.getElementById(input_id);

  if (el.selectize !== undefined) {
    el = el.selectize;
    if (disabled) {
      el.lock();
      el.disable();
    } else {
      el.unlock();
      el.enable();
    }
  } else {
    // Fallback in case selectize is not enabled
    el.disabled = disabled ? "disabled" : "";
  }
});
