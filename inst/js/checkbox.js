$(window).on("shown.bs.modal", function () {
  $("td:not(.radio-active)")
    .has("input")
    .addClass("radio-active")
    .children()
    .wrap("<label class='radio-parent'></label>");
  $("td.radio-active label.radio-parent").append(
    "<span class='checkmark fade in'></span>"
  );
});
