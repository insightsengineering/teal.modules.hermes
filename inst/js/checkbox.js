// used to add the css class for the custom
// radiomatrix buttons on the modal for combination assignment
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
