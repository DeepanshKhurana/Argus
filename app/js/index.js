export function toggleIcon() {
  $('.argus-icon').click((evt) => {
    target = evt.currentTarget;

    var target = evt.currentTarget;

    // This condition is reversed becuase if the eye has a class, that means
    // it will be toggled. In other words, hasClass tells wasClass

    var mode = $(target).hasClass('fa-eye') ? 'add' : 'view';

    $(target).toggleClass('fa-eye fa-eye-dropper');

    Shiny.setInputValue(
      'app-app_mode',
      mode,
      { priority: 'event' }
      );
  });
};

window.onload = toggleIcon;
