export function toggleIcon() {
  $('.argus-icon').click((evt) => {
    const target = evt.currentTarget;
    const selectedApp = $('#app-selector-application').val();
    const selectedTable = $('#app-selector-table').val();

    // This condition is reversed becuase if the eye has a class, that means
    // it will be toggled. In other words, hasClass tells wasClass

    const mode = $(target).hasClass('fa-eye') ? 'add' : 'view';
    $(target).toggleClass('fa-eye fa-eye-dropper');

    Shiny.setInputValue(
      'app-app_mode',
      mode,
      { priority: 'event' },
    );

    Shiny.setInputValue(
      'app-selected_app',
      selectedApp,
      { priority: 'event' },
    );

    Shiny.setInputValue(
      'app-selected_table',
      selectedTable,
      { priority: 'event' },
    );
  });
}

window.onload = toggleIcon;
