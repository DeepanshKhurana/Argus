export function toggleIconMode(target) {
  const mode = $(target).hasClass('fa-eye') ? 'add' : 'view';
  $(target).toggleClass('fa-eye fa-eye-dropper');
  Shiny.setInputValue(
    'app-app_mode',
    mode,
    { priority: 'event' },
  );
}

export function attachToggleIconListener() {
  $('.argus-icon').click((evt) => {
    const target = evt.currentTarget;
    toggleIconMode(target);
  });
}

window.onload = attachToggleIconListener;
