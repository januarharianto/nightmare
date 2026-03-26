// Arrow key navigation for search results
$(document).on('keydown', '#search-search_box', function(e) {
  if (e.key !== 'ArrowDown' && e.key !== 'ArrowUp' && e.key !== 'Enter') return;

  var items = $('.search-result-item');
  if (items.length === 0) return;

  var activeIndex = items.index($('.search-result-item.keyboard-active'));

  if (e.key === 'ArrowDown') {
    e.preventDefault();
    activeIndex = (activeIndex < items.length - 1) ? activeIndex + 1 : 0;
  } else if (e.key === 'ArrowUp') {
    e.preventDefault();
    activeIndex = (activeIndex > 0) ? activeIndex - 1 : items.length - 1;
  } else if (e.key === 'Enter') {
    if (activeIndex >= 0) {
      e.preventDefault();
      items.eq(activeIndex).click();
      return;
    }
  }

  items.removeClass('keyboard-active');
  var activeItem = items.eq(activeIndex);
  activeItem.addClass('keyboard-active');

  // Scroll into view
  var container = activeItem.closest('.search-results-list');
  if (container.length) {
    var itemTop = activeItem.position().top;
    var itemHeight = activeItem.outerHeight();
    var containerHeight = container.height();
    var scrollTop = container.scrollTop();

    if (itemTop < 0) {
      container.scrollTop(scrollTop + itemTop);
    } else if (itemTop + itemHeight > containerHeight) {
      container.scrollTop(scrollTop + itemTop + itemHeight - containerHeight);
    }
  }
});

// Clear keyboard selection when search results change
$(document).on('shiny:value', function(e) {
  if (e.name && e.name.indexOf('search_results_list') !== -1) {
    $('.search-result-item.keyboard-active').removeClass('keyboard-active');
  }
});
