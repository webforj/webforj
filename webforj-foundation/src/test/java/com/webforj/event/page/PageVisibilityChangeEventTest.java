package com.webforj.event.page;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.webforj.Page;
import com.webforj.PageVisibilityState;
import org.junit.jupiter.api.Test;

class PageVisibilityChangeEventTest {

  @Test
  void shouldExposeStateAndPage() {
    Page page = mock(Page.class);

    PageVisibilityChangeEvent event =
        new PageVisibilityChangeEvent(page, PageVisibilityState.VISIBLE);

    assertSame(page, event.getPage());
    assertSame(page, event.getSource());
    assertEquals(PageVisibilityState.VISIBLE, event.getState());
  }

  @Test
  void shouldReportVisibleWhenStateIsVisible() {
    PageVisibilityChangeEvent event =
        new PageVisibilityChangeEvent(mock(Page.class), PageVisibilityState.VISIBLE);

    assertTrue(event.isVisible());
    assertFalse(event.isHidden());
  }

  @Test
  void shouldReportHiddenWhenStateIsHidden() {
    PageVisibilityChangeEvent event =
        new PageVisibilityChangeEvent(mock(Page.class), PageVisibilityState.HIDDEN);

    assertTrue(event.isHidden());
    assertFalse(event.isVisible());
  }
}
