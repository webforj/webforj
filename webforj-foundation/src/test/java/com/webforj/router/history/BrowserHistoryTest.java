package com.webforj.router.history;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

import com.webforj.Page;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class BrowserHistoryTest {

  MockedStatic<Page> mockedPage;
  Page page;
  BrowserHistory history;

  @BeforeEach
  void setUp() {
    page = mock(Page.class);
    mockedPage = mockStatic(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(page);
    mockedPage.when(Page::isPresent).thenCallRealMethod();
    mockedPage.when(() -> Page.ifPresent(any())).thenCallRealMethod();
    history = new BrowserHistory();
  }

  @AfterEach
  void teardown() {
    mockedPage.close();
  }

  @Test
  void shouldPushTheLocationAsStringLiteral() {
    ArgumentCaptor<String> script = ArgumentCaptor.forClass(String.class);

    history.pushState(null, new Location("/about?q=it's);#a'("));

    verify(page).executeJsVoidAsync(script.capture());
    assertEquals("""
        window.history.pushState(JSON.parse(atob('e30=')), '', \
        "/about?q\\u003dit\\u0027s);#a\\u0027(")""", script.getValue());
  }

  @Test
  void shouldReplaceTheLocationAsStringLiteral() {
    ArgumentCaptor<String> script = ArgumentCaptor.forClass(String.class);

    history.replaceState(null, new Location("/about?q=it's);#a'("));

    verify(page).executeJsVoidAsync(script.capture());
    assertEquals("""
        window.history.replaceState(JSON.parse(atob('e30=')), '', \
        "/about?q\\u003dit\\u0027s);#a\\u0027(")""", script.getValue());
  }
}
