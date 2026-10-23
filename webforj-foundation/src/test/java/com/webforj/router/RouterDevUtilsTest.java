package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.component.Component;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class RouterDevUtilsTest {

  @Test
  void shouldLogNavigationWithoutOptions() {
    Environment environment = mock(Environment.class);
    Page page = mock(Page.class);

    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class);
        MockedStatic<Page> mockedPage = mockStatic(Page.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
      mockedPage.when(Page::getCurrent).thenReturn(page);
      when(environment.isDebug()).thenReturn(true);

      NavigationContext context = new NavigationContext();
      context.setLocation(new Location("/test"));
      context.setRouteParameters(new ParametersBag());

      assertDoesNotThrow(() -> RouterDevUtils.logNavigationAction(context, Component.class));
      verify(page).executeJsVoidAsync(anyString());
    }
  }

  @Test
  void shouldLogNavigationValuesAsStringLiterals() {
    Environment environment = mock(Environment.class);
    Page page = mock(Page.class);

    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class);
        MockedStatic<Page> mockedPage = mockStatic(Page.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
      mockedPage.when(Page::getCurrent).thenReturn(page);
      when(environment.isDebug()).thenReturn(true);

      NavigationContext context = new NavigationContext();
      context.setLocation(new Location("/about?q=it's);#a'("));
      context.setRouteParameters(new ParametersBag(Map.of("id", "it's)")));
      ArgumentCaptor<String> script = ArgumentCaptor.forClass(String.class);

      RouterDevUtils.logNavigationAction(context, Component.class);

      verify(page).executeJsVoidAsync(script.capture());
      assertTrue(script.getValue().contains("""
          '%cNavigation:%c ' + "/about?q\\u003dit\\u0027s);#a\\u0027(\""""));
      assertTrue(script.getValue().contains("""
          console.info('Component :', "com.webforj.component.Component");"""));
      assertTrue(script.getValue().contains("""
          console.info("id : ", "it\\u0027s)");"""));
      assertFalse(script.getValue().contains("it's"));
    }
  }
}
