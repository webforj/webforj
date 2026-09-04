package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
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
import org.junit.jupiter.api.Test;
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
}
