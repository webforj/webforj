package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.component.Component;
import com.webforj.component.Composite;
import com.webforj.component.element.Element;
import org.junit.jupiter.api.Test;

class WebforjSecurityConfigurerRouteTest {

  @Test
  void shouldRejectNullLoginPageComponent() {
    var configurer = WebforjSecurityConfigurer.webforj();
    var error = assertThrows(NullPointerException.class,
        () -> configurer.loginPage((Class<? extends Component>) null));

    assertEquals("Login page component must not be null", error.getMessage());
  }

  @Test
  void shouldRejectNullOauth2LoginPageComponent() {
    var configurer = WebforjSecurityConfigurer.webforj();
    var error = assertThrows(NullPointerException.class, () -> configurer.oauth2Login(null));

    assertEquals("Login page component must not be null", error.getMessage());
  }

  @Test
  void shouldRejectOauth2LoginPageComponentWithoutRoute() {
    var configurer = WebforjSecurityConfigurer.webforj();
    var error = assertThrows(IllegalArgumentException.class,
        () -> configurer.oauth2Login(UnroutedView.class));

    assertEquals("Failed to resolve route path for component: " + UnroutedView.class.getName(),
        error.getMessage());
  }

  @Test
  void shouldRejectNullAccessDeniedPageComponent() {
    var configurer = WebforjSecurityConfigurer.webforj();
    var error = assertThrows(NullPointerException.class,
        () -> configurer.accessDeniedPage((Class<? extends Component>) null));

    assertEquals("Access denied page component must not be null", error.getMessage());
  }

  static class UnroutedView extends Composite<Element> {
  }
}
