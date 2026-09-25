package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.component.Component;
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
  void shouldRejectNullAccessDeniedPageComponent() {
    var configurer = WebforjSecurityConfigurer.webforj();
    var error = assertThrows(NullPointerException.class,
        () -> configurer.accessDeniedPage((Class<? extends Component>) null));

    assertEquals("Access denied page component must not be null", error.getMessage());
  }
}
