package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.servlet.WebforjServlet;
import org.junit.jupiter.api.Test;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.core.Ordered;

class WebforjServletShutdownListenerTest {

  @Test
  @SuppressWarnings("unchecked")
  void shouldDestroyTheServletOnContextClose() {
    ServletRegistrationBean<WebforjServlet> registration = mock(ServletRegistrationBean.class);
    WebforjServlet servlet = mock(WebforjServlet.class);
    when(registration.getServlet()).thenReturn(servlet);

    new WebforjServletShutdownListener(registration)
        .onApplicationEvent(mock(ContextClosedEvent.class));

    verify(servlet).destroy();
  }

  @Test
  @SuppressWarnings("unchecked")
  void shouldRunAtTheLowestPrecedence() {
    ServletRegistrationBean<WebforjServlet> registration = mock(ServletRegistrationBean.class);

    assertEquals(Ordered.LOWEST_PRECEDENCE,
        new WebforjServletShutdownListener(registration).getOrder());
  }
}
