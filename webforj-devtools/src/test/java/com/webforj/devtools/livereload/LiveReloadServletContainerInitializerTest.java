package com.webforj.devtools.livereload;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import jakarta.servlet.ServletContext;
import java.util.Set;
import org.junit.jupiter.api.Test;

class LiveReloadServletContainerInitializerTest {

  @Test
  void shouldRegisterTheContextListenerOnStartup() {
    ServletContext context = mock(ServletContext.class);

    new LiveReloadServletContainerInitializer().onStartup(Set.of(), context);

    verify(context).addListener(any(WatchContextListener.class));
  }
}
