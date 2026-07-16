package com.webforj.devtools.livereload;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import jakarta.servlet.DispatcherType;
import jakarta.servlet.FilterRegistration;
import jakarta.servlet.ServletContext;
import java.util.EnumSet;
import java.util.Set;
import org.junit.jupiter.api.Test;

class LiveReloadServletContainerInitializerTest {

  @Test
  void shouldRegisterTheContextListenerOnStartup() {
    ServletContext context = mock(ServletContext.class);
    when(context.addFilter(anyString(), any(LiveReloadRestartFilter.class)))
        .thenReturn(mock(FilterRegistration.Dynamic.class));

    new LiveReloadServletContainerInitializer().onStartup(Set.of(), context);

    verify(context).addListener(any(WatchContextListener.class));
  }

  @Test
  void shouldStartWhenTheFilterNameIsAlreadyRegistered() {
    ServletContext context = mock(ServletContext.class);
    when(context.addFilter(anyString(), any(LiveReloadRestartFilter.class))).thenReturn(null);

    new LiveReloadServletContainerInitializer().onStartup(Set.of(), context);

    verify(context).addListener(any(WatchContextListener.class));
  }

  @Test
  void shouldRegisterTheRestartNoticeFilterOnEveryRequest() {
    ServletContext context = mock(ServletContext.class);
    FilterRegistration.Dynamic registration = mock(FilterRegistration.Dynamic.class);
    when(context.addFilter(anyString(), any(LiveReloadRestartFilter.class)))
        .thenReturn(registration);

    new LiveReloadServletContainerInitializer().onStartup(Set.of(), context);

    verify(registration).addMappingForUrlPatterns(eq(EnumSet.of(DispatcherType.REQUEST)), eq(false),
        eq("/*"));
  }
}
