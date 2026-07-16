package com.webforj.devtools.livereload;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import java.io.IOException;
import org.junit.jupiter.api.Test;

class LiveReloadRestartFilterTest {

  @Test
  void shouldPassEveryRequestThrough() throws IOException, ServletException {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    ServletRequest request = mock(ServletRequest.class);
    ServletResponse response = mock(ServletResponse.class);
    FilterChain chain = mock(FilterChain.class);

    new LiveReloadRestartFilter(lifecycle).doFilter(request, response, chain);

    verify(chain).doFilter(request, response);
    verifyNoInteractions(lifecycle);
  }

  @Test
  void shouldNotifyRestartingOnDestroy() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadRestartFilter(lifecycle).destroy();

    verify(lifecycle).notifyRestarting();
  }
}
