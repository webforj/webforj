package com.webforj.spring.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import org.junit.jupiter.api.Test;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.core.Ordered;

class LiveReloadRestartListenerTest {

  @Test
  void shouldNotifyRestartingOnContextClose() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    new LiveReloadRestartListener(lifecycle).onApplicationEvent(mock(ContextClosedEvent.class));

    verify(lifecycle).notifyRestarting();
  }

  @Test
  void shouldRunAtTheHighestPrecedence() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);

    assertEquals(Ordered.HIGHEST_PRECEDENCE, new LiveReloadRestartListener(lifecycle).getOrder());
  }
}
