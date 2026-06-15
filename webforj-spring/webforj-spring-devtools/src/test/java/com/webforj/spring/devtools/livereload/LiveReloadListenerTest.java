package com.webforj.spring.devtools.livereload;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import com.webforj.devtools.livereload.LiveReloadOptions;
import org.junit.jupiter.api.Test;
import org.springframework.context.event.ContextRefreshedEvent;

class LiveReloadListenerTest {

  @Test
  void shouldStartTheLifecycleOnContextRefreshed() {
    LiveReloadLifecycle lifecycle = mock(LiveReloadLifecycle.class);
    LiveReloadOptions options = new LiveReloadOptions().setEnabled(true).setWebsocketPort(40000);

    new LiveReloadListener(lifecycle, options)
        .onApplicationEvent(mock(ContextRefreshedEvent.class));

    verify(lifecycle).start(options);
  }
}
