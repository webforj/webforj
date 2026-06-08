package com.webforj.spring.devtools.livereload;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import com.webforj.devtools.livereload.LiveReloadOptions;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 * Brings the live reload up on every Spring context start.
 *
 * <p>
 * A DevTools restart closes the context, which stops the previous live reload and releases the
 * reload port, then opens a new one. This listener starts the live reload on the new context, so
 * the browser that dropped when the previous context closed reconnects and reloads itself. The
 * reload after a Java change therefore needs no surviving state across the restart.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class LiveReloadListener implements ApplicationListener<ContextRefreshedEvent> {

  private final LiveReloadLifecycle lifecycle;
  private final LiveReloadOptions options;

  /**
   * Creates the listener bound to the live reload lifecycle and its configuration.
   *
   * @param lifecycle the live reload lifecycle owned by the context
   * @param options the live reload configuration
   */
  public LiveReloadListener(LiveReloadLifecycle lifecycle, LiveReloadOptions options) {
    this.lifecycle = lifecycle;
    this.options = options;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onApplicationEvent(ContextRefreshedEvent event) {
    lifecycle.start(options);
  }
}
