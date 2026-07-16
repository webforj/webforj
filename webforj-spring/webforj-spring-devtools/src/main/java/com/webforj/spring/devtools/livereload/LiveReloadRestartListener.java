package com.webforj.spring.devtools.livereload;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.core.Ordered;

/**
 * Notifies the connected browsers before the context closes for a restart.
 *
 * <p>
 * The listener runs at the highest precedence, ahead of the listener that destroys the webforJ
 * servlet, so the browsers learn about the restart before the sessions are torn down and keep their
 * page as it is until the reload brings the application back.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class LiveReloadRestartListener implements ApplicationListener<ContextClosedEvent>, Ordered {

  private final LiveReloadLifecycle lifecycle;

  /**
   * Creates the listener bound to the live reload lifecycle.
   *
   * @param lifecycle the live reload lifecycle owned by the context
   */
  public LiveReloadRestartListener(LiveReloadLifecycle lifecycle) {
    this.lifecycle = lifecycle;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onApplicationEvent(ContextClosedEvent event) {
    lifecycle.notifyRestarting();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getOrder() {
    return Ordered.HIGHEST_PRECEDENCE;
  }
}
