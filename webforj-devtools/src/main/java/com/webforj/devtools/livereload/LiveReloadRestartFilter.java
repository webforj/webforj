package com.webforj.devtools.livereload;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import java.io.IOException;

/**
 * Sends the restart notice when the container tears the deployment down.
 *
 * <p>
 * A servlet container destroys the filters before the servlets and both before it notifies any
 * context listener, so the destruction of this filter is the earliest standard callback of a
 * redeploy. Sending the notice here reaches the browsers while the sessions are still untouched,
 * and the notice from {@link WatchContextListener#contextDestroyed} stays as the fallback for a
 * container with another teardown order. The filter passes every request through unchanged.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class LiveReloadRestartFilter implements Filter {

  private final LiveReloadLifecycle lifecycle;

  /**
   * Creates the filter bound to the given lifecycle.
   *
   * @param lifecycle the live reload lifecycle owned by the deployment
   */
  public LiveReloadRestartFilter(LiveReloadLifecycle lifecycle) {
    this.lifecycle = lifecycle;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
      throws IOException, ServletException {
    chain.doFilter(request, response);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy() {
    lifecycle.notifyRestarting();
  }
}
