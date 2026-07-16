package com.webforj.spring;

import com.webforj.servlet.WebforjServlet;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.core.Ordered;

/**
 * Destroys the {@link WebforjServlet} when the application context closes.
 *
 * <p>
 * The servlet cleanup releases licensing and terminates the runtime threads. Closing the context is
 * the only signal that fires on every shutdown, including restarts triggered by Spring devtools, so
 * the servlet is destroyed here explicitly instead of relying on the embedded container. The
 * listener runs at the lowest precedence, so listeners that must reach the connected browsers
 * before the sessions are torn down, such as the devtools restart notice, always run first.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class WebforjServletShutdownListener
    implements ApplicationListener<ContextClosedEvent>, Ordered {

  private static final System.Logger logger =
      System.getLogger(WebforjServletShutdownListener.class.getName());

  private final ServletRegistrationBean<WebforjServlet> registration;

  /**
   * Creates the listener bound to the webforJ servlet registration.
   *
   * @param registration the webforJ servlet registration
   */
  public WebforjServletShutdownListener(ServletRegistrationBean<WebforjServlet> registration) {
    this.registration = registration;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onApplicationEvent(ContextClosedEvent event) {
    logger.log(System.Logger.Level.DEBUG, "Destroying WebforjServlet on context close");
    registration.getServlet().destroy();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getOrder() {
    return Ordered.LOWEST_PRECEDENCE;
  }
}
