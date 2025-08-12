package com.webforj.spring;

import java.lang.System.Logger;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

/**
 * Injects the {@link ApplicationContext} into the {@link ContextHolder}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@Component
public class ContextInjector implements ApplicationContextAware, DisposableBean {
  Logger logger = System.getLogger(ContextInjector.class.getName());

  @Override
  public void setApplicationContext(ApplicationContext context) throws BeansException {
    ContextHolder.setContext(context);
    logger.log(Logger.Level.DEBUG, "Injected ApplicationContext into ContextHolder.");
  }

  @Override
  public void destroy() throws Exception {
    ContextHolder.clear();
    logger.log(Logger.Level.DEBUG, "Cleared ApplicationContext from ContextHolder.");
  }
}
