package com.webforj.spring;

import com.webforj.conceiver.DefaultConceiver;
import com.webforj.conceiver.exception.ConceiverException;
import java.lang.System.Logger;
import org.springframework.context.ApplicationContext;

/**
 * A spring conceiver that uses Spring to provide beans.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
public class SpringConceiver extends DefaultConceiver {
  Logger logger = System.getLogger(SpringConceiver.class.getName());

  @Override
  public <T> T get(Class<T> classOfT) {
    ApplicationContext context = ContextHolder.getContext();
    if (context == null) {
      throw new ConceiverException("No application context found in ContextHolder.");
    }

    logger.log(Logger.Level.DEBUG,
        "Getting bean of type " + classOfT.getName() + " using SpringConceiver.");

    try {
      if (context.getBeanNamesForType(classOfT).length == 1) {
        logger.log(Logger.Level.DEBUG, "Found bean of type " + classOfT.getName() + " in context.");
        return context.getBean(classOfT);
      } else {
        logger.log(Logger.Level.DEBUG,
            "Creating bean of type " + classOfT.getName() + " in context.");
        return context.getAutowireCapableBeanFactory().createBean(classOfT);
      }
    } catch (Exception e) {
      throw new ConceiverException(
          "Failed to get bean of type " + classOfT.getName() + " using SpringConceiver.", e);
    }
  }
}
