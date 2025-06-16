package com.webforj.spring;

import com.webforj.conceiver.DefaultConceiver;
import com.webforj.conceiver.exception.ConceiverException;
import java.lang.System.Logger;
import org.springframework.beans.BeanInstantiationException;
import org.springframework.context.ApplicationContext;

/**
 * A spring conceiver that uses Spring to provide beans.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class SpringConceiver extends DefaultConceiver {
  private static final Logger logger = System.getLogger(SpringConceiver.class.getName());

  /**
   * {@inheritDoc}
   */
  @Override
  public <T> T get(Class<T> classOfT) {
    ApplicationContext context = getApplicationContext();

    logger.log(Logger.Level.DEBUG, "Resolving instance of {0} via SpringConceiver",
        classOfT.getName());

    try {
      String[] beanNames = context.getBeanNamesForType(classOfT);

      if (beanNames.length == 1) {
        logger.log(Logger.Level.DEBUG,
            "Located single bean of type {0}, returning existing instance", classOfT.getName());
        return context.getBean(classOfT);
      } else if (beanNames.length > 1) {
        logger.log(Logger.Level.DEBUG,
            "Multiple beans ({0}) of type {1} detected, instantiating new object", beanNames.length,
            classOfT.getName());
        try {
          return context.getAutowireCapableBeanFactory().createBean(classOfT);
        } catch (BeanInstantiationException e) {
          throw new ConceiverException(String.format(
              "Unable to create instance of %s. Multiple beans of this type (%d) exist in the "
                  + "Spring context. Consider using @Primary annotation or qualifying the bean to "
                  + "resolve autowiring conflicts.",
              classOfT.getName(), beanNames.length), e);
        }
      } else {
        logger.log(Logger.Level.DEBUG,
            "No existing bean of type {0} available, creating new instance", classOfT.getName());
        return context.getAutowireCapableBeanFactory().createBean(classOfT);
      }
    } catch (ConceiverException e) {
      throw e;
    } catch (Exception e) {
      throw new ConceiverException("SpringConceiver failed to instantiate " + classOfT.getName()
          + ". Check Spring configuration and dependencies.", e);
    }
  }

  /**
   * Gets the ApplicationContext.
   *
   * @return the ApplicationContext
   * @throws ConceiverException if the ApplicationContext is not available
   */
  private ApplicationContext getApplicationContext() {
    var context = ContextHolder.getContext();
    if (context == null) {
      throw new ConceiverException("No ApplicationContext available. "
          + "SpringConceiver requires Spring context to be properly initialized.");
    }

    return context;
  }
}
