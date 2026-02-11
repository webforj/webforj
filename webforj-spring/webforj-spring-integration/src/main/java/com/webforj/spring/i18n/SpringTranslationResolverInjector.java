package com.webforj.spring.i18n;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.environment.ObjectTable;
import com.webforj.i18n.TranslationResolver;
import com.webforj.spring.ContextHolder;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;

/**
 * Injects the Spring-configured {@link TranslationResolver} into webforJ during application
 * initialization.
 *
 * <p>
 * The listener runs early in the application lifecycle to ensure the translation resolver is
 * available before any components attempt to use translations. If a resolver has already been set
 * programmatically, the Spring resolver will not override it.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@AppListenerPriority(0)
public class SpringTranslationResolverInjector implements AppLifecycleListener {
  private static final Logger logger =
      System.getLogger(SpringTranslationResolverInjector.class.getName());

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    // Skip if a resolver was already set programmatically
    if (ObjectTable.contains(TranslationResolver.class.getName())) {
      logger.log(Level.DEBUG, "TranslationResolver already configured, skipping Spring injection");
      return;
    }

    ApplicationContext context = ContextHolder.getContext();

    if (context != null) {
      try {
        TranslationResolver resolver = context.getBean(TranslationResolver.class);
        App.setTranslationResolver(resolver);
        logger.log(Level.DEBUG, "Injected Spring TranslationResolver into webforJ");
      } catch (NoSuchBeanDefinitionException e) {
        logger.log(Level.DEBUG, "No TranslationResolver bean found in Spring context");
      } catch (Exception e) {
        logger.log(Level.WARNING, "Failed to inject TranslationResolver from Spring context", e);
      }
    } else {
      logger.log(Level.DEBUG, "No Spring ApplicationContext available");
    }
  }
}
