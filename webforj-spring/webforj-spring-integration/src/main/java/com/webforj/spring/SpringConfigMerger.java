package com.webforj.spring;

import com.typesafe.config.Config;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.annotation.AppListenerPriority;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import org.springframework.context.ApplicationContext;

/**
 * Merges Spring Boot configuration into the webforJ environment during application initialization.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@AppListenerPriority(0)
public class SpringConfigMerger implements AppLifecycleListener {
  private static final Logger logger = System.getLogger(SpringConfigMerger.class.getName());

  @Override
  public void onWillCreate(Environment env) {
    ApplicationContext context = ContextHolder.getContext();

    if (context != null) {
      try {
        // Get the specific webforJ config bean by name
        Config springConfig = context.getBean("webforjConfig", Config.class);

        logger.log(Level.INFO, "Merging Spring configuration into webforJ environment");
        env.setConfig(springConfig);
        logger.log(Level.DEBUG, "Spring configuration successfully merged");
      } catch (Exception e) {
        logger.log(Level.DEBUG, "No webforjConfig bean found in Spring context", e);
      }
    } else {
      logger.log(Level.DEBUG, "No Spring ApplicationContext available");
    }
  }
}
