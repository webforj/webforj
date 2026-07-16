package com.webforj.spring.devtools.livereload;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import com.webforj.devtools.livereload.LiveReloadOptions;
import com.webforj.spring.SpringConfigurationProperties;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Registers the Spring DevTools restart to browser reload bridge.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@Configuration
@ConditionalOnClass(name = "org.springframework.boot.devtools.restart.Restarter")
@ConditionalOnProperty(prefix = "webforj.devtools.livereload", name = "enabled",
    havingValue = "true", matchIfMissing = false)
public class LiveReloadSocketConfiguration {

  /**
   * Creates the live reload lifecycle owned by the context, stopped on teardown.
   *
   * @return the live reload lifecycle
   */
  @Bean(destroyMethod = "stop")
  LiveReloadLifecycle liveReloadLifecycle() {
    return new LiveReloadLifecycle();
  }

  /**
   * Creates the listener that brings the live reload up on every context start.
   *
   * @param properties the webforJ configuration bound by the Spring integration
   * @param lifecycle the live reload lifecycle owned by the context
   * @return the context start to live reload listener
   */
  @Bean
  LiveReloadListener liveReloadListener(SpringConfigurationProperties properties,
      LiveReloadLifecycle lifecycle) {
    return new LiveReloadListener(lifecycle, readOptions(properties.getDevtools().getLivereload()));
  }

  /**
   * Creates the listener that notifies the connected browsers before a context close.
   *
   * @param lifecycle the live reload lifecycle owned by the context
   * @return the context close to browser notice listener
   */
  @Bean
  LiveReloadRestartListener liveReloadRestartListener(LiveReloadLifecycle lifecycle) {
    return new LiveReloadRestartListener(lifecycle);
  }

  /**
   * Creates the listener that turns a static resource change into a resource update for the
   * connected browsers.
   *
   * @param lifecycle the live reload lifecycle owned by the context
   * @return the static resource change to resource update listener
   */
  @Bean
  @ConditionalOnProperty(prefix = "webforj.devtools.livereload", name = "static-resources-enabled",
      havingValue = "true", matchIfMissing = true)
  LiveReloadResourceChangeListener liveReloadResourceChangeListener(LiveReloadLifecycle lifecycle) {
    return new LiveReloadResourceChangeListener(lifecycle);
  }

  private static LiveReloadOptions readOptions(
      SpringConfigurationProperties.LiveReload livereload) {
    LiveReloadOptions options = new LiveReloadOptions().setEnabled(true);
    if (livereload.getWebsocketPort() != null) {
      options.setWebsocketPort(livereload.getWebsocketPort());
    }

    if (livereload.getStaticResourcesEnabled() != null) {
      options.setStaticResourcesEnabled(livereload.getStaticResourcesEnabled());
    }

    return options;
  }
}
