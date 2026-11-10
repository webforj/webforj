package com.webforj.spring.security;

import org.springframework.context.ApplicationContext;
import org.springframework.security.config.ObjectPostProcessor;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.annotation.web.configurers.oauth2.server.resource.OAuth2ResourceServerConfigurer;
import org.springframework.security.oauth2.server.resource.web.authentication.BearerTokenAuthenticationFilter;

/**
 * Configures the resource server of a webforJ application to read the bearer token from a cookie
 * and to keep its login in the session.
 *
 * @author Christian Liebhardt
 * @since 26.10
 */
final class WebforjBearerTokenConfigurer
    extends AbstractHttpConfigurer<WebforjBearerTokenConfigurer, HttpSecurity> {
  private final String cookieName;

  WebforjBearerTokenConfigurer(String cookieName) {
    this.cookieName = cookieName;
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if no resource server is configured
   */
  @Override
  public void init(HttpSecurity http) {
    OAuth2ResourceServerConfigurer<?> resourceServer =
        http.getConfigurer(OAuth2ResourceServerConfigurer.class);
    if (resourceServer == null) {
      throw new IllegalStateException("The bearer token cookie needs a resource server. "
          + "Configure it with HttpSecurity.oauth2ResourceServer.");
    }

    ApplicationContext context = http.getSharedObject(ApplicationContext.class);
    WebforjBearerTokenResolver resolver = new WebforjBearerTokenResolver(cookieName,
        context.getBean(WebforjFrameworkRequestMatcher.class));
    WebforjBearerTokenContextRepository repository = new WebforjBearerTokenContextRepository();
    WebforjBearerTokenFailureHandler failureHandler =
        new WebforjBearerTokenFailureHandler(resolver, repository);

    resourceServer.bearerTokenResolver(resolver);
    resourceServer
        .withObjectPostProcessor(new ObjectPostProcessor<BearerTokenAuthenticationFilter>() {
          @Override
          public <O extends BearerTokenAuthenticationFilter> O postProcess(O filter) {
            filter.setSecurityContextRepository(repository);
            filter.setAuthenticationFailureHandler(failureHandler);
            return filter;
          }
        });
    http.addFilterBefore(
        new WebforjBearerTokenFilter(resolver, repository, getSecurityContextHolderStrategy()),
        BearerTokenAuthenticationFilter.class);
  }
}
