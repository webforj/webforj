package com.webforj.spring.security.bearer;

import com.webforj.spring.security.WebforjFrameworkRequestMatcher;
import com.webforj.spring.security.WebforjSecurityConfigurer;
import java.util.concurrent.atomic.AtomicReference;
import org.springframework.context.ApplicationContext;
import org.springframework.security.config.ObjectPostProcessor;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.annotation.web.configurers.ExceptionHandlingConfigurer;
import org.springframework.security.config.annotation.web.configurers.LogoutConfigurer;
import org.springframework.security.config.annotation.web.configurers.oauth2.server.resource.OAuth2ResourceServerConfigurer;
import org.springframework.security.oauth2.server.resource.web.BearerTokenAuthenticationEntryPoint;
import org.springframework.security.oauth2.server.resource.web.authentication.BearerTokenAuthenticationFilter;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.access.ExceptionTranslationFilter;

/**
 * Configures the resource server of a webforJ application to read the bearer token from a cookie
 * and to keep its login in the session.
 *
 * <p>
 * {@link WebforjSecurityConfigurer#bearerToken(String)} applies it.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.10
 */
public final class WebforjBearerTokenConfigurer
    extends AbstractHttpConfigurer<WebforjBearerTokenConfigurer, HttpSecurity> {
  private final String cookieName;

  /**
   * Creates a configurer that reads the bearer token from the given cookie.
   *
   * @param cookieName the name of the cookie that holds the token
   */
  public WebforjBearerTokenConfigurer(String cookieName) {
    this.cookieName = cookieName;
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * A configured logout deletes the token cookie.
   * </p>
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

    LogoutConfigurer<?> logout = http.getConfigurer(LogoutConfigurer.class);
    if (logout != null) {
      logout.deleteCookies(cookieName);
    }

    ApplicationContext context = http.getSharedObject(ApplicationContext.class);
    WebforjFrameworkRequestMatcher frameworkMatcher =
        context.getBean(WebforjFrameworkRequestMatcher.class);
    WebforjBearerTokenResolver resolver =
        new WebforjBearerTokenResolver(cookieName, frameworkMatcher);
    WebforjBearerTokenContextRepository repository =
        new WebforjBearerTokenContextRepository(frameworkMatcher);
    AtomicReference<AuthenticationEntryPoint> entryPoint =
        new AtomicReference<>(new BearerTokenAuthenticationEntryPoint());
    WebforjBearerTokenFailureHandler failureHandler = new WebforjBearerTokenFailureHandler(resolver,
        repository,
        (request, response, exception) -> entryPoint.get().commence(request, response, exception));

    ExceptionHandlingConfigurer<?> exceptionHandling =
        http.getConfigurer(ExceptionHandlingConfigurer.class);
    if (exceptionHandling != null) {
      exceptionHandling
          .withObjectPostProcessor(new ObjectPostProcessor<ExceptionTranslationFilter>() {
            @Override
            public <O extends ExceptionTranslationFilter> O postProcess(O filter) {
              entryPoint.set(filter.getAuthenticationEntryPoint());
              return filter;
            }
          });
    }

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
