package com.webforj.spring.security;

import org.springframework.context.ApplicationContext;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

/**
 * Configures Spring Security for webforJ applications.
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public class WebforjSecurityConfigurer
    extends AbstractHttpConfigurer<WebforjSecurityConfigurer, HttpSecurity> {

  private String loginPage;
  private String loginProcessingUrl;

  /**
   * Creates a new instance of WebforjSecurityConfigurer.
   *
   * @return a new WebforjSecurityConfigurer instance
   */
  public static WebforjSecurityConfigurer webforj() {
    return new WebforjSecurityConfigurer();
  }

  /**
   * Configures form-based authentication using the default authentication path from configuration.
   *
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer formLogin() {
    return formLogin(null);
  }

  /**
   * Configures form-based authentication.
   *
   * @param loginPage the login page URL (e.g., "/login")
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer formLogin(String loginPage) {
    return formLogin(loginPage, loginPage);
  }

  /**
   * Configures form-based authentication with separate URLs for the login page and processing.
   *
   * @param loginPage the login page URL (e.g., "/login")
   * @param loginProcessingUrl the URL that processes login submissions
   *
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer formLogin(String loginPage, String loginProcessingUrl) {
    this.loginPage = loginPage;
    this.loginProcessingUrl = loginProcessingUrl;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init(HttpSecurity http) throws Exception {
    ApplicationContext context = http.getSharedObject(ApplicationContext.class);

    // If both login endpoints are null, try to read from configuration
    if (loginPage == null && loginProcessingUrl == null) {
      SpringSecurityConfigurationProperties properties =
          context.getBean(SpringSecurityConfigurationProperties.class);

      if (properties.getAuthenticationPath() != null) {
        loginPage = properties.getAuthenticationPath();
        loginProcessingUrl = properties.getAuthenticationPath();
      }
    }

    // Configure form login if specified
    if (loginPage != null) {
      configureFormLogin(http);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void configure(HttpSecurity http) throws Exception {
    ApplicationContext context = http.getSharedObject(ApplicationContext.class);

    // Configure CSRF with webforJ exemptions
    configureCsrf(http, context);
  }

  void configureCsrf(HttpSecurity http, ApplicationContext context) throws Exception {
    WebforjFrameworkRequestMatcher frameworkMatcher =
        context.getBean(WebforjFrameworkRequestMatcher.class);

    http.csrf(csrf -> {
      // Exempt webforJ framework internal requests
      csrf.ignoringRequestMatchers(frameworkMatcher);

      // Exempt login page if configured
      if (loginPage != null) {
        csrf.ignoringRequestMatchers(new AntPathRequestMatcher(loginPage));
      }
    });
  }

  void configureFormLogin(HttpSecurity http) throws Exception {
    http.formLogin(form -> {
      form.loginPage(loginPage);

      if (loginProcessingUrl != null) {
        form.loginProcessingUrl(loginProcessingUrl);
      } else {
        form.loginProcessingUrl(loginPage);
      }

      form.permitAll();
    });
  }
}
