package com.webforj.spring.security;

import com.webforj.component.Component;
import com.webforj.router.RoutePathResolver;
import java.util.function.Consumer;
import org.springframework.context.ApplicationContext;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.annotation.web.configurers.AuthorizeHttpRequestsConfigurer;

/**
 * Configures Spring Security for webforJ applications.
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class WebforjSecurityConfigurer
    extends AbstractHttpConfigurer<WebforjSecurityConfigurer, HttpSecurity> {

  private String loginPage;
  private String loginProcessingUrl;
  private String logoutUrl;
  private String logoutSuccessUrl;
  private String accessDeniedPage;
  private Consumer<AuthorizeHttpRequestsConfigurer<HttpSecurity>.AuthorizedUrl> anyRequestRule =
      AuthorizeHttpRequestsConfigurer.AuthorizedUrl::permitAll;

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
  public WebforjSecurityConfigurer loginPage() {
    return loginPage((String) null);
  }

  /**
   * Configures form-based authentication.
   *
   * @param loginPage the login page URL (e.g., "/login")
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer loginPage(String loginPage) {
    return loginPage(loginPage, loginPage);
  }

  /**
   * Configures form-based authentication using a Component class.
   *
   * @param loginPageComponent the login page component class
   * @return this configurer for method chaining
   * @throws IllegalArgumentException if the component doesn't have a @Route annotation
   */
  public WebforjSecurityConfigurer loginPage(Class<? extends Component> loginPageComponent) {
    String path = RoutePathResolver.resolvePath(loginPageComponent);
    if (path == null) {
      throw new IllegalArgumentException(
          "Component " + loginPageComponent.getName() + " does not have a @Route annotation");
    }
    return loginPage(path, path);
  }

  /**
   * Configures form-based authentication with separate URLs for the login page and processing.
   *
   * @param loginPage the login page URL (e.g., "/login")
   * @param loginProcessingUrl the URL that processes login submissions
   *
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer loginPage(String loginPage, String loginProcessingUrl) {
    this.loginPage = loginPage;
    this.loginProcessingUrl = loginProcessingUrl;
    return this;
  }

  /**
   * Configures logout with default settings. Uses "/logout" as the logout URL and redirects to the
   * login page with "?logout" parameter.
   *
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer logout() {
    return logout("/logout", null);
  }

  /**
   * Configures logout with a custom logout URL. Redirects to the login page with "?logout"
   * parameter after successful logout.
   *
   * @param logoutUrl the URL that processes logout requests
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer logout(String logoutUrl) {
    return logout(logoutUrl, null);
  }

  /**
   * Configures logout with custom URLs.
   *
   * @param logoutUrl the URL that processes logout requests
   * @param logoutSuccessUrl the URL to redirect to after successful logout
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer logout(String logoutUrl, String logoutSuccessUrl) {
    this.logoutUrl = logoutUrl;
    this.logoutSuccessUrl = logoutSuccessUrl;
    return this;
  }

  /**
   * Configures the access denied page.
   *
   * @param accessDeniedPage the access denied page URL (e.g., "/access-denied")
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer accessDeniedPage(String accessDeniedPage) {
    this.accessDeniedPage = accessDeniedPage;
    return this;
  }

  /**
   * Configures the access denied page using a Component class.
   *
   * @param accessDeniedPageComponent the access denied page component class
   * @return this configurer for method chaining
   * @throws IllegalArgumentException if the component doesn't have a @Route annotation
   */
  public WebforjSecurityConfigurer accessDeniedPage(
      Class<? extends Component> accessDeniedPageComponent) {
    String path = RoutePathResolver.resolvePath(accessDeniedPageComponent);
    if (path == null) {
      throw new IllegalArgumentException("Component " + accessDeniedPageComponent.getName()
          + " does not have a @Route annotation");
    }
    return accessDeniedPage(path);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init(HttpSecurity http) throws Exception {
    ApplicationContext context = http.getSharedObject(ApplicationContext.class);

    // Get the properties bean
    SpringSecurityConfigurationProperties properties =
        context.getBean(SpringSecurityConfigurationProperties.class);

    // If both login endpoints are null, try to read from configuration
    if (loginPage == null && loginProcessingUrl == null) {
      if (properties.getAuthenticationPath() != null) {
        loginPage = properties.getAuthenticationPath();
        loginProcessingUrl = properties.getAuthenticationPath();
      }
    } else if (loginPage != null) {
      // If loginPage was set programmatically via formLogin(), update the properties
      // so that SpringRouteSecurityConfiguration can use it
      if (properties.getAuthenticationPath() == null) {
        properties.setAuthenticationPath(loginPage);
      }
    }

    // Configure form login if specified
    if (loginPage != null) {
      configureFormLogin(http, context);

      // Auto-configure logout when form login is enabled and logout wasn't explicitly configured
      if (logoutUrl == null) {
        logoutUrl = "/logout";
      }
    }

    // Configure logout if specified
    if (logoutUrl != null) {
      // Set default logout success URL if not specified
      if (logoutSuccessUrl == null && loginPage != null) {
        logoutSuccessUrl = loginPage + "?logout";
      } else if (logoutSuccessUrl == null) {
        logoutSuccessUrl = "/login?logout";
      }

      configureLogout(http, context);
    }

    // Update properties for webforJ router access denied handling
    if (accessDeniedPage != null && properties.getDenyPath() == null) {
      properties.setDenyPath(accessDeniedPage);
    }

    // Configure CSRF with webforJ exemptions
    configureCsrf(http, context);

    // Configure authorization rules
    configureAuthorization(http, context);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void configure(HttpSecurity http) throws Exception {
    // All configuration is done in init() method
  }

  void configureCsrf(HttpSecurity http, ApplicationContext context) throws Exception {
    WebforjFrameworkRequestMatcher frameworkMatcher =
        context.getBean(WebforjFrameworkRequestMatcher.class);

    http.csrf(csrf -> {
      // Exempt webforJ framework internal requests
      csrf.ignoringRequestMatchers(frameworkMatcher);

      // Disable CSRF for the login endpoint
      if (loginProcessingUrl != null) {
        csrf.ignoringRequestMatchers(loginProcessingUrl);
      }

      // Disable CSRF for the logout endpoint
      if (logoutUrl != null) {
        csrf.ignoringRequestMatchers(logoutUrl);
      }
    });
  }

  void configureAuthorization(HttpSecurity http, ApplicationContext context) throws Exception {
    WebforjFrameworkRequestMatcher frameworkMatcher =
        context.getBean(WebforjFrameworkRequestMatcher.class);

    http.authorizeHttpRequests(registry -> {
      // Permit all webforJ framework internal requests
      registry.requestMatchers(frameworkMatcher).permitAll();

      // Permit login page if configured
      if (loginPage != null) {
        registry.requestMatchers(loginPage).permitAll();
        if (loginProcessingUrl != null && !loginProcessingUrl.equals(loginPage)) {
          registry.requestMatchers(loginProcessingUrl).permitAll();
        }
      }

      // Permit access denied page if configured
      if (accessDeniedPage != null) {
        registry.requestMatchers(accessDeniedPage).permitAll();
      }

      // Configure the final anyRequest rule
      anyRequestRule.accept(registry.anyRequest());
    });
  }

  void configureFormLogin(HttpSecurity http, ApplicationContext context) throws Exception {
    http.formLogin(form -> {
      form.loginPage(loginPage);

      if (loginProcessingUrl != null) {
        form.loginProcessingUrl(loginProcessingUrl);
      } else {
        form.loginProcessingUrl(loginPage);
      }

      // Use webforJ's authentication success handler to respect stored locations
      try {
        WebforjAuthenticationSuccessHandler successHandler =
            context.getBean(WebforjAuthenticationSuccessHandler.class);
        form.successHandler(successHandler);
      } catch (Exception e) {
        // pass
      }
    });
  }

  void configureLogout(HttpSecurity http, ApplicationContext context) throws Exception {
    http.logout(logout -> {
      logout.logoutUrl(logoutUrl);

      if (logoutSuccessUrl != null) {
        logout.logoutSuccessUrl(logoutSuccessUrl);
      }

      logout.permitAll();
    });
  }
}
