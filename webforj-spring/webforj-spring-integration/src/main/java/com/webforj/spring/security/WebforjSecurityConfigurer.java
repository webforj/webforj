package com.webforj.spring.security;

import com.webforj.component.Component;
import com.webforj.router.RoutePathResolver;
import com.webforj.router.RoutePattern;
import java.util.Collections;
import java.util.Objects;
import java.util.function.Consumer;
import org.springframework.context.ApplicationContext;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.annotation.web.configurers.AuthorizeHttpRequestsConfigurer;
import org.springframework.security.oauth2.client.oidc.web.logout.OidcClientInitiatedLogoutSuccessHandler;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;
import org.springframework.security.web.csrf.CsrfFilter;

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
  private String oauth2LoginPage;
  private String logoutUrl;
  private String logoutSuccessUrl;
  private String accessDeniedPage;
  private boolean formLoginConfigured;
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
   * @throws NullPointerException if the component class is null
   * @throws IllegalArgumentException if unable to resolve the route path for the component
   */
  public WebforjSecurityConfigurer loginPage(Class<? extends Component> loginPageComponent) {
    Objects.requireNonNull(loginPageComponent, "Login page component must not be null");

    String urlPath = resolveRouteUrl(loginPageComponent);
    return loginPage(urlPath, urlPath);
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
    formLoginConfigured = true;
    this.loginPage = loginPage;
    this.loginProcessingUrl = loginProcessingUrl;
    return this;
  }

  /**
   * Configures login through external OAuth2 or OpenID Connect providers.
   *
   * <p>
   * The given component is the login page that offers the configured providers.
   * </p>
   *
   * <p>
   * Requires {@code spring-security-oauth2-client} on the classpath, in a Spring Boot application
   * through {@code spring-boot-starter-oauth2-client}, and a {@link ClientRegistrationRepository}
   * bean, for example one built from the {@code spring.security.oauth2.client.registration.*}
   * properties.
   * </p>
   *
   * @param loginPageComponent the login page component class
   * @return this configurer for method chaining
   * @throws NullPointerException if the component class is null
   * @throws IllegalArgumentException if unable to resolve the route path for the component
   */
  public WebforjSecurityConfigurer oauth2Login(Class<? extends Component> loginPageComponent) {
    Objects.requireNonNull(loginPageComponent, "Login page component must not be null");

    oauth2LoginPage = resolveRouteUrl(loginPageComponent);
    return this;
  }

  /**
   * Configures logout with default settings. Uses "/logout" as the logout URL and redirects to the
   * login page with "?logout" parameter for form login, or starts provider logout for OAuth2 login.
   *
   * <p>
   * With OAuth2 login, the success URL is sent to the provider as the post logout redirect URI and
   * must be registered with the provider.
   * </p>
   *
   * @return this configurer for method chaining
   */
  public WebforjSecurityConfigurer logout() {
    return logout("/logout", null);
  }

  /**
   * Configures logout with a custom logout URL. Redirects to the login page with "?logout"
   * parameter for form login, or starts provider logout for OAuth2 login.
   *
   * <p>
   * With OAuth2 login, the success URL is sent to the provider as the post logout redirect URI and
   * must be registered with the provider.
   * </p>
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
   * <p>
   * With OAuth2 login, the success URL is sent to the provider as the post logout redirect URI and
   * must be registered with the provider.
   * </p>
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
   * @throws NullPointerException if the component class is null
   * @throws IllegalArgumentException if unable to resolve the route path for the component
   */
  public WebforjSecurityConfigurer accessDeniedPage(
      Class<? extends Component> accessDeniedPageComponent) {
    Objects.requireNonNull(accessDeniedPageComponent,
        "Access denied page component must not be null");

    return accessDeniedPage(resolveRouteUrl(accessDeniedPageComponent));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void init(HttpSecurity http) {
    ApplicationContext context = http.getSharedObject(ApplicationContext.class);

    // Get the properties bean
    SpringSecurityConfigurationProperties properties =
        context.getBean(SpringSecurityConfigurationProperties.class);

    if (formLoginConfigured && loginPage == null && properties.getAuthenticationPath() != null) {
      loginPage = properties.getAuthenticationPath();
      if (loginProcessingUrl == null) {
        loginProcessingUrl = properties.getAuthenticationPath();
      }
    }

    if (formLoginConfigured && loginPage != null && properties.getAuthenticationPath() == null) {
      properties.setAuthenticationPath(loginPage);
    } else if (loginPage == null && oauth2LoginPage != null) {
      properties.setAuthenticationPath(oauth2LoginPage);
    }

    WebforjAuthenticationSuccessHandler successHandler =
        context.getBeanProvider(WebforjAuthenticationSuccessHandler.class).getIfUnique();

    if (!formLoginConfigured && oauth2LoginPage == null
        && properties.getAuthenticationPath() != null) {
      loginPage = properties.getAuthenticationPath();
      loginProcessingUrl = properties.getAuthenticationPath();
    }

    // Configure form login if specified
    if (loginPage != null) {
      configureFormLogin(http, successHandler);
    }

    if (oauth2LoginPage != null) {
      configureOauth2Login(http, successHandler);
    }

    if (getEntryLoginPage() != null && logoutUrl == null) {
      logoutUrl = "/logout";
    }

    // Configure logout if specified
    if (logoutUrl != null) {
      // Set default logout success URL if not specified
      if (logoutSuccessUrl == null && getEntryLoginPage() != null) {
        logoutSuccessUrl = getEntryLoginPage() + "?logout";
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
  public void configure(HttpSecurity http) {
    // All configuration is done in init() method
  }

  private void configureCsrf(HttpSecurity http, ApplicationContext context) {
    WebforjFrameworkRequestMatcher frameworkMatcher =
        context.getBean(WebforjFrameworkRequestMatcher.class);

    http.csrf(csrf -> csrf.ignoringRequestMatchers(frameworkMatcher));
    http.addFilterAfter(new WebforjCsrfTokenFilter(), CsrfFilter.class);
  }

  private void configureAuthorization(HttpSecurity http, ApplicationContext context) {
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

      if (oauth2LoginPage != null) {
        registry.requestMatchers(oauth2LoginPage).permitAll();
      }

      // Permit access denied page if configured
      if (accessDeniedPage != null) {
        registry.requestMatchers(accessDeniedPage).permitAll();
      }

      // Configure the final anyRequest rule
      anyRequestRule.accept(registry.anyRequest());
    });
  }

  private void configureFormLogin(HttpSecurity http,
      WebforjAuthenticationSuccessHandler successHandler) {
    http.formLogin(form -> {
      form.loginPage(loginPage);

      if (loginProcessingUrl != null) {
        form.loginProcessingUrl(loginProcessingUrl);
      } else {
        form.loginProcessingUrl(loginPage);
      }

      if (successHandler != null) {
        form.successHandler(successHandler);
      }
    });
  }

  private void configureOauth2Login(HttpSecurity http,
      WebforjAuthenticationSuccessHandler successHandler) {
    http.oauth2Login(oauth2 -> {
      oauth2.loginPage(oauth2LoginPage);
      if (successHandler != null) {
        oauth2.successHandler(successHandler);
      }
    });
  }

  private void configureLogout(HttpSecurity http, ApplicationContext context) {
    http.logout(logout -> {
      logout.logoutUrl(logoutUrl);

      if (oauth2LoginPage != null) {
        logout.logoutSuccessHandler(createOidcLogoutSuccessHandler(context));
      } else if (logoutSuccessUrl != null) {
        logout.logoutSuccessUrl(logoutSuccessUrl);
      }

      logout.permitAll();
    });
  }

  private LogoutSuccessHandler createOidcLogoutSuccessHandler(ApplicationContext context) {
    OidcClientInitiatedLogoutSuccessHandler handler = new OidcClientInitiatedLogoutSuccessHandler(
        context.getBean(ClientRegistrationRepository.class));
    handler.setPostLogoutRedirectUri(
        logoutSuccessUrl.startsWith("/") ? "{baseUrl}" + logoutSuccessUrl : logoutSuccessUrl);
    handler.setDefaultTargetUrl(logoutSuccessUrl);
    return handler;
  }

  private String getEntryLoginPage() {
    return loginPage != null ? loginPage : oauth2LoginPage;
  }

  private String resolveRouteUrl(Class<? extends Component> component) {
    Objects.requireNonNull(component, "component cannot be null");
    String path = RoutePathResolver.resolvePath(component);
    if (path == null) {
      throw new IllegalArgumentException(
          "Failed to resolve route path for component: " + component.getName());
    }

    return patternToUrl(path);
  }

  private String patternToUrl(String pattern) {
    RoutePattern routePattern = new RoutePattern(pattern);
    return routePattern.generateUrl(Collections.emptyMap());
  }
}
