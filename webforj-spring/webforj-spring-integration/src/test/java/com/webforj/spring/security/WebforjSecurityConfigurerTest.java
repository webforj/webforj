package com.webforj.spring.security;

import static org.hamcrest.Matchers.startsWith;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestBuilders.formLogin;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.user;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.webforj.component.Composite;
import com.webforj.component.element.Element;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.spring.SpringConfigurationProperties;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.client.endpoint.OAuth2AccessTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AuthorizationCodeGrantRequest;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.client.registration.InMemoryClientRegistrationRepository;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.endpoint.OAuth2AccessTokenResponse;
import org.springframework.security.oauth2.core.oidc.OidcIdToken;
import org.springframework.security.oauth2.core.oidc.user.DefaultOidcUser;
import org.springframework.security.oauth2.core.user.DefaultOAuth2User;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.csrf.CsrfToken;
import org.springframework.test.context.NestedTestConfiguration;
import org.springframework.test.context.NestedTestConfiguration.EnclosingConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.web.SpringJUnitWebConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.web.util.UriUtils;

@SpringJUnitWebConfig
class WebforjSecurityConfigurerTest {

  @Nested
  @SpringJUnitWebConfig(TestSecurityConfig.class)
  class HttpSecurityConfiguration {

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
    }

    @Test
    void shouldConfigureFormLoginCorrectly() throws Exception {
      mockMvc.perform(formLogin("/authenticate").user("user").password("password"))
          .andExpect(status().is3xxRedirection());
    }

    @Test
    void shouldRejectLoginProcessingUrlWithoutCsrfToken() throws Exception {
      mockMvc
          .perform(post("/authenticate").contentType("application/x-www-form-urlencoded")
              .param("username", "user").param("password", "password"))
          .andExpect(status().isForbidden());
    }

    @Test
    void shouldRejectLogoutUrlWithoutCsrfToken() throws Exception {
      mockMvc.perform(post("/signout")).andExpect(status().isForbidden());
    }

    @Test
    void shouldStoreCsrfTokenInSessionOnPageRequest() throws Exception {
      MockHttpSession session = new MockHttpSession();

      CsrfToken stored = loadStoredToken(session);

      assertEquals("_csrf", stored.getParameterName());
      assertNotNull(stored.getToken());
    }

    @Test
    void shouldAcceptLoginProcessingUrlWithStoredCsrfToken() throws Exception {
      MockHttpSession session = new MockHttpSession();
      CsrfToken stored = loadStoredToken(session);

      mockMvc
          .perform(post("/authenticate").session(session)
              .contentType("application/x-www-form-urlencoded").param("username", "user")
              .param("password", "password").param(stored.getParameterName(), stored.getToken()))
          .andExpect(status().is3xxRedirection());
    }

    @Test
    void shouldAcceptLogoutUrlWithStoredCsrfToken() throws Exception {
      MockHttpSession session = new MockHttpSession();
      CsrfToken stored = loadStoredToken(session);

      mockMvc
          .perform(
              post("/signout").session(session).param(stored.getParameterName(), stored.getToken()))
          .andExpect(status().is3xxRedirection());
    }

    @Test
    void shouldBypassCsrfForFrameworkRequests() throws Exception {
      mockMvc.perform(post("/webapprmi")).andExpect(status().isNotFound());
    }

    @Test
    void shouldApplyCsrfProtectionToOtherEndpoints() throws Exception {
      mockMvc.perform(post("/other-endpoint")).andExpect(status().isForbidden());
    }

    @Test
    void shouldPermitAccessToConfiguredLoginPage() throws Exception {
      mockMvc.perform(get("/signin")).andExpect(status().isNotFound());
    }

    @Nested
    @TestPropertySource(properties = "webforj.security.authentication-path=/login")
    class ConfiguredAuthenticationPath {

      @Autowired
      private SpringSecurityConfigurationProperties properties;

      @Test
      void shouldPreserveConfiguredAuthenticationPathForFormLogin() {
        assertEquals("/login", properties.getAuthenticationPath());
      }
    }

    private CsrfToken loadStoredToken(MockHttpSession session) throws Exception {
      mockMvc.perform(get("/signin").session(session));
      return (CsrfToken) session.getAttribute(WebforjCsrfTokenFilter.SESSION_ATTRIBUTE_NAME);
    }
  }

  @Nested
  @NestedTestConfiguration(EnclosingConfiguration.OVERRIDE)
  @SpringJUnitWebConfig(Oauth2Login.Oauth2LoginSecurityConfig.class)
  class Oauth2Login {

    @Autowired
    private WebApplicationContext context;

    @Autowired
    private SpringSecurityConfigurationProperties properties;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
    }

    @Test
    void shouldRedirectToProviderAuthorizationUri() throws Exception {
      mockMvc.perform(get("/oauth2/authorization/acme")).andExpect(status().is3xxRedirection())
          .andExpect(
              header().string("Location", startsWith("https://provider.example/authorize?")));
    }

    @Test
    void shouldRedirectToStoredLocationAfterProviderCallback() throws Exception {
      MockHttpSession session = new MockHttpSession();
      session.setAttribute(AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY, "/dashboard");
      String location = mockMvc.perform(get("/oauth2/authorization/acme").session(session))
          .andReturn().getResponse().getRedirectedUrl();
      String state =
          UriComponentsBuilder.fromUriString(location).build().getQueryParams().getFirst("state");

      mockMvc
          .perform(get("/login/oauth2/code/acme").session(session).param("code", "x").param("state",
              UriUtils.decode(state, StandardCharsets.UTF_8)))
          .andExpect(redirectedUrl("/dashboard"));
    }

    @Test
    void shouldRedirectToOauth2LoginPageAfterLogout() throws Exception {
      mockMvc.perform(post("/logout").with(csrf())).andExpect(redirectedUrl("/sso?logout"));
    }

    @Test
    void shouldRedirectToProviderEndSessionEndpointAfterOidcLogout() throws Exception {
      OidcIdToken idToken = new OidcIdToken("id-token", Instant.now(),
          Instant.now().plusSeconds(60), Map.of("sub", "jdoe"));
      DefaultOidcUser user =
          new DefaultOidcUser(AuthorityUtils.createAuthorityList("ROLE_USER"), idToken);
      OAuth2AuthenticationToken token =
          new OAuth2AuthenticationToken(user, user.getAuthorities(), "acme");

      mockMvc.perform(post("/logout").with(csrf()).with(authentication(token)))
          .andExpect(redirectedUrl("https://provider.example/logout?id_token_hint=id-token"
              + "&post_logout_redirect_uri=http://localhost/sso?logout"));
    }

    @Test
    void shouldSetAuthenticationPathToOauth2LoginPage() {
      assertEquals("/sso", properties.getAuthenticationPath());
    }

    @Nested
    @NestedTestConfiguration(EnclosingConfiguration.INHERIT)
    @TestPropertySource(properties = "webforj.security.authentication-path=/login")
    class ConfiguredAuthenticationPath {

      @Autowired
      private SpringSecurityConfigurationProperties configuredProperties;

      @Test
      void shouldPreserveConfiguredAuthenticationPathOverOauth2Login() {
        assertEquals("/login", configuredProperties.getAuthenticationPath());
      }
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import({SpringSecurityAutoConfiguration.class, ClientRegistrationConfig.class})
    static class Oauth2LoginSecurityConfig {

      @Bean
      SecurityFilterChain buildFilterChain(HttpSecurity http) {
        return http.with(WebforjSecurityConfigurer.webforj(),
            configurer -> configurer.oauth2Login(SsoLoginView.class)).build();
      }

      @Bean
      OAuth2AccessTokenResponseClient<OAuth2AuthorizationCodeGrantRequest> createTokenClient() {
        return request -> OAuth2AccessTokenResponse.withToken("token")
            .tokenType(OAuth2AccessToken.TokenType.BEARER).expiresIn(3600).build();
      }

      @Bean
      OAuth2UserService<OAuth2UserRequest, OAuth2User> createUserService() {
        return request -> new DefaultOAuth2User(AuthorityUtils.createAuthorityList("ROLE_USER"),
            Map.of("sub", "jdoe"), "sub");
      }

      @Bean
      SpringConfigurationProperties createSpringConfigurationProperties() {
        SpringConfigurationProperties properties = new SpringConfigurationProperties();
        properties.setServletMapping("/*");
        return properties;
      }
    }
  }

  @Nested
  @NestedTestConfiguration(EnclosingConfiguration.OVERRIDE)
  @SpringJUnitWebConfig(Oauth2LoginWithSharedRepository.SecurityConfig.class)
  class Oauth2LoginWithSharedRepository {

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
    }

    @Test
    void shouldRedirectToProviderEndSessionEndpointWithoutRepositoryBean() throws Exception {
      OidcIdToken idToken = new OidcIdToken("id-token", Instant.now(),
          Instant.now().plusSeconds(60), Map.of("sub", "jdoe"));
      DefaultOidcUser user =
          new DefaultOidcUser(AuthorityUtils.createAuthorityList("ROLE_USER"), idToken);
      OAuth2AuthenticationToken token =
          new OAuth2AuthenticationToken(user, user.getAuthorities(), "acme");

      mockMvc.perform(post("/logout").with(csrf()).with(authentication(token)))
          .andExpect(redirectedUrl("https://provider.example/logout?id_token_hint=id-token"
              + "&post_logout_redirect_uri=http://localhost/sso?logout"));
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import(SpringSecurityAutoConfiguration.class)
    static class SecurityConfig {

      @Bean
      SecurityFilterChain buildFilterChain(HttpSecurity http) {
        ClientRegistrationRepository repository =
            new ClientRegistrationConfig().createClientRegistrationRepository();

        return http
            .with(WebforjSecurityConfigurer.webforj(),
                configurer -> configurer.oauth2Login(SsoLoginView.class))
            .oauth2Login(oauth2 -> oauth2.clientRegistrationRepository(repository)).build();
      }

      @Bean
      SpringConfigurationProperties createSpringConfigurationProperties() {
        SpringConfigurationProperties properties = new SpringConfigurationProperties();
        properties.setServletMapping("/*");
        return properties;
      }
    }
  }

  @Nested
  @NestedTestConfiguration(EnclosingConfiguration.OVERRIDE)
  @SpringJUnitWebConfig(FormAndOauth2Login.FormAndOauth2LoginSecurityConfig.class)
  class FormAndOauth2Login {

    @Autowired
    private WebApplicationContext context;

    @Autowired
    private SpringSecurityConfigurationProperties properties;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
    }

    @Test
    void shouldPreferFormLoginPageAsAuthenticationPath() {
      assertEquals("/signin", properties.getAuthenticationPath());
    }

    @Test
    void shouldPermitCustomOauth2LogoutUrlAndUseAbsoluteSuccessUrl() throws Exception {
      OidcIdToken idToken = new OidcIdToken("id-token", Instant.now(),
          Instant.now().plusSeconds(60), Map.of("sub", "jdoe"));
      DefaultOidcUser user =
          new DefaultOidcUser(AuthorityUtils.createAuthorityList("ROLE_USER"), idToken);
      OAuth2AuthenticationToken token =
          new OAuth2AuthenticationToken(user, user.getAuthorities(), "acme");

      mockMvc.perform(post("/signout").with(csrf()).with(authentication(token)))
          .andExpect(redirectedUrl("https://provider.example/logout?id_token_hint=id-token"
              + "&post_logout_redirect_uri=http://localhost/home"));
    }

    @Nested
    @NestedTestConfiguration(EnclosingConfiguration.INHERIT)
    @TestPropertySource(properties = "webforj.security.authentication-path=/login")
    class ConfiguredAuthenticationPath {

      @Autowired
      private SpringSecurityConfigurationProperties configuredProperties;

      @Test
      void shouldPreserveConfiguredAuthenticationPathOverFormAndOauth2Login() {
        assertEquals("/login", configuredProperties.getAuthenticationPath());
      }
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import({SpringSecurityAutoConfiguration.class, ClientRegistrationConfig.class})
    static class FormAndOauth2LoginSecurityConfig {

      @Bean
      SecurityFilterChain buildFilterChain(HttpSecurity http) {
        return http.with(WebforjSecurityConfigurer.webforj(), configurer -> {
          configurer.loginPage("/signin", "/authenticate");
          configurer.oauth2Login(SsoLoginView.class);
          configurer.logout("/signout", "/home");
        }).build();
      }

      @Bean
      OAuth2AccessTokenResponseClient<OAuth2AuthorizationCodeGrantRequest> createTokenClient() {
        return request -> OAuth2AccessTokenResponse.withToken("token")
            .tokenType(OAuth2AccessToken.TokenType.BEARER).expiresIn(3600).build();
      }

      @Bean
      OAuth2UserService<OAuth2UserRequest, OAuth2User> createUserService() {
        return request -> new DefaultOAuth2User(AuthorityUtils.createAuthorityList("ROLE_USER"),
            Map.of("sub", "jdoe"), "sub");
      }

      @Bean
      SpringConfigurationProperties createSpringConfigurationProperties() {
        SpringConfigurationProperties properties = new SpringConfigurationProperties();
        properties.setServletMapping("/*");
        return properties;
      }
    }
  }

  @Nested
  @NestedTestConfiguration(EnclosingConfiguration.OVERRIDE)
  @SpringJUnitWebConfig(FormAndOauth2WithoutConfiguredPath.SecurityConfig.class)
  class FormAndOauth2WithoutConfiguredPath {

    @Autowired
    private SpringSecurityConfigurationProperties properties;

    @Test
    void shouldUseOauth2LoginPageAsAuthenticationPath() {
      assertEquals("/sso", properties.getAuthenticationPath());
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import({SpringSecurityAutoConfiguration.class, ClientRegistrationConfig.class})
    static class SecurityConfig {

      @Bean
      SecurityFilterChain buildFilterChain(HttpSecurity http) {
        return http.with(WebforjSecurityConfigurer.webforj(), configurer -> {
          configurer.loginPage();
          configurer.oauth2Login(SsoLoginView.class);
        }).build();
      }

      @Bean
      SpringConfigurationProperties createSpringConfigurationProperties() {
        SpringConfigurationProperties properties = new SpringConfigurationProperties();
        properties.setServletMapping("/*");
        return properties;
      }
    }
  }

  @Nested
  @NestedTestConfiguration(EnclosingConfiguration.OVERRIDE)
  @TestPropertySource(properties = "webforj.security.authentication-path=/login")
  @SpringJUnitWebConfig(FormLoginWithoutPageAndOauth2.SecurityConfig.class)
  class FormLoginWithoutPageAndOauth2 {

    @Autowired
    private WebApplicationContext context;

    @Autowired
    private SpringSecurityConfigurationProperties properties;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
    }

    @Test
    void shouldUseConfiguredAuthenticationPathForFormLoginBesideOauth2Login() throws Exception {
      assertEquals("/login", properties.getAuthenticationPath());
      mockMvc
          .perform(
              post("/login").with(csrf()).param("username", "user").param("password", "password"))
          .andExpect(status().is3xxRedirection());
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import({SpringSecurityAutoConfiguration.class, ClientRegistrationConfig.class})
    static class SecurityConfig {

      @Bean
      SecurityFilterChain buildFilterChain(HttpSecurity http) {
        return http.with(WebforjSecurityConfigurer.webforj(), configurer -> {
          configurer.loginPage();
          configurer.oauth2Login(SsoLoginView.class);
        }).build();
      }

      @Bean
      UserDetailsService userDetailsService() {
        return new InMemoryUserDetailsManager(
            User.withUsername("user").password("{noop}password").roles("USER").build());
      }

      @Bean
      SpringConfigurationProperties createSpringConfigurationProperties() {
        SpringConfigurationProperties properties = new SpringConfigurationProperties();
        properties.setServletMapping("/*");
        return properties;
      }
    }
  }

  @Nested
  @NestedTestConfiguration(EnclosingConfiguration.OVERRIDE)
  @TestPropertySource(properties = "webforj.security.authentication-path=/login")
  @SpringJUnitWebConfig(FormLoginWithExplicitProcessingUrl.SecurityConfig.class)
  class FormLoginWithExplicitProcessingUrl {

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
    }

    @Test
    void shouldKeepExplicitProcessingUrlWhenAuthenticationPathIsConfigured() throws Exception {
      mockMvc
          .perform(
              post("/process").with(csrf()).param("username", "user").param("password", "password"))
          .andExpect(status().is3xxRedirection());
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import(SpringSecurityAutoConfiguration.class)
    static class SecurityConfig {

      @Bean
      SecurityFilterChain buildFilterChain(HttpSecurity http) {
        return http.with(WebforjSecurityConfigurer.webforj(),
            configurer -> configurer.loginPage(null, "/process")).build();
      }

      @Bean
      UserDetailsService userDetailsService() {
        return new InMemoryUserDetailsManager(
            User.withUsername("user").password("{noop}password").roles("USER").build());
      }

      @Bean
      SpringConfigurationProperties createSpringConfigurationProperties() {
        SpringConfigurationProperties properties = new SpringConfigurationProperties();
        properties.setServletMapping("/*");
        return properties;
      }
    }
  }

  @Nested
  @NestedTestConfiguration(EnclosingConfiguration.OVERRIDE)
  @SpringJUnitWebConfig(WithoutOauth2Client.SecurityConfig.class)
  class WithoutOauth2Client {

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
    }

    @Test
    void shouldLoadConfigurerWithoutOauth2ClientClasses() throws Exception {
      String repositoryName = ClientRegistrationRepository.class.getName();
      HidingClassLoader loader = SecurityConfig.LOADER;
      Class<?> configurerType =
          Class.forName(WebforjSecurityConfigurer.class.getName(), true, loader);

      assertSame(loader, configurerType.getClassLoader());
      assertThrows(ClassNotFoundException.class, () -> Class.forName(repositoryName, true, loader));
    }

    @Test
    void shouldRedirectToLoginPageAfterLogout() throws Exception {
      mockMvc.perform(post("/logout").with(csrf()).with(user("user")))
          .andExpect(redirectedUrl("/login?logout"));
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import(SpringSecurityAutoConfiguration.class)
    static class SecurityConfig {
      static final HidingClassLoader LOADER =
          new HidingClassLoader("org.springframework.security.oauth2.client.",
              "com.webforj.spring.security.WebforjSecurityConfigurer",
              "com.webforj.spring.security.WebforjCsrfTokenFilter");

      @Bean
      @SuppressWarnings("unchecked")
      SecurityFilterChain buildFilterChain(HttpSecurity http) throws Exception {
        Class<?> configurerType =
            Class.forName(WebforjSecurityConfigurer.class.getName(), true, LOADER);
        Object configurer = configurerType.getMethod("webforj").invoke(null);
        configurerType.getMethod("loginPage", String.class).invoke(configurer, "/login");
        configurerType.getMethod("logout").invoke(configurer);

        return http
            .with((AbstractHttpConfigurer<?, HttpSecurity>) configurer, Customizer.withDefaults())
            .build();
      }

      @Bean
      SpringConfigurationProperties createSpringConfigurationProperties() {
        SpringConfigurationProperties properties = new SpringConfigurationProperties();
        properties.setServletMapping("/*");
        return properties;
      }
    }
  }

  @Configuration
  @EnableWebSecurity
  @EnableWebMvc
  @Import(SpringSecurityAutoConfiguration.class)
  static class TestSecurityConfig {

    @Bean
    SecurityFilterChain filterChain(HttpSecurity http) {
      return http.with(WebforjSecurityConfigurer.webforj(), configurer -> {
        configurer.loginPage("/signin", "/authenticate");
        configurer.logout("/signout", "/goodbye");
      }).build();
    }

    @Bean
    UserDetailsService userDetailsService() {
      return new InMemoryUserDetailsManager(
          User.withUsername("user").password("{noop}password").roles("USER").build());
    }

    @Bean
    SpringConfigurationProperties springConfigurationProperties() {
      SpringConfigurationProperties properties = new SpringConfigurationProperties();
      properties.setServletMapping("/*");
      return properties;
    }
  }

  @Configuration
  static class ClientRegistrationConfig {

    @Bean
    ClientRegistrationRepository createClientRegistrationRepository() {
      return new InMemoryClientRegistrationRepository(
          ClientRegistration.withRegistrationId("acme").clientId("client").clientSecret("secret")
              .authorizationGrantType(AuthorizationGrantType.AUTHORIZATION_CODE)
              .redirectUri("{baseUrl}/login/oauth2/code/{registrationId}").scope("profile")
              .authorizationUri("https://provider.example/authorize")
              .tokenUri("https://provider.example/token")
              .userInfoUri("https://provider.example/userinfo").userNameAttributeName("sub")
              .providerConfigurationMetadata(
                  Map.of("end_session_endpoint", "https://provider.example/logout"))
              .build());
    }
  }

  @Route("/sso")
  static class SsoLoginView extends Composite<Element> {
  }
}
