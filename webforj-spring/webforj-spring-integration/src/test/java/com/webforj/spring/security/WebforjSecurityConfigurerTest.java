package com.webforj.spring.security;

import static org.hamcrest.Matchers.startsWith;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestBuilders.formLogin;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.user;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.cookie;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.nimbusds.jose.crypto.RSASSASigner;
import com.nimbusds.jose.JOSEException;
import com.nimbusds.jose.jwk.gen.RSAKeyGenerator;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.PlainJWT;
import com.nimbusds.jwt.SignedJWT;
import com.webforj.component.Composite;
import com.webforj.component.element.Element;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.spring.SpringConfigurationProperties;
import jakarta.servlet.http.Cookie;
import java.nio.charset.StandardCharsets;
import java.security.interfaces.RSAPublicKey;
import java.security.Principal;
import java.time.Instant;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.mock.web.MockServletContext;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.Customizer;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.core.GrantedAuthority;
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
import org.springframework.security.oauth2.core.endpoint.OAuth2AccessTokenResponse;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.oidc.OidcIdToken;
import org.springframework.security.oauth2.core.oidc.user.DefaultOidcUser;
import org.springframework.security.oauth2.core.user.DefaultOAuth2User;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationConverter;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;
import org.springframework.security.oauth2.server.resource.authentication.JwtGrantedAuthoritiesConverter;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.csrf.CsrfToken;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.test.context.junit.jupiter.web.SpringJUnitWebConfig;
import org.springframework.test.context.NestedTestConfiguration;
import org.springframework.test.context.NestedTestConfiguration.EnclosingConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;
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
    void shouldRedirectToOauth2LoginPageAfterOidcLogoutWithoutProviderLogout() throws Exception {
      OidcIdToken idToken = new OidcIdToken("id-token", Instant.now(),
          Instant.now().plusSeconds(60), Map.of("sub", "jdoe"));
      DefaultOidcUser user =
          new DefaultOidcUser(AuthorityUtils.createAuthorityList("ROLE_USER"), idToken);
      OAuth2AuthenticationToken token =
          new OAuth2AuthenticationToken(user, user.getAuthorities(), "acme");

      mockMvc.perform(post("/logout").with(csrf()).with(authentication(token)))
          .andExpect(redirectedUrl("/sso?logout"));
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
              + "&post_logout_redirect_uri=http://localhost/sso"));
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
                configurer -> configurer.oauth2Login(SsoLoginView.class).oauth2Logout())
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
  @SpringJUnitWebConfig(Oauth2Logout.SecurityConfig.class)
  class Oauth2Logout {

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
    }

    @Test
    void shouldSendAbsoluteSuccessUrlToProviderAfterOidcLogout() throws Exception {
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

    @Test
    void shouldRedirectToSuccessUrlAfterLogoutOfOtherSessions() throws Exception {
      mockMvc.perform(post("/signout").with(csrf()).with(user("user")))
          .andExpect(redirectedUrl("/home"));
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import({SpringSecurityAutoConfiguration.class, ClientRegistrationConfig.class})
    static class SecurityConfig {

      @Bean
      SecurityFilterChain buildFilterChain(HttpSecurity http) {
        return http.with(WebforjSecurityConfigurer.webforj(), configurer -> {
          configurer.oauth2Login(SsoLoginView.class);
          configurer.oauth2Logout();
          configurer.logout("/signout", "/home");
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
    void shouldPermitCustomLogoutUrlAndUseSuccessUrlWithoutProviderLogout() throws Exception {
      OidcIdToken idToken = new OidcIdToken("id-token", Instant.now(),
          Instant.now().plusSeconds(60), Map.of("sub", "jdoe"));
      DefaultOidcUser user =
          new DefaultOidcUser(AuthorityUtils.createAuthorityList("ROLE_USER"), idToken);
      OAuth2AuthenticationToken token =
          new OAuth2AuthenticationToken(user, user.getAuthorities(), "acme");

      mockMvc.perform(post("/signout").with(csrf()).with(authentication(token)))
          .andExpect(redirectedUrl("/home"));
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


  @Nested
  @NestedTestConfiguration(EnclosingConfiguration.OVERRIDE)
  @SpringJUnitWebConfig(BearerToken.BearerTokenSecurityConfig.class)
  class BearerToken {

    private static final String KEY =
        HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY;

    private static final AuthenticationEntryPoint CUSTOM_ENTRY_POINT =
        (request, response, exception) -> {
          response.setStatus(418);
          response.setHeader("X-Entry-Point", "custom");
        };

    @Autowired
    private WebApplicationContext context;

    @Autowired
    private CountingJwtDecoder decoder;

    @Autowired
    private TokenSource tokens;

    @Autowired
    private ObjectProvider<HttpSecurity> httpSecurityProvider;

    private final MockHttpSession session = new MockHttpSession();
    private MockMvc mockMvc;

    @BeforeEach
    void setUp() {
      mockMvc = MockMvcBuilders.webAppContextSetup(context).apply(springSecurity()).build();
      decoder.resetCount();
    }

    @ParameterizedTest
    @EnumSource(TokenTransport.class)
    void shouldKeepBearerLoginInSession(TokenTransport transport) throws Exception {
      String token = tokens.createValid("ada", "USER");

      mockMvc.perform(transport.createRequest(token).session(session))
          .andExpect(content().string("ada FACTOR_BEARER,ROLE_USER"));

      SecurityContext stored = (SecurityContext) session.getAttribute(KEY);
      assertInstanceOf(JwtAuthenticationToken.class, stored.getAuthentication());
      assertEquals("ada", stored.getAuthentication().getName());
    }

    @Test
    void shouldRemoveStoredBearerLoginWithoutToken() throws Exception {
      String token = tokens.createValid("ada", "USER");
      mockMvc.perform(get("/account").session(session).cookie(new Cookie("jwt", token)));

      mockMvc.perform(get("/account").session(session));

      assertNull(session.getAttribute(KEY));
    }

    @Test
    void shouldAnswerRequestWithoutTokenAnonymously() throws Exception {
      String token = tokens.createValid("ada", "USER");
      mockMvc.perform(get("/account").session(session).cookie(new Cookie("jwt", token)));

      mockMvc.perform(get("/account").session(session)).andExpect(content().string("anonymous"));
    }

    @Test
    void shouldKeepStoredFormLoginWithoutToken() throws Exception {
      SecurityContext formLogin = new SecurityContextImpl(UsernamePasswordAuthenticationToken
          .authenticated("ada", null, List.of(new SimpleGrantedAuthority("ROLE_USER"))));
      session.setAttribute(KEY, formLogin);

      mockMvc.perform(get("/account").session(session));

      assertSame(formLogin, session.getAttribute(KEY));
    }

    @Test
    void shouldReplaceStoredBearerLoginWithLoginOfNewToken() throws Exception {
      String rootToken = tokens.createValid("root", "ADMIN");
      String adaToken = tokens.createValid("ada", "USER");
      mockMvc.perform(get("/account").session(session).cookie(new Cookie("jwt", rootToken)));

      mockMvc.perform(
          get("/account").session(session).header(HttpHeaders.AUTHORIZATION, "Bearer " + adaToken))
          .andExpect(content().string("ada FACTOR_BEARER,ROLE_USER"));
    }

    @ParameterizedTest
    @EnumSource(RejectedToken.class)
    void shouldAnswerRejectedCookieTokenAnonymously(RejectedToken rejected) throws Exception {
      String validToken = tokens.createValid("ada", "USER");
      String rejectedToken = tokens.createRejected(rejected, "ada", "USER");
      mockMvc.perform(get("/account").session(session).cookie(new Cookie("jwt", validToken)));
      decoder.resetCount();

      mockMvc.perform(get("/account").session(session).cookie(new Cookie("jwt", rejectedToken)))
          .andExpect(status().isOk()).andExpect(content().string("anonymous"));

      assertNull(session.getAttribute(KEY));
      assertEquals(1, decoder.getCount());
    }

    @ParameterizedTest
    @EnumSource(RejectedToken.class)
    void shouldAnswerRejectedHeaderTokenWithUnauthorized(RejectedToken rejected) throws Exception {
      String rejectedToken = tokens.createRejected(rejected, "ada", "USER");

      mockMvc
          .perform(get("/account").session(session).header(HttpHeaders.AUTHORIZATION,
              "Bearer " + rejectedToken))
          .andExpect(status().isUnauthorized())
          .andExpect(header().string(HttpHeaders.WWW_AUTHENTICATE, startsWith("Bearer")));

      assertEquals(1, decoder.getCount());
    }

    @Test
    void shouldAnswerRejectedHeaderTokenWithEntryPointOfResourceServer() throws Exception {
      String rejectedToken = tokens.createRejected(RejectedToken.EXPIRED, "ada", "USER");
      HttpSecurity http = httpSecurityProvider.getObject();
      http.with(WebforjSecurityConfigurer.webforj(), configurer -> configurer.bearerToken("jwt"));
      http.oauth2ResourceServer(server -> server.jwt(jwt -> jwt.decoder(decoder))
          .authenticationEntryPoint(CUSTOM_ENTRY_POINT));
      MockMvc chainMvc = createMockMvc(http.build());

      chainMvc.perform(get("/account").header(HttpHeaders.AUTHORIZATION, "Bearer " + rejectedToken))
          .andExpect(status().isIAmATeapot()).andExpect(header().string("X-Entry-Point", "custom"));
    }

    @Test
    void shouldAnswerRejectedHeaderTokenWithEntryPointOfResourceServerBesideLoginPage()
        throws Exception {
      String rejectedToken = tokens.createRejected(RejectedToken.EXPIRED, "ada", "USER");
      HttpSecurity http = httpSecurityProvider.getObject();
      http.with(WebforjSecurityConfigurer.webforj(),
          configurer -> configurer.loginPage("/signin").bearerToken("jwt"));
      http.oauth2ResourceServer(server -> server.jwt(jwt -> jwt.decoder(decoder))
          .authenticationEntryPoint(CUSTOM_ENTRY_POINT));
      MockMvc chainMvc = createMockMvc(http.build());

      chainMvc
          .perform(get("/account").header(HttpHeaders.ACCEPT, "text/html")
              .header(HttpHeaders.AUTHORIZATION, "Bearer " + rejectedToken))
          .andExpect(status().isIAmATeapot()).andExpect(header().string("X-Entry-Point", "custom"));
    }

    @Test
    void shouldAnswerRejectedHeaderTokenWithUnauthorizedWithoutExceptionHandling()
        throws Exception {
      String rejectedToken = tokens.createRejected(RejectedToken.EXPIRED, "ada", "USER");
      HttpSecurity http = httpSecurityProvider.getObject();
      http.exceptionHandling(AbstractHttpConfigurer::disable);
      http.with(WebforjSecurityConfigurer.webforj(), configurer -> configurer.bearerToken("jwt"));
      http.oauth2ResourceServer(server -> server.jwt(jwt -> jwt.decoder(decoder)));
      MockMvc chainMvc = createMockMvc(http.build());

      chainMvc.perform(get("/account").header(HttpHeaders.AUTHORIZATION, "Bearer " + rejectedToken))
          .andExpect(status().isUnauthorized())
          .andExpect(header().string(HttpHeaders.WWW_AUTHENTICATE, startsWith("Bearer")));
    }

    @Test
    void shouldIgnoreTokenCookieOnPostWithoutCsrfToken() throws Exception {
      String token = tokens.createValid("ada", "USER");

      mockMvc.perform(post("/account").session(session).cookie(new Cookie("jwt", token)))
          .andExpect(status().isForbidden());

      assertEquals(0, decoder.getCount());
    }

    @Test
    void shouldExemptPostWithHeaderTokenFromCsrf() throws Exception {
      String token = tokens.createValid("ada", "USER");

      mockMvc
          .perform(post("/account").session(session).header(HttpHeaders.AUTHORIZATION,
              "Bearer " + token))
          .andExpect(status().isOk()).andExpect(content().string("ada FACTOR_BEARER,ROLE_USER"));
    }

    @Test
    void shouldKeepStoredBearerLoginOnPostWithoutToken() throws Exception {
      String token = tokens.createValid("ada", "USER");
      mockMvc.perform(get("/account").session(session).cookie(new Cookie("jwt", token)));

      mockMvc.perform(post("/account").with(csrf()).session(session)).andExpect(status().isOk())
          .andExpect(content().string("ada FACTOR_BEARER,ROLE_USER"));
    }

    @Test
    void shouldUseTokenCookieOnFrameworkRequest() throws Exception {
      String token = tokens.createValid("ada", "USER");

      mockMvc.perform(post("/webapprmi").session(session).cookie(new Cookie("jwt", token)));

      SecurityContext stored = (SecurityContext) session.getAttribute(KEY);
      assertEquals("ada", stored.getAuthentication().getName());
    }

    @Test
    void shouldDeleteTokenCookieOnLogout() throws Exception {
      String token = tokens.createValid("ada", "USER");

      mockMvc
          .perform(post("/logout").with(csrf()).session(session).cookie(new Cookie("jwt", token)))
          .andExpect(cookie().maxAge("jwt", 0));
    }

    @Test
    void shouldRejectBearerTokenWithoutResourceServer() {
      HttpSecurity http = httpSecurityProvider.getObject();
      http.with(WebforjSecurityConfigurer.webforj(), configurer -> configurer.bearerToken("jwt"));

      IllegalStateException exception =
          assertThrows(IllegalStateException.class, () -> http.build());

      assertEquals("The bearer token cookie needs a resource server. "
          + "Configure it with HttpSecurity.oauth2ResourceServer.", exception.getMessage());
    }

    @Test
    void shouldBuildWithoutResourceServerClasses() throws Exception {
      ClassLoader loader = new HidingClassLoader(
          "org.springframework.security.oauth2.server.resource.", "com.webforj.spring.security.");
      Class<?> configType = Class.forName(PlainSecurityConfig.class.getName(), true, loader);
      AnnotationConfigWebApplicationContext plainContext =
          new AnnotationConfigWebApplicationContext();
      plainContext.setClassLoader(loader);
      plainContext.setServletContext(new MockServletContext());
      plainContext.register(configType);

      assertDoesNotThrow(plainContext::refresh);

      assertEquals(1, plainContext.getBeansOfType(SecurityFilterChain.class).size());
      plainContext.close();
    }

    private MockMvc createMockMvc(SecurityFilterChain chain) {
      return MockMvcBuilders.webAppContextSetup(context).addFilters(new FilterChainProxy(chain))
          .build();
    }

    enum TokenTransport {
      COOKIE, HEADER;

      MockHttpServletRequestBuilder createRequest(String token) {
        MockHttpServletRequestBuilder request = get("/account");
        return this == COOKIE ? request.cookie(new Cookie("jwt", token))
            : request.header(HttpHeaders.AUTHORIZATION, "Bearer " + token);
      }
    }

    enum RejectedToken {
      EXPIRED, OTHER_KEY, UNSIGNED
    }

    @Configuration
    @EnableWebSecurity
    @EnableWebMvc
    @Import(SpringSecurityAutoConfiguration.class)
    static class BearerTokenSecurityConfig {

      @Bean
      SecurityFilterChain filterChain(HttpSecurity http, CountingJwtDecoder decoder) {
        JwtGrantedAuthoritiesConverter authorities = new JwtGrantedAuthoritiesConverter();
        authorities.setAuthoritiesClaimName("roles");
        authorities.setAuthorityPrefix("ROLE_");
        JwtAuthenticationConverter converter = new JwtAuthenticationConverter();
        converter.setJwtGrantedAuthoritiesConverter(authorities);
        http.with(WebforjSecurityConfigurer.webforj(), configurer -> configurer.bearerToken("jwt"));
        http.oauth2ResourceServer(server -> server
            .jwt(jwt -> jwt.decoder(decoder).jwtAuthenticationConverter(converter)));
        return http.build();
      }

      @Bean
      TokenSource tokenSource() throws JOSEException {
        return new TokenSource();
      }

      @Bean
      CountingJwtDecoder countingJwtDecoder(TokenSource tokens) throws JOSEException {
        return new CountingJwtDecoder(
            NimbusJwtDecoder.withPublicKey(tokens.getPublicKey()).build());
      }

      @Bean
      AccountController accountController() {
        return new AccountController();
      }

      @Bean
      SpringConfigurationProperties springConfigurationProperties() {
        SpringConfigurationProperties properties = new SpringConfigurationProperties();
        properties.setServletMapping("/*");
        return properties;
      }
    }

    @Configuration(proxyBeanMethods = false)
    @EnableWebSecurity
    static class PlainSecurityConfig {

      @Bean
      SecurityFilterChain filterChain(HttpSecurity http) {
        return http.with(WebforjSecurityConfigurer.webforj()).build();
      }

      @Bean
      SpringSecurityConfigurationProperties springSecurityConfigurationProperties() {
        return new SpringSecurityConfigurationProperties();
      }

      @Bean
      WebforjFrameworkRequestMatcher webforjFrameworkRequestMatcher() {
        return new WebforjFrameworkRequestMatcher(new SpringConfigurationProperties());
      }
    }

    @RestController
    static class AccountController {

      @RequestMapping("/account")
      String readAccount(Principal principal) {
        if (!(principal instanceof Authentication authentication)) {
          return "anonymous";
        }

        return authentication.getName() + " " + authentication.getAuthorities().stream()
            .map(GrantedAuthority::getAuthority).sorted().collect(Collectors.joining(","));
      }
    }

    static class CountingJwtDecoder implements JwtDecoder {
      private final JwtDecoder delegate;
      private final AtomicInteger count = new AtomicInteger();

      CountingJwtDecoder(JwtDecoder delegate) {
        this.delegate = delegate;
      }

      @Override
      public Jwt decode(String token) {
        count.incrementAndGet();
        return delegate.decode(token);
      }

      int getCount() {
        return count.get();
      }

      void resetCount() {
        count.set(0);
      }
    }

    static class TokenSource {
      private final RSAKey key = new RSAKeyGenerator(2048).generate();
      private final RSAKey otherKey = new RSAKeyGenerator(2048).generate();

      TokenSource() throws JOSEException {}

      RSAPublicKey getPublicKey() throws JOSEException {
        return key.toRSAPublicKey();
      }

      String createValid(String subject, String role) throws JOSEException {
        return sign(key, createClaims(subject, role, Instant.now().plusSeconds(3600)));
      }

      String createRejected(RejectedToken rejected, String subject, String role)
          throws JOSEException {
        Instant future = Instant.now().plusSeconds(3600);
        return switch (rejected) {
          case EXPIRED -> sign(key, createClaims(subject, role, Instant.now().minusSeconds(3600)));
          case OTHER_KEY -> sign(otherKey, createClaims(subject, role, future));
          case UNSIGNED -> new PlainJWT(createClaims(subject, role, future)).serialize();
        };
      }

      private JWTClaimsSet createClaims(String subject, String role, Instant expiresAt) {
        return new JWTClaimsSet.Builder().subject(subject).claim("roles", List.of(role))
            .issueTime(new Date()).expirationTime(Date.from(expiresAt)).build();
      }

      private String sign(RSAKey signingKey, JWTClaimsSet claims) throws JOSEException {
        SignedJWT jwt = new SignedJWT(new JWSHeader(JWSAlgorithm.RS256), claims);
        jwt.sign(new RSASSASigner(signingKey));
        return jwt.serialize();
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
