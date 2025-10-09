package com.webforj.spring.security;

import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestBuilders.formLogin;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.webforj.spring.SpringConfigurationProperties;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.test.context.junit.jupiter.web.SpringJUnitWebConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

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
    void shouldBypassCsrfForLoginProcessingUrl() throws Exception {
      mockMvc
          .perform(post("/authenticate").contentType("application/x-www-form-urlencoded")
              .param("username", "user").param("password", "password"))
          .andExpect(status().is3xxRedirection());
    }

    @Test
    void shouldBypassCsrfForLogoutUrl() throws Exception {
      mockMvc.perform(post("/signout")).andExpect(status().is3xxRedirection());
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
  }

  @Configuration
  @EnableWebSecurity
  @EnableWebMvc
  @Import(SpringSecurityAutoConfiguration.class)
  static class TestSecurityConfig {

    @Bean
    SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
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
}
