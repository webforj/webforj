package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.User;

@ExtendWith(MockitoExtension.class)
class SpringRouteSecurityContextTest {

  private SpringRouteSecurityContext securityContext;

  @Mock
  private Authentication authentication;

  @BeforeEach
  void setUp() {
    securityContext = new SpringRouteSecurityContext();
  }

  @AfterEach
  void tearDown() {
    SecurityContextHolder.clearContext();
  }

  @Nested
  class AuthenticationStatus {

    @Test
    void shouldReturnFalseWhenNoAuthentication() {
      assertFalse(securityContext.isAuthenticated());
    }

    @Test
    void shouldReturnFalseWhenAuthenticationNotAuthenticated() {
      when(authentication.isAuthenticated()).thenReturn(false);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertFalse(securityContext.isAuthenticated());
    }

    @Test
    void shouldReturnTrueWhenFullyAuthenticated() {
      when(authentication.isAuthenticated()).thenReturn(true);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertTrue(securityContext.isAuthenticated());
    }
  }

  @Nested
  class PrincipalAccess {

    @Test
    void shouldReturnEmptyWhenNoAuthentication() {
      Optional<Object> principal = securityContext.getPrincipal();
      assertFalse(principal.isPresent());
    }

    @Test
    void shouldReturnPrincipalWhenAuthenticated() {
      User user = new User("testuser", "password", Collections.emptyList());
      when(authentication.getPrincipal()).thenReturn(user);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      Optional<Object> principal = securityContext.getPrincipal();
      assertTrue(principal.isPresent());
      assertEquals(user, principal.get());
    }

    @Test
    void shouldReturnEmptyWhenPrincipalIsNull() {
      when(authentication.getPrincipal()).thenReturn(null);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      Optional<Object> principal = securityContext.getPrincipal();
      assertFalse(principal.isPresent());
    }
  }

  @Nested
  class RoleChecking {

    @Test
    void shouldReturnFalseWhenNoAuthentication() {
      assertFalse(securityContext.hasRole("ADMIN"));
    }

    @Test
    void shouldReturnTrueWhenRolePresent() {
      Collection<? extends GrantedAuthority> authorities = Arrays.asList(
          new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
      when(authentication.getAuthorities()).thenReturn((Collection) authorities);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertTrue(securityContext.hasRole("ADMIN"));
      assertTrue(securityContext.hasRole("USER"));
    }

    @Test
    void shouldHandleRolePrefixAutomatically() {
      Collection<? extends GrantedAuthority> authorities =
          Arrays.asList(new SimpleGrantedAuthority("ROLE_ADMIN"));
      when(authentication.getAuthorities()).thenReturn((Collection) authorities);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertTrue(securityContext.hasRole("ADMIN"));
      assertTrue(securityContext.hasRole("ROLE_ADMIN"));
    }

    @Test
    void shouldReturnFalseWhenRoleNotPresent() {
      Collection<? extends GrantedAuthority> authorities =
          Arrays.asList(new SimpleGrantedAuthority("ROLE_USER"));
      when(authentication.getAuthorities()).thenReturn((Collection) authorities);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertFalse(securityContext.hasRole("ADMIN"));
    }

    @Test
    void shouldHandleEmptyAuthorities() {
      when(authentication.getAuthorities()).thenReturn(Collections.emptyList());
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertFalse(securityContext.hasRole("ADMIN"));
    }
  }

  @Nested
  class AuthorityChecking {

    @Test
    void shouldReturnFalseWhenNoAuthentication() {
      assertFalse(securityContext.hasAuthority("READ_PRIVILEGE"));
    }

    @Test
    void shouldReturnTrueWhenAuthorityPresent() {
      Collection<? extends GrantedAuthority> authorities =
          Arrays.asList(new SimpleGrantedAuthority("READ_PRIVILEGE"),
              new SimpleGrantedAuthority("WRITE_PRIVILEGE"));
      when(authentication.getAuthorities()).thenReturn((Collection) authorities);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertTrue(securityContext.hasAuthority("READ_PRIVILEGE"));
      assertTrue(securityContext.hasAuthority("WRITE_PRIVILEGE"));
    }

    @Test
    void shouldReturnFalseWhenAuthorityNotPresent() {
      Collection<? extends GrantedAuthority> authorities =
          Arrays.asList(new SimpleGrantedAuthority("READ_PRIVILEGE"));
      when(authentication.getAuthorities()).thenReturn((Collection) authorities);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertFalse(securityContext.hasAuthority("WRITE_PRIVILEGE"));
    }

    @Test
    void shouldNotApplyRolePrefixToAuthorities() {
      Collection<? extends GrantedAuthority> authorities =
          Arrays.asList(new SimpleGrantedAuthority("ADMIN"));
      when(authentication.getAuthorities()).thenReturn((Collection) authorities);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      assertTrue(securityContext.hasAuthority("ADMIN"));
      assertFalse(securityContext.hasAuthority("ROLE_ADMIN"));
    }
  }

  @Nested
  class AttributeHandling {

    @Test
    void shouldReturnEmptyWhenNoAuthentication() {
      Optional<Object> attribute = securityContext.getAttribute("test");
      assertFalse(attribute.isPresent());
    }

    @Test
    void shouldReturnEmptyWhenDetailsNotMap() {
      when(authentication.getDetails()).thenReturn("not a map");
      SecurityContextHolder.getContext().setAuthentication(authentication);

      Optional<Object> attribute = securityContext.getAttribute("test");
      assertFalse(attribute.isPresent());
    }

    @Test
    void shouldReturnAttributeFromDetailsMap() {
      Map<String, Object> details = new HashMap<>();
      details.put("sessionId", "123456");
      details.put("remoteAddress", "192.168.1.1");
      when(authentication.getDetails()).thenReturn(details);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      Optional<Object> sessionId = securityContext.getAttribute("sessionId");
      assertTrue(sessionId.isPresent());
      assertEquals("123456", sessionId.get());

      Optional<Object> remoteAddress = securityContext.getAttribute("remoteAddress");
      assertTrue(remoteAddress.isPresent());
      assertEquals("192.168.1.1", remoteAddress.get());
    }

    @Test
    void shouldReturnEmptyForNonExistentAttribute() {
      Map<String, Object> details = new HashMap<>();
      details.put("sessionId", "123456");
      when(authentication.getDetails()).thenReturn(details);
      SecurityContextHolder.getContext().setAuthentication(authentication);

      Optional<Object> attribute = securityContext.getAttribute("nonexistent");
      assertFalse(attribute.isPresent());
    }

    @Test
    void shouldThrowUnsupportedOperationExceptionForSetAttribute() {
      assertThrows(UnsupportedOperationException.class,
          () -> securityContext.setAttribute("test", "value"));
    }
  }
}
