package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import jakarta.servlet.http.HttpSession;
import java.util.Optional;
import java.util.function.Consumer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

@ExtendWith(MockitoExtension.class)
class WebforjSecurityContextHolderStrategyTest {

  private WebforjSecurityContextHolderStrategy strategy;

  @Mock
  private Environment environment;

  @Mock
  private HttpSession httpSession;

  @Mock
  private HttpSession.Accessor sessionAccessor;

  private MockedStatic<Environment> environmentMock;

  @BeforeEach
  void setUp() {
    strategy = new WebforjSecurityContextHolderStrategy();
    environmentMock = mockStatic(Environment.class);
  }

  @AfterEach
  void tearDown() {
    if (environmentMock != null) {
      environmentMock.close();
    }
  }

  @Nested
  class ContextRetrieval {

    @Test
    void shouldCreateEmptyContextWhenNoEnvironment() {
      environmentMock.when(Environment::getCurrent).thenReturn(null);

      SecurityContext context = strategy.getContext();

      assertNotNull(context);
      assertTrue(context instanceof SecurityContextImpl);
    }

    @Test
    void shouldCreateEmptyContextWhenNoSessionAccessor() {
      environmentMock.when(Environment::getCurrent).thenReturn(environment);
      when(environment.getSessionAccessor()).thenReturn(Optional.empty());

      SecurityContext context = strategy.getContext();

      assertNotNull(context);
      assertTrue(context instanceof SecurityContextImpl);
    }

    @Test
    void shouldRetrieveContextFromHttpSession() {
      SecurityContext storedContext = new SecurityContextImpl();
      UserDetails user = User.withUsername("testuser").password("password").roles("USER").build();
      Authentication auth =
          new UsernamePasswordAuthenticationToken(user, null, user.getAuthorities());
      storedContext.setAuthentication(auth);

      environmentMock.when(Environment::getCurrent).thenReturn(environment);
      when(environment.getSessionAccessor()).thenReturn(Optional.of(sessionAccessor));

      // Simulate session access
      doAnswer(invocation -> {
        Consumer<HttpSession> consumer = invocation.getArgument(0);
        consumer.accept(httpSession);
        return null;
      }).when(sessionAccessor).access(any());

      when(httpSession
          .getAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY))
          .thenReturn(storedContext);

      SecurityContext retrievedContext = strategy.getContext();

      assertNotNull(retrievedContext);
      assertSame(storedContext, retrievedContext);
      assertSame(auth, retrievedContext.getAuthentication());
    }
  }
}
