package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.spring.ContextHolder;
import jakarta.servlet.http.HttpSession;
import java.lang.reflect.Method;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.security.web.csrf.DefaultCsrfToken;

@ExtendWith(MockitoExtension.class)
class RouteSecurityObserverRegistrarTest {

  @Mock
  private App app;

  @Mock
  private Page page;

  @Mock
  private ApplicationContext context;

  private MockedStatic<Page> pageMock;
  private MockedStatic<ContextHolder> contextHolderMock;
  private final RouteSecurityObserverRegistrar registrar = new RouteSecurityObserverRegistrar();

  @BeforeEach
  void setUp() {
    pageMock = mockStatic(Page.class);
    contextHolderMock = mockStatic(ContextHolder.class);
    contextHolderMock.when(ContextHolder::getContext).thenReturn(context);
  }

  @AfterEach
  void tearDown() {
    contextHolderMock.close();
    pageMock.close();
  }

  @Test
  void shouldAddNoCsrfMetaWithoutSecurityConfiguration() {
    when(context.getBean(SpringRouteSecurityConfiguration.class))
        .thenThrow(new NoSuchBeanDefinitionException(SpringRouteSecurityConfiguration.class));

    registrar.onWillRun(app);

    verify(page, never()).setMeta(anyString(), anyString());
  }

  @Test
  void shouldRunWithoutSpringSecurityOnTheClassPath() throws Exception {
    ClassLoader loader = new SpringSecurityFreeClassLoader();
    Class<?> configurationType =
        Class.forName(SpringRouteSecurityConfiguration.class.getName(), true, loader);
    when(context.getBean(ArgumentMatchers.<Class<Object>>any()))
        .thenReturn(mock(configurationType));
    Class<?> registrarType =
        Class.forName(RouteSecurityObserverRegistrar.class.getName(), true, loader);
    Object registrar = registrarType.getDeclaredConstructor().newInstance();
    Method onWillRun = registrarType.getMethod("onWillRun", App.class);

    assertDoesNotThrow(() -> onWillRun.invoke(registrar, app));

    verify(page, never()).setMeta(anyString(), anyString());
  }

  @Nested
  class WithSecurityConfiguration {

    @Mock
    private SpringRouteSecurityConfiguration securityConfiguration;

    @Mock
    private Environment environment;

    @Mock
    private HttpSession httpSession;

    private MockedStatic<Environment> environmentMock;

    @BeforeEach
    void setUp() {
      when(context.getBean(SpringRouteSecurityConfiguration.class))
          .thenReturn(securityConfiguration);
      when(securityConfiguration.isEnabled()).thenReturn(false);
      environmentMock = mockStatic(Environment.class);
      environmentMock.when(Environment::getCurrent).thenReturn(environment);
      HttpSession.Accessor accessor = SessionAccessorStub.create(httpSession);
      when(environment.getSessionAccessor()).thenReturn(Optional.of(accessor));
    }

    @AfterEach
    void tearDown() {
      environmentMock.close();
    }

    @Test
    void shouldAddStoredTokenAsCsrfMeta() {
      pageMock.when(Page::getCurrent).thenReturn(page);
      when(httpSession.getAttribute(WebforjCsrfTokenFilter.SESSION_ATTRIBUTE_NAME))
          .thenReturn(new DefaultCsrfToken("X-CSRF-TOKEN", "_csrf", "masked-token"));

      registrar.onWillRun(app);

      verify(page).setMeta("_csrf", "masked-token");
      verify(page).setMeta("_csrf_parameter", "_csrf");
    }

    @Test
    void shouldAddNoCsrfMetaWithoutStoredToken() {
      registrar.onWillRun(app);

      verify(page, never()).setMeta(anyString(), anyString());
    }
  }
}
