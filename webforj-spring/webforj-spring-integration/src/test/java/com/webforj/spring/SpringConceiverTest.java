package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import com.webforj.annotation.Routify;
import com.webforj.conceiver.exception.ConceiverException;
import com.webforj.router.annotation.Route;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.BeanInstantiationException;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.env.MockEnvironment;

@ExtendWith(MockitoExtension.class)
class SpringConceiverTest {

  private static final String LIVE_RELOAD_KEY = "webforj.devtools.livereload.enabled";
  private static final String ROUTE_BEAN_NAME = "routeTestView";

  @Mock
  private ConfigurableApplicationContext applicationContext;

  @Mock
  private AutowireCapableBeanFactory beanFactory;

  @Mock
  private ObjectProvider<ComponentRegistrar> registrarProvider;

  @Mock
  private ComponentRegistrar registrar;

  private SpringConceiver conceiver;

  public static class TestService {
  }

  @Routify
  public static class RoutifyTestApp {
  }

  @Route("/test")
  public static class RouteTestView {
  }

  @BeforeEach
  void setUp() {
    conceiver = new SpringConceiver();
    ContextHolder.setContext(applicationContext);
    lenient().when(applicationContext.getEnvironment()).thenReturn(new MockEnvironment());
  }

  @Nested
  class BeanResolution {

    @Test
    void shouldReturnExistingBeanWhenOnlyOneExists() {
      String[] singleBeanName = {"testService"};
      TestService expectedService = new TestService();

      when(applicationContext.getBeanNamesForType(TestService.class)).thenReturn(singleBeanName);
      when(applicationContext.getBean(TestService.class)).thenReturn(expectedService);

      TestService result = conceiver.get(TestService.class);

      assertNotNull(result);
      assertEquals(expectedService, result);
    }

    @Test
    void shouldCreateNewInstanceWhenNoBeanExists() {
      String[] noBeans = {};
      TestService expectedService = new TestService();

      when(applicationContext.getBeanNamesForType(TestService.class)).thenReturn(noBeans);
      when(applicationContext.getAutowireCapableBeanFactory()).thenReturn(beanFactory);
      when(beanFactory.createBean(TestService.class)).thenReturn(expectedService);

      TestService result = conceiver.get(TestService.class);

      assertNotNull(result);
      assertEquals(expectedService, result);
    }

    @Test
    void shouldCreateNewInstanceWhenMultipleBeansExist() {
      String[] multipleBeans = {"testService1", "testService2"};
      TestService expectedService = new TestService();

      when(applicationContext.getBeanNamesForType(TestService.class)).thenReturn(multipleBeans);
      when(applicationContext.getAutowireCapableBeanFactory()).thenReturn(beanFactory);
      when(beanFactory.createBean(TestService.class)).thenReturn(expectedService);

      TestService result = conceiver.get(TestService.class);

      assertNotNull(result);
      assertEquals(expectedService, result);
    }

    @Test
    void shouldBypassSpringBeanLookupForRoutifyClasses() {
      RoutifyTestApp freshInstance = new RoutifyTestApp();

      when(applicationContext.getAutowireCapableBeanFactory()).thenReturn(beanFactory);
      when(beanFactory.createBean(RoutifyTestApp.class)).thenReturn(freshInstance);

      RoutifyTestApp result = conceiver.get(RoutifyTestApp.class);

      assertNotNull(result);
      assertEquals(freshInstance, result);

      verify(applicationContext, never()).getBeanNamesForType(RoutifyTestApp.class);
      verify(applicationContext, never()).getBean(RoutifyTestApp.class);
    }

    @Test
    void shouldRefreshTheRouteDefinitionBeforeResolvingTheBean() {
      ConfigurableListableBeanFactory listableBeanFactory =
          mock(ConfigurableListableBeanFactory.class,
              withSettings().extraInterfaces(BeanDefinitionRegistry.class));
      RouteTestView expectedView = new RouteTestView();

      when(applicationContext.getEnvironment())
          .thenReturn(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(applicationContext.getBeanFactory()).thenReturn(listableBeanFactory);
      when(applicationContext.getBeanProvider(ComponentRegistrar.class))
          .thenReturn(registrarProvider);
      when(registrarProvider.getIfAvailable()).thenReturn(registrar);
      when(applicationContext.getBeanNamesForType(RouteTestView.class))
          .thenReturn(new String[] {ROUTE_BEAN_NAME});
      when(applicationContext.getBean(RouteTestView.class)).thenReturn(expectedView);

      RouteTestView result = conceiver.get(RouteTestView.class);

      assertEquals(expectedView, result);
      InOrder order = inOrder(registrar, applicationContext);
      order.verify(registrar).updateRouteDefinition((BeanDefinitionRegistry) listableBeanFactory,
          RouteTestView.class);
      order.verify(applicationContext).getBean(RouteTestView.class);
    }

    @Test
    void shouldResolveTheRouteBeanWhenNoRegistrarIsAvailable() {
      RouteTestView expectedView = new RouteTestView();

      when(applicationContext.getEnvironment())
          .thenReturn(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(applicationContext.getBeanProvider(ComponentRegistrar.class))
          .thenReturn(registrarProvider);
      when(applicationContext.getBeanNamesForType(RouteTestView.class))
          .thenReturn(new String[] {ROUTE_BEAN_NAME});
      when(applicationContext.getBean(RouteTestView.class)).thenReturn(expectedView);

      RouteTestView result = conceiver.get(RouteTestView.class);

      assertEquals(expectedView, result);
    }

    @Test
    void shouldNotTouchTheRegistrarWhenLiveReloadIsDisabled() {
      RouteTestView expectedView = new RouteTestView();

      when(applicationContext.getEnvironment()).thenReturn(new MockEnvironment());
      when(applicationContext.getBeanNamesForType(RouteTestView.class))
          .thenReturn(new String[] {ROUTE_BEAN_NAME});
      when(applicationContext.getBean(RouteTestView.class)).thenReturn(expectedView);

      conceiver.get(RouteTestView.class);

      verify(applicationContext, never()).getBeanProvider(ComponentRegistrar.class);
      verifyNoInteractions(registrar);
    }

    @Test
    void shouldReadTheLiveReloadSettingOnce() {
      RouteTestView expectedView = new RouteTestView();

      when(applicationContext.getEnvironment()).thenReturn(new MockEnvironment());
      when(applicationContext.getBeanNamesForType(RouteTestView.class))
          .thenReturn(new String[] {ROUTE_BEAN_NAME});
      when(applicationContext.getBean(RouteTestView.class)).thenReturn(expectedView);

      conceiver.get(RouteTestView.class);
      conceiver.get(RouteTestView.class);

      verify(applicationContext, times(1)).getEnvironment();
    }

    @Test
    void shouldNotRefreshDefinitionsOfClassesWithoutRoute() {
      TestService expectedService = new TestService();

      when(applicationContext.getBeanNamesForType(TestService.class))
          .thenReturn(new String[] {"testService"});
      when(applicationContext.getBean(TestService.class)).thenReturn(expectedService);

      conceiver.get(TestService.class);

      verify(applicationContext, never()).getBeanProvider(ComponentRegistrar.class);
    }
  }

  @Nested
  class ExceptionHandling {

    @Test
    void shouldThrowConceiverExceptionWhenApplicationContextIsNull() {
      ContextHolder.setContext(null);

      assertThrows(ConceiverException.class, () -> conceiver.get(TestService.class));
    }

    @Test
    void shouldThrowConceiverExceptionWhenBeanCreationFailsWithMultipleBeans() {
      String[] multipleBeans = {"testService1", "testService2", "testService3"};

      when(applicationContext.getBeanNamesForType(TestService.class)).thenReturn(multipleBeans);
      when(applicationContext.getAutowireCapableBeanFactory()).thenReturn(beanFactory);
      doThrow(new BeanInstantiationException(TestService.class, "Autowiring failed"))
          .when(beanFactory).createBean(TestService.class);

      ConceiverException exception =
          assertThrows(ConceiverException.class, () -> conceiver.get(TestService.class));

      assertEquals(BeanInstantiationException.class, exception.getCause().getClass());
    }

    @Test
    void shouldWrapGenericExceptionInConceiverException() {
      String[] singleBean = {"testService"};
      RuntimeException originalException = new RuntimeException("Generic failure");

      when(applicationContext.getBeanNamesForType(TestService.class)).thenReturn(singleBean);
      when(applicationContext.getBean(TestService.class)).thenThrow(originalException);

      ConceiverException exception =
          assertThrows(ConceiverException.class, () -> conceiver.get(TestService.class));

      assertEquals(originalException, exception.getCause());
    }

    @Test
    void shouldNotWrapConceiverException() {
      ConceiverException originalException = new ConceiverException("Original conceiver error");
      String[] singleBean = {"testService"};

      when(applicationContext.getBeanNamesForType(TestService.class)).thenReturn(singleBean);
      when(applicationContext.getBean(TestService.class)).thenThrow(originalException);

      ConceiverException exception =
          assertThrows(ConceiverException.class, () -> conceiver.get(TestService.class));

      assertEquals(originalException, exception);
    }
  }
}
