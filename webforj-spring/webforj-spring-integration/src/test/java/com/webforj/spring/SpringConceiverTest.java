package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.annotation.Routify;
import com.webforj.conceiver.exception.ConceiverException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.BeanInstantiationException;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.context.ConfigurableApplicationContext;

@ExtendWith(MockitoExtension.class)
class SpringConceiverTest {

  @Mock
  private ConfigurableApplicationContext applicationContext;

  @Mock
  private AutowireCapableBeanFactory beanFactory;

  private SpringConceiver conceiver;

  public static class TestService {
  }

  @Routify
  public static class RoutifyTestApp {
  }

  @BeforeEach
  void setUp() {
    conceiver = new SpringConceiver();
    ContextHolder.setContext(applicationContext);
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
