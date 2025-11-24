package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.router.RouteRegistry;
import com.webforj.router.annotation.Route;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;

@ExtendWith(MockitoExtension.class)
class SpringRouteRegistryProviderTest {

  @Mock
  private ConfigurableApplicationContext applicationContext;

  @Mock(extraInterfaces = {BeanDefinitionRegistry.class})
  private ConfigurableListableBeanFactory beanFactory;

  @Mock
  private ComponentRegistrar componentRegistrar;

  private SpringRouteRegistryProvider provider;

  @BeforeEach
  void setUp() {
    provider = new SpringRouteRegistryProvider();
  }

  @AfterEach
  void tearDown() {
    ContextHolder.setContext(null);
  }

  @Nested
  class ContextAvailability {

    @Test
    void shouldReturnEmptyRegistryWhenContextNotAvailable() {
      ContextHolder.setContext(null);

      RouteRegistry registry = provider.createRouteRegistry(new String[] {"com.example"});

      assertNotNull(registry);
      assertEquals(0, registry.getAvailableRouteEntires().size());
    }

    @Test
    void shouldReturnEmptyRegistryWhenContextNotConfigurable() {
      ApplicationContext nonConfigurableContext = mock(ApplicationContext.class);
      ContextHolder.setContext(nonConfigurableContext);

      RouteRegistry registry = provider.createRouteRegistry(new String[] {"com.example"});

      assertNotNull(registry);
      assertEquals(0, registry.getAvailableRouteEntires().size());
    }
  }

  @Nested
  class PackageRegistration {

    @BeforeEach
    void setUpContext() {
      ContextHolder.setContext(applicationContext);
      when(applicationContext.getBeanFactory()).thenReturn(beanFactory);
      when(applicationContext.getBean(ComponentRegistrar.class)).thenReturn(componentRegistrar);
    }

    @Test
    void shouldCallEnsurePackagesRegistered() {
      String[] packages = {"com.example.routes"};
      when(beanFactory.getBeanNamesForAnnotation(Route.class)).thenReturn(new String[0]);

      provider.createRouteRegistry(packages);

      verify(componentRegistrar).ensurePackagesRegistered(eq((BeanDefinitionRegistry) beanFactory),
          eq(packages));
    }

    @Test
    void shouldCallEnsurePackagesRegisteredWithEmptyPackages() {
      String[] packages = new String[0];
      when(beanFactory.getBeanNamesForAnnotation(Route.class)).thenReturn(new String[0]);

      provider.createRouteRegistry(packages);

      verify(componentRegistrar).ensurePackagesRegistered(eq((BeanDefinitionRegistry) beanFactory),
          eq(packages));
    }

    @Test
    void shouldCallEnsurePackagesRegisteredWithNullPackages() {
      when(beanFactory.getBeanNamesForAnnotation(Route.class)).thenReturn(new String[0]);

      provider.createRouteRegistry(null);

      verify(componentRegistrar).ensurePackagesRegistered(eq((BeanDefinitionRegistry) beanFactory),
          eq((String[]) null));
    }
  }

  @Nested
  class ErrorHandling {

    @BeforeEach
    void setUpContext() {
      ContextHolder.setContext(applicationContext);
      when(applicationContext.getBeanFactory()).thenReturn(beanFactory);
      when(applicationContext.getBean(ComponentRegistrar.class)).thenReturn(componentRegistrar);
    }

    @Test
    void shouldReturnEmptyRegistryOnClassNotFoundException() {
      String[] beanNames = {"testRoute"};
      when(beanFactory.getBeanNamesForAnnotation(Route.class)).thenReturn(beanNames);

      BeanDefinition beanDefinition = mock(BeanDefinition.class);
      when(beanFactory.getBeanDefinition("testRoute")).thenReturn(beanDefinition);
      when(beanDefinition.getBeanClassName()).thenReturn("com.example.NonExistentClass");

      RouteRegistry registry = provider.createRouteRegistry(new String[] {"com.example"});

      assertNotNull(registry);
      assertEquals(0, registry.getAvailableRouteEntires().size());
    }

    @Test
    void shouldReturnEmptyRegistryOnBeanFactoryException() {
      when(beanFactory.getBeanNamesForAnnotation(Route.class))
          .thenThrow(new RuntimeException("Bean factory error"));

      RouteRegistry registry = provider.createRouteRegistry(new String[] {"com.example"});

      assertNotNull(registry);
      assertEquals(0, registry.getAvailableRouteEntires().size());
    }
  }
}
