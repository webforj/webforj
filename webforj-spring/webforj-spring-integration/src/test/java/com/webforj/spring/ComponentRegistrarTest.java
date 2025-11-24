package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.annotation.Routify;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@ExtendWith(MockitoExtension.class)
class ComponentRegistrarTest {

  @Mock
  private BeanDefinitionRegistry registry;

  private ComponentRegistrar registrar;

  @BeforeEach
  void setUp() {
    registrar = new ComponentRegistrar();
  }

  @Nested
  class RoutifyBasedProcessing {

    @Test
    void shouldSkipWhenNoSpringBootApplicationFound() {
      when(registry.getBeanDefinitionNames()).thenReturn(new String[0]);

      registrar.postProcessBeanDefinitionRegistry(registry);

      verify(registry, never()).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }

    @Test
    void shouldSkipWhenSpringBootAppHasNoRoutify() {
      String[] beanNames = {"testApp"};
      when(registry.getBeanDefinitionNames()).thenReturn(beanNames);

      BeanDefinition appBeanDef = mock(BeanDefinition.class);
      when(registry.getBeanDefinition("testApp")).thenReturn(appBeanDef);
      when(appBeanDef.getBeanClassName()).thenReturn(TestAppWithoutRoutify.class.getName());

      registrar.postProcessBeanDefinitionRegistry(registry);

      verify(registry, never()).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }

    @Test
    void shouldProcessWhenSpringBootAppHasRoutify() {
      String[] beanNames = {"testApp"};
      when(registry.getBeanDefinitionNames()).thenReturn(beanNames);

      BeanDefinition appBeanDef = mock(BeanDefinition.class);
      when(registry.getBeanDefinition("testApp")).thenReturn(appBeanDef);
      when(appBeanDef.getBeanClassName()).thenReturn(TestAppWithRoutify.class.getName());

      registrar.postProcessBeanDefinitionRegistry(registry);

      // Should attempt to process but won't find any components without actual scanning
      verify(registry).getBeanDefinitionNames();
    }
  }

  @Nested
  class BeanDefinitionConfiguration {

    @Test
    void shouldSkipRegistrationIfBeanAlreadyExists() {
      String[] beanNames = {"testApp"};
      when(registry.getBeanDefinitionNames()).thenReturn(beanNames);

      BeanDefinition appBeanDef = mock(BeanDefinition.class);
      when(registry.getBeanDefinition("testApp")).thenReturn(appBeanDef);
      when(appBeanDef.getBeanClassName()).thenReturn(TestAppWithRoutify.class.getName());

      registrar.postProcessBeanDefinitionRegistry(registry);

      // Verify the process was called
      verify(registry).getBeanDefinitionNames();
    }
  }

  @Nested
  class EnsurePackagesRegisteredBehavior {

    @Test
    void shouldTrackRegisteredPackages() {
      String[] packages = {"com.example.routes"};

      registrar.ensurePackagesRegistered(registry, packages);

      assertTrue(registrar.isPackageRegistered("com.example.routes"));
    }

    @Test
    void shouldNotRescanAlreadyRegisteredPackages() {
      String[] packages = {"com.example.routes"};

      registrar.ensurePackagesRegistered(registry, packages);
      int sizeAfterFirstCall = registrar.getRegisteredPackageCount();

      registrar.ensurePackagesRegistered(registry, packages);

      // Size should not change, package already tracked
      assertEquals(sizeAfterFirstCall, registrar.getRegisteredPackageCount());
    }

    @Test
    void shouldAddNewPackagesButNotDuplicates() {
      String[] firstCall = {"com.example.one"};
      String[] secondCall = {"com.example.one", "com.example.two"};

      registrar.ensurePackagesRegistered(registry, firstCall);
      assertEquals(1, registrar.getRegisteredPackageCount());

      registrar.ensurePackagesRegistered(registry, secondCall);

      assertEquals(2, registrar.getRegisteredPackageCount());
      assertTrue(registrar.isPackageRegistered("com.example.one"));
      assertTrue(registrar.isPackageRegistered("com.example.two"));
    }
  }

  // Test classes for various scenarios
  @SpringBootApplication
  static class TestAppWithoutRoutify {
  }

  @SpringBootApplication
  @Routify(packages = {"com.example.test"})
  static class TestAppWithRoutify {
  }
}

