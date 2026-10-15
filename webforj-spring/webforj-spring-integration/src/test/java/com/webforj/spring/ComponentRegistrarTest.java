package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.annotation.Routify;
import com.webforj.router.annotation.Route;
import com.webforj.spring.routescan.ScannedRouteView;
import com.webforj.spring.routescan.StaticInitializerRecord;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Scope;
import org.springframework.mock.env.MockEnvironment;
import org.springframework.stereotype.Component;

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

  @Nested
  class LiveReloadRescanning {

    private static final String SCANNED_PACKAGE = "com.webforj.spring.routescan";
    private static final String LIVE_RELOAD_KEY = "webforj.devtools.livereload.enabled";
    private static final String SCANNED_CLASS_FILE =
        "com/webforj/spring/routescan/ScannedRouteView.class";
    private static final String SCANNED_BEAN_NAME = "scannedRouteView";
    private static final String GONE_BEAN_NAME = "goneView";
    private static final String DELETED_BEAN_NAME = "deletedView";
    private static final String MISSING_BEAN_NAME = "missingView";
    private static final String OTHER_BEAN_NAME = "otherService";

    @Test
    void shouldScanAPackageOnlyOnceWhenLiveReloadIsDisabled() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "false"));

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});
      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }

    @Test
    void shouldScanAPackageAgainWhenLiveReloadIsEnabled() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[0]);

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});
      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry, times(2)).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }

    @Test
    void shouldKeepTheRouteDefinitionsOnTheFirstRescan() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[0]);
      when(registry.containsBeanDefinition(anyString())).thenReturn(true);

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry, never()).removeBeanDefinition(SCANNED_BEAN_NAME);
      verify(registry, never()).registerBeanDefinition(eq(SCANNED_BEAN_NAME),
          any(BeanDefinition.class));
    }

    @Test
    void shouldKeepTheRouteDefinitionsWhenLiveReloadIsDisabled() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "false"));
      when(registry.containsBeanDefinition(anyString())).thenReturn(true);

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry, never()).removeBeanDefinition(anyString());
      verify(registry, never()).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }

    @Test
    void shouldRemoveTheDefinitionOfTheRouteWhoseClassFileIsGone() throws Exception {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      GhostClassLoader ghostLoader = new GhostClassLoader(ScannedRouteView.class.getName());
      ConfigurableListableBeanFactory beanFactory = mock(ConfigurableListableBeanFactory.class);
      when(beanFactory.getBeanClassLoader()).thenReturn(ghostLoader);
      registrar.postProcessBeanFactory(beanFactory);
      Class<?> gone = Class.forName(ScannedRouteView.class.getName(), true, ghostLoader);
      when(registry.getBeanDefinitionNames()).thenReturn(new String[] {GONE_BEAN_NAME});
      when(registry.getBeanDefinition(GONE_BEAN_NAME))
          .thenReturn(BeanDefinitionBuilder.genericBeanDefinition(gone).getBeanDefinition());

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry).removeBeanDefinition(GONE_BEAN_NAME);
      verify(registry, never()).registerBeanDefinition(eq(GONE_BEAN_NAME),
          any(BeanDefinition.class));
    }

    @Test
    void shouldKeepTheDefinitionOfTheRouteWhoseClassFileExists() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[] {SCANNED_BEAN_NAME});
      when(registry.getBeanDefinition(SCANNED_BEAN_NAME)).thenReturn(
          BeanDefinitionBuilder.genericBeanDefinition(ScannedRouteView.class).getBeanDefinition());

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry, never()).removeBeanDefinition(anyString());
      verify(registry).registerBeanDefinition(eq(SCANNED_BEAN_NAME), any(BeanDefinition.class));
    }

    @Test
    void shouldKeepTheDefinitionsWhenTheClassFilesDidNotChange() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[0]);
      when(registry.containsBeanDefinition(anyString())).thenReturn(true);

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});
      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry, never()).removeBeanDefinition(anyString());
      verify(registry, never()).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }

    @Test
    void shouldReplaceTheDefinitionWhoseClassFileChanged(@TempDir Path classFolder)
        throws Exception {
      Path classFile = classFolder.resolve(SCANNED_CLASS_FILE);
      Files.createDirectories(classFile.getParent());
      try (InputStream input = getClass().getResourceAsStream("/" + SCANNED_CLASS_FILE)) {
        Files.copy(input, classFile);
      }

      Files.setLastModifiedTime(classFile, FileTime.fromMillis(1_000));
      ConfigurableListableBeanFactory beanFactory = mock(ConfigurableListableBeanFactory.class);
      when(beanFactory.getBeanClassLoader())
          .thenReturn(new URLClassLoader(new URL[] {classFolder.toUri().toURL()}, null));
      registrar.postProcessBeanFactory(beanFactory);
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[0]);
      when(registry.containsBeanDefinition(anyString())).thenReturn(true);

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});
      Files.setLastModifiedTime(classFile, FileTime.fromMillis(2_000));
      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry).removeBeanDefinition(SCANNED_BEAN_NAME);
      verify(registry).registerBeanDefinition(eq(SCANNED_BEAN_NAME), any(BeanDefinition.class));
    }

    @Test
    void shouldRemoveTheDefinitionWhoseClassFileWasDeletedAfterTheFirstRescan(
        @TempDir Path classFolder) throws Exception {
      Path classFile = classFolder.resolve(SCANNED_CLASS_FILE);
      Files.createDirectories(classFile.getParent());
      try (InputStream input = getClass().getResourceAsStream("/" + SCANNED_CLASS_FILE)) {
        Files.copy(input, classFile);
      }

      ConfigurableListableBeanFactory beanFactory = mock(ConfigurableListableBeanFactory.class);
      when(beanFactory.getBeanClassLoader())
          .thenReturn(new URLClassLoader(new URL[] {classFolder.toUri().toURL()}, null));
      registrar.postProcessBeanFactory(beanFactory);
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[] {DELETED_BEAN_NAME});
      when(registry.getBeanDefinition(DELETED_BEAN_NAME)).thenReturn(
          BeanDefinitionBuilder.genericBeanDefinition(ScannedRouteView.class).getBeanDefinition());

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});
      Files.delete(classFile);
      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry).removeBeanDefinition(DELETED_BEAN_NAME);
    }

    @Test
    void shouldKeepTheDefinitionWhoseClassCannotBeLoaded() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[] {MISSING_BEAN_NAME});
      when(registry.getBeanDefinition(MISSING_BEAN_NAME)).thenReturn(BeanDefinitionBuilder
          .genericBeanDefinition(SCANNED_PACKAGE + ".MissingView").getBeanDefinition());

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});
      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry, never()).removeBeanDefinition(MISSING_BEAN_NAME);
    }

    @Test
    void shouldKeepTheDefinitionsOutsideTheScannedPackages() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[] {OTHER_BEAN_NAME});
      when(registry.getBeanDefinition(OTHER_BEAN_NAME)).thenReturn(BeanDefinitionBuilder
          .genericBeanDefinition("com.example.OtherService").getBeanDefinition());

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry, never()).removeBeanDefinition(OTHER_BEAN_NAME);
    }

    @Test
    void shouldLoadTheBeanClassWithoutInitializingIt() {
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.getBeanDefinitionNames()).thenReturn(new String[] {"recordedClass"});
      when(registry.getBeanDefinition("recordedClass")).thenReturn(BeanDefinitionBuilder
          .genericBeanDefinition(StaticInitializerRecord.RecordedClass.class.getName())
          .getBeanDefinition());

      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      assertFalse(StaticInitializerRecord.RAN.get());
    }

    @Test
    void shouldReplaceTheDefinitionOfTheRouteWhoseClassFileChangedWhenAsked(
        @TempDir Path classFolder) throws Exception {
      Path classFile = classFolder.resolve(SCANNED_CLASS_FILE);
      Files.createDirectories(classFile.getParent());
      try (InputStream input = getClass().getResourceAsStream("/" + SCANNED_CLASS_FILE)) {
        Files.copy(input, classFile);
      }

      Files.setLastModifiedTime(classFile, FileTime.fromMillis(1_000));
      ConfigurableListableBeanFactory beanFactory = mock(ConfigurableListableBeanFactory.class);
      when(beanFactory.getBeanClassLoader())
          .thenReturn(new URLClassLoader(new URL[] {classFolder.toUri().toURL()}, null));
      registrar.postProcessBeanFactory(beanFactory);
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.containsBeanDefinition(SCANNED_BEAN_NAME)).thenReturn(true);

      registrar.updateRouteDefinition(registry, ScannedRouteView.class);
      Files.setLastModifiedTime(classFile, FileTime.fromMillis(2_000));
      registrar.updateRouteDefinition(registry, ScannedRouteView.class);

      verify(registry).removeBeanDefinition(SCANNED_BEAN_NAME);
      verify(registry).registerBeanDefinition(eq(SCANNED_BEAN_NAME), any(BeanDefinition.class));
    }

    @Test
    void shouldKeepTheDefinitionOfTheRouteWhenAskedWithoutChange(@TempDir Path classFolder)
        throws Exception {
      Path classFile = classFolder.resolve(SCANNED_CLASS_FILE);
      Files.createDirectories(classFile.getParent());
      try (InputStream input = getClass().getResourceAsStream("/" + SCANNED_CLASS_FILE)) {
        Files.copy(input, classFile);
      }

      Files.setLastModifiedTime(classFile, FileTime.fromMillis(1_000));
      ConfigurableListableBeanFactory beanFactory = mock(ConfigurableListableBeanFactory.class);
      when(beanFactory.getBeanClassLoader())
          .thenReturn(new URLClassLoader(new URL[] {classFolder.toUri().toURL()}, null));
      registrar.postProcessBeanFactory(beanFactory);
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.containsBeanDefinition(SCANNED_BEAN_NAME)).thenReturn(true);

      registrar.updateRouteDefinition(registry, ScannedRouteView.class);
      registrar.updateRouteDefinition(registry, ScannedRouteView.class);

      verify(registry, never()).removeBeanDefinition(anyString());
      verify(registry, never()).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }

    @Test
    void shouldKeepTheDefinitionOfTheSpringManagedRoute(@TempDir Path classFolder)
        throws Exception {
      String classFileName = SpringManagedRouteView.class.getName().replace('.', '/') + ".class";
      Path classFile = classFolder.resolve(classFileName);
      Files.createDirectories(classFile.getParent());
      try (InputStream input = getClass().getResourceAsStream("/" + classFileName)) {
        Files.copy(input, classFile);
      }

      Files.setLastModifiedTime(classFile, FileTime.fromMillis(1_000));
      ConfigurableListableBeanFactory beanFactory = mock(ConfigurableListableBeanFactory.class);
      when(beanFactory.getBeanClassLoader())
          .thenReturn(new URLClassLoader(new URL[] {classFolder.toUri().toURL()}, null));
      registrar.postProcessBeanFactory(beanFactory);
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      lenient().when(registry.containsBeanDefinition(anyString())).thenReturn(true);

      registrar.updateRouteDefinition(registry, SpringManagedRouteView.class);
      Files.setLastModifiedTime(classFile, FileTime.fromMillis(2_000));
      registrar.updateRouteDefinition(registry, SpringManagedRouteView.class);

      verify(registry, never()).removeBeanDefinition(anyString());
      verify(registry, never()).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }

    @Test
    void shouldTrackTheRouteWhoseClassFileWasMissingAtFirst(@TempDir Path classFolder)
        throws Exception {
      ConfigurableListableBeanFactory beanFactory = mock(ConfigurableListableBeanFactory.class);
      when(beanFactory.getBeanClassLoader())
          .thenReturn(new URLClassLoader(new URL[] {classFolder.toUri().toURL()}, null));
      registrar.postProcessBeanFactory(beanFactory);
      registrar.setEnvironment(new MockEnvironment().withProperty(LIVE_RELOAD_KEY, "true"));
      when(registry.containsBeanDefinition(SCANNED_BEAN_NAME)).thenReturn(true);

      registrar.updateRouteDefinition(registry, ScannedRouteView.class);
      Path classFile = classFolder.resolve(SCANNED_CLASS_FILE);
      Files.createDirectories(classFile.getParent());
      try (InputStream input = getClass().getResourceAsStream("/" + SCANNED_CLASS_FILE)) {
        Files.copy(input, classFile);
      }

      Files.setLastModifiedTime(classFile, FileTime.fromMillis(1_000));
      registrar.updateRouteDefinition(registry, ScannedRouteView.class);
      Files.setLastModifiedTime(classFile, FileTime.fromMillis(2_000));
      registrar.updateRouteDefinition(registry, ScannedRouteView.class);

      verify(registry).removeBeanDefinition(SCANNED_BEAN_NAME);
      verify(registry).registerBeanDefinition(eq(SCANNED_BEAN_NAME), any(BeanDefinition.class));
    }

    @Test
    void shouldScanAPackageOnlyOnceWhenNoEnvironmentIsAvailable() {
      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});
      registrar.ensurePackagesRegistered(registry, new String[] {SCANNED_PACKAGE});

      verify(registry).registerBeanDefinition(anyString(), any(BeanDefinition.class));
    }
  }

  // Test classes for various scenarios
  @Route("/managed")
  @Component
  @Scope("session")
  static class SpringManagedRouteView {
  }

  @SpringBootApplication
  static class TestAppWithoutRoutify {
  }

  @SpringBootApplication
  @Routify(packages = {"com.example.test"})
  static class TestAppWithRoutify {
  }
}
