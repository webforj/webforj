package com.webforj.testbed.junit;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.button.Button;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.annotation.WebforjTest;
import com.webforj.testbed.junit.fixture.JunitApp;
import com.webforj.testbed.junit.fixture.JunitView;
import com.webforj.testbed.snapshot.SnapshotStore;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolutionException;
import org.junit.jupiter.api.io.TempDir;

class WebforjTestbedExtensionTest {
  private final WebforjTestbedExtension extension = new WebforjTestbedExtension();
  @TempDir
  Path directory;
  private String previousDirectory;
  private String previousCi;

  @BeforeEach
  void setSnapshotDirectory() {
    previousDirectory = System.getProperty(SnapshotStore.DIRECTORY_PROPERTY);
    previousCi = System.getProperty(SnapshotStore.CI_PROPERTY);
    System.setProperty(SnapshotStore.DIRECTORY_PROPERTY, directory.toString());
    System.setProperty(SnapshotStore.CI_PROPERTY, "false");
  }

  @AfterEach
  void cleanUp() {
    SnapshotStore.endTest();
    WebforjTestbed.tearDown();
    restore(SnapshotStore.DIRECTORY_PROPERTY, previousDirectory);
    restore(SnapshotStore.CI_PROPERTY, previousCi);
  }

  @Test
  void shouldSetUpAndTearDownWithoutAnAnnotation() throws Exception {
    ExtensionContext context = context(PlainFixture.class);

    extension.beforeEach(context);

    assertTrue(WebforjTestbed.isPresent());
    assertTrue(WebforjTestbed.getCurrent().getApp().isEmpty());

    extension.afterEach(context);

    assertFalse(WebforjTestbed.isPresent());
    extension.afterAll(context);
  }

  @Test
  void shouldUseTheDefaultAnnotationOptions() throws Exception {
    ExtensionContext context = context(ComponentFixture.class);

    extension.beforeEach(context);

    assertTrue(WebforjTestbed.getCurrent().getApp().isEmpty());
    assertTrue(WebforjTestbed.getCurrent().getRouter().isEmpty());
  }

  @Test
  void shouldLaunchTheAnnotatedApplication() throws Exception {
    extension.beforeEach(context(AppFixture.class));

    assertInstanceOf(JunitApp.class, WebforjTestbed.getCurrent().getApp().orElseThrow());
    assertTrue(WebforjTestbed.getCurrent().navigate(JunitView.class).isAttached());
  }

  @Test
  void shouldRegisterTheAnnotatedRouteClasses() throws Exception {
    extension.beforeEach(context(RouteFixture.class));

    assertTrue(WebforjTestbed.getCurrent().navigate(JunitView.class).isAttached());
  }

  @Test
  void shouldScanTheAnnotatedRoutePackages() throws Exception {
    extension.beforeEach(context(PackageFixture.class));

    assertTrue(WebforjTestbed.getCurrent().navigate(JunitView.class).isAttached());
  }

  @Test
  void shouldResolveOnlyTestbedParameters() throws Exception {
    ExtensionContext context = context(PlainFixture.class);
    ParameterContext supported = mock(ParameterContext.class);
    ParameterContext unsupported = mock(ParameterContext.class);
    var parameters = PlainFixture.class
        .getDeclaredMethod("parameters", WebforjTestbed.class, String.class).getParameters();
    when(supported.getParameter()).thenReturn(parameters[0]);
    when(unsupported.getParameter()).thenReturn(parameters[1]);
    extension.beforeEach(context);

    assertTrue(extension.supportsParameter(supported, context));
    assertFalse(extension.supportsParameter(unsupported, context));
    assertSame(WebforjTestbed.getCurrent(), extension.resolveParameter(supported, context));
  }

  @Test
  void shouldRejectParameterResolutionWithoutSetup() throws Exception {
    ExtensionContext context = context(PlainFixture.class);
    ParameterContext parameter = mock(ParameterContext.class);

    assertThrows(ParameterResolutionException.class,
        () -> extension.resolveParameter(parameter, context));
  }

  @Test
  void shouldTrackSnapshotsAndIgnoreDisabledTests() throws Exception {
    ExtensionContext context = context(PlainFixture.class);
    extension.beforeEach(context);
    Button button = new Button("Save");
    WebforjTestbed.getCurrent().add(button);

    assertTrue(SnapshotStore.verify(button, null, new SnapshotOptions()).isEmpty());
    extension.afterEach(context);
    extension.afterAll(context);

    assertTrue(Files.exists(snapshotFile(PlainFixture.class, "shouldRun")));
  }

  @Test
  void shouldReportObsoleteSnapshotsAfterAllTestsRun() throws Exception {
    ExtensionContext context = context(PlainFixture.class);
    writeObsoleteSnapshot(PlainFixture.class);
    extension.beforeEach(context);
    extension.afterEach(context);

    assertThrows(AssertionError.class, () -> extension.afterAll(context));
  }

  @Test
  void shouldSkipObsoleteSnapshotsForFilteredRuns() throws Exception {
    ExtensionContext context = context(FilteredFixture.class);
    writeObsoleteSnapshot(FilteredFixture.class);
    extension.beforeEach(context);
    extension.afterEach(context);

    assertDoesNotThrow(() -> extension.afterAll(context));
  }

  private void writeObsoleteSnapshot(Class<?> type) throws IOException {
    Path file = snapshotFile(type, "obsolete");
    Files.createDirectories(file.getParent());
    Files.writeString(file, "# webforJ testbed snapshot v1\n- Button\n");
  }

  private Path snapshotFile(Class<?> type, String name) {
    String className = type.getName();
    String packagePath = type.getPackageName().replace('.', '/');

    return directory.resolve(packagePath)
        .resolve(className.substring(className.lastIndexOf('.') + 1) + "." + name + ".yml");
  }

  private static ExtensionContext context(Class<?> type) throws Exception {
    ExtensionContext parent = mock(ExtensionContext.class);
    ExtensionContext child = mock(ExtensionContext.class);
    ExtensionContext.Store store = mock(ExtensionContext.Store.class);
    Map<String, Object> values = new HashMap<>();
    when(parent.getStore(any())).thenReturn(store);
    when(child.getParent()).thenReturn(Optional.of(parent));
    when(child.getTestMethod()).thenReturn(Optional.of(type.getDeclaredMethod("shouldRun")));
    when(child.getRequiredTestMethod()).thenReturn(type.getDeclaredMethod("shouldRun"));
    when(child.getTestClass()).thenReturn(Optional.of(type));
    when(child.getRequiredTestClass()).thenAnswer(invocation -> type);
    when(store.getOrComputeIfAbsent(anyString(), any(), eq(java.util.Set.class)))
        .thenAnswer(invocation -> {
          String key = invocation.getArgument(0);
          Function<String, Object> factory = invocation.getArgument(1);
          return values.computeIfAbsent(key, factory);
        });
    return child;
  }

  private static void restore(String property, String value) {
    if (value == null) {
      System.clearProperty(property);
    } else {
      System.setProperty(property, value);
    }
  }

  static class PlainFixture {
    @Test
    void shouldRun() { // NOSONAR
      // the extension reads this method and never runs it
    }

    @Test
    @Disabled("The extension must leave this method out of the snapshots")
    void shouldSkip() { // NOSONAR
      // the extension reads this method and never runs it
    }

    void parameters(WebforjTestbed context, String other) {
      // the extension reads the parameter types and never calls this method
    }
  }

  @WebforjTest
  static class ComponentFixture extends PlainFixture {
    @Override
    @Test
    void shouldRun() { // NOSONAR
      // the extension reads this method and never runs it
    }
  }

  @WebforjTest(app = JunitApp.class)
  static class AppFixture extends ComponentFixture {
    @Override
    @Test
    void shouldRun() { // NOSONAR
      // the extension reads this method and never runs it
    }
  }

  @WebforjTest(routeClasses = JunitView.class)
  static class RouteFixture extends ComponentFixture {
    @Override
    @Test
    void shouldRun() { // NOSONAR
      // the extension reads this method and never runs it
    }
  }

  @WebforjTest(routePackages = "com.webforj.testbed.junit.fixture")
  static class PackageFixture extends ComponentFixture {
    @Override
    @Test
    void shouldRun() { // NOSONAR
      // the extension reads this method and never runs it
    }
  }

  static class FilteredFixture extends PlainFixture {
    @Override
    @Test
    void shouldRun() { // NOSONAR
      // the extension reads this method and never runs it
    }

    @Test
    void shouldRunAnother() { // NOSONAR
      // the extension reads this method and never runs it
    }
  }
}
