package com.webforj.testbed.junit;

import com.webforj.App;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.WebforjTestbedOptions;
import com.webforj.testbed.annotation.WebforjTest;
import com.webforj.testbed.snapshot.SnapshotStore;
import java.lang.reflect.Method;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolutionException;
import org.junit.jupiter.api.extension.ParameterResolver;
import org.junit.platform.commons.annotation.Testable;
import org.junit.platform.commons.support.AnnotationSupport;
import org.junit.platform.commons.support.HierarchyTraversalMode;

/**
 * The JUnit extension behind {@link WebforjTest}.
 *
 * <p>
 * The extension sets up a {@link WebforjTestbed} before each test, tears it down after each test
 * and resolves {@link WebforjTestbed} test method parameters.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class WebforjTestbedExtension
    implements BeforeEachCallback, AfterEachCallback, AfterAllCallback, ParameterResolver {
  private static final Namespace NAMESPACE = Namespace.create(WebforjTestbedExtension.class);
  private static final String EXECUTED_TESTS = "executedTests";
  private static final String VERIFIED_SNAPSHOTS = "verifiedSnapshots";

  @Override
  public void beforeEach(ExtensionContext context) {
    WebforjTestbed.setup(optionsOf(context));
    SnapshotStore.beginTest(context.getRequiredTestClass(),
        context.getRequiredTestMethod().getName());
    classNames(context, EXECUTED_TESTS).add(context.getRequiredTestMethod().getName());
  }

  @Override
  public void afterEach(ExtensionContext context) {
    try {
      classNames(context, VERIFIED_SNAPSHOTS).addAll(SnapshotStore.endTest());
    } finally {
      WebforjTestbed.tearDown();
    }
  }

  // Obsolete snapshots are only known once every test of the class ran. A filtered run, for
  // example one method started from the IDE, skips the check instead of reporting the others.
  @Override
  public void afterAll(ExtensionContext context) {
    Class<?> testClass = context.getRequiredTestClass();
    Set<String> executed = classNames(context, EXECUTED_TESTS);
    boolean allRan =
        testMethodsOf(testClass).stream().map(Method::getName).allMatch(executed::contains);
    if (!allRan) {
      return;
    }

    SnapshotStore.findObsolete(testClass, classNames(context, VERIFIED_SNAPSHOTS))
        .ifPresent(message -> {
          throw new AssertionError(message);
        });
  }

  @Override
  public boolean supportsParameter(ParameterContext parameterContext,
      ExtensionContext extensionContext) {
    return parameterContext.getParameter().getType() == WebforjTestbed.class;
  }

  @Override
  public Object resolveParameter(ParameterContext parameterContext,
      ExtensionContext extensionContext) {
    if (!WebforjTestbed.isPresent()) {
      throw new ParameterResolutionException("No test context is set up for this test.");
    }

    return WebforjTestbed.getCurrent();
  }

  @SuppressWarnings("unchecked")
  private static Set<String> classNames(ExtensionContext context, String key) {
    ExtensionContext classContext = context;
    Optional<ExtensionContext> parent = classContext.getParent();
    while (classContext.getTestMethod().isPresent() && parent.isPresent()) {
      classContext = parent.get();
      parent = classContext.getParent();
    }

    return classContext.getStore(NAMESPACE).getOrComputeIfAbsent(key,
        ignored -> new LinkedHashSet<String>(), Set.class);
  }

  private static List<Method> testMethodsOf(Class<?> testClass) {
    return AnnotationSupport
        .findAnnotatedMethods(testClass, Testable.class, HierarchyTraversalMode.TOP_DOWN).stream()
        .filter(method -> !AnnotationSupport.isAnnotated(method, Disabled.class)).toList();
  }

  private WebforjTestbedOptions optionsOf(ExtensionContext context) {
    WebforjTestbedOptions options = new WebforjTestbedOptions();
    Optional<WebforjTest> annotation = context.getTestClass()
        .flatMap(testClass -> AnnotationSupport.findAnnotation(testClass, WebforjTest.class));

    annotation.ifPresent(webforjTest -> {
      if (webforjTest.app() != App.class) {
        options.setApp(webforjTest.app());
      }

      if (webforjTest.routePackages().length > 0) {
        options.setRoutePackages(webforjTest.routePackages());
      }

      if (webforjTest.routeClasses().length > 0) {
        options.setRouteClasses(webforjTest.routeClasses());
      }
    });

    return options;
  }
}
