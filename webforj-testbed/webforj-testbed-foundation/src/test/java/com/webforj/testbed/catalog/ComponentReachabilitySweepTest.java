package com.webforj.testbed.catalog;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.Component;
import com.webforj.testbed.WebforjTestbed;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.ClassInfo;
import io.github.classgraph.ScanResult;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.opentest4j.AssertionFailedError;

class ComponentReachabilitySweepTest {

  @Test
  void shouldAttachAnswerReadersAndDestroyEveryComponent() {
    List<Class<? extends Component>> catalog = discover();
    assertTrue(catalog.size() >= 80,
        "Expected the sweep to discover the component catalog but found " + catalog.size());

    List<String> failures = new ArrayList<>();
    for (Class<? extends Component> componentClass : catalog) {
      WebforjTestbed context = WebforjTestbed.setup();
      try {
        Component component = componentClass.getConstructor().newInstance();
        if (!component.isAttached()) {
          context.add(component);
        }
        ComponentMethodVerifier.verifyEveryPublicReader(component);
        component.destroy();
      } catch (AssertionFailedError e) {
        failures.add(e.getMessage());
      } catch (InvocationTargetException e) {
        failures.add(componentClass.getName() + " failed while attaching: " + e.getCause());
      } catch (ReflectiveOperationException | RuntimeException e) {
        failures.add(componentClass.getName() + " failed while attaching: " + e);
      } finally {
        WebforjTestbed.tearDown();
      }
    }

    if (!failures.isEmpty()) {
      fail(failures.size() + " components fail under the testbed:" + System.lineSeparator()
          + String.join(System.lineSeparator() + System.lineSeparator(), failures));
    }
  }

  private static List<Class<? extends Component>> discover() {
    List<Class<? extends Component>> catalog = new ArrayList<>();
    try (ScanResult scan = new ClassGraph().enableClassInfo().ignoreClassVisibility()
        .acceptPackages("com.webforj.component")
        .rejectPackages("com.webforj.component.fontchooser", "com.webforj.component.htmledit")
        .scan()) {
      for (ClassInfo classInfo : scan.getSubclasses(Component.class.getName())) {
        if (!classInfo.isPublic() || classInfo.isAbstract() || classInfo.isInnerClass()) {
          continue;
        }

        Class<? extends Component> componentClass =
            classInfo.loadClass().asSubclass(Component.class);
        if (hasPublicDefaultConstructor(componentClass)) {
          catalog.add(componentClass);
        }
      }
    }

    catalog.sort((left, right) -> left.getName().compareTo(right.getName()));

    return catalog;
  }

  private static boolean hasPublicDefaultConstructor(Class<?> componentClass) {
    for (Constructor<?> constructor : componentClass.getConstructors()) {
      if (constructor.getParameterCount() == 0 && Modifier.isPublic(constructor.getModifiers())) {
        return true;
      }
    }

    return false;
  }
}
