package com.webforj.testbed.catalog;

import com.webforj.component.Component;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.ClassInfo;
import io.github.classgraph.ScanResult;
import java.util.ArrayList;
import java.util.List;

/**
 * Discovers the concrete public components of the foundation module.
 */
public final class FoundationComponents {

  private FoundationComponents() {}

  /**
   * Returns every public concrete component class shipped by the foundation module, in name order.
   *
   * @return the foundation component classes
   */
  public static List<Class<? extends Component>> discover() {
    List<Class<? extends Component>> catalog = new ArrayList<>();

    try (ScanResult scan = new ClassGraph().enableClassInfo().ignoreClassVisibility()
        .acceptPackages("com.webforj.component")
        .rejectPackages("com.webforj.component.fontchooser", "com.webforj.component.htmledit")
        .scan()) {
      for (ClassInfo classInfo : scan.getSubclasses(Component.class.getName())) {
        if (isFoundationComponent(classInfo)) {
          catalog.add(classInfo.loadClass().asSubclass(Component.class));
        }
      }
    }

    catalog.sort((left, right) -> left.getSimpleName().compareTo(right.getSimpleName()));

    return catalog;
  }

  private static boolean isFoundationComponent(ClassInfo classInfo) {
    if (!classInfo.isPublic() || classInfo.isAbstract() || classInfo.isInnerClass()) {
      return false;
    }

    Object element = classInfo.getClasspathElementFile();

    return element != null && element.toString().contains("webforj-foundation");
  }
}
