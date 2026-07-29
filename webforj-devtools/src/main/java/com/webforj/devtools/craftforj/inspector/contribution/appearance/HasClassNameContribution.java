package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasClassName;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.ListConcernContribution;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * Contribution for the HasClassName concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasClassNameContribution extends ListConcernContribution<HasClassName<?>> {

  /**
   * Creates the HasClassName contribution.
   */
  public HasClassNameContribution() {
    super(HasClassName.class, "ClassNames", FeatureCategory.APPEARANCE);
    setGetter(this::getClassNames);
    setAddHandler(HasClassName::addClassName);
    setRemoveHandler(HasClassName::removeClassName);
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "addClassName";
  }

  /**
   * Reads the class names reflectively, which stands in until the concern exposes a getter.
   */
  @SuppressWarnings("java:S3011")
  private List<String> getClassNames(HasClassName<?> component) {
    Object target = component;

    if (component instanceof Composite) {
      target = ComponentUtil.getBoundComponent(component);
    }

    try {
      Field field = findField(target.getClass(), "classNames");
      if (field != null) {
        field.setAccessible(true);
        List<String> list = (List<String>) field.get(target);
        if (list != null) {
          return new ArrayList<>(new LinkedHashSet<>(list));
        }
      }
    } catch (Exception e) {
      // Ignore reflection errors
    }

    return Collections.emptyList();
  }

  private Field findField(Class<?> clazz, String fieldName) {
    while (clazz != null) {
      try {
        return clazz.getDeclaredField(fieldName);
      } catch (NoSuchFieldException e) {
        clazz = clazz.getSuperclass();
      }
    }
    return null;
  }
}
