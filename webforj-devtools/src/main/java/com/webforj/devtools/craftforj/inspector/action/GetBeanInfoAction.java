package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.data.BeanIntrospection;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import java.beans.IntrospectionException;
import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;

/**
 * Returns reflection metadata for an application bean class.
 *
 * <p>
 * The response carries property names, types and annotations only, never source. Classes owned by
 * the platform itself are refused so the surface stays limited to application code.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetBeanInfoAction implements CraftforjActionHandler<GetBeanInfoAction.Response> {

  /** Action name. */
  public static final String ACTION = "inspector.getBeanInfo";

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Response handle(JsonObject params) {
    String className = params.has("className") ? params.get("className").getAsString() : null;
    if (className == null || className.isEmpty()) {
      throw new CraftforjActionException("Missing className parameter");
    }

    Class<?> type = loadClass(className);
    if (type.getClassLoader() == null) {
      throw new CraftforjActionException("Class is not an application class: " + className);
    }

    try {
      BeanIntrospection introspection = BeanIntrospection.of(type);
      List<PropertyView> properties = new ArrayList<>();
      for (BeanIntrospection.Property property : introspection.getProperties()) {
        List<String> annotations = new ArrayList<>();
        for (Annotation annotation : property.annotations()) {
          annotations.add(annotation.toString());
        }
        properties.add(new PropertyView(property.getName(),
            property.descriptor().getPropertyType().getName(), annotations));
      }

      return new Response(className, properties);
    } catch (IntrospectionException e) {
      throw new CraftforjActionException("Failed to introspect class: " + className);
    }
  }

  private static Class<?> loadClass(String className) {
    try {
      return Class.forName(className, false, Thread.currentThread().getContextClassLoader());
    } catch (ClassNotFoundException | LinkageError e) {
      throw new CraftforjActionException("Class not found: " + className);
    }
  }

  /**
   * Response carrying bean metadata.
   */
  public static class Response {

    private final String className;
    private final List<PropertyView> properties;

    Response(String className, List<PropertyView> properties) {
      this.className = className;
      this.properties = properties;
    }

    /**
     * Gets the fully qualified bean class name.
     *
     * @return the class name
     */
    public String getClassName() {
      return className;
    }

    /**
     * Gets the bean properties.
     *
     * @return the properties
     */
    public List<PropertyView> getProperties() {
      return properties;
    }
  }

  /**
   * One bean property with its type and annotations.
   */
  public static class PropertyView {

    private final String name;
    private final String type;
    private final List<String> annotations;

    PropertyView(String name, String type, List<String> annotations) {
      this.name = name;
      this.type = type;
      this.annotations = annotations;
    }

    /**
     * Gets the property name.
     *
     * @return the name
     */
    public String getName() {
      return name;
    }

    /**
     * Gets the property type name.
     *
     * @return the type name
     */
    public String getType() {
      return type;
    }

    /**
     * Gets the annotations present on the property.
     *
     * @return the annotation strings
     */
    public List<String> getAnnotations() {
      return annotations;
    }
  }
}
