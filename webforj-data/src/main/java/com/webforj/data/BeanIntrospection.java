package com.webforj.data;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.UnaryOperator;

/**
 * Cached, annotation aware introspection of a JavaBean class.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BeanIntrospection {

  private static final ClassValue<CacheEntry> CACHE = new ClassValue<>() {
    @Override
    protected CacheEntry computeValue(Class<?> type) {
      try {
        return new CacheEntry(compute(type), null);
      } catch (IntrospectionException e) {
        return new CacheEntry(null, e);
      }
    }
  };

  private final Class<?> beanClass;
  private final List<Property> properties;
  private final Map<String, Property> byName;

  private BeanIntrospection(Class<?> beanClass, List<Property> properties) {
    this.beanClass = beanClass;
    this.properties = Collections.unmodifiableList(properties);
    Map<String, Property> map = new LinkedHashMap<>();
    for (Property p : properties) {
      map.put(p.getName(), p);
    }
    this.byName = Collections.unmodifiableMap(map);
  }

  /**
   * Returns the introspection view for a bean class. Skips properties that have no read method.
   * Cached per class.
   *
   * @param beanClass the bean class.
   *
   * @return the introspection view.
   *
   * @throws IntrospectionException if the JavaBeans introspector fails.
   */
  public static BeanIntrospection of(Class<?> beanClass) throws IntrospectionException {
    CacheEntry entry = CACHE.get(beanClass);
    if (entry.error() != null) {
      throw entry.error();
    }

    return entry.result();
  }

  /**
   * Returns the property descriptor for the given property name or dotted path. For a path, the
   * returned descriptor is the leaf class's descriptor.
   *
   * @param propertyName the property name or path.
   *
   * @return the property descriptor.
   *
   * @throws IllegalArgumentException if the name or path does not resolve to a property.
   */
  public PropertyDescriptor getPropertyDescriptor(String propertyName) {
    Property p = getProperty(propertyName);

    if (p == null) {
      throw new IllegalArgumentException(
          "Property '" + propertyName + "' not found in bean class '" + beanClass.getName()
              + "'. Verify the property name and its declaration in the bean class.");
    }

    return p.descriptor();
  }

  /**
   * Returns the property for the given name or dotted path, or {@code null} if no such property
   * resolves on the bean.
   *
   * @param propertyName the property name or path.
   *
   * @return the property, or {@code null}.
   */
  public Property getProperty(String propertyName) {
    if (propertyName == null || propertyName.isEmpty()) {
      return null;
    }

    int dot = propertyName.indexOf('.');
    if (dot < 0) {
      return byName.get(propertyName);
    }

    String[] segments = propertyName.split("\\.");
    if (segments.length == 0) {
      return null;
    }

    List<PropertyDescriptor> chain = new ArrayList<>(segments.length);
    Property current = byName.get(segments[0]);
    if (current == null) {
      return null;
    }

    chain.add(current.descriptor());

    for (int i = 1; i < segments.length; i++) {
      Class<?> currentClass = current.getPropertyType();
      BeanIntrospection sub;
      try {
        sub = BeanIntrospection.of(currentClass);
      } catch (IntrospectionException e) {
        return null;
      }

      current = sub.byName.get(segments[i]);
      if (current == null) {
        return null;
      }

      chain.add(current.descriptor());
    }

    UnaryOperator<Object> reader = composedReader(chain);
    BiConsumer<Object, Object> writer = composedWriter(chain);

    return new Property(current.descriptor(), current.annotations(), propertyName, reader, writer);
  }

  /**
   * Returns every readable property in declaration order.
   *
   * @return the properties.
   */
  public List<Property> getProperties() {
    return properties;
  }

  /**
   * Returns the bean class.
   *
   * @return the bean class.
   */
  public Class<?> getBeanClass() {
    return beanClass;
  }

  private static UnaryOperator<Object> composedReader(List<PropertyDescriptor> chain) {
    return bean -> {
      Object current = bean;
      for (PropertyDescriptor pd : chain) {
        if (current == null) {
          return null;
        }

        try {
          current = pd.getReadMethod().invoke(current);
        } catch (IllegalAccessException | InvocationTargetException e) {
          throw new MutatorException("Failed to invoke getter for '" + pd.getName() + "'.", e);
        }
      }

      return current;
    };
  }

  private static BiConsumer<Object, Object> composedWriter(List<PropertyDescriptor> chain) {
    return (bean, value) -> {
      if (bean == null) {
        throw new MutatorException(
            "Cannot write nested property '" + chain.get(chain.size() - 1).getName()
                + "' on a null bean. Reads tolerate a null bean and return null. Writes do not.");
      }

      Object current = bean;
      for (int i = 0; i < chain.size() - 1; i++) {
        current = ensureIntermediate(current, chain.get(i));
      }

      writeLeaf(current, chain.get(chain.size() - 1), value);
    };
  }

  private static Object ensureIntermediate(Object owner, PropertyDescriptor pd) {
    Object next = invokeGetter(owner, pd);
    if (next != null) {
      return next;
    }

    if (pd.getWriteMethod() == null) {
      throw new MutatorException("Cannot instantiate intermediate '" + pd.getName()
          + "': no setter to install the new instance.");
    }

    next = instantiate(pd);
    invokeSetter(owner, pd, next);

    return next;
  }

  private static Object invokeGetter(Object owner, PropertyDescriptor pd) {
    try {
      return pd.getReadMethod().invoke(owner);
    } catch (IllegalAccessException | InvocationTargetException e) {
      throw new MutatorException("Failed to invoke getter for '" + pd.getName() + "'.", e);
    }
  }

  private static void invokeSetter(Object owner, PropertyDescriptor pd, Object value) {
    try {
      pd.getWriteMethod().invoke(owner, value);
    } catch (IllegalAccessException | InvocationTargetException e) {
      throw new MutatorException("Failed to invoke setter for '" + pd.getName() + "'.", e);
    }
  }

  private static Object instantiate(PropertyDescriptor pd) {
    try {
      return pd.getPropertyType().getDeclaredConstructor().newInstance();
    } catch (NoSuchMethodException | IllegalAccessException | InstantiationException
        | InvocationTargetException e) {
      throw new MutatorException(
          "Failed to instantiate intermediate '" + pd.getPropertyType().getName()
              + "' for path segment '" + pd.getName() + "'. Provide a no-arg constructor.",
          e);
    }
  }

  private static void writeLeaf(Object owner, PropertyDescriptor leaf, Object value) {
    if (leaf.getWriteMethod() == null) {
      throw new MutatorException("Property '" + leaf.getName() + "' has no setter.");
    }

    invokeSetter(owner, leaf, value);
  }

  private static BeanIntrospection compute(Class<?> beanClass) throws IntrospectionException {
    try {
      BeanInfo info = Introspector.getBeanInfo(beanClass, Object.class);
      Map<Class<?>, Map<String, Field>> fieldCache = new LinkedHashMap<>();
      Map<String, Integer> declarationOrder = collectDeclarationOrder(beanClass);
      List<Property> result = new ArrayList<>();
      for (PropertyDescriptor pd : info.getPropertyDescriptors()) {
        if (pd.getReadMethod() == null) {
          continue;
        }
        List<Annotation> annotations = new ArrayList<>();
        Field field = findField(beanClass, pd.getName(), fieldCache);
        if (field != null) {
          Collections.addAll(annotations, field.getAnnotations());
        }
        Collections.addAll(annotations, pd.getReadMethod().getAnnotations());
        if (pd.getWriteMethod() != null) {
          Collections.addAll(annotations, pd.getWriteMethod().getAnnotations());
        }
        result.add(new Property(pd, annotations));
      }
      result.sort((a, b) -> {
        Integer ai = declarationOrder.get(a.getName());
        Integer bi = declarationOrder.get(b.getName());
        if (ai == null && bi == null) {
          return a.getName().compareTo(b.getName());
        }
        if (ai == null) {
          return 1;
        }
        if (bi == null) {
          return -1;
        }

        return Integer.compare(ai, bi);
      });

      return new BeanIntrospection(beanClass, result);
    } finally {
      Introspector.flushFromCaches(beanClass);
    }
  }

  private static Map<String, Integer> collectDeclarationOrder(Class<?> beanClass) {
    List<Class<?>> chain = new ArrayList<>();
    for (Class<?> c = beanClass; c != null && c != Object.class; c = c.getSuperclass()) {
      chain.add(c);
    }

    Collections.reverse(chain);
    Map<String, Integer> order = new LinkedHashMap<>();
    int index = 0;

    for (Class<?> c : chain) {
      for (Field f : c.getDeclaredFields()) {
        if (!order.containsKey(f.getName())) {
          order.put(f.getName(), index++);
        }
      }
    }

    return order;
  }

  private static Field findField(Class<?> beanClass, String name,
      Map<Class<?>, Map<String, Field>> cache) {
    for (Class<?> c = beanClass; c != null && c != Object.class; c = c.getSuperclass()) {
      Map<String, Field> declared =
          cache.computeIfAbsent(c, BeanIntrospection::declaredFieldsByName);
      Field f = declared.get(name);
      if (f != null) {
        return f;
      }
    }

    return null;
  }

  private static Map<String, Field> declaredFieldsByName(Class<?> c) {
    Map<String, Field> map = new LinkedHashMap<>();
    for (Field f : c.getDeclaredFields()) {
      map.put(f.getName(), f);
    }

    return map;
  }

  private static Class<?> box(Class<?> type) {
    if (!type.isPrimitive()) {
      return type;
    }
    if (type == int.class) {
      return Integer.class;
    }
    if (type == long.class) {
      return Long.class;
    }
    if (type == double.class) {
      return Double.class;
    }
    if (type == float.class) {
      return Float.class;
    }
    if (type == boolean.class) {
      return Boolean.class;
    }
    if (type == short.class) {
      return Short.class;
    }
    if (type == byte.class) {
      return Byte.class;
    }
    if (type == char.class) {
      return Character.class;
    }

    return type;
  }

  /**
   * One property reflective view.
   *
   * @param descriptor the leaf JavaBeans descriptor.
   * @param annotations field, getter, and setter annotations of the leaf.
   * @param pathName the dotted path used to obtain this property, or {@code null} when the property
   *        is a flat property of the root bean.
   * @param reader function reading the value from a parent bean instance. Returns {@code null} if
   *        any intermediate is {@code null}.
   * @param writer function writing a value into a parent bean instance. Instantiates {@code null}
   *        intermediates via their no arg constructor before continuing.
   */
  public record Property(PropertyDescriptor descriptor, List<Annotation> annotations,
      String pathName, UnaryOperator<Object> reader, BiConsumer<Object, Object> writer) {

    public Property {
      annotations = List.copyOf(annotations);
    }

    /**
     * Constructs a flat property. Reader and writer are derived from the descriptor.
     *
     * @param descriptor the descriptor.
     * @param annotations the annotations.
     */
    public Property(PropertyDescriptor descriptor, List<Annotation> annotations) {
      this(descriptor, annotations, null, flatReader(descriptor), flatWriter(descriptor));
    }

    /**
     * Returns the property name. For a dotted path lookup, returns the full path. For a flat
     * property, returns the descriptor's name.
     *
     * @return the property name.
     */
    public String getName() {
      return pathName != null ? pathName : descriptor.getName();
    }

    /**
     * Returns the property type, boxed to its wrapper for primitives.
     *
     * @return the property type.
     */
    public Class<?> getPropertyType() {
      return box(descriptor.getPropertyType());
    }

    /**
     * Reads the value of this property from the given parent bean. Returns {@code null} if any
     * intermediate along a dotted path is {@code null}.
     *
     * @param bean the parent bean instance.
     *
     * @return the leaf value, or {@code null}.
     */
    public Object read(Object bean) {
      return reader.apply(bean);
    }

    /**
     * Writes the value of this property into the given parent bean. Instantiates {@code null}
     * intermediates along a dotted path via their no arg constructor.
     *
     * @param bean the parent bean instance.
     * @param value the value to write.
     */
    public void write(Object bean, Object value) {
      writer.accept(bean, value);
    }

    private static UnaryOperator<Object> flatReader(PropertyDescriptor descriptor) {
      return bean -> {
        if (bean == null) {
          return null;
        }
        try {
          return descriptor.getReadMethod().invoke(bean);
        } catch (IllegalAccessException | InvocationTargetException e) {
          throw new MutatorException("Failed to invoke getter for '" + descriptor.getName() + "'.",
              e);
        }
      };
    }

    private static BiConsumer<Object, Object> flatWriter(PropertyDescriptor descriptor) {
      return (bean, value) -> {
        if (bean == null) {
          throw new MutatorException("Cannot write property '" + descriptor.getName()
              + "' on a null bean. Reads tolerate a null bean and return null. Writes do not.");
        }
        if (descriptor.getWriteMethod() == null) {
          throw new MutatorException("Property '" + descriptor.getName() + "' has no setter.");
        }
        try {
          descriptor.getWriteMethod().invoke(bean, value);
        } catch (IllegalAccessException | InvocationTargetException e) {
          throw new MutatorException("Failed to invoke setter for '" + descriptor.getName() + "'.",
              e);
        }
      };
    }
  }

  private record CacheEntry(BeanIntrospection result, IntrospectionException error) {}
}
