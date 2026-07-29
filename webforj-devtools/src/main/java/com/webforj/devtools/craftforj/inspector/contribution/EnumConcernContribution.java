package com.webforj.devtools.craftforj.inspector.contribution;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.SelectOption;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Base class for enum-based contributions like HasTheme and HasExpanse.
 *
 * <p>
 * This class handles the complexity of finding enum types from component's type hierarchy and
 * converting between enum names and values.
 * </p>
 *
 * <p>
 * Example:
 * </p>
 *
 * <pre>
 * public class HasThemeContribution extends EnumConcernContribution&lt;HasTheme&lt;?, ?&gt;&gt; {
 *   public HasThemeContribution() {
 *     super(HasTheme.class, "Theme", FeatureCategory.APPEARANCE);
 *     setGetter(HasTheme::getTheme);
 *     setSetter((c, v) -&gt; ((HasTheme) c).setTheme(v));
 *   }
 * }
 * </pre>
 *
 * @param <T> the concern interface type
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class EnumConcernContribution<T> implements FeatureHandler {

  private static final Logger LOGGER = Logger.getLogger(EnumConcernContribution.class.getName());

  private final Class<?> concernInterface;
  private final String propertyName;
  private final FeatureCategory category;
  private Function<T, Enum<?>> getter;
  private BiConsumer<T, Enum<?>> setter;
  private int enumTypeParameterIndex = 1;

  /**
   * Creates a new enum contribution.
   *
   * @param concernInterface the concern interface class
   * @param propertyName the property name
   * @param category the feature category
   */
  protected EnumConcernContribution(Class<?> concernInterface, String propertyName,
      FeatureCategory category) {
    this.concernInterface = concernInterface;
    this.propertyName = propertyName;
    this.category = category;
  }

  @Override
  public Class<?> getFeatureInterface() {
    return concernInterface;
  }

  @Override
  public FeatureCategory getCategory() {
    return category;
  }

  @Override
  @SuppressWarnings("unchecked")
  public Optional<FeatureProperty> get(Component component) {
    Component target = getTargetComponent(component);
    if (target == null) {
      return Optional.empty();
    }

    try {
      Enum<?> value = getter.apply((T) target);
      Class<?> enumClass = findEnumClass(target);
      List<SelectOption> options = getEnumOptions(enumClass);
      String currentValue = value != null ? toFullyQualifiedName(enumClass, value.name()) : null;

      return Optional.of(FeatureProperty.builder(propertyName, getFeatureType()).select(options)
          .value(currentValue).build());
    } catch (Exception e) {
      LOGGER.log(Level.FINE, "Failed to get enum property from component", e);

      return Optional.empty();
    }
  }

  @Override
  @SuppressWarnings({"unchecked", "rawtypes"})
  public boolean set(Component component, Object value) {
    Component target = getTargetComponent(component);
    if (target == null) {
      return false;
    }

    try {
      String valueStr = value != null ? value.toString() : null;
      if (valueStr == null || valueStr.isEmpty()) {
        return false;
      }

      // Parse fully qualified name (e.g., "com.example.Theme.PRIMARY")
      String enumName = parseEnumName(valueStr);

      Class<?> enumClass = findEnumClass(target);
      if (enumClass == null || !enumClass.isEnum()) {
        return false;
      }

      Enum<?> enumValue = Enum.valueOf((Class<Enum>) enumClass, enumName);
      setter.accept((T) target, enumValue);

      return true;
    } catch (Exception e) {
      LOGGER.log(Level.FINE, "Failed to set enum property on component", e);

      return false;
    }
  }

  /**
   * Finds the enum class for this feature from the component's type hierarchy.
   *
   * @param component the component to find the enum class for
   * @return the enum class, or null if not found
   */
  public Class<?> findEnumClass(Component component) {
    Class<?> current = component.getClass();
    while (current != null && current != Object.class) {
      for (Type type : current.getGenericInterfaces()) {
        Class<?> enumClass = extractEnumClass(type, concernInterface, enumTypeParameterIndex);
        if (enumClass != null) {
          return enumClass;
        }
      }

      Type superType = current.getGenericSuperclass();
      if (superType != null) {
        Class<?> enumClass = extractEnumClass(superType, concernInterface, enumTypeParameterIndex);
        if (enumClass != null) {
          return enumClass;
        }
      }

      current = current.getSuperclass();
    }

    return null;
  }

  /**
   * Sets the index of the enum type parameter in the feature interface.
   *
   * @param index the type parameter index (default is 1)
   */
  protected void setEnumTypeParameterIndex(int index) {
    this.enumTypeParameterIndex = index;
  }

  /**
   * Sets the getter function for the enum value.
   *
   * @param getter function to get enum value from component
   */
  protected void setGetter(Function<T, Enum<?>> getter) {
    this.getter = getter;
  }

  /**
   * Sets the setter function for the enum value.
   *
   * @param setter function to set enum value on component
   */
  protected void setSetter(BiConsumer<T, Enum<?>> setter) {
    this.setter = setter;
  }

  /**
   * Gets the available enum options with value and label.
   *
   * @param enumClass the enum class
   * @return list of select options
   */
  protected List<SelectOption> getEnumOptions(Class<?> enumClass) {
    if (enumClass != null && enumClass.isEnum()) {
      return Arrays.stream(enumClass.getEnumConstants()).map(e -> {
        String name = ((Enum<?>) e).name();
        return new SelectOption(toFullyQualifiedName(enumClass, name), name);
      }).toList();
    }

    return List.of();
  }

  /**
   * Parses the enum name from a potentially fully qualified value.
   */
  private String parseEnumName(String value) {
    int lastDot = value.lastIndexOf('.');
    return lastDot >= 0 ? value.substring(lastDot + 1) : value;
  }

  /**
   * Creates a fully qualified enum value name.
   */
  private String toFullyQualifiedName(Class<?> enumClass, String enumName) {
    if (enumClass == null) {
      return enumName;
    }
    // Use getCanonicalName() to get proper dot-separated name for nested classes
    // getName() returns $ for nested classes, but we need . for imports

    return enumClass.getCanonicalName() + "." + enumName;
  }

  private Class<?> extractEnumClass(Type type, Class<?> targetInterface, int enumIndex) {
    if (!(type instanceof ParameterizedType pt)) {
      return null;
    }

    return extractEnumClassFromParameterizedType(pt, targetInterface, enumIndex);
  }

  private Class<?> extractEnumClassFromParameterizedType(ParameterizedType pt,
      Class<?> targetInterface, int enumIndex) {
    Type rawType = pt.getRawType();
    if (!(rawType instanceof Class<?> rawClass)) {
      return null;
    }

    if (targetInterface.equals(rawClass)) {
      Class<?> enumClass = extractEnumFromTypeArgs(pt, enumIndex);
      if (enumClass != null) {
        return enumClass;
      }
    }

    if (!targetInterface.isAssignableFrom(rawClass)) {
      return null;
    }

    for (Type iface : rawClass.getGenericInterfaces()) {
      Class<?> result = extractEnumClass(iface, targetInterface, enumIndex);
      if (result != null) {
        return result;
      }
    }

    return null;
  }

  private Class<?> extractEnumFromTypeArgs(ParameterizedType pt, int enumIndex) {
    Type[] typeArgs = pt.getActualTypeArguments();
    if (typeArgs.length <= enumIndex) {
      return null;
    }
    if (!(typeArgs[enumIndex] instanceof Class<?> argClass)) {
      return null;
    }

    return argClass.isEnum() ? argClass : null;
  }
}
