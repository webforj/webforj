package com.webforj.devtools.craftforj.inspector.contribution;

import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.ScalarSourceGenerator;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Base class for layout item contributions applied through the parent layout's API.
 *
 * <p>
 * Layout containers like FlexLayout and ColumnsLayout expose per-item configuration as methods on
 * the container that take the child as an argument, e.g. {@code flexLayout.setItemGrow(1, item)} or
 * {@code columnsLayout.setSpan(item, 2)}. Contributions extending this class surface those
 * properties on the child component in the inspector, but both the live change and the generated
 * source code go through the parent's API.
 * </p>
 *
 * <p>
 * The parent instance is supplied by the client, which owns the component tree: the server-side
 * hierarchy is incomplete, so parenthood is resolved from the rendered DOM on the client and sent
 * along with property changes.
 * </p>
 *
 * @param <P> the parent layout component type
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class LayoutItemContribution<P extends Component> implements FeatureHandler {

  private static final Logger LOGGER = Logger.getLogger(LayoutItemContribution.class.getName());

  private final Class<P> parentClass;
  private final Class<?> concernInterface;
  private final String propertyName;
  private final String methodName;
  private final SourceChange.ItemPosition itemPosition;
  private UnaryOperator<FeatureProperty.Builder> builderConfig = FeatureProperty.Builder::text;
  private Function<Component, Object> getter;
  private ItemSetter<P> setter;
  private BiConsumer<P, Component> resetter;

  /**
   * Setter invoked on the parent layout with the child item and the new value.
   *
   * @param <P> the parent layout component type
   */
  @FunctionalInterface
  public interface ItemSetter<P> {

    /**
     * Applies the value to the item through the parent layout.
     *
     * @param parent the parent layout
     * @param item the child item
     * @param value the new value
     */
    void set(P parent, Component item, Object value);
  }

  /**
   * A source expression for a property value together with its required imports.
   *
   * @param expression the JavaParser expression for the value argument
   * @param imports the fully qualified names to import
   */
  public record ValueExpression(Expression expression, List<String> imports) {

    /**
     * Creates a value expression without imports.
     *
     * @param expression the JavaParser expression
     * @return the value expression
     */
    public static ValueExpression of(Expression expression) {
      return new ValueExpression(expression, List.of());
    }
  }

  /**
   * Creates a new layout item contribution.
   *
   * @param parentClass the parent layout class whose API applies the property
   * @param concernInterface the interface the child must implement to satisfy the parent API's type
   *        bound (e.g. HasStyle, HasAttribute)
   * @param propertyName the property display name
   * @param methodName the parent API method name used for live changes and source generation
   * @param itemPosition the position of the item argument in the parent API method
   */
  protected LayoutItemContribution(Class<P> parentClass, Class<?> concernInterface,
      String propertyName, String methodName, SourceChange.ItemPosition itemPosition) {
    this.parentClass = parentClass;
    this.concernInterface = concernInterface;
    this.propertyName = propertyName;
    this.methodName = methodName;
    this.itemPosition = itemPosition;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<?> getFeatureInterface() {
    return concernInterface;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FeatureCategory getCategory() {
    return FeatureCategory.LAYOUT;
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Layout items never fall back to the bound component: the generated source passes the child
   * itself as an argument to the parent API, so the child's declared type must implement the
   * concern directly.
   * </p>
   */
  @Override
  public boolean supports(Component component, boolean allowBoundFallback) {
    return concernInterface.isInstance(component);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean supportsParent(String parentType) {
    if (parentType == null || parentType.isEmpty()) {
      return false;
    }

    if (parentClass.getName().equals(parentType)) {
      return true;
    }

    return loadClass(parentType).map(parentClass::isAssignableFrom).orElse(false);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isParentScoped() {
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<FeatureProperty> get(Component component) {
    if (!concernInterface.isInstance(component) || getter == null) {
      return Optional.empty();
    }

    try {
      Object value = getter.apply(component);
      FeatureProperty.Builder builder = FeatureProperty.builder(propertyName, getFeatureType());
      builder = builderConfig.apply(builder);
      builder.value(value).parentScoped(true);

      return Optional.of(builder.build());
    } catch (Exception e) {
      LOGGER.log(Level.FINE, "Failed to get layout item property from component", e);

      return Optional.empty();
    }
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Always fails: layout item properties require the parent layout, use
   * {@link #set(Component, Component, Object)}.
   * </p>
   */
  @Override
  public boolean set(Component component, Object value) {
    LOGGER.log(Level.FINE, "Layout item property requires a parent component");
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean set(Component component, Component parent, Object value) {
    P layout = resolveParent(parent);
    if (layout == null || !concernInterface.isInstance(component) || setter == null) {
      return false;
    }

    try {
      if (isEmptyValue(value) && resetter != null) {
        resetter.accept(layout, component);
      } else {
        setter.set(layout, component, value);
      }

      return true;
    } catch (Exception e) {
      LOGGER.log(Level.FINE, "Failed to set layout item property", e);

      return false;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getSourceMethodName(String propertyName) {
    return methodName;
  }

  /**
   * Builds the parent-scoped source change for this property.
   *
   * <p>
   * The returned change carries the full argument list with the item variable already positioned
   * ({@code setSpan(item, 2)} or {@code setItemGrow(1, item)}), plus the item reference metadata
   * used to match existing calls. Returns null when the value is empty, which means the existing
   * call must be removed instead.
   * </p>
   *
   * @param property the property carrying the new value and type metadata
   * @param itemVariableName the child's variable name in the parent's source file
   * @return the source change, or null when the value is empty (reset to default)
   */
  public SourceChange buildItemSourceChange(FeatureProperty property, String itemVariableName) {
    if (isEmptyValue(property.getValue())) {
      return null;
    }

    ValueExpression value = toSourceExpression(property);
    if (value == null) {
      return null;
    }

    List<Expression> arguments = new ArrayList<>();
    if (itemPosition == SourceChange.ItemPosition.FIRST) {
      arguments.add(new NameExpr(itemVariableName));
      arguments.add(value.expression());
    } else {
      arguments.add(value.expression());
      arguments.add(new NameExpr(itemVariableName));
    }

    SourceChange.Builder builder = SourceChange.builder().methodCall(methodName, arguments)
        .itemRef(itemVariableName, itemPosition);
    value.imports().forEach(builder::addImport);

    return builder.build();
  }

  /**
   * Gets the argument count of a devtools-generated item call for this property.
   *
   * <p>
   * Used to match existing calls by arity so method overloads (breakpoint variants, container-level
   * overloads of the same name) are never touched.
   * </p>
   *
   * @return the argument count (item plus value arguments)
   */
  public int getItemCallArgumentCount() {
    return 2;
  }

  /**
   * Gets the position of the item argument in the parent API method.
   *
   * @return the item position
   */
  public SourceChange.ItemPosition getItemPosition() {
    return itemPosition;
  }

  /**
   * Converts the property value to a source expression.
   *
   * <p>
   * Called only for non-empty values. Use {@link #scalarExpression(FeatureProperty)} or
   * {@link #enumExpression(FeatureProperty, Class)} for the common cases.
   * </p>
   *
   * @param property the property carrying the new value and type metadata
   * @return the value expression, or null to treat the value as empty
   */
  protected abstract ValueExpression toSourceExpression(FeatureProperty property);

  /**
   * Builds a scalar value expression using the property's Java type.
   *
   * @param property the property
   * @return the value expression
   */
  protected ValueExpression scalarExpression(FeatureProperty property) {
    return ValueExpression
        .of(ScalarSourceGenerator.toExpression(property.getValue(), property.getJavaType()));
  }

  /**
   * Builds an enum constant expression from a fully qualified enum value.
   *
   * @param property the property whose value is a fully qualified enum constant name
   * @param enumClass the enum class the constant belongs to
   * @return the value expression with the enum import
   */
  protected ValueExpression enumExpression(FeatureProperty property, Class<?> enumClass) {
    String value = String.valueOf(property.getValue());
    String constantName = value.substring(value.lastIndexOf('.') + 1);

    boolean known = false;
    for (Object constant : enumClass.getEnumConstants()) {
      if (((Enum<?>) constant).name().equals(constantName)) {
        known = true;
        break;
      }
    }

    if (!known) {
      throw new SourceModificationException(
          "Property '" + propertyName + "': invalid enum value '" + constantName + "'");
    }

    return new ValueExpression(
        new FieldAccessExpr(new NameExpr(enumClass.getSimpleName()), constantName),
        List.of(enumClass.getCanonicalName()));
  }

  /**
   * Checks whether a value means "reset to default".
   *
   * @param value the value to check
   * @return true when the value is null or an empty string
   */
  protected static boolean isEmptyValue(Object value) {
    return value == null || (value instanceof String s && s.isEmpty());
  }

  /**
   * Sets the builder configuration function controlling the editor type.
   *
   * @param config the builder configuration function
   */
  protected void setBuilderConfig(UnaryOperator<FeatureProperty.Builder> config) {
    this.builderConfig = config;
  }

  /**
   * Sets the getter reading the current value from the child item.
   *
   * <p>
   * Parent layouts apply item settings as styles or attributes on the child, so the current value
   * is always readable from the child itself without the parent instance.
   * </p>
   *
   * @param getter function to read the value from the child
   */
  protected void setGetter(Function<Component, Object> getter) {
    this.getter = getter;
  }

  /**
   * Sets the setter applying the value through the parent layout's API.
   *
   * @param setter the item setter
   */
  protected void setSetter(ItemSetter<P> setter) {
    this.setter = setter;
  }

  /**
   * Sets the resetter clearing the value when the property is emptied.
   *
   * <p>
   * Optional: when absent, empty values are passed to the setter which must handle them.
   * </p>
   *
   * @param resetter the resetter invoked with the parent and the child
   */
  protected void setResetter(BiConsumer<P, Component> resetter) {
    this.resetter = resetter;
  }

  private P resolveParent(Component parent) {
    if (parentClass.isInstance(parent)) {
      return parentClass.cast(parent);
    }

    if (parent instanceof Composite) {
      Component bound = ComponentUtil.getBoundComponent(parent);
      if (parentClass.isInstance(bound)) {
        return parentClass.cast(bound);
      }
    }

    return null;
  }

  private Optional<Class<?>> loadClass(String className) {
    Class<?> result = tryLoad(className, Thread.currentThread().getContextClassLoader());
    if (result == null) {
      result = tryLoad(className, parentClass.getClassLoader());
    }

    return Optional.ofNullable(result);
  }

  private Class<?> tryLoad(String className, ClassLoader loader) {
    if (loader == null) {
      return null;
    }

    try {
      return Class.forName(className, false, loader);
    } catch (ClassNotFoundException | LinkageError e) {
      return null;
    }
  }
}
