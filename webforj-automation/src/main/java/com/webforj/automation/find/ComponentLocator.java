package com.webforj.automation.find;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasLabel;
import com.webforj.concern.HasPlaceholder;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasTheme;
import com.webforj.data.concern.ValueAware;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BooleanSupplier;
import java.util.function.Predicate;

/**
 * Finds components in the tree of the context under test.
 *
 * <p>
 * A locator starts from {@link #of(Class)} with the component type to find, narrows the result with
 * chainable criteria and ends with a terminal operation such as {@link #locate()} or
 * {@link #all()}.
 * </p>
 *
 * <pre>{@code
 * Button save = ComponentLocator.of(Button.class).byText("Save").locate();
 * List<TextField> fields = ComponentLocator.of(TextField.class).all();
 * }</pre>
 *
 * @param <T> the component type to find
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ComponentLocator<T extends Component> {
  private final Class<T> type;
  private final List<Predicate<T>> filters = new ArrayList<>();
  private Component root = null;

  private ComponentLocator(Class<T> type) {
    this.type = type;
  }

  /**
   * Creates a locator for components of the given type.
   *
   * <p>
   * The locator searches the whole component tree of the context, every frame and everything below
   * it.
   * </p>
   *
   * @param <T> the component type to find
   * @param type the component type to find
   *
   * @return the locator
   */
  public static <T extends Component> ComponentLocator<T> of(Class<T> type) {
    Objects.requireNonNull(type, "The component type cannot be null");

    return new ComponentLocator<>(type);
  }

  /**
   * Restricts the search to the tree below the given component.
   *
   * @param root the component to search below
   *
   * @return the locator itself
   */
  public ComponentLocator<T> within(Component root) {
    Objects.requireNonNull(root, "The root component cannot be null");
    this.root = root;

    return this;
  }

  /**
   * Keeps only components whose name equals the given name.
   *
   * @param name the component name to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byName(String name) {
    filters.add(component -> Objects.equals(component.getName(), name));

    return this;
  }

  /**
   * Keeps only components whose given attribute equals the given value.
   *
   * <p>
   * Attributes are the ones the application assigns through {@code setAttribute(...)}, for example
   * {@code byAttribute("id", "save")} for the id a stylesheet or a browser test targets. The
   * generated identifiers of {@link Component#getComponentId()} and
   * {@link Component#getClientComponentId()} are runtime bookkeeping, cannot be known when the test
   * is written, and take no part in this criteria. Components without attribute support never
   * match.
   * </p>
   *
   * @param attribute the attribute name
   * @param value the attribute value to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byAttribute(String attribute, String value) {
    Objects.requireNonNull(attribute, "The attribute name cannot be null");
    filters.add(component -> component instanceof HasAttribute<?> hasAttribute
        && Objects.equals(hasAttribute.getAttribute(attribute), value));

    return this;
  }

  /**
   * Keeps only components that have the given attribute, whatever its value.
   *
   * @param attribute the attribute name
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byAttribute(String attribute) {
    Objects.requireNonNull(attribute, "The attribute name cannot be null");
    filters.add(component -> component instanceof HasAttribute<?> hasAttribute
        && isSatisfied(() -> hasAttribute.hasAttribute(attribute)));

    return this;
  }

  /**
   * Keeps only components whose text equals the given text.
   *
   * @param text the text to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byText(String text) {
    filters.add(component -> component instanceof HasText hasText
        && Objects.equals(hasText.getText(), text));

    return this;
  }

  /**
   * Keeps only components whose text contains the given text.
   *
   * @param text the text to look for
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byTextContaining(String text) {
    filters.add(component -> component instanceof HasText hasText && hasText.getText() != null
        && hasText.getText().contains(text));

    return this;
  }

  /**
   * Keeps only components whose label equals the given label.
   *
   * @param label the label to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byLabel(String label) {
    filters.add(component -> component instanceof HasLabel<?> hasLabel
        && isSatisfied(() -> Objects.equals(hasLabel.getLabel(), label)));

    return this;
  }

  /**
   * Keeps only components whose placeholder equals the given placeholder.
   *
   * @param placeholder the placeholder to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byPlaceholder(String placeholder) {
    filters.add(component -> component instanceof HasPlaceholder<?> hasPlaceholder
        && isSatisfied(() -> Objects.equals(hasPlaceholder.getPlaceholder(), placeholder)));

    return this;
  }

  /**
   * Keeps only components whose value equals the given value.
   *
   * @param value the value to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byValue(Object value) {
    filters.add(component -> component instanceof ValueAware<?, ?> valueAware
        && Objects.equals(valueAware.getValue(), value));

    return this;
  }

  /**
   * Keeps only components whose theme equals the given theme.
   *
   * @param theme the theme enum constant of the component, for example {@code ButtonTheme.PRIMARY}
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byTheme(Object theme) {
    filters.add(component -> component instanceof HasTheme<?, ?> hasTheme
        && Objects.equals(hasTheme.getTheme(), theme));

    return this;
  }

  /**
   * Keeps only components whose expanse equals the given expanse.
   *
   * @param expanse the expanse enum constant of the component, for example {@code Expanse.LARGE}
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byExpanse(Object expanse) {
    filters.add(component -> component instanceof HasExpanse<?, ?> hasExpanse
        && Objects.equals(hasExpanse.getExpanse(), expanse));

    return this;
  }

  /**
   * Keeps only components whose given style property equals the given value.
   *
   * @param property the style property name
   * @param value the style value to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byStyle(String property, String value) {
    Objects.requireNonNull(property, "The style property cannot be null");
    filters.add(component -> component instanceof HasStyle<?> hasStyle
        && isSatisfied(() -> Objects.equals(hasStyle.getStyle(property), value)));

    return this;
  }

  /**
   * Keeps only components that have the given style property set, whatever its value.
   *
   * @param property the style property name
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byStyle(String property) {
    Objects.requireNonNull(property, "The style property cannot be null");
    filters.add(component -> component instanceof HasStyle<?> hasStyle && isSatisfied(() -> {
      String value = hasStyle.getStyle(property);
      return value != null && !value.isEmpty();
    }));

    return this;
  }

  /**
   * Keeps only components that have the given class name.
   *
   * @param className the class name to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> byClassName(String className) {
    Objects.requireNonNull(className, "The class name cannot be null");
    filters.add(component -> component instanceof HasClassName<?> hasClassName
        && isSatisfied(() -> hasClassName.getClassNameList().contains(className)));

    return this;
  }

  /**
   * Keeps only components below which the given locator finds at least one component.
   *
   * <p>
   * The candidate itself does not count, only the components below it.
   * </p>
   *
   * @param inner the locator to run below every candidate
   *
   * @return the locator itself
   */
  public ComponentLocator<T> has(ComponentLocator<?> inner) {
    Objects.requireNonNull(inner, "The inner locator cannot be null");
    filters.add(component -> {
      for (Component child : ComponentTree.getChildren(component)) {
        if (!inner.find(child).isEmpty()) {
          return true;
        }
      }

      return false;
    });

    return this;
  }

  /**
   * Keeps only components matching the given predicate.
   *
   * @param predicate the predicate to match
   *
   * @return the locator itself
   */
  public ComponentLocator<T> filter(Predicate<T> predicate) {
    Objects.requireNonNull(predicate, "The predicate cannot be null");
    filters.add(predicate);

    return this;
  }

  /**
   * Returns the only component matching the criteria.
   *
   * @return the matching component
   * @throws IllegalStateException if no component or more than one component matches
   */
  public T locate() {
    List<T> results = all();
    if (results.size() != 1) {
      throw new IllegalStateException("Expected exactly one " + type.getSimpleName() + " but found "
          + results.size() + " in the tree:" + System.lineSeparator() + printScope());
    }

    return results.get(0);
  }

  /**
   * Returns the first component matching the criteria.
   *
   * @return the first matching component, or an empty optional when nothing matches
   */
  public Optional<T> first() {
    List<T> results = all();

    return results.isEmpty() ? Optional.empty() : Optional.of(results.get(0));
  }

  /**
   * Returns the last component matching the criteria.
   *
   * @return the last matching component, or an empty optional when nothing matches
   */
  public Optional<T> last() {
    List<T> results = all();

    return results.isEmpty() ? Optional.empty() : Optional.of(results.getLast());
  }

  /**
   * Returns all components matching the criteria.
   *
   * @return the matching components in tree order, never null
   */
  public List<T> all() {
    List<T> results = new ArrayList<>();
    for (Component scopeRoot : scopeRoots()) {
      results.addAll(find(scopeRoot));
    }

    return results;
  }

  /**
   * Returns whether any component matches the criteria.
   *
   * @return true when at least one component matches
   */
  public boolean isPresent() {
    return !all().isEmpty();
  }

  /**
   * Returns the number of components matching the criteria.
   *
   * @return the number of matches
   */
  public int count() {
    return all().size();
  }

  List<T> find(Component scopeRoot) {
    List<T> results = new ArrayList<>();
    ComponentTree.walk(scopeRoot, component -> {
      // a composite stands for the chain it binds, each component of the chain can be located
      for (Component current = component; current != null; current =
          current instanceof Composite<?> ? ComponentUtil.getBoundComponent(current) : null) {
        if (type.isInstance(current)) {
          T candidate = type.cast(current);
          if (matches(candidate)) {
            results.add(candidate);
          }
        }
      }
    });

    return results;
  }

  private static boolean isSatisfied(BooleanSupplier condition) {
    try {
      return condition.getAsBoolean();
    } catch (UnsupportedOperationException e) {
      return false;
    }
  }

  private boolean matches(T candidate) {
    for (Predicate<T> filter : filters) {
      if (!filter.test(candidate)) {
        return false;
      }
    }

    return true;
  }

  private List<Component> scopeRoots() {
    return root != null ? List.of(root) : ComponentTree.getRoots();
  }

  private String printScope() {
    return root != null ? ComponentTree.render(root) : ComponentTree.renderAll();
  }
}
