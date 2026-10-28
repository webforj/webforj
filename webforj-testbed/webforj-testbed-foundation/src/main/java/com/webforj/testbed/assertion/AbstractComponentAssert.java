package com.webforj.testbed.assertion;

import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.Component;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClientValidation;
import com.webforj.concern.HasComponents;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasHelperText;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasLabel;
import com.webforj.concern.HasPlaceholder;
import com.webforj.concern.HasProperty;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasTheme;
import com.webforj.concern.HasTooltip;
import com.webforj.concern.HasVisibility;
import com.webforj.data.concern.ReadOnlyAware;
import com.webforj.data.concern.RequiredAware;
import com.webforj.data.concern.ValueAware;
import com.webforj.data.repository.HasRepository;
import com.webforj.data.selection.IndexMultiSelectionAware;
import com.webforj.data.selection.IndexSingleSelectionAware;
import com.webforj.data.selection.ItemMultiSelectionAware;
import com.webforj.data.selection.ItemSingleSelectionAware;
import com.webforj.data.selection.KeyMultiSelectionAware;
import com.webforj.data.selection.KeySingleSelectionAware;
import com.webforj.testbed.snapshot.SnapshotStore;
import java.util.List;
import java.util.Objects;

/**
 * The base for assertions on webforJ components.
 *
 * <p>
 * Every assertion reads the component through the webforJ contract that publishes the state it
 * checks, so a component of the application and a component of webforJ are asserted the same way.
 * An assertion on a component that does not implement the contract fails naming it.
 * </p>
 *
 * @param <S> the assertion type returned by every assertion for chaining
 * @param <A> the component type under test
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public abstract class AbstractComponentAssert<S extends AbstractComponentAssert<S, A>, //
    A extends Component> {

  /**
   * The component under test.
   */
  protected final A actual;

  /**
   * The assertion itself, returned by every assertion for chaining.
   */
  protected final S myself;

  /**
   * Creates a new assertion.
   *
   * @param actual the component under test
   * @param selfType the assertion type
   */
  @SuppressWarnings("unchecked")
  protected AbstractComponentAssert(A actual, Class<?> selfType) {
    this.actual = actual;
    this.myself = (S) selfType.cast(this);
  }

  /**
   * Asserts that there is a component to assert on.
   *
   * @return the assertion itself
   */
  public S isNotNull() {
    if (actual == null) {
      failWithMessage("Expected a component but there is none.");
    }

    return myself;
  }

  /**
   * Asserts that the component is attached.
   *
   * @return the assertion itself
   */
  public S isAttached() {
    isNotNull();
    if (!actual.isAttached()) {
      failWithMessage("Expected %s to be attached but it is not.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is not attached.
   *
   * @return the assertion itself
   */
  public S isNotAttached() {
    isNotNull();
    if (actual.isAttached()) {
      failWithMessage("Expected %s to not be attached but it is.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is destroyed.
   *
   * @return the assertion itself
   */
  public S isDestroyed() {
    isNotNull();
    if (!actual.isDestroyed()) {
      failWithMessage("Expected %s to be destroyed but it is not.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is visible.
   *
   * @return the assertion itself
   */
  public S isVisible() {
    if (!capability(HasVisibility.class).isVisible()) {
      failWithMessage("Expected %s to be visible but it is hidden.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is hidden.
   *
   * @return the assertion itself
   */
  public S isHidden() {
    if (capability(HasVisibility.class).isVisible()) {
      failWithMessage("Expected %s to be hidden but it is visible.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is enabled.
   *
   * @return the assertion itself
   */
  public S isEnabled() {
    if (!capability(HasEnablement.class).isEnabled()) {
      failWithMessage("Expected %s to be enabled but it is disabled.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is disabled.
   *
   * @return the assertion itself
   */
  public S isDisabled() {
    if (capability(HasEnablement.class).isEnabled()) {
      failWithMessage("Expected %s to be disabled but it is enabled.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is read only.
   *
   * @return the assertion itself
   */
  public S isReadOnly() {
    if (!capability(ReadOnlyAware.class).isReadOnly()) {
      failWithMessage("Expected %s to be read only but it is editable.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is editable, not read only.
   *
   * @return the assertion itself
   */
  public S isEditable() {
    if (capability(ReadOnlyAware.class).isReadOnly()) {
      failWithMessage("Expected %s to be editable but it is read only.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component requires a value.
   *
   * @return the assertion itself
   */
  public S isRequired() {
    if (!capability(RequiredAware.class).isRequired()) {
      failWithMessage("Expected %s to be required but it is optional.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component does not require a value.
   *
   * @return the assertion itself
   */
  public S isOptional() {
    if (capability(RequiredAware.class).isRequired()) {
      failWithMessage("Expected %s to be optional but it is required.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component has the focus.
   *
   * @return the assertion itself
   */
  public S isFocused() {
    if (!capability(HasFocusStatus.class).hasFocus()) {
      failWithMessage("Expected %s to have the focus but it does not.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component does not have the focus.
   *
   * @return the assertion itself
   */
  public S isNotFocused() {
    if (capability(HasFocusStatus.class).hasFocus()) {
      failWithMessage("Expected %s to not have the focus but it does.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is marked invalid.
   *
   * @return the assertion itself
   */
  public S isInvalid() {
    if (!capability(HasClientValidation.class).isInvalid()) {
      failWithMessage("Expected %s to be invalid but it is valid.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component is not marked invalid.
   *
   * @return the assertion itself
   */
  public S isValid() {
    if (capability(HasClientValidation.class).isInvalid()) {
      failWithMessage("Expected %s to be valid but it is invalid.", componentName());
    }

    return myself;
  }

  /**
   * Asserts that the component shows the given invalid message.
   *
   * @param expected the expected message
   * @return the assertion itself
   */
  public S hasInvalidMessage(String expected) {
    String message = capability(HasClientValidation.class).getInvalidMessage();
    if (!Objects.equals(message, expected)) {
      failWithMessage("Expected %s to have the invalid message <%s> but it has <%s>.",
          componentName(), expected, message);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given text.
   *
   * @param expected the expected text
   * @return the assertion itself
   */
  public S hasText(String expected) {
    String text = capability(HasText.class).getText();
    if (!Objects.equals(text, expected)) {
      failWithMessage("Expected %s to have text <%s> but it has <%s>.", componentName(), expected,
          text);
    }

    return myself;
  }

  /**
   * Asserts that the text of the component contains the given text.
   *
   * @param expected the text to look for
   * @return the assertion itself
   */
  public S hasTextContaining(String expected) {
    String text = capability(HasText.class).getText();
    if (text == null || !text.contains(expected)) {
      failWithMessage("Expected the text of %s to contain <%s> but it is <%s>.", componentName(),
          expected, text);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given HTML content.
   *
   * @param expected the expected HTML
   * @return the assertion itself
   */
  public S hasHtml(String expected) {
    String html = capability(HasHtml.class).getHtml();
    if (!Objects.equals(html, expected)) {
      failWithMessage("Expected %s to have the HTML <%s> but it has <%s>.", componentName(),
          expected, html);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given value.
   *
   * @param expected the expected value
   * @return the assertion itself
   */
  public S hasValue(Object expected) {
    Object value = capability(ValueAware.class).getValue();
    if (!Objects.equals(value, expected)) {
      failWithMessage("Expected %s to have value <%s> but it has <%s>.", componentName(), expected,
          value);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given label.
   *
   * @param expected the expected label
   * @return the assertion itself
   */
  public S hasLabel(String expected) {
    String label = capability(HasLabel.class).getLabel();
    if (!Objects.equals(label, expected)) {
      failWithMessage("Expected %s to have the label <%s> but it has <%s>.", componentName(),
          expected, label);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given placeholder.
   *
   * @param expected the expected placeholder
   * @return the assertion itself
   */
  public S hasPlaceholder(String expected) {
    String placeholder = capability(HasPlaceholder.class).getPlaceholder();
    if (!Objects.equals(placeholder, expected)) {
      failWithMessage("Expected %s to have the placeholder <%s> but it has <%s>.", componentName(),
          expected, placeholder);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given helper text.
   *
   * @param expected the expected helper text
   * @return the assertion itself
   */
  public S hasHelperText(String expected) {
    String helperText = capability(HasHelperText.class).getHelperText();
    if (!Objects.equals(helperText, expected)) {
      failWithMessage("Expected %s to have the helper text <%s> but it has <%s>.", componentName(),
          expected, helperText);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given tooltip text.
   *
   * @param expected the expected tooltip text
   * @return the assertion itself
   */
  public S hasTooltipText(String expected) {
    String tooltip = capability(HasTooltip.class).getTooltipText();
    if (!Objects.equals(tooltip, expected)) {
      failWithMessage("Expected %s to have the tooltip <%s> but it has <%s>.", componentName(),
          expected, tooltip);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given name.
   *
   * @param expected the expected name
   * @return the assertion itself
   */
  public S hasName(String expected) {
    isNotNull();
    if (!Objects.equals(actual.getName(), expected)) {
      failWithMessage("Expected %s to have name <%s> but it has <%s>.", componentName(), expected,
          actual.getName());
    }

    return myself;
  }

  /**
   * Asserts that the component has the given theme.
   *
   * @param expected the expected theme
   * @return the assertion itself
   */
  public S hasTheme(Object expected) {
    Object theme = capability(HasTheme.class).getTheme();
    if (!Objects.equals(theme, expected)) {
      failWithMessage("Expected %s to have the theme <%s> but it has <%s>.", componentName(),
          expected, theme);
    }

    return myself;
  }

  /**
   * Asserts that the component has the given expanse.
   *
   * @param expected the expected expanse
   * @return the assertion itself
   */
  public S hasExpanse(Object expected) {
    Object expanse = capability(HasExpanse.class).getExpanse();
    if (!Objects.equals(expanse, expected)) {
      failWithMessage("Expected %s to have the expanse <%s> but it has <%s>.", componentName(),
          expected, expanse);
    }

    return myself;
  }

  /**
   * Asserts that the given attribute of the component has the given value.
   *
   * @param attribute the attribute name
   * @param expected the expected value
   *
   * @return the assertion itself
   */
  public S hasAttribute(String attribute, String expected) {
    String value = capability(HasAttribute.class).getAttribute(attribute);
    if (!Objects.equals(value, expected)) {
      failWithMessage("Expected the attribute <%s> of %s to be <%s> but it is <%s>.", attribute,
          componentName(), expected, value);
    }

    return myself;
  }

  /**
   * Asserts that the given property of the component has the given value.
   *
   * @param property the property name
   * @param expected the expected value
   *
   * @return the assertion itself
   */
  public S hasProperty(String property, Object expected) {
    Object value = capability(HasProperty.class).getProperty(property);
    if (!Objects.equals(value, expected)) {
      failWithMessage("Expected the property <%s> of %s to be <%s> but it is <%s>.", property,
          componentName(), expected, value);
    }

    return myself;
  }

  /**
   * Asserts that the given style property of the component has the given value.
   *
   * @param property the style property
   * @param expected the expected value
   *
   * @return the assertion itself
   */
  public S hasStyle(String property, String expected) {
    String value = capability(HasStyle.class).getStyle(property);
    if (!Objects.equals(value, expected)) {
      failWithMessage("Expected the style <%s> of %s to be <%s> but it is <%s>.", property,
          componentName(), expected, value);
    }

    return myself;
  }

  /**
   * Asserts that the component contains the given child component.
   *
   * @param child the expected child
   * @return the assertion itself
   */
  public S hasComponent(Component child) {
    if (!capability(HasComponents.class).hasComponent(child)) {
      failWithMessage("Expected %s to contain %s but it does not.", componentName(),
          child.getClass().getSimpleName());
    }

    return myself;
  }

  /**
   * Asserts that the component contains the given number of child components.
   *
   * @param expected the expected number of children
   * @return the assertion itself
   */
  public S hasComponentCount(int expected) {
    int count = capability(HasComponents.class).getComponentCount();
    if (count != expected) {
      failWithMessage("Expected %s to contain <%s> components but it contains <%s>.",
          componentName(), expected, count);
    }

    return myself;
  }

  /**
   * Asserts that the repository of the component holds the given number of items.
   *
   * @param expected the expected number of items
   * @return the assertion itself
   */
  public S hasItemCount(int expected) {
    int count = capability(HasRepository.class).getRepository().size();
    if (count != expected) {
      failWithMessage("Expected %s to hold <%s> items but it holds <%s>.", componentName(),
          expected, count);
    }

    return myself;
  }

  /**
   * Asserts that the given item is selected in the component.
   *
   * @param item the expected selected item
   * @return the assertion itself
   */
  public S hasSelected(Object item) {
    isNotNull();
    if (actual instanceof ItemMultiSelectionAware<?> aware) {
      requireSelected(aware.getSelectedItems().contains(item), item, aware.getSelectedItems());
    } else {
      Object selected = capability(ItemSingleSelectionAware.class).getSelected();
      requireSelected(Objects.equals(selected, item), item, selected);
    }

    return myself;
  }

  /**
   * Asserts that the item at the given index is selected in the component.
   *
   * @param index the expected selected index
   * @return the assertion itself
   */
  public S hasSelectedIndex(int index) {
    isNotNull();
    if (actual instanceof IndexMultiSelectionAware aware) {
      requireSelected(aware.getSelectedIndices().contains(index), index,
          aware.getSelectedIndices());
    } else {
      int selected = capability(IndexSingleSelectionAware.class).getSelectedIndex();
      requireSelected(selected == index, index, selected);
    }

    return myself;
  }

  /**
   * Asserts that the item with the given key is selected in the component.
   *
   * @param key the expected selected key
   * @return the assertion itself
   */
  public S hasSelectedKey(Object key) {
    isNotNull();
    if (actual instanceof KeyMultiSelectionAware aware) {
      requireSelected(aware.getSelectedKeys().contains(key), key, aware.getSelectedKeys());
    } else {
      Object selected = capability(KeySingleSelectionAware.class).getSelectedKey();
      requireSelected(Objects.equals(selected, key), key, selected);
    }

    return myself;
  }

  /**
   * Asserts that nothing is selected in the component.
   *
   * @return the assertion itself
   */
  public S hasNoSelection() {
    isNotNull();
    Object selected = switch (actual) {
      case ItemMultiSelectionAware<?> aware -> toSelection(aware.getSelectedItems());
      case ItemSingleSelectionAware<?> aware -> aware.getSelected();
      case IndexMultiSelectionAware aware -> toSelection(aware.getSelectedIndices());
      case IndexSingleSelectionAware aware -> {
        int index = aware.getSelectedIndex();
        yield index == -1 ? null : index;
      }
      case KeyMultiSelectionAware aware -> toSelection(aware.getSelectedKeys());
      default -> capability(KeySingleSelectionAware.class).getSelectedKey();
    };

    if (selected != null) {
      failWithMessage("Expected %s to have no selection but it has <%s> selected.", componentName(),
          selected);
    }

    return myself;
  }

  /**
   * Asserts that the component tree below the component matches the stored snapshot named after the
   * running test method.
   *
   * @return the assertion itself
   */
  public S matchesSnapshot() {
    isNotNull();
    SnapshotStore.verify(actual, null, storedSnapshotOptions())
        .ifPresent(message -> failWithMessage("%s", message));

    return myself;
  }

  /**
   * Asserts that the component tree below the component matches the stored snapshot with the given
   * name.
   *
   * @param name the snapshot name
   * @return the assertion itself
   */
  public S matchesSnapshot(String name) {
    return matchesSnapshot(name, storedSnapshotOptions());
  }

  /**
   * Asserts that the component tree below the component matches the stored snapshot with the given
   * name, rendered with the given options.
   *
   * @param name the snapshot name
   * @param options the rendering options
   * @return the assertion itself
   */
  public S matchesSnapshot(String name, SnapshotOptions options) {
    Objects.requireNonNull(name, "The snapshot name cannot be null");
    Objects.requireNonNull(options, "The snapshot options cannot be null");
    isNotNull();
    SnapshotStore.verify(actual, name, options)
        .ifPresent(message -> failWithMessage("%s", message));

    return myself;
  }

  /**
   * Returns the component under test as the given contract, failing when it does not implement it.
   *
   * @param <C> the contract type
   * @param capability the contract
   *
   * @return the component as the contract
   */
  protected <C> C capability(Class<C> capability) {
    isNotNull();
    if (!capability.isInstance(actual)) {
      failWithMessage("%s does not support %s.", componentName(), capability.getSimpleName());
    }

    return capability.cast(actual);
  }

  /**
   * Fails the assertion with the given message.
   *
   * @param message the message, a format of {@link String#format(String, Object...)}
   * @param arguments the arguments of the format
   *
   * @throws AssertionError always
   */
  protected void failWithMessage(String message, Object... arguments) {
    throw new AssertionError(String.format(message, arguments));
  }

  /**
   * Returns the name of the component under test for failure messages.
   *
   * @return the component name
   */
  protected String componentName() {
    return actual.getClass().getSimpleName();
  }

  private void requireSelected(boolean selected, Object expected, Object current) {
    if (!selected) {
      failWithMessage("Expected %s to have <%s> selected but it has <%s> selected.",
          componentName(), expected,
          current instanceof List<?> ? current : String.valueOf(current));
    }
  }


  // A stored snapshot pins the whole picture, the state the client holds included.
  private static SnapshotOptions storedSnapshotOptions() {
    return new SnapshotOptions().setBrowserState(true).setHiddenComponents(true)
        .setComposites(true);
  }

  private static Object toSelection(List<?> selected) {
    return selected.isEmpty() ? null : selected;
  }

}
