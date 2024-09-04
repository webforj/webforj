package com.webforj.component.icons;

import com.webforj.component.element.Element;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.event.BlurEvent;
import com.webforj.component.event.FocusEvent;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasFocus;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * An icon button is an selectable SVG image that represents a capability, or some other concept or
 * specific entity with meaning for the user.
 *
 * <p>
 * webforJ does not bundle an icon library by default. but it configures several icon pools that the
 * user can choose from. Then the icons will be loaded from a CDN on demand.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see Icon
 * @see DwcIcon
 * @see FeatherIcon
 * @see FontAwesomeIcon
 * @see TablerIcon
 */
@NodeName("dwc-icon-button")
public class IconButton extends Icon implements HasEnablement<Icon>, HasFocus<IconButton> {

  // Properties
  private final PropertyDescriptor<Boolean> disabledProp =
      PropertyDescriptor.property("disabled", true);
  private final PropertyDescriptor<Boolean> focusRingEnabledProp =
      PropertyDescriptor.property("focusVisible", false);
  private final PropertyDescriptor<Integer> tabTraversableProp =
      PropertyDescriptor.property("tabTraversable", 0);

  /**
   * Creates a new icon button.
   *
   * @param name the icon name
   * @param pool the icon pool
   */
  public IconButton(String name, String pool) {
    super(name, pool);
  }

  /**
   * Creates a new icon button from the given icon.
   *
   * @param icon the icon
   */
  public IconButton(Icon icon) {
    super(icon.getName(), icon.getPool());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public IconButton setEnabled(boolean enabled) {
    set(disabledProp, !enabled);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return !get(disabledProp);
  }

  /**
   * When true, The button will show the focus ring when it is focused by keyboard.
   *
   * @param enabled true to show the focus ring
   * @return the component itself
   */
  public IconButton setFocusRingEnabled(boolean enabled) {
    set(focusRingEnabledProp, enabled);
    return this;
  }

  /**
   * Gets the focus ring visibility.
   *
   * @return true if the focus ring is visible
   */
  public boolean isFocusRingEnabled() {
    return get(focusRingEnabledProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public IconButton focus() {
    getOriginalElement().focus();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isFocusable() {
    return get(tabTraversableProp) > 0;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public IconButton setFocusable(boolean focusable) {
    set(tabTraversableProp, focusable ? 1 : 0);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<FocusEvent> addFocusListener(EventListener<FocusEvent> listener) {
    return getOriginalElement().addFocusListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<BlurEvent> addBlurListener(EventListener<BlurEvent> listener) {
    return getOriginalElement().addBlurListener(listener);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
