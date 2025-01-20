package com.webforj.component.icons;

import com.webforj.component.Theme;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.concern.HasElementClickListener;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasLabel;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasTooltip;
import com.webforj.concern.HasVisibility;


/**
 * An icon is a nonselectable SVG image that represents an application, a capability, or some other
 * concept or specific entity with meaning for the user.
 *
 * <p>
 * webforJ does not bundle an icon library by default. but it configures several icon pools that the
 * user can choose from. Then the icons will be loaded from a CDN on demand.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see IconButton
 * @see DwcIcon
 * @see FeatherIcon
 * @see FontAwesomeIcon
 * @see TablerIcon
 */
@NodeName("dwc-icon")
public class Icon extends ElementComposite implements HasStyle<Icon>, HasClassName<Icon>,
    HasVisibility<Icon>, HasLabel<Icon>, HasElementClickListener<Icon>, HasAttribute<Icon>,
    HasTooltip<Icon>, HasSize<Icon>, HasExpanse<Icon, IconExpanse> {

  // Properties
  private final PropertyDescriptor<String> nameProp = PropertyDescriptor.property("name", "");
  private final PropertyDescriptor<String> poolProp = PropertyDescriptor.property("pool", "");
  private final PropertyDescriptor<Theme> themeProp =
      PropertyDescriptor.property("theme", Theme.DEFAULT);
  private final PropertyDescriptor<IconExpanse> expanseProp =
      PropertyDescriptor.property("expanse", IconExpanse.XSMALL);
  private final PropertyDescriptor<String> labelProp = PropertyDescriptor.property("label", "");

  /**
   * Creates a new icon.
   *
   * @param name the icon name
   * @param pool the icon pool
   */
  public Icon(String name, String pool) {
    setName(name);
    setPool(pool);
  }

  /**
   * Sets the icon name.
   *
   * @param name the icon name
   * @return the component itself
   */
  @Override
  public Icon setName(String name) {
    super.setName(name);
    set(nameProp, name);
    return this;
  }

  /**
   * Gets the icon name.
   *
   * @return the icon name
   */
  @Override
  public String getName() {
    return get(nameProp);
  }

  /**
   * Sets the icon pool.
   *
   * @param pool the icon pool
   * @return the component itself
   */
  public Icon setPool(String pool) {
    set(poolProp, pool);
    return this;
  }

  /**
   * Gets the icon pool.
   *
   * @return the icon pool
   */
  public String getPool() {
    return get(poolProp);
  }

  /**
   * Sets the icon theme.
   *
   * @param theme the icon theme
   * @return the component itself
   */
  public Icon setTheme(Theme theme) {
    set(themeProp, theme);
    return this;
  }

  /**
   * Gets the icon theme.
   *
   * @return the icon theme
   */
  public Theme getTheme() {
    return get(themeProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Icon setExpanse(IconExpanse expanse) {
    set(expanseProp, expanse);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public IconExpanse getExpanse() {
    return get(expanseProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Icon setLabel(String label) {
    set(labelProp, label);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getLabel() {
    return get(labelProp);
  }
}
