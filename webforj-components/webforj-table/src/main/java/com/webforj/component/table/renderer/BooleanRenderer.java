package com.webforj.component.table.renderer;

import com.webforj.component.Theme;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconDefinition;
import com.webforj.component.icons.TablerIcon;

/**
 * A renderer that displays boolean values as configurable icons for true, false, and null states.
 * The column should return a {@code Boolean} value.
 *
 * <pre>{@code
 * BooleanRenderer<MusicRecord> renderer = new BooleanRenderer<>();
 * renderer.setTrueIcon(TablerIcon.create("thumb-up").setTheme(Theme.SUCCESS));
 * renderer.setFalseIcon(TablerIcon.create("thumb-down").setTheme(Theme.DANGER));
 *
 * table.addColumn("inStock", r -> r.getOnHand() > 0).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class BooleanRenderer<T> extends Renderer<T> {
  private IconDefinition<?> trueIcon = TablerIcon.create("circle-check").setTheme(Theme.SUCCESS);
  private IconDefinition<?> falseIcon = TablerIcon.create("circle-x").setTheme(Theme.DANGER);
  private IconDefinition<?> nullIcon = TablerIcon.create("circle-minus").setTheme(Theme.DEFAULT);
  private boolean showNull = false;

  /**
   * Creates a new boolean renderer with the given true and false icons.
   *
   * @param trueIcon the icon for true values
   * @param falseIcon the icon for false values
   */
  public BooleanRenderer(IconDefinition<?> trueIcon, IconDefinition<?> falseIcon) {
    setTrueIcon(trueIcon);
    setFalseIcon(falseIcon);
  }

  /**
   * Creates a new boolean renderer.
   */
  public BooleanRenderer() {
    // default
  }

  /**
   * Sets the icon displayed for true values.
   *
   * @param trueIcon the icon definition
   * @return this renderer
   */
  public BooleanRenderer<T> setTrueIcon(IconDefinition<?> trueIcon) {
    this.trueIcon = trueIcon;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the icon for true values.
   *
   * @return the icon definition
   */
  public IconDefinition<?> getTrueIcon() {
    return trueIcon;
  }

  /**
   * Sets the icon displayed for false values.
   *
   * @param falseIcon the icon definition
   * @return this renderer
   */
  public BooleanRenderer<T> setFalseIcon(IconDefinition<?> falseIcon) {
    this.falseIcon = falseIcon;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the icon for false values.
   *
   * @return the icon definition
   */
  public IconDefinition<?> getFalseIcon() {
    return falseIcon;
  }

  /**
   * Sets the icon displayed for null values.
   *
   * @param nullIcon the icon definition
   * @return this renderer
   */
  public BooleanRenderer<T> setNullIcon(IconDefinition<?> nullIcon) {
    this.nullIcon = nullIcon;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the icon for null values.
   *
   * @return the icon definition
   */
  public IconDefinition<?> getNullIcon() {
    return nullIcon;
  }

  /**
   * Sets whether to show an icon for null values.
   *
   * @param showNull true to show the null icon
   * @return this renderer
   */
  public BooleanRenderer<T> setShowNull(boolean showNull) {
    this.showNull = showNull;
    fireChangeEvent();
    return this;
  }

  /**
   * Checks whether the null icon is shown.
   *
   * @return true if the null icon is shown
   */
  public boolean isShowNull() {
    return showNull;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    StringBuilder sb = new StringBuilder();
    sb.append("<% if (cell.value === true || cell.value === 'true') { %>");
    sb.append(buildIconHtml(trueIcon));
    sb.append("<% } else if (cell.value === false || cell.value === 'false') { %>");
    sb.append(buildIconHtml(falseIcon));
    if (showNull) {
      sb.append("<% } else { %>");
      sb.append(buildIconHtml(nullIcon));
    }
    sb.append("<% } %>");
    return sb.toString();
  }

  private static String buildIconHtml(IconDefinition<?> icon) {
    String themeAttr = "";
    if (icon instanceof Icon iconInstance && iconInstance.getTheme() != null) {
      themeAttr = " theme='" + toAttributeValue(iconInstance.getTheme()) + "'";
    }

    return "<dwc-icon name='" + icon.getName() + "' pool='" + icon.getPool() + "'" + themeAttr
        + "></dwc-icon>";
  }
}
