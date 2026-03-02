package com.webforj.component.table.renderer;

import com.webforj.component.badge.BadgeExpanse;
import com.webforj.component.badge.BadgeTheme;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.icons.IconDefinition;
import java.awt.Color;

/**
 * A renderer that displays the column's value function result inside a {@code dwc-badge} component.
 *
 * <pre>{@code
 * BadgeRenderer<MusicRecord> renderer = new BadgeRenderer<>();
 * renderer.setTheme(BadgeTheme.PRIMARY);
 *
 * table.addColumn("musicType", MusicRecord::getMusicType).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-badge")
public class BadgeRenderer<T> extends AbstractElementRenderer<T> {
  private static final String EXPANSE_ATTR = "expanse";
  private static final String STYLE_ATTR = "style";

  private BadgeTheme theme;
  private BadgeExpanse expanse;
  private Color color;
  private IconDefinition<?> icon;

  /**
   * Creates a new badge renderer with the given content and theme.
   *
   * @param content the content of the badge
   * @param theme the theme to set
   */
  public BadgeRenderer(String content, BadgeTheme theme) {
    this(content);
    setTheme(theme);
  }

  /**
   * Creates a new badge renderer with the given content.
   *
   * @param content the content of the badge
   */
  public BadgeRenderer(String content) {
    super(content, null);
  }

  /**
   * Creates a new badge renderer.
   */
  public BadgeRenderer() {
    this(null);
  }

  /**
   * Sets the theme of the badge.
   *
   * @param theme the theme to set
   * @return this renderer
   */
  public BadgeRenderer<T> setTheme(BadgeTheme theme) {
    this.theme = theme;
    setAttribute("theme", theme);
    return this;
  }

  /**
   * Returns the theme of the badge.
   *
   * @return the theme
   */
  public BadgeTheme getTheme() {
    return theme;
  }

  /**
   * Sets the expanse of the badge.
   *
   * @param expanse the expanse to set
   * @return this renderer
   */
  public BadgeRenderer<T> setExpanse(BadgeExpanse expanse) {
    this.expanse = expanse;
    setAttribute(EXPANSE_ATTR, expanse);
    return this;
  }

  /**
   * Returns the expanse of the badge.
   *
   * @return the expanse
   */
  public BadgeExpanse getExpanse() {
    return expanse;
  }

  /**
   * Sets a custom color seed for the badge.
   *
   * @param color the color seed, or {@code null} to remove
   * @return this renderer
   */
  public BadgeRenderer<T> setColor(Color color) {
    this.color = color;
    if (color != null) {
      setAttribute(STYLE_ATTR, "--dwc-badge-seed:" + toHex(color));
    } else {
      removeAttribute(STYLE_ATTR);
    }
    return this;
  }

  /**
   * Returns the custom color seed.
   *
   * @return the color seed, or {@code null} if not set
   */
  public Color getColor() {
    return color;
  }

  /**
   * Sets an icon to display inside the badge.
   *
   * @param icon the icon to display, or {@code null} to remove
   * @return this renderer
   */
  public BadgeRenderer<T> setIcon(IconDefinition<?> icon) {
    this.icon = icon;
    return this;
  }

  /**
   * Returns the icon displayed inside the badge.
   *
   * @return the icon, or {@code null} if not set
   */
  public IconDefinition<?> getIcon() {
    return icon;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    setAttribute(EXPANSE_ATTR, "", false);

    if (icon != null) {
      String iconHtml =
          "<dwc-icon name='" + icon.getName() + "' pool='" + icon.getPool() + "'></dwc-icon> ";
      String html = super.build();
      String content = getContent();
      String resolvedContent = content == null ? "<%= cell.value %>" : content;
      return html.replace(resolvedContent, iconHtml + resolvedContent);
    }

    return super.build();
  }

  private static String toHex(Color color) {
    return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
  }
}
