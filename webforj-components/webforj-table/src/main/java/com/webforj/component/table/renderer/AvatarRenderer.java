package com.webforj.component.table.renderer;

import com.webforj.component.avatar.AvatarExpanse;
import com.webforj.component.avatar.AvatarShape;
import com.webforj.component.avatar.AvatarTheme;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.icons.IconDefinition;
import com.webforj.utilities.Assets;

/**
 * A renderer that displays a {@code dwc-avatar} in a table cell.
 *
 * <p>
 * When {@code label} is not explicitly set, it defaults to the column's value function result, and
 * initials are derived automatically. The column should return a name.
 * </p>
 *
 * <pre>{@code
 * AvatarRenderer<MusicRecord> renderer = new AvatarRenderer<>();
 * renderer.setTheme(AvatarTheme.PRIMARY);
 * renderer.setIcon(TablerIcon.create("user"));
 *
 * table.addColumn("avatar", MusicRecord::getArtist).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-avatar")
public class AvatarRenderer<T> extends AbstractVoidElementRenderer<T> {
  private static final String LABEL_ATTR = "label";
  private static final String INITIALS_ATTR = "initials";
  private static final String EXPANSE_ATTR = "expanse";

  private String initials;
  private String label;
  private String src;
  private AvatarShape shape;
  private AvatarExpanse expanse;
  private AvatarTheme theme;
  private IconDefinition<?> icon;

  /**
   * Creates a new avatar renderer with the given label and initials.
   *
   * @param label the label for the avatar
   * @param initials the initials for the avatar
   */
  public AvatarRenderer(String label, String initials) {
    this(label);
    setInitials(initials);
  }

  /**
   * Creates a new avatar renderer with the given label.
   *
   * @param label the label for the avatar
   */
  public AvatarRenderer(String label) {
    this();
    setLabel(label);
  }

  /**
   * Creates a new avatar renderer.
   */
  public AvatarRenderer() {
    setContent("");
  }

  /**
   * Sets the initials to display when no image is available.
   *
   * @param initials the initials to display
   * @return this renderer
   */
  public AvatarRenderer<T> setInitials(String initials) {
    this.initials = initials;
    setAttribute(INITIALS_ATTR, initials);
    return this;
  }

  /**
   * Returns the initials for the avatar.
   *
   * @return the initials
   */
  public String getInitials() {
    return initials;
  }

  /**
   * Sets the accessible label for the avatar.
   *
   * @param label the accessible label
   * @return this renderer
   */
  public AvatarRenderer<T> setLabel(String label) {
    this.label = label;
    setAttribute(LABEL_ATTR, label);
    return this;
  }

  /**
   * Returns the accessible label.
   *
   * @return the accessible label
   */
  public String getLabel() {
    return label;
  }

  /**
   * Sets the image source URL for the avatar.
   *
   * <p>
   * If the URL begins with {@code context://}, it will be resolved as a context URL. If it starts
   * with {@code ws://}, it will be resolved as a web server URL.
   * </p>
   *
   * @param src the image source URL
   * @return this renderer
   */
  public AvatarRenderer<T> setSrc(String src) {
    this.src = src;
    if (src != null && src.contains("<%")) {
      setAttribute("src", src);
    } else {
      setAttribute("src", Assets.resolveImageSource(src));
    }
    return this;
  }

  /**
   * Returns the image source URL for the avatar.
   *
   * @return the image source URL
   */
  public String getSrc() {
    return src;
  }

  /**
   * Sets the shape of the avatar.
   *
   * @param shape the shape to set
   * @return this renderer
   */
  public AvatarRenderer<T> setShape(AvatarShape shape) {
    this.shape = shape;
    setAttribute("shape", shape);
    return this;
  }

  /**
   * Returns the shape of the avatar.
   *
   * @return the shape
   */
  public AvatarShape getShape() {
    return shape;
  }

  /**
   * Sets the expanse of the avatar.
   *
   * @param expanse the expanse to set
   * @return this renderer
   */
  public AvatarRenderer<T> setExpanse(AvatarExpanse expanse) {
    this.expanse = expanse;
    setAttribute(EXPANSE_ATTR, expanse);
    return this;
  }

  /**
   * Returns the expanse of the avatar.
   *
   * @return the expanse
   */
  public AvatarExpanse getExpanse() {
    return expanse;
  }

  /**
   * Sets the theme of the avatar.
   *
   * @param theme the theme to set
   * @return this renderer
   */
  public AvatarRenderer<T> setTheme(AvatarTheme theme) {
    this.theme = theme;
    setAttribute("theme", theme);
    return this;
  }

  /**
   * Returns the theme of the avatar.
   *
   * @return the theme
   */
  public AvatarTheme getTheme() {
    return theme;
  }

  /**
   * Sets an icon to display inside the avatar.
   *
   * @param icon the icon to display, or {@code null} to remove
   * @return this renderer
   */
  public AvatarRenderer<T> setIcon(IconDefinition<?> icon) {
    this.icon = icon;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the icon displayed inside the avatar.
   *
   * @return the icon, or {@code null} if not set
   */
  public IconDefinition<?> getIcon() {
    return icon;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    if (label == null) {
      setAttribute(LABEL_ATTR, "<%= cell.value %>", false);
      if (initials == null) {
        setAttribute(INITIALS_ATTR,
            "<%= cell.value.trim().split(' ').filter(s => s).map(s => s[0]).join('') %>", false);
      }
    } else if (initials == null) {
      setAttribute(INITIALS_ATTR, computeInitials(label), false);
    }

    setAttribute(EXPANSE_ATTR, "", false);

    if (icon != null) {
      String iconHtml =
          "<dwc-icon name='" + icon.getName() + "' pool='" + icon.getPool() + "'></dwc-icon>";
      String html = super.build();
      String closeTag = "</" + getNodeName() + ">";
      return html.replace(closeTag, iconHtml + closeTag);
    }

    return super.build();
  }

  private static String computeInitials(String label) {
    if (label == null || label.isBlank()) {
      return "";
    }

    String[] parts = label.trim().split(" ");
    StringBuilder result = new StringBuilder();
    for (String part : parts) {
      if (!part.isEmpty()) {
        result.append(part.charAt(0));
      }
    }

    return result.toString();
  }
}
