package com.webforj.component.table.renderer;

import com.webforj.component.Theme;
import com.webforj.component.element.annotation.NodeName;
import java.util.EnumSet;
import java.util.Set;

/**
 * A renderer that displays the column's value function result as styled text with optional color
 * and text decorations.
 *
 * <pre>{@code
 * TextRenderer<MusicRecord> renderer = new TextRenderer<>();
 * renderer.setTheme(Theme.PRIMARY);
 * renderer.setDecorations(EnumSet.of(TextDecoration.BOLD));
 *
 * table.addColumn("title", MusicRecord::getTitle).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("span")
public class TextRenderer<T> extends AbstractElementRenderer<T> {
  /**
   * Text decoration options.
   */
  public enum TextDecoration {
    /** Bold text. */
    BOLD("font-weight:bold"),

    /** Italic text. */
    ITALIC("font-style:italic"),

    /** Underlined text. */
    UNDERLINE("text-decoration:underline"),

    /** Strikethrough text. */
    STRIKETHROUGH("text-decoration:line-through"),

    /** Uppercase text. */
    UPPERCASE("text-transform:uppercase"),

    /** Lowercase text. */
    LOWERCASE("text-transform:lowercase"),

    /** Capitalize first letter of each word. */
    CAPITALIZE("text-transform:capitalize");

    private final String css;

    TextDecoration(String css) {
      this.css = css;
    }

    String getCss() {
      return css;
    }
  }

  private static final String STYLE_ATTR = "style";

  private Theme theme;
  private int shade = -1;
  private final EnumSet<TextDecoration> decorations = EnumSet.noneOf(TextDecoration.class);

  /**
   * Creates a new text renderer with the given content and theme.
   *
   * @param content the content to display
   * @param theme the theme
   */
  public TextRenderer(String content, Theme theme) {
    this(content);
    setTheme(theme);
  }

  /**
   * Creates a new text renderer with the given content.
   *
   * @param content the content to display
   */
  public TextRenderer(String content) {
    super(content, null);
  }

  /**
   * Creates a new text renderer.
   */
  public TextRenderer() {
    this(null);
  }

  /**
   * Sets the theme.
   *
   * @param theme the theme
   * @return this renderer
   */
  public TextRenderer<T> setTheme(Theme theme) {
    this.theme = theme;
    updateStyle();
    return this;
  }

  /**
   * Returns the theme.
   *
   * @return the theme
   */
  public Theme getTheme() {
    return theme;
  }

  /**
   * Sets the shade level for fine color control.
   *
   * <p>
   * Valid range is 5 to 95 in steps of 5. Set to -1 to use the base theme color without a shade.
   * </p>
   *
   * @param shade the shade level (5-95) or -1 for the base color
   * @return this renderer
   */
  public TextRenderer<T> setShade(int shade) {
    this.shade = shade;
    updateStyle();
    return this;
  }

  /**
   * Returns the shade level.
   *
   * @return the shade level or -1 if using base color
   */
  public int getShade() {
    return shade;
  }

  /**
   * Sets the text decorations.
   *
   * @param decorations the decorations to set
   * @return this renderer
   */
  public TextRenderer<T> setDecorations(Set<TextDecoration> decorations) {
    this.decorations.clear();
    this.decorations.addAll(decorations);
    updateStyle();
    return this;
  }

  /**
   * Returns the current text decorations.
   *
   * @return an unmodifiable set of active decorations
   */
  public Set<TextDecoration> getDecorations() {
    return Set.copyOf(decorations);
  }

  private void updateStyle() {
    StringBuilder style = new StringBuilder();

    if (theme != null) {
      String paletteName = toAttributeValue(theme);
      if (shade > 0) {
        style.append("color:var(--dwc-color-").append(paletteName).append("-").append(shade)
            .append(")");
      } else {
        style.append("color:var(--dwc-color-").append(paletteName).append(")");
      }
    }

    for (TextDecoration d : decorations) {
      if (!style.isEmpty()) {
        style.append(";");
      }
      style.append(d.getCss());
    }

    if (!style.isEmpty()) {
      setAttribute(STYLE_ATTR, style.toString());
    } else {
      removeAttribute(STYLE_ATTR);
    }
  }
}
