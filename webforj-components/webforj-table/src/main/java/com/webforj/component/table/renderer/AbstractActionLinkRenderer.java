package com.webforj.component.table.renderer;

import com.webforj.component.Theme;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconDefinition;

/**
 * Base class for renderers that display a clickable link with an optional icon.
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("a")
abstract class AbstractActionLinkRenderer<T> extends AbstractElementRenderer<T> {
  private IconDefinition<?> icon;
  private Theme iconTheme = Theme.PRIMARY;

  /**
   * Creates a new link renderer with the given icon.
   *
   * @param icon the icon to display before the link text
   */
  protected AbstractActionLinkRenderer(IconDefinition<?> icon) {
    super(null, null);
    this.icon = icon;
    applyIconTheme(icon);
  }

  /**
   * Sets the icon to display before the link text.
   *
   * @param icon the icon definition
   */
  public void setIcon(IconDefinition<?> icon) {
    this.icon = icon;
    applyIconTheme(icon);
    fireChangeEvent();
  }

  /**
   * Returns the icon displayed before the link text.
   *
   * @return the icon, or {@code null} if not set
   */
  public IconDefinition<?> getIcon() {
    return icon;
  }

  /**
   * Returns the href prefix for the link (e.g., {@code "mailto:"} or {@code "tel:"}).
   *
   * @return the href prefix
   */
  protected abstract String getHrefPrefix();

  /** {@inheritDoc} */
  @Override
  public String build() {
    setAttribute("href", "<%= '" + getHrefPrefix() + "' + cell.value %>", false);
    setAttribute("tabindex", "-1", false);
    setAttribute("style", "color:inherit;text-decoration:none", false);

    if (icon != null) {
      String iconHtml = "<dwc-icon name='" + icon.getName() + "' pool='" + icon.getPool()
          + "' theme='" + toAttributeValue(iconTheme) + "'></dwc-icon> ";
      String html = super.build();
      String content = getContent();
      String resolvedContent = content == null ? "<%= cell.value %>" : content;
      return html.replace(resolvedContent, iconHtml + resolvedContent);
    }

    return super.build();
  }

  private void applyIconTheme(IconDefinition<?> icon) {
    if (icon instanceof Icon iconInstance && iconInstance.getTheme() != null
        && iconInstance.getTheme() != Theme.DEFAULT) {
      iconTheme = iconInstance.getTheme();
    }
  }
}
