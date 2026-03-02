package com.webforj.component.table.renderer;

import com.webforj.component.Theme;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconDefinition;
import com.webforj.component.icons.TablerIcon;

/**
 * A renderer that displays an email address as a clickable {@code mailto:} link in a table cell.
 *
 * <p>
 * Clicking the link opens the user's default email client.
 * </p>
 *
 * <pre>{@code
 * EmailRenderer<Contact> renderer = new EmailRenderer<>();
 * table.addColumn("email", Contact::getEmail).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("a")
public class EmailRenderer<T> extends AbstractElementRenderer<T> {
  private IconDefinition<?> icon = TablerIcon.create("mail");
  private Theme iconTheme = Theme.PRIMARY;

  /**
   * Creates a new email renderer with the given icon.
   *
   * @param icon the icon to display before the email address
   */
  public EmailRenderer(IconDefinition<?> icon) {
    super(null, null);
    this.icon = icon;
    applyIconTheme(icon);
  }

  /**
   * Creates a new email renderer with the default {@code mail} icon.
   */
  public EmailRenderer() {
    super(null, null);
  }

  /**
   * Sets the icon to display before the email address.
   *
   * @param icon the icon definition
   * @return this renderer
   */
  public EmailRenderer<T> setIcon(IconDefinition<?> icon) {
    this.icon = icon;
    applyIconTheme(icon);
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the icon displayed before the email address.
   *
   * @return the icon, or {@code null} if not set
   */
  public IconDefinition<?> getIcon() {
    return icon;
  }

  /**
   * Sets the name of the icon.
   *
   * @param name the icon name
   * @return this renderer
   */
  public EmailRenderer<T> setName(String name) {
    if (icon != null) {
      icon.setName(name);
      fireChangeEvent();
    }

    return this;
  }

  /**
   * Returns the name of the icon.
   *
   * @return the icon name, or {@code null} if no icon is set
   */
  public String getName() {
    return icon != null ? icon.getName() : null;
  }

  /**
   * Sets the pool of the icon.
   *
   * @param pool the icon pool
   * @return this renderer
   */
  public EmailRenderer<T> setPool(String pool) {
    if (icon != null) {
      icon.setPool(pool);
      fireChangeEvent();
    }

    return this;
  }

  /**
   * Returns the pool of the icon.
   *
   * @return the icon pool, or {@code null} if no icon is set
   */
  public String getPool() {
    return icon != null ? icon.getPool() : null;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    setAttribute("href", "<%= 'mailto:' + cell.value %>", false);
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
    if (icon instanceof Icon iconInstance && iconInstance.getTheme() != null) {
      iconTheme = iconInstance.getTheme();
    }
  }
}
