package com.webforj.component.table.renderer;

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
public class EmailRenderer<T> extends AbstractActionLinkRenderer<T> {

  /**
   * Creates a new email renderer with the given icon.
   *
   * @param icon the icon to display before the email address
   */
  public EmailRenderer(IconDefinition<?> icon) {
    super(icon);
  }

  /**
   * Creates a new email renderer with the default {@code mail} icon.
   */
  public EmailRenderer() {
    super(TablerIcon.create("mail"));
  }

  /** {@inheritDoc} */
  @Override
  protected String getHrefPrefix() {
    return "mailto:";
  }
}
