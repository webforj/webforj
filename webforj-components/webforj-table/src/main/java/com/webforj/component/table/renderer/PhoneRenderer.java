package com.webforj.component.table.renderer;

import com.webforj.component.icons.IconDefinition;
import com.webforj.component.icons.TablerIcon;

/**
 * A renderer that displays a phone number as a clickable {@code tel:} link in a table cell.
 *
 * <p>
 * Clicking the link opens the user's default phone dialer.
 * </p>
 *
 * <pre>{@code
 * PhoneRenderer<Contact> renderer = new PhoneRenderer<>();
 * table.addColumn("phone", Contact::getPhone).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class PhoneRenderer<T> extends AbstractActionLinkRenderer<T> {

  /**
   * Creates a new phone renderer with the given icon.
   *
   * @param icon the icon to display before the phone number
   */
  public PhoneRenderer(IconDefinition<?> icon) {
    super(icon);
  }

  /**
   * Creates a new phone renderer with the default {@code phone} icon.
   */
  public PhoneRenderer() {
    super(TablerIcon.create("phone"));
  }

  /** {@inheritDoc} */
  @Override
  protected String getHrefPrefix() {
    return "tel:";
  }
}
