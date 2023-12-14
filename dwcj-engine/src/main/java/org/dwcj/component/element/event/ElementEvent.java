package org.dwcj.component.element.event;

import java.util.Map;
import org.dwcj.component.Component;
import org.dwcj.component.element.Element;
import org.dwcj.component.event.ComponentEvent;

/**
 * Represents an event associated with an {@link Element} component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class ElementEvent extends ComponentEvent<Element> {

  private final String type;
  private final int id;
  private final transient ElementEventOptions options;

  /**
   * Creates a new ElementEvent instance.
   *
   * @param component The component associated with the event.
   * @param payload A map containing additional data associated with the event.
   * @param type The type of the event.
   * @param options The event options.
   */
  public ElementEvent(Element component, Map<String, Object> payload, String type, int id,
      ElementEventOptions options) {
    super(component, payload);
    this.type = type;
    this.id = id;
    this.options = options;
  }

  /**
   * Get the event id.
   *
   * @return The event id.
   */
  public int getId() {
    return id;
  }

  /**
   * Get the type of the event.
   *
   * @return The event type as a string.
   */
  public String getType() {
    return type;
  }

  /**
   * Get the event options.
   *
   * @return The event options.
   */
  public ElementEventOptions getOptions() {
    return options;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element getComponent() {
    return (Element) super.getComponent();
  }
}
