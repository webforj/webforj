package org.dwcj.component.html.elements;

import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.component.html.HtmlComponent;

/**
 * Component representing a {@code hr} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr">HTML hr Tag</a>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@NodeName("hr")
public class Break extends HtmlComponent<Break> {

  /**
   * Creates a new Break.
   */
  public Break() {
    super();
  }
}
