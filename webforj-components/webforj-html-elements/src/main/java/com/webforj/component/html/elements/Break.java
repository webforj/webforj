package com.webforj.component.html.elements;

import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponent;

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
