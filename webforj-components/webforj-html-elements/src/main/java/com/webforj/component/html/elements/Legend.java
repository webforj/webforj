package com.webforj.component.html.elements;

import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponent;

/**
 * Component representing a {@code legend} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend">HTML legend
 *      Tag</a>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@NodeName("legend")
public class Legend extends HtmlComponent<Legend> {

  /**
   * Creates a new legend.
   */
  public Legend() {
    super();
  }

  /**
   * Creates a new legend with the given text.
   *
   * @param text the text
   */
  public Legend(String text) {
    this();
    setText(text);
  }
}
