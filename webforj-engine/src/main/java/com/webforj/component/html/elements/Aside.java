package com.webforj.component.html.elements;

import com.webforj.component.Component;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code aside} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside">HTML aside Tag</a>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
@NodeName("aside")
public class Aside extends HtmlComponentContainer<Aside> {

  /**
   * Creates a new empty aside.
   */
  public Aside() {
    super();
  }

  /**
   * Creates a new aside with the given text.
   *
   * @param text the text
   */
  public Aside(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new aside with the given child components.
   *
   * @param components the child components
   */
  public Aside(Component... components) {
    super(components);
  }
}
