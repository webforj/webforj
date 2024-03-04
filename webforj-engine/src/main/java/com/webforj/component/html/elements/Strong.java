package com.webforj.component.html.elements;

import com.webforj.component.Component;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code strong} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong">Html strong
 *      Tag</a>
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
@NodeName("strong")
public class Strong extends HtmlComponentContainer<Strong> {

  /**
   * Creates a new empty strong.
   */
  public Strong() {
    super();
  }

  /**
   * Creates a new strong with the given text.
   *
   * @param text the text
   */
  public Strong(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new strong with the given child components.
   *
   * @param components the child components
   */
  public Strong(Component... components) {
    super(components);
  }
}
