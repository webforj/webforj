package com.webforj.component.html.elements;

import com.webforj.component.Component;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code h1} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1">HTML h1 Tag</a>
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
@NodeName("h1")
public class H1 extends HtmlComponentContainer<H1> {

  /**
   * Creates a new empty heading.
   */
  public H1() {
    super();
  }

  /**
   * Creates a new heading with the given text.
   *
   * @param text the text
   */
  public H1(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new heading with the given child components.
   *
   * @param components the child components
   */
  public H1(Component... components) {
    super(components);
  }
}
