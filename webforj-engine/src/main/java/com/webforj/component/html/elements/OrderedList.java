package com.webforj.component.html.elements;

import com.webforj.component.Component;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code ol} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol">HTML ol Tag</a>
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
@NodeName("ol")
public class OrderedList extends HtmlComponentContainer<OrderedList> {

  /**
   * Creates a new empty OrderedList.
   */
  public OrderedList() {
    super();
  }

  /**
   * Creates a new OrderedList with the given text.
   *
   * @param text the text
   */
  public OrderedList(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new OrderedList with the given child components.
   *
   * @param components the child components
   */
  public OrderedList(Component... components) {
    super(components);
  }
}
