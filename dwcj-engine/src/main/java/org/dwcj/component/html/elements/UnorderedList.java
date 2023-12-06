package org.dwcj.component.html.elements;

import org.dwcj.component.Component;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code ul} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul">HTML ul Tag</a>
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
@NodeName("ul")
public class UnorderedList extends HtmlComponentContainer<UnorderedList> {

  /**
   * Creates a new empty UnorderedList.
   */
  public UnorderedList() {
    super();
  }

  /**
   * Creates a new UnorderedList with the given text.
   *
   * @param text the text
   */
  public UnorderedList(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new UnorderedList with the given child components.
   *
   * @param components the child components
   */
  public UnorderedList(Component... components) {
    super(components);
  }
}
