package org.dwcj.component.html.elements;

import org.dwcj.component.Component;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code footer} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer">HTML footer
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
@NodeName("footer")
public class Footer extends HtmlComponentContainer<Footer> {

  /**
   * Creates a new empty footer.
   */
  public Footer() {
    super();
  }

  /**
   * Creates a new footer with the given text.
   *
   * @param text the text
   */
  public Footer(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new footer with the given child components.
   *
   * @param components the child components
   */
  public Footer(Component... components) {
    super(components);
  }
}
