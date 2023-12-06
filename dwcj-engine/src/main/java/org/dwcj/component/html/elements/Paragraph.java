package org.dwcj.component.html.elements;

import org.dwcj.component.Component;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code p} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p">HTML p Tag</a>
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
@NodeName("p")
public class Paragraph extends HtmlComponentContainer<Paragraph> {

  /**
   * Creates a new empty Paragraph.
   */
  public Paragraph() {
    super();
  }

  /**
   * Creates a new Paragraph with the given text.
   *
   * @param text the text
   */
  public Paragraph(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new Paragraph with the given child components.
   *
   * @param components the child components
   */
  public Paragraph(Component... components) {
    super(components);
  }
}
