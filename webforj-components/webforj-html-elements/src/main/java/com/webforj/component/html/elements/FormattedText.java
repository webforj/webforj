package com.webforj.component.html.elements;

import com.webforj.component.Component;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code pre} element. This component represents preformatted text which
 * will be presented exactly as written.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre">HTML pre Tag</a>
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
@NodeName("pre")
public class FormattedText extends HtmlComponentContainer<FormattedText> {

  /**
   * Creates a new empty FormattedText.
   */
  public FormattedText() {
    super();
  }

  /**
   * Creates a new FormattedText with the given text.
   *
   * @param text the text
   */
  public FormattedText(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new FormattedText with the given child components.
   *
   * @param components the child components
   */
  public FormattedText(Component... components) {
    super(components);
  }
}
