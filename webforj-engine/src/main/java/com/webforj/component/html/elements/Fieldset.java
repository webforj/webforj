package com.webforj.component.html.elements;

import com.webforj.component.Component;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code fieldset} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset">Html fieldset
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
@NodeName("fieldset")
public class Fieldset extends HtmlComponentContainer<Fieldset> {

  /**
   * Creates a new empty fieldset.
   */
  public Fieldset() {
    super();
  }

  /**
   * Creates a new fieldset with the given text as {@link Legend}.
   *
   * @param text the legend's text
   */
  public Fieldset(String text) {
    this();
    add(new Legend(text));
  }

  /**
   * Creates a new fieldset with the given child components.
   *
   * @param components the child components
   */
  public Fieldset(Component... components) {
    super(components);
  }
}
