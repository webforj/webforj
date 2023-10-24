package org.dwcj.concern.legacy;


/**
 * Interface which facilitates controls that implement it the ability to set the text of the
 * control, if applicable, in a way that makes sense for the specific control itself.
 *
 * @deprecated Use {@link HasText} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasText {

  /**
   * get the text property of the control.
   *
   * @return the text
   */
  public String getText();

  /**
   * set the text of the control Each control implementing this interface has a text property, which
   * might be visible in different ways (caption, title, contents of edit) or sometimes not visible
   * at all.
   *
   * @param text the text to set
   * @return the control itself
   */
  public LegacyHasText setText(String text);
}
