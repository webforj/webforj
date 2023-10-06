package org.dwcj.component.field;

public class AbstractDwcFieldMock extends AbstractDwcField<AbstractDwcFieldMock, String> {

  /**
   * {@inheritDoc}
   */
  @Override
  public String getValue() {
    return getText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractDwcFieldMock setValue(String value) {
    setText(value);
    return this;
  }
}
