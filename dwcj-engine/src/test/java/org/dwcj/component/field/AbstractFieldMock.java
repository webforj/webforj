package org.dwcj.component.field;

public class AbstractFieldMock extends AbstractField<AbstractFieldMock, String> {

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
  public AbstractFieldMock setValue(String value) {
    setText(value);
    return this;
  }
}
