package com.webforj.component.field;

public class DwcFieldMock extends DwcFieldInitializer<DwcFieldMock, String> {

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
  public DwcFieldMock setValue(String value) {
    setText(value);
    return this;
  }

  @Override
  protected String convertValue(String value) {
    return value;
  }
}
