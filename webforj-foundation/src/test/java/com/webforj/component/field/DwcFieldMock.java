package com.webforj.component.field;

import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;

class DwcFieldMock extends DwcFieldInitializer<DwcFieldMock, String> {

  public DwcFieldMock(String label, String value, String placeholder) {
    super(label, value, placeholder);
  }

  public DwcFieldMock(String label, String value,
      EventListener<ValueChangeEvent<String>> listener) {
    super(label, value, listener);
  }

  public DwcFieldMock(String label, String value) {
    super(label, value);
  }

  public DwcFieldMock(String label, EventListener<ValueChangeEvent<String>> listener) {
    super(label, listener);
  }

  public DwcFieldMock(EventListener<ValueChangeEvent<String>> listener) {
    super(listener);
  }

  public DwcFieldMock(String label) {
    super(label);
  }

  public DwcFieldMock() {
    super();
  }

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
