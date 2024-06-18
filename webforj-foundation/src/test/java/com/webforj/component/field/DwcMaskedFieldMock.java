package com.webforj.component.field;

import com.webforj.component.window.Window;

public class DwcMaskedFieldMock extends DwcMaskedField<DwcMaskedFieldMock, String> {

  @Override
  protected String convertValue(String value) {
    return value;
  }

  @Override
  protected void onCreate(Window window) {
    throw new UnsupportedOperationException("Unimplemented method 'onCreate'");
  }

  @Override
  public String getMaskedValue() {
    return null;
  }
}
