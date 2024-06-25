package com.webforj.component.field;

import com.webforj.component.window.Window;

class DwcDateTimeMaskedFieldMock
    extends DwcDateTimeMaskedField<DwcDateTimeMaskedFieldMock, String> {

  @Override
  public String getMaskedValue() {
    throw new UnsupportedOperationException("Unimplemented method 'getMaskedValue'");
  }

  @Override
  protected String convertValue(String value) {
    throw new UnsupportedOperationException("Unimplemented method 'convertValue'");
  }

  @Override
  protected void onCreate(Window window) {
    throw new UnsupportedOperationException("Unimplemented method 'onCreate'");
  }
}
