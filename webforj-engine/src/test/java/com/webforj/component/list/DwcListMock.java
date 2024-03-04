package com.webforj.component.list;

import com.webforj.component.window.Window;

class DwcListMock extends DwcList<DwcListMock> {

  @Override
  protected void onCreate(Window window) {}

  @Override
  public DwcListMock deselect() {
    return getSelf();
  }
}
