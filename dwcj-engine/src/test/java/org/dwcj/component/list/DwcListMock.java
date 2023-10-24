package org.dwcj.component.list;

import org.dwcj.component.window.Window;

class DwcListMock extends DwcList<DwcListMock> {

  @Override
  protected void onCreate(Window window) {}

  @Override
  public DwcListMock deselect() {
    return getSelf();
  }
}
