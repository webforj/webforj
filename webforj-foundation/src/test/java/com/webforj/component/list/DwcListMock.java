package com.webforj.component.list;

import com.webforj.component.window.Window;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

class DwcListMock extends DwcList<DwcListMock, ListItem> {

  @Override
  protected void onCreate(Window window) {
    // no-op
  }

  @Override
  public DwcListMock deselect() {
    return getSelf();
  }

  @Override
  public ListenerRegistration<ValueChangeEvent<ListItem>> addValueChangeListener(
      EventListener<ValueChangeEvent<ListItem>> listener) {
    return null;
  }
}
