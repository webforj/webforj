package com.webforj.component;

import com.webforj.component.window.Window;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

public class DwcValidatableComponentMock
    extends DwcValidatableComponent<DwcValidatableComponentMock, String> {

  @Override
  public ListenerRegistration<ValueChangeEvent<String>> addValueChangeListener(
      EventListener<ValueChangeEvent<String>> listener) {
    throw new UnsupportedOperationException("Unimplemented method 'addValueChangeListener'");
  }

  @Override
  protected void onCreate(Window window) {
    throw new UnsupportedOperationException("Unimplemented method 'onCreate'");
  }
}
