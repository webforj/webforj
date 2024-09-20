package com.webforj.conceiver;

import com.webforj.component.Component;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

class EventWithTwoArgs extends ComponentEvent<Component> {
  public EventWithTwoArgs(Component source, Map<String, Object> data) {
    super(source, data);
  }
}
