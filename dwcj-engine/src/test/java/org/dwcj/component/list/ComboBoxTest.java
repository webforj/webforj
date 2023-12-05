package org.dwcj.component.list;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.bbj.proxies.sysgui.BBjListEdit;
import org.dwcj.component.ReflectionUtils;
import org.dwcj.component.event.KeypressEvent;
import org.dwcj.component.event.ModifyEvent;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ComboBoxTest {

  @Mock
  BBjListEdit control;

  @InjectMocks
  ComboBox component = new ComboBox();

  @Test
  void shouldConfigureAllowCustomValue() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setAllowCustomValue(true);
    assertEquals(true, component.isAllowCustomValue());

    assertEquals(true, component.getProperty("customValue"));
  }

  @Test
  @DisplayName("adding/removing supported events")
  void addingRemovingSupportedEvents() {
    EventListener<ModifyEvent> modifyListener = event -> {
    };
    EventListener<KeypressEvent> keypressListener = event -> {
    };

    ListenerRegistration<ModifyEvent> r1 = component.onModify(modifyListener);
    ListenerRegistration<KeypressEvent> r2 = component.onKeypress(keypressListener);
    assertEquals(1, component.getEventListeners(ModifyEvent.class).size());
    assertEquals(1, component.getEventListeners(KeypressEvent.class).size());

    r1.remove();
    r2.remove();
    assertEquals(0, component.getEventListeners(ModifyEvent.class).size());
    assertEquals(0, component.getEventListeners(KeypressEvent.class).size());
  }
}
