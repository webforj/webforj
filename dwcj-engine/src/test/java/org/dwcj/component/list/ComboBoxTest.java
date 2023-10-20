package org.dwcj.component.list;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.bbj.proxies.sysgui.BBjListEdit;
import org.dwcj.component.ReflectionUtils;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.event.KeypressEvent;
import org.dwcj.component.event.ModifyEvent;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
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

    assertEquals(true, component.getProperty("custom-value"));
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    @DisplayName("adding/removing supported events")
    void addingRemovingSupportedEvents() {
      ComponentEventListener<ModifyEvent> modifyListener = event -> {
      };
      ComponentEventListener<KeypressEvent> keypressListener = event -> {
      };

      component.onModify(modifyListener);
      component.onKeypress(keypressListener);
      assertEquals(1, component.getEventListeners(ModifyEvent.class).size());
      assertEquals(1, component.getEventListeners(KeypressEvent.class).size());

      component.removeModifyListener(modifyListener);
      component.removeKeypressListener(keypressListener);
      assertEquals(0, component.getEventListeners(ModifyEvent.class).size());
      assertEquals(0, component.getEventListeners(KeypressEvent.class).size());
    }
  }
}
