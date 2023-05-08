package org.dwcj.component.checkbox;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.CheckedChangedEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

/**Checkbox tests. */
@ExtendWith(MockitoExtension.class)
class CheckBoxTest {

  @Mock
  MouseEnterEventSink mouseEnterEventSink;

  @Mock
  MouseExitEventSink mouseExitEventSink;

  @Mock
  RightMouseDownEventSink rightMouseDownEventSink;

  @Mock
  CheckedChangedEventSink checkedChangedEventSink;

  @Mock
  FocusEventSink focusEventSink;

  @Mock
  BlurEventSink blurEventSink;

  @Spy
  EventDispatcher dispatcher;
  
  @InjectMocks
  CheckBox component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Test
  @DisplayName("Test hasFocus")
  void testHasFocus() {
    Boolean returnedBoolean = component.hasFocus();
    assertNotNull(returnedBoolean);
  }

}
