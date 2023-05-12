package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.startup.type.BBjException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.mocks.CustomObjectMock;
import org.dwcj.mocks.DwcComponentMock;
import org.dwcj.mocks.DwcHelperMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

/** Test for the AbstractSink. */
@ExtendWith(MockitoExtension.class)
class AbstractSinkTest {

  @InjectMocks
  AbstractSinkMock sink;

  @Mock
  DwcComponentMock component;

  @Spy
  EventDispatcher dispatcher;

  @Mock
  BBjStaticText control;

  @Mock
  DwcHelperMock dwcjHelper;

  @BeforeEach
  void setUp() {
    sink.setHelper(dwcjHelper);
  }

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(sink, "control", null, true);
  }

  @Test
  void isControlWhenControlisNull() throws IllegalAccessException {
    nullifyControl();
    assertEquals(false, sink.isControl());
  }

  @Test
  void isControl() throws IllegalAccessException {
    assertEquals(true, sink.isControl());
  }

  @Test
  void setCallback() throws IllegalAccessException, BBjException {
    FieldUtils.writeField(sink, "dwcjHelper", dwcjHelper, true);

    when(dwcjHelper.getEventProxy(any(), eq("handleEvent"))).thenReturn(new CustomObjectMock());

    sink.setCallback();

    verify(control, times(1)).setCallback(eq(0), any(), eq("onEvent"));
  }

  @Test
  void removeCallback() throws IllegalAccessException, BBjException {
    FieldUtils.writeField(sink, "control", control, true);
    sink.removeCallback();

    verify(control, times(1)).clearCallback(0);
  }
}
