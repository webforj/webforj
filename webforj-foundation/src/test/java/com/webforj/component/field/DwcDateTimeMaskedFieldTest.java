package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.startup.type.BBjException;
import java.util.Locale;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcDateTimeMaskedFieldTest {
  @Mock
  BBjInputD control;

  @InjectMocks
  DwcDateTimeMaskedFieldMock component;

  @Test
  void shouldSetGetPattern() throws BBjException {
    String expectedPattern = "[0-9]{3}";
    component.setPattern(expectedPattern);
    assertEquals(expectedPattern, component.getPattern());

    verify(control, times(1)).setProperty("pattern", expectedPattern);
    verify(control, times(0)).getProperty("pattern");
  }

  @Test
  void shouldSetGetLocale() throws BBjException {
    Locale expectedLocale = Locale.CANADA;
    component.setLocale(expectedLocale);
    assertEquals(expectedLocale, component.getLocale());

    verify(control, times(1)).setProperty("locale", expectedLocale.toString());
    verify(control, times(0)).getProperty("locale");
  }

  @Test
  void shouldSetGetAllowCustomValue() throws BBjException {
    component.setAllowCustomValue(true);
    assertEquals(true, component.isAllowCustomValue());

    verify(control, times(1)).setProperty("customValue", true);
    verify(control, times(0)).getProperty("customValue");
  }
}
